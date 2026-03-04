#' Validate Media Source
#'
#' @description
#' Validates if `source` is one of the accepted media source values.
#'
#' @param source Media source to process.
#' @param allowed Character vector of accepted source values.
#'
#' @return Invisible `TRUE` when validation passes.
#' @export
validate_source <- function(source, allowed = c("ct", "under", "over")) {
  if (!source %in% allowed) {
    cli::cli_abort(
      "Source {source} is not an accepted value. Accepted values are 'ct', 'under', 'over'"
    )
  }

  invisible(TRUE)
}

#' Load Source Data for Photo Matching
#'
#' @description
#' Maps a media source (`ct`, `under`, `over`) to its expected worksheet name
#' and loads all datasets from Excel files.
#'
#' @param source Source code to load.
#' @param path Directory containing source Excel files.
#' @param na Values to treat as missing.
#'
#' @return A named list of tibbles returned by [read_sheet()].
#' @export
load_source_data <- function(source, path = "Example/12", na = c("NA", "-")) {
  source <- as.character(source)

  sheet_name <- dplyr::recode_values(
    source,
    "ct" ~ "Camera_trap",
    "under" ~ "Underpasses",
    "over" ~ "Overpasses",
    default = NA_character_
  )

  if (is.na(sheet_name)) {
    cli::cli_abort("Source {source} is not mapped to a sheet.")
  }

  read_sheet(
    path = path,
    recurse = FALSE,
    sheet = sheet_name,
    na = na
  )
}

#' Ensure Dataset Names Match Media Inventory
#'
#' @description
#' Ensures dataset names in loaded spreadsheets and media inventory are aligned.
#'
#' @param datasets Named list of spreadsheet tables.
#' @param media_list Named list of media inventories by dataset.
#' @param first_take If `TRUE`, aborts when names differ.
#'
#' @return Invisible `TRUE` when check passes.
#' @export
ensure_dataset_names_match <- function(
    datasets,
    media_list,
    first_take = TRUE
) {
  datasets_not_in_common <- dplyr::setdiff(names(datasets), names(media_list))

  if (first_take && length(datasets_not_in_common) != 0) {
    cli::cli_abort(
      "There are different datasets between media folder and Excel sheets."
    )
  }

  invisible(TRUE)
}

#' Extract Expected Filenames From Sheet
#'
#' @description
#' Extracts distinct, non-missing expected filenames for a given source.
#'
#' @param df Dataset table loaded from spreadsheet.
#' @param source Source identifier (`ct`, `under`, `over`).
#'
#' @return Character vector of expected filenames.
#' @export
extract_filenames_on_sheet <- function(df, source) {
  column <- if (source == "ct") "Camera_vision_photo" else "Structure_photo"
  col_sym <- rlang::sym(column)

  df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(column))) |>
    dplyr::filter(!is.na(!!col_sym)) |>
    dplyr::pull(!!col_sym)
}

#' Filter Media Candidates by Source
#'
#' @description
#' Removes media already placed in the source-specific destination folder.
#'
#' @param dataset_media Tibble of media records for one dataset.
#' @param source Source identifier (`ct`, `under`, `over`).
#'
#' @return Filtered tibble with media candidates.
#' @export
media_candidates <- function(dataset_media, source) {
  dataset_media |>
    dplyr::filter(!stringr::str_detect(value, glue::glue("\\/{source}\\/")))
}

#' Build String Distance Table
#'
#' @description
#' Computes Levenshtein distances between expected filenames and media
#' candidates.
#'
#' @param filenames_on_sheet Character vector of expected filenames.
#' @param media Character vector of media filenames.
#'
#' @return Tibble with distance matrix in wide format and `match_exactly`.
#' @export
stringdist_table <- function(filenames_on_sheet, media) {
  stringdist::stringdistmatrix(
    filenames_on_sheet,
    media,
    method = "lv",
    useNames = "strings"
  ) |>
    as.data.frame() |>
    tibble::rownames_to_column("sheet") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      match_exactly = dplyr::if_any(dplyr::where(is.numeric), ~ . == 0)
    )
}

#' Build Match Candidates
#'
#' @description
#' Converts distance table to long format and computes helper matching flags.
#'
#' @param df_stringdist Tibble returned by `stringdist_table()`.
#'
#' @return Tibble with candidate matches and helper flags.
#' @export
build_match_candidates <- function(df_stringdist) {
  df_stringdist |>
    tidyr::pivot_longer(
      cols = -c(sheet, match_exactly),
      names_to = "file",
      values_to = "stringdist"
    ) |>
    dplyr::mutate(
      match_file_no_extension = sheet == tools::file_path_sans_ext(file),
      match_file_diff_capitalization = stringr::str_to_upper(sheet) ==
        stringr::str_to_upper(file),
      match_partially = dplyr::if_any(
        dplyr::where(is.numeric),
        ~ dplyr::between(., 1, 5)
      )
    ) |>
    dplyr::relocate(match_exactly, .after = stringdist) |>
    dplyr::arrange(dplyr::desc(match_file_no_extension), file)
}

#' Remove Ambiguous or Already Processed Matches
#'
#' @description
#' Prunes low-confidence candidates and excludes already processed files.
#'
#' @param match_candidates Candidate table returned by `build_match_candidates()`.
#' @param files_to_exclude Optional filenames already copied in this run.
#' @param already_processed_files Optional filenames already present in the
#'   destination folder for the dataset/source.
#'
#' @return Filtered tibble of match candidates.
#' @export
dedupe_matches <- function(
    match_candidates,
    files_to_exclude = character(),
    already_processed_files = character()
) {
  filtered_candidates <- match_candidates |>
    dplyr::filter(!file %in% files_to_exclude)

  dup_sheet_file <- filtered_candidates |>
    dplyr::filter(
      match_file_no_extension == TRUE |
        match_file_diff_capitalization == TRUE
    ) |>
    dplyr::select(sheet, file)

  filtered_candidates |>
    dplyr::mutate(
      keep = dplyr::case_when(
        match_file_no_extension == FALSE &
          match_file_diff_capitalization == FALSE &
          sheet %in% dup_sheet_file$sheet ~
          "REMOVE",
        match_file_no_extension == FALSE &
          match_file_diff_capitalization == FALSE &
          file %in% dup_sheet_file$file ~
          "REMOVE",
        sheet %in% already_processed_files ~ "REMOVE",
        TRUE ~ "KEEP"
      )
    ) |>
    dplyr::filter(keep == "KEEP") |>
    dplyr::select(-keep)
}

#' Copy Exact Filename Matches
#'
#' @description
#' Copies exact matches to the target source folder and returns copied files.
#'
#' @param df_stringdist Distance table returned by `stringdist_table()`.
#' @param media_without_source Candidate media table.
#' @param target_dir Destination directory for copied files.
#'
#' @return Tibble with copied filenames and destination paths.
#' @export
copy_exact_matches <- function(
    df_stringdist,
    media_without_source,
    target_dir
) {
  media_match_exactly <- df_stringdist |>
    dplyr::filter(match_exactly == TRUE)

  if (nrow(media_match_exactly) == 0) {
    return(tibble::tibble(file = character(), full_path_to_copy = character()))
  }

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  files_found_in_sheet <- df_stringdist |>
    dplyr::filter(match_exactly == TRUE) |>
    dplyr::pull(sheet) |>
    tibble::enframe(value = "file") |>
    dplyr::inner_join(media_without_source, by = "file") |>
    dplyr::mutate(full_path_to_copy = glue::glue("{target_dir}/{file}"))

  file.copy(
    from = files_found_in_sheet$value,
    to = files_found_in_sheet$full_path_to_copy,
    overwrite = FALSE,
    copy.date = TRUE
  )

  files_found_in_sheet
}

#' Process One Dataset for One Source
#'
#' @description
#' Runs extraction, matching, exact-copy, and deduplication for a
#' dataset/source pair.
#'
#' @param df Dataset table for one dataset.
#' @param dataset_name Dataset identifier.
#' @param source Source identifier (`ct`, `under`, `over`).
#' @param media_list Named list of media inventories.
#' @param media_files_types_list Named nested list with files already under
#'   `ct`/`under`/`over` by dataset.
#' @param media_root Root path for destination media tree.
#'
#' @return Tibble of match candidates for manual review.
#' @export
process_dataset <- function(
    df,
    dataset_name,
    source,
    media_list,
    media_files_types_list = list(),
    media_root = "Example/Media"
) {
  cli::cli_alert_info("Processing dataset {dataset_name}.")

  filenames_on_sheet <- extract_filenames_on_sheet(df, source)

  if (length(filenames_on_sheet) == 0) {
    return(tibble::tibble())
  }

  dataset_media <- media_list[[dataset_name]]

  if (is.null(dataset_media) || nrow(dataset_media) == 0) {
    return(tibble::tibble())
  }

  media_without_source <- media_candidates(dataset_media, source)

  if (nrow(media_without_source) == 0) {
    return(tibble::tibble())
  }

  media <- media_without_source |>
    dplyr::distinct(file) |>
    dplyr::pull(file)

  df_stringdist <- stringdist_table(filenames_on_sheet, media)

  files_copied <- copy_exact_matches(
    df_stringdist,
    media_without_source,
    glue::glue("{media_root}/{dataset_name}/{source}")
  )

  already_processed_files <- purrr::pluck(
    media_files_types_list,
    dataset_name,
    source,
    "file",
    .default = character()
  )

  match_candidates <- build_match_candidates(df_stringdist)

  dedupe_matches(
    match_candidates,
    files_to_exclude = files_copied$file,
    already_processed_files = already_processed_files
  )
}

#' Check Media Matches for a Source
#'
#' @description
#' Orchestrates the matching workflow for a specific source across all datasets.
#'
#' @param source Source identifier (`ct`, `under`, `over`).
#' @param media_list Named list of media inventories by dataset.
#' @param media_files_types_list Named nested list with media already under
#'   destination source folders.
#' @param data_path Path containing source Excel files.
#' @param first_take If `TRUE`, enforces strict dataset-name consistency.
#' @param na Values to treat as missing while reading spreadsheets.
#' @param media_root Root path for destination media tree.
#'
#' @return Tibble with match candidates for the given source.
#' @export
check_match_media <- function(
    source = NULL,
    media_list,
    media_files_types_list = list(),
    data_path = "Example/12",
    first_take = TRUE,
    na = c("NA", "-"),
    media_root = "Example/Media"
) {
  validate_source(source)
  cli::cli_alert("Starting source {source}")

  datasets <- load_source_data(source, path = data_path, na = na)
  ensure_dataset_names_match(datasets, media_list, first_take = first_take)

  datasets_with_content <- datasets |>
    purrr::keep(~ nrow(.x) > 0)

  res <- purrr::imap(
    datasets_with_content,
    ~ process_dataset(
      .x,
      .y,
      source,
      media_list,
      media_files_types_list = media_files_types_list,
      media_root = media_root
    )
  )

  res <- res |>
    purrr::keep(~ nrow(.x) > 0)

  if (length(res) == 0) {
    return(tibble::tibble())
  }

  res |>
    dplyr::bind_rows(.id = "dataset") |>
    dplyr::mutate(source = source, .after = dataset)
}

#' Save Partial Match Report
#'
#' @description
#' Saves partial (non-exact) candidate matches to an `.xlsx` file.
#'
#' @param result Named list returned by `run_check_match_media()`.
#' @param output_file Path to output workbook.
#'
#' @return Named list of partial matches grouped by dataset.
#' @export
save_partial_matches <- function(
    result,
    output_file = glue::glue(
      "Example/Output/12/check_names_photos_{lubridate::today()}.xlsx"
    )
) {
  partial_tbl <- result |>
    purrr::map(
      ~ {
        required_cols <- c("match_exactly", "match_partially")

        if (!all(required_cols %in% names(.x))) {
          return(tibble::tibble())
        }

        .x |>
          dplyr::filter(
            match_exactly == FALSE,
            match_partially == TRUE
          )
      }
    ) |>
    dplyr::bind_rows()

  if (nrow(partial_tbl) == 0) {
    empty_report <- tibble::tibble(message = "No partial matches found.")

    openxlsx2::write_xlsx(
      x = list(partial_matches = empty_report),
      file = output_file,
      as_table = TRUE,
      overwrite = TRUE
    )

    return(list())
  }

  partial_res <- partial_tbl |>
    tidyr::nest(.by = dataset) |>
    dplyr::arrange(dataset) |>
    dplyr::mutate(data = purrr::set_names(data, dataset)) |>
    dplyr::pull(data)

  openxlsx2::write_xlsx(
    x = partial_res,
    file = output_file,
    as_table = TRUE,
    overwrite = TRUE
  )

  partial_res
}

#' Remove Root-Level Media Already Matched Exactly
#'
#' @description
#' Removes files from root media folders when exact matches were copied into
#' source-specific folders.
#'
#' @param result List returned by `run_check_match_media()`.
#' @param media_tbl Media inventory tibble.
#' @param sources Character vector of processed sources.
#'
#' @return Logical vector from `file.remove()`.
#' @export
cleanup_media_root <- function(result, media_tbl, sources) {
  sources_regex <- glue::glue_collapse(sources, "|")

  files_to_delete <- result |>
    purrr::map(
      ~ .x |>
        dplyr::filter(
          match_exactly == TRUE
        ) |>
        dplyr::distinct(sheet, .keep_all = TRUE) |>
        dplyr::select(dataset, file = sheet)
    ) |>
    dplyr::bind_rows() |>
    dplyr::inner_join(media_tbl, by = c("dataset", "file")) |>
    dplyr::filter(
      !stringr::str_detect(value, glue::glue("\\/{sources_regex}\\/"))
    ) |>
    dplyr::pull(value)

  file.remove(files_to_delete)
}

#' Run Full Media Matching Workflow
#'
#' @description
#' Runs media matching for all sources, optionally cleans root media files,
#' writes partial-match workbook, and returns partial results.
#'
#' @param media_list Named list of media inventories by dataset.
#' @param media_files Media inventory tibble used by cleanup.
#' @param media_files_types_list Named nested list with media already under
#'   destination source folders.
#' @param sources Character vector of sources to process.
#' @param data_path Path containing source Excel files.
#' @param output_file Path to output workbook.
#' @param first_take If `TRUE`, enforces strict dataset-name consistency.
#' @param cleanup If `TRUE`, removes root-level files already matched exactly.
#' @param na Values to treat as missing while reading spreadsheets.
#' @param media_root Root path for destination media tree.
#'
#' @return Named list of partial matches by dataset.
#' @export
run_check_match_media <- function(
    media_list,
    media_files,
    media_files_types_list = list(),
    sources = c("ct", "under", "over"),
    data_path = "Example/12",
    output_file = glue::glue(
      "Example/Output/12/check_names_photos_{lubridate::today()}.xlsx"
    ),
    first_take = TRUE,
    cleanup = FALSE,
    na = c("NA", "-"),
    media_root = "Example/Media"
) {
  result <- purrr::map(purrr::set_names(sources), function(source) {
    check_match_media(
      source = source,
      media_list = media_list,
      media_files_types_list = media_files_types_list,
      data_path = data_path,
      first_take = first_take,
      na = na,
      media_root = media_root
    )
  })

  if (cleanup == TRUE) {
    cleanup_media_root(result, media_files, sources)
  }

  save_partial_matches(result, output_file = output_file)
}
