#' Check Problem Intervals Against Installation Period
#'
#' @description
#' For each dataset table, dynamically identifies `ProblemX` date pairs,
#' computes intervals, and flags rows where problem intervals are invalid or
#' outside installation period boundaries.
#'
#' `check_problem` flags:
#' - `ProblemX (neg)`: `ProblemX_from >= ProblemX_to`
#' - `ProblemX`: interval starts before `Start_date` or ends after `End_date`
#'
#' Output columns are reordered by problem index:
#' `ProblemX_from`, `ProblemX_to`, `ProblemX_interval`, with `check_problem`
#' relocated after `install_period`.
#'
#' @param ct_list Named list of data frames containing camera-trap records.
#'
#' @return A named list of data frames with computed intervals and
#'   `check_problem`.
#' @export
check_problem_intervals <- function(ct_list) {
  sort_problem_labels <- function(labels) {
    if (length(labels) == 0) {
      return(character())
    }

    tibble::tibble(label = labels) |>
      dplyr::mutate(
        problem_n = stringr::str_extract(label, "[0-9]+") |>
          as.integer(),
        is_neg = stringr::str_detect(label, "\\(neg\\)$")
      ) |>
      dplyr::arrange(problem_n, is_neg, label) |>
      dplyr::pull(label)
  }

  order_problem_columns <- function(df) {
    problem_col_info <- tibble::tibble(col = names(df)) |>
      dplyr::mutate(
        match = stringr::str_match(
          col,
          "^Problem([0-9]+)_(from|to|interval)$"
        ),
        problem_n = match[, 2] |> as.integer(),
        problem_part = match[, 3]
      ) |>
      dplyr::filter(!is.na(problem_n)) |>
      dplyr::mutate(
        part_order = dplyr::recode_values(
          problem_part,
          "from" ~ 1L,
          "to" ~ 2L,
          "interval" ~ 3L,
          default = 99L
        )
      ) |>
      dplyr::arrange(problem_n, part_order, col)

    ordered_problem_cols <- problem_col_info$col
    other_cols <- setdiff(names(df), c(ordered_problem_cols, "check_problem"))

    df_out <- df |>
      dplyr::select(
        dplyr::all_of(other_cols),
        dplyr::all_of(ordered_problem_cols),
        dplyr::any_of("check_problem")
      )

    if (all(c("install_period", "check_problem") %in% names(df_out))) {
      df_out <- dplyr::relocate(df_out, check_problem, .after = install_period)
    }

    df_out
  }

  ct_list |>
    purrr::map(\(x) {
      problem_numbers <- names(x) |>
        stringr::str_extract("^Problem[0-9]+") |>
        stringr::str_remove("^Problem") |>
        as.integer() |>
        (\(v) v[!is.na(v)])()

      max_problem <- if (length(problem_numbers) == 0) 0L else max(problem_numbers)

      problem_ids <- if (max_problem == 0L) {
        character()
      } else {
        paste0("Problem", seq_len(max_problem))
      }

      problem_ids <- problem_ids[
        paste0(problem_ids, "_from") %in% names(x) &
          paste0(problem_ids, "_to") %in% names(x)
      ]

      interval_cols <- purrr::map(problem_ids, \(id) {
        problem_from_col <- paste0(id, "_from")
        problem_to_col <- paste0(id, "_to")
        interval_name <- paste0(id, "_interval")

        rlang::set_names(
          list(
            rlang::expr(
              dplyr::if_else(
                is.na(.data[[!!problem_from_col]]) |
                  is.na(.data[[!!problem_to_col]]),
                lubridate::interval(NA, NA),
                lubridate::interval(
                  .data[[!!problem_from_col]],
                  .data[[!!problem_to_col]]
                )
              )
            )
          ),
          interval_name
        )
      }) |>
        purrr::list_c()

      x |>
        dplyr::filter(
          camera_problem_is_yes(Camera_problem),
          dplyr::if_any(dplyr::starts_with("Problem"), ~ !is.na(.x))
        ) |>
        dplyr::mutate(
          install_period = lubridate::interval(Start_date, End_date),
          !!!interval_cols
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          check_problem = {
            row_data <- dplyr::pick(dplyr::everything())
            install_start <- lubridate::int_start(install_period)
            install_end <- lubridate::int_end(install_period)

            invalid_problems <- purrr::map_chr(problem_ids, \(problem_id) {
              problem_from <- row_data[[paste0(problem_id, "_from")]]
              problem_to <- row_data[[paste0(problem_id, "_to")]]

              if (anyNA(c(problem_from, problem_to))) {
                return(NA_character_)
              }

              if (isTRUE(problem_from >= problem_to)) {
                return(paste0(problem_id, " (neg)"))
              }

              if (anyNA(c(install_start, install_end))) {
                return(NA_character_)
              }

              starts_before_install <- isTRUE(problem_from < install_start)
              ends_after_install <- isTRUE(problem_to > install_end)

              if (starts_before_install || ends_after_install) {
                return(problem_id)
              }

              NA_character_
            }) |>
              (\(z) z[!is.na(z)])()

            if (length(invalid_problems) == 0) {
              NA_character_
            } else {
              invalid_problems |>
                sort_problem_labels() |>
                paste(collapse = ", ")
            }
          }
        ) |>
        dplyr::ungroup() |>
        (\(df) {
          problem_empty <- df |>
            dplyr::select(dplyr::starts_with("Problem")) |>
            purrr::map_lgl(\(col) all(is.na(col)))

          cols_drop <- names(problem_empty)[problem_empty]

          df |>
            dplyr::select(-dplyr::all_of(cols_drop)) |>
            order_problem_columns()
        })()
    }) |>
    purrr::discard(~ nrow(.x) == 0)
}
