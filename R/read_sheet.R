#' Read Datapaper Excel Sheets
#'
#' @description
#' Reads all Excel files in a folder, applies project column conventions,
#' and returns a named list of tibbles (one per dataset).
#'
#' @param path Folder containing `.xlsx` files.
#' @param sheet Sheet name to read from each workbook.
#' @param na Values to treat as missing.
#' @param results If `FALSE`, returns only the named file paths.
#' @param set_col_types If `TRUE`, uses package-internal metadata via `set_column_types()`;
#'   otherwise reads all columns as text.
#' @param recurse If `TRUE`, searches recursively in `path`.
#'
#' @return A named list of data frames, or named file paths when
#'   `results = FALSE`.
#' @export
read_sheet <- function(
    path = "Excel",
    sheet = NULL,
    na = "",
    results = TRUE,
    set_col_types = TRUE,
    recurse = TRUE
) {
  excel <- list.files(
    path = path,
    pattern = "^\\w.+xlsx$",
    full.names = TRUE,
    recursive = recurse
  )

  names <- excel |>
    stringr::str_split("/|\\.") |>
    purrr::map_vec(\(x) dplyr::nth(x, -2))

  load <- excel |>
    purrr::set_names(names)

  if (!results) {
    return(load)
  }

  if (set_col_types) {
    column_types <- set_column_types(sheet = sheet)
  } else {
    column_types <- "text"
  }

  result <- load |>
    purrr::map(function(file) {
      df <- withCallingHandlers(
        readxl::read_xlsx(
          path = file,
          sheet = sheet,
          na = na,
          col_names = TRUE,
          col_types = column_types
        )
      ) |>
        janitor::remove_empty("rows")

      names(df) <- df |>
        janitor::clean_names() |>
        colnames() |>
        stringr::str_to_sentence()

      return(df)
    })

  return(result)
}
