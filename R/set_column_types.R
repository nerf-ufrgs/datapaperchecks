#' Read Column Types Metadata
#'
#' @description
#' Reads column type definitions from `support/column_types.xlsx` for a given
#' sheet.
#'
#' @param sheet Character scalar with the sheet name in
#'   `support/column_types.xlsx`.
#'
#' @return A character vector of column types.
#' @export
set_column_types <- function(sheet = NULL) {
  readxl::read_excel(
    path = "support/column_types.xlsx",
    sheet = sheet
  ) |>
    dplyr::select(2) |>
    dplyr::pull()
}
