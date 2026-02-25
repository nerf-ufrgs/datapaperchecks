#' Read Column Types Metadata
#'
#' @description
#' Returns the predefined column type metadata stored internally in the package
#' for a given spreadsheet.
#'
#' @param sheet Character scalar with the target spreadsheet name (for example,
#'   `"Camera_trap"` or `"Underpasses"`).
#'
#' @return A character vector of column types.
#' @export
set_column_types <- function(sheet = NULL) {
  if (is.null(sheet) || length(sheet) != 1 || is.na(sheet) || sheet == "") {
    cli::cli_abort("Argument {.arg sheet} must be a single, non-empty sheet name.")
  }

  if (!sheet %in% names(.datapaper_column_types)) {
    cli::cli_abort(c(
      "Unknown sheet name: {.val {sheet}}.",
      "i" = "Available sheets: {.val {names(.datapaper_column_types)}}"
    ))
  }

  .datapaper_column_types[[sheet]]
}
