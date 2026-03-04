#' Extract a Component from Safe/Quietly Results
#'
#' @description
#' Extracts a specific component from a list of outputs usually created by
#' wrappers such as `purrr::safely()` or `purrr::quietly()`, binding results
#' across datasets.
#'
#' @param x A named list where each element is itself a list containing
#'   a common component to extract.
#' @param component Character scalar with the component name to extract.
#'
#' @return A data frame created by row-binding the selected component across
#'   all list elements, with a `dataset` identifier column.
extract_issue <- function(x, component) {
  nms <- x |>
    purrr::map(names) |>
    purrr::reduce(union)

  trans <- x |>
    purrr::transpose(.names = nms)

  trans |>
    purrr::pluck(component) |>
    dplyr::bind_rows(.id = "dataset")
}
