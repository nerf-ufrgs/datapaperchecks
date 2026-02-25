#' Identify Positive Camera Problem Answers
#'
#' @description
#' Returns `TRUE` when normalized answers indicate a positive problem flag
#' (`Sí`, `Sim`, `Yes`).
#'
#' @param x Character vector with camera-problem labels.
#'
#' @return Logical vector.
#' @export
camera_problem_is_yes <- function(x) {
  normalize_camera_problem(x) %in% c("si", "sim", "yes")
}
