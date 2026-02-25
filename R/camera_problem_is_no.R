#' Identify Negative Camera Problem Answers
#'
#' @description
#' Returns `TRUE` when normalized answers indicate a negative problem flag
#' (`No`, `Não`, `No sé`, `Não sei`).
#'
#' @param x Character vector with camera-problem labels.
#'
#' @return Logical vector.
#' @export
camera_problem_is_no <- function(x) {
  normalize_camera_problem(x) %in% c("no", "nao", "no se", "nao sei")
}
