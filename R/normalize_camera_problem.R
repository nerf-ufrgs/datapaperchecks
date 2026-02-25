#' Normalize Camera Problem Labels
#'
#' @description
#' Normalizes free-text `Camera_problem` values by lowercasing, transliterating
#' accents, removing punctuation and squishing spaces.
#'
#' @param x Character vector with camera-problem labels.
#'
#' @return A normalized character vector.
#' @export
normalize_camera_problem <- function(x) {
  x_norm <- x |>
    as.character()

  x_norm[is.na(x_norm)] <- ""

  x_norm |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringi::stri_trans_general(id = "Latin-ASCII") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}
