#' Create Unique Camera IDs Within Structures
#'
#' @description
#' Appends alphabetical suffixes to duplicated `Camera_id` values within each
#' `Structure_id`, preserving the original ID in `Camera_id_orig`.
#'
#' @param x A data frame containing at least `Structure_id` and `Camera_id`.
#' @param sep Separator between original ID and suffix.
#'
#' @return A data frame with updated `Camera_id`, plus helper metadata
#'   (`Camera_id_orig`, `double`).
#' @export
unique_id <- function(x, sep = "_") {
  x_with_id <- x |>
    tibble::rowid_to_column()

  x_with_id |>
    dplyr::group_by(Structure_id, Camera_id) |>
    dplyr::add_count(Structure_id, Camera_id, name = "double") |>
    dplyr::mutate(
      Camera_id_orig = Camera_id,
      Dup_id = dplyr::row_number(Camera_id),
      Dup_form_name = dplyr::if_else(
        condition = double == 1,
        true = Camera_id,
        false = stringr::str_c(Camera_id, sep, LETTERS[Dup_id])
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(rowid, Camera_id_orig, Dup_form_name, double) |>
    dplyr::left_join(x_with_id, ., by = "rowid") |>
    dplyr::mutate(
      Camera_id = dplyr::if_else(
        !is.na(Dup_form_name),
        Dup_form_name,
        Camera_id
      )
    ) |>
    dplyr::relocate(Camera_id_orig, .after = Camera_id) |>
    dplyr::select(-Dup_form_name, -rowid)
}
