#' Derive OSM Feature Type from Infrastructure Type
#'
#' @description
#' Maps `Infrastructure_type` values to a coarse OSM feature category
#' (`highway`, `railway`, or `man_made`).
#'
#' @param df A data frame containing `Infrastructure_type`.
#'
#' @return The input data frame with a new `feature` column.
#' @export
set_feature_from_infrastructure <- function(df) {
  result <- df |>
    dplyr::mutate(
      feature = dplyr::case_when(
        is.na(Infrastructure_type) ~ NA_character_,
        Infrastructure_type %in% c("Ducto", "Gasoduto") ~ "man_made",
        stringr::str_detect(Infrastructure_type, "Ferro") ~ "railway",
        TRUE ~ "highway"
      )
    )
  return(result)
}
