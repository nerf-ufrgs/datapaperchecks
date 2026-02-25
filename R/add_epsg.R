#' Assign EPSG Codes from Coordinate Metadata
#'
#' @description
#' Creates an `epsg` column based on `type`, `Datum`, `hemis`, and `zone`
#' combinations used in the datapaper templates.
#'
#' @param df A data frame containing coordinate system metadata.
#'
#' @return The input data frame with an integer `epsg` column.
#' @export
add_epsg <- function(df) {
  df |>
    dplyr::mutate(
      epsg = dplyr::case_when(
        type == "Geodetic" & Datum == "WGS84" ~ 4326L,
        type == "Geodetic" & Datum == "SIRGAS2000" ~ 4674L,
        type == "Geodetic" & Datum == "Corrego_Alegre" ~ 5524L,
        type == "Geodetic" & Datum == "SAD69" ~ 4618L,
        type == "Projected" & Datum == "WGS84" & hemis == "N" ~
          32600L + as.integer(zone),
        type == "Projected" & Datum == "WGS84" & hemis == "S" ~
          32700L + as.integer(zone),
        type == "Projected" & Datum == "SIRGAS2000" & hemis == "S" ~
          31960L + as.integer(zone),
        TRUE ~ NA_integer_
      )
    )
}
