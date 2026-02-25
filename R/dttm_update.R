#' Merge Date and Time Columns into Datetime
#'
#' @description
#' Updates a date/datetime column using hour, minute and second values extracted
#' from another time column.
#'
#' @param x A data frame.
#' @param date_col Name of the date/datetime column (character).
#' @param time_col Name of the time column (character).
#'
#' @return The updated data frame.
#' @export
dttm_update <- function(x, date_col, time_col) {
  date_sym <- rlang::sym(date_col)
  time_sym <- rlang::sym(time_col)

  x |>
    dplyr::mutate(
      !!date_sym := lubridate:::update_datetime(
        !!date_sym,
        hour = lubridate::hour(!!time_sym),
        minute = lubridate::minute(!!time_sym),
        second = lubridate::second(!!time_sym)
      )
    )
}
