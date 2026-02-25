#' Compute Distance to Nearest OSM Feature
#'
#' @description
#' Queries OpenStreetMap features around input points, finds the nearest line
#' feature for each point, and returns distances plus metadata.
#'
#' @param x An `sf` object with point geometries.
#' @param feature One of `"highway"`, `"railway"`, or `"man_made"`.
#' @param crs_metric Metric CRS used for distance calculations.
#' @param buffer Search buffer around points (meters in `crs_metric`).
#' @param thresh Threshold (meters) used to flag `out_thresh`.
#'
#' @return A list with class `nearest_osm_dist` containing:
#'   `data`, `bbox_buffer`, and `osm_lines`.
#' @export
calc_nearest_osm_dist <- function(
    x,
    feature = c("highway", "railway", "man_made"),
    crs_metric = 3857,
    buffer = 1000,
    thresh = 50
) {
  feature <- match.arg(feature)

  bb <- x |>
    sf::st_transform(crs_metric) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_buffer(buffer) |>
    sf::st_transform(4326) |>
    sf::st_bbox()

  osm_query <- osmdata::opq(bbox = bb) |>
    osmdata::add_osm_feature(
      key = feature,
      value = if (feature == "highway") {
        c(
          "motorway",
          "trunk",
          "primary",
          "secondary",
          "tertiary",
          "unclassified",
          "residential"
        )
      } else if (feature == "railway") {
        c("rail", "narrow_gauge", "disused", "abandoned")
      } else {
        c("pipeline", "goods_conveyor")
      }
    )

  osm_lines <- osm_query |>
    osmdata::osmdata_sf() |>
    purrr::pluck("osm_lines")

  if (is.null(osm_lines)) {
    cli::cli_alert("There are no features within the buffer")
  }

  osm_lines_sf <- osm_lines |>
    tibble::rownames_to_column("id_osm") |>
    tibble::rowid_to_column("rowid") |>
    sf::st_as_sf()

  pts_m <- sf::st_transform(x, crs_metric)
  lines_m <- sf::st_transform(osm_lines_sf, crs_metric)

  idx_nearest <- sf::st_nearest_feature(pts_m, lines_m)

  df <- x |>
    dplyr::mutate(idx_nearest = idx_nearest) |>
    dplyr::inner_join(
      osm_lines_sf |> sf::st_drop_geometry(),
      by = c("idx_nearest" = "rowid")
    )

  dists <- sf::st_distance(pts_m, lines_m[idx_nearest, ], by_element = TRUE)

  final_data <- df |>
    dplyr::mutate(
      distance_to = as.numeric(dists),
      out_thresh = distance_to > thresh
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "Dataset",
        "Infrastructure_type",
        "Structure_id",
        "id_osm",
        !!feature,
        "name",
        "source",
        "feature",
        "surface",
        "distance_to",
        "out_thresh"
      ))
    ) |>
    dplyr::rename(
      feature_type = !!feature
    )

  result <- list(
    data = final_data,
    bbox_buffer = bb |>
      sf::st_as_sfc() |>
      tibble::enframe("id", "geometry") |>
      sf::st_as_sf(),
    osm_lines = osm_lines_sf |>
      dplyr::filter(rowid %in% unique(idx_nearest))
  )
  class(result) <- "nearest_osm_dist"
  return(result)
}
