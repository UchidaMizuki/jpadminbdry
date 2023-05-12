#' @export
admin_boundary <- function(year) {
  year <- as.character(year)
  year <- arg_match(year, admin_boundary_year)

  admin_boundary_data[[year]] |>
    dplyr::left_join(admin_boundary_geom |>
                       dplyr::select(!"area"),
                     by = "id") |>
    dplyr::select(!"id") |>
    sf::st_as_sf()
}
