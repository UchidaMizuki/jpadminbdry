source("data-raw/setup.R")

# internal ----------------------------------------------------------------

admin_boundary_data <- read_rds("data-raw/admin_boundary/data.rds")
admin_boundary_geom <- read_rds("data-raw/admin_boundary/geom.rds")
admin_boundary_year <- names2(admin_boundary_data)

usethis::use_data(admin_boundary_data, admin_boundary_geom, admin_boundary_year,
                  internal = TRUE,
                  overwrite = TRUE)
