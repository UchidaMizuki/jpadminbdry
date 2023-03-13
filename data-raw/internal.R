source("data-raw/setup.R")

# internal ----------------------------------------------------------------

admin_boundary_data <- read_rds("data-raw/admin_boundary/data.rds")
admin_boundary_geom <- read_rds("data-raw/admin_boundary/geom.rds")

usethis::use_data(admin_boundary_data, admin_boundary_geom,
                  internal = TRUE,
                  overwrite = TRUE)
