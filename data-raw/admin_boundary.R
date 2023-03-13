source("data-raw/setup.R")

# admin_boundary ----------------------------------------------------------

url_admin_boundary <- "https://nlftp.mlit.go.jp/ksj/gml"

table_admin_boundary <- read_html(str_c(url_admin_boundary, "/datalist/KsjTmplt-N03-v3_1.html")) |>
  html_element("table.mb30.responsive-table")
table_admin_boundary <- table_admin_boundary |>
  html_table() |>
  rename(pref_name = 地域,
         geodetic_system = 測地系,
         year = 年度) |>
  select(pref_name, geodetic_system, year) |>
  add_column(url = table_admin_boundary |>
               html_elements("a") |>
               html_attr("onclick") |>
               str_extract("/data/[^']+") |>
               str_c(url_admin_boundary, ... = _)) |>
  filter(pref_name != "全国",
         geodetic_system == "世界測地系") |>
  select(!geodetic_system) |>
  separate_wider_regex(year,
                       patterns = c(era_name = "大正|昭和|平成|令和",
                                    year = "\\d+",
                                    "年")) |>
  mutate(year = as.integer(year),
         year = case_match(era_name,
                           "大正" ~ 1911L + year,
                           "昭和" ~ 1925L + year,
                           "平成" ~ 1988L + year,
                           "令和" ~ 2018L + year),
         .keep = "unused")

table_admin_boundary |>
  mutate(list(pref_name, year, url) |>
           pmap(\(pref_name, year, url) {
             exdir <- path("data-raw/admin_boundary/year", year, pref_name)
             dir_create(exdir)

             file <- dir_ls(exdir,
                            recurse = TRUE,
                            regexp = "shp$")
             if (vec_is_empty(file)) {
               destfile <- file_temp(ext = "zip")
               curl::curl_download(url, destfile)
               zip::unzip(destfile,
                          exdir = exdir)
             }
           },
           .progress = TRUE))

JGD2000 <- 4612
JGD2011 <- 6668

dir_ls("data-raw/admin_boundary/year",
       type = "directory") |>
  walk(\(dir) {
    inform(dir)
    file <- path(dir,
                 ext = "gpkg")
    if (!file_exists(file)) {
      admin_boundary <- dir_ls(dir,
                               recurse = TRUE,
                               regexp = "shp$") |>
        map(\(file) {
          admin_boundary <- read_sf(file,
                                    options = "ENCODING=CP932")
          if (is.na(st_crs(admin_boundary))) {
            st_crs(admin_boundary) <- JGD2000
          }
          admin_boundary
        },
        .progress = TRUE) |>
        list_rbind() |>
        st_as_sf() |>
        st_transform(JGD2011) |>
        rename(pref_name = N03_001,
               subpref_name = N03_002,
               county_name = N03_003,
               city_name = N03_004,
               city_code = N03_007) |>
        select(pref_name, subpref_name, county_name, city_name, city_code) |>
        filter(!is.na(city_code)) |>
        st_cast("POLYGON") |>
        group_by(city_code, pref_name, subpref_name, county_name, city_name) |>
        summarise(do_union = FALSE,
                  .groups = "drop")

      admin_boundary <- admin_boundary |>
        rmapshaper::ms_simplify(keep = 5e6 / as.double(lobstr::obj_size(admin_boundary)),
                                sys = TRUE)

      write_sf(admin_boundary, file)
      gc()
    }
  })

file_admin_boundary <- tibble(file = dir_ls("data-raw/admin_boundary/year",
                                            regexp = "gpkg$"),
                              year = file |>
                                str_extract("\\d{4}(?=\\.gpkg$)") |>
                                as.integer())

for (file in vec_chop(file_admin_boundary)) {
  year <- file$year
  cli::cli_inform("Year: {year}")

  if (!exists("admin_boundary_data")) {
    admin_boundary <- read_sf(file$file) |>
      rowid_to_column("id")
    admin_boundary_data <- list(admin_boundary |>
                                  st_drop_geometry() |>
                                  relocate(!id)) |>
      set_names(year)
    admin_boundary_geom <- admin_boundary |>
      select(id) |>
      mutate(area = st_area(geom) |>
               units::set_units(km^2))
  } else if (!year %in% names(admin_boundary_data)) {
    admin_boundary_new <- read_sf(file$file) |>
      rowid_to_column("id_new") |>
      mutate(area = st_area(geom) |>
               units::set_units(km^2))
    id <- admin_boundary_new |>
      select(id_new, area) |>
      rename(geom_new = geom,
             area_new = area) |>
      quietly(st_join)(admin_boundary_geom,
                       left = FALSE) |>
      pluck("result") |>
      filter(near(area_new, area,
                  tol = units::set_units(1e-2, km^2))) |>
      select(!c(area_new, area)) |>
      as_tibble() |>
      left_join(admin_boundary_geom |>
                  select(id) |>
                  as_tibble(),
                by = join_by(id)) |>
      mutate(distance = st_distance(geom_new, geom,
                                    by_element = TRUE,
                                    which = "Hausdorff"),
             .keep = "unused") |>
      filter(distance < units::set_units(1e-3, degree)) |>
      slice_min(distance, n = 1L) |>
      select(!distance)

    admin_boundary_new <- admin_boundary_new |>
      left_join(id,
                by = join_by(id_new)) |>
      select(!id_new)
    loc <- is.na(admin_boundary_new$id)
    vec_slice(admin_boundary_new$id, loc) <- vec_size(admin_boundary_geom) + seq_len(sum(loc))

    admin_boundary_data[[as.character(year)]] <- admin_boundary_new |>
      st_drop_geometry() |>
      select(!area)
    admin_boundary_geom <- bind_rows(admin_boundary_geom,
                                     vec_slice(admin_boundary_new, loc) |>
                                       select(id, area))
  }
}

usethis::use_data(admin_boundary_data, admin_boundary_geom,
                  overwrite = TRUE)
