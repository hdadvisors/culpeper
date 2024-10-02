## Setup ------------------------------

library(tidyverse)
library(janitor)
library(sf)
library(mapview)


## County boundary --------------------

county <- read_sf("gis/raw/county.shp")

write_rds(county, "gis/county.rds")


## Parcels (Development Analysis) -----

parcel_subdiv_csv <- read_csv("data/raw/parcel-data.csv") |> 
  select(
    id = 3,
    acres = 14,
    zone = 15,
    driveway = 30,
    val_total = 33,
    val_bldg = 35,
    use_code = 44,
    use_desc = 45,
    area_lvg = 49,
    year_built = 81,
    sale_year = 86,
    sale_price = 90
  )

parcel_subdiv_data <- read_sf("gis/raw/parcel.shp") |> 
  st_drop_geometry() |> 
  select(
    id = 4,
    parcel_desc = 17,
    ag_dist = 48,
    conservation = 49,
    subd = 53,
    block = 54,
    lot = 55,
    sublot = 56
  ) |> 
  left_join(parcel_subdiv_csv) |> 
  mutate(sale_year = as.integer(str_sub(sale_year, 1, 4))) |> 
  filter(!is.na(subd))

parcel_subdiv <- parcel_subdiv_data |> 
  filter(
    str_detect(use_desc, "SFD|Agricultural"),
    !str_detect(use_desc, "Comm")
  ) |> 
    mutate(
      dev = fct_case_when(
        str_detect(use_code, "R|H|T") ~ "Developed",
        str_detect(use_code, "V") ~ "Vacant"
      )
    ) |> 
    mutate(
      use = fct_case_when(
        str_detect(use_code, "100") ~ "Urban",
        str_detect(use_code, "200") ~ "Suburban",
        str_detect(use_code, "500|600") ~ "Agricultural"
      )
    ) |> 
    mutate(
      zone = case_when(
        zone %in% c("R1", "RA", "A1", "R3", "PUD", "R2") ~ zone,
        .default = "Other"
      )
    )

write_rds(parcel_subdiv, "data/parcel_subdiv.rds")


## Parcels (Parcel Analysis) ----------

parcel_csv <- read_csv("data/raw/parcel-data.csv") |> 
  select(
    id = 3,
    street = 92,
    city = 93,
    state = 94,
    zip = 95
    )

parcel_data <- read_sf("gis/raw/parcel.shp") |> 
  select(2:4, 11:12, 15, 17, 30:31, 47:49, 53:56) |> 
  clean_names() |> 
  rename(
    id = av_interna,
    desc = descriptio,
    conservation = conservati
  ) |> 
  left_join(parcel_csv) |> 
  select(1:16, 18:21, 19)

parcel <- parcel_data |> 
  mutate(geometry = st_make_valid(geometry))

write_rds(parcel, "gis/parcel.rds")


## Zoning -----------------------------

zoning <- read_sf("gis/raw/zoning.shp") |> 
  clean_names() |> 
  filter(zoning != "NO DATA") |> 
  select(2, 5)

write_rds(zoning, "gis/zoning.rds")


## Buildings --------------------------

buildings <- read_sf("gis/raw/buildings.shp") |> 
  clean_names() |> 
  select(1, 12)

write_rds(buildings, "gis/buildings.rds")


## Roads ------------------------------

roads <- read_sf("gis/raw/roads.shp") |> 
  clean_names() |> 
  select(-1) |> 
  mutate(
    road_type = case_match(
      road_type,
      0 ~ "Highway",
      1 ~ "Secondary",
      2 ~ "Private"
    )
  )

write_rds(roads, "gis/roads.rds")


## FEMA Floodplain --------------------

fema <- read_sf("gis/raw/fema.shp") |> 
  clean_names() |> 
  select(5, 26) |> 
  mutate(geometry = st_make_valid(geometry))

write_rds(fema, "gis/fema.rds")


## Water ------------------------------

water_buffer <- read_sf("gis/raw/water-buffer.shp") |> 
  clean_names() |> 
  select(2, 6)

water <- read_sf("gis/raw/water.shp") |> 
  clean_names() |> 
  filter(shape_area > 0.25*43560) |> # 1/4 acre
  select(1, 9)

write_rds(water, "gis/water.rds")


## Streams ----------------------------

streams <- read_sf("gis/raw/streams.shp") |> 
  clean_names() |> 
  select(1, 9)

write_rds(streams, "gis/streams.rds")

streams_deq <- read_sf("gis/raw/streams_deq.shp") |>
  st_transform(st_crs(county)) |> 
  clean_names() |> 
  st_filter(county) |> 
  
# streams_deq_buff <- st_buffer(streams_deq, dist = 5)
# 
# streams_deq_join <- st_union(streams_deq, water)

  
## Subdivisions -----------------------

subdivision <- read_sf("gis/raw/subdivision.shp") |> 
  clean_names() |> 
  select(1, 9)

write_rds(streams, "gis/streams.rds")


## Subdivision History ----------------

subdiv_hist <- read_csv("data/raw/subdivision-history.csv") |> 
  clean_names() |> 
  pivot_longer(
    cols = 2:11,
    names_to = "var",
    values_to = "val"
  ) |> 
  mutate(type = str_to_title(str_extract(var, "[^_]+$")), .after = 2) |> 
  mutate(var = str_extract(var, "^[^_]+")) |> 
  mutate(
    var = fct_case_when(
      var == "total" ~ "Total",
      var == "major" ~ "Major",
      var == "minor" ~ "Minor",
      var == "x10" ~ "10 Acre",
      var == "family" ~ "Family"
    )
  ) |> 
  pivot_wider(names_from = type, values_from = val) 

write_rds(subdiv_hist, "data/subdiv_hist.rds")


## MLS Export -------------------------

# Closed sales
# 01/01/1993 - 12/31/2023
# New construction only
# Filter where subdivision is named

mls <- read_csv("data/raw/mls-export.csv") |> 
  clean_names() |> 
  select(
    id = 1,
    type = 7,
    sqft = 8,
    9,
    13,
    15,
    acres = 21,
    22,
    subd = 26
  ) |> 
  filter(
    !subd %in% c("NONE AVAILABLE", "UNKNOWN")
  ) |> 
  mutate(
    close_date = mdy(close_date),
    close_price = as.numeric(str_remove_all(close_price, "[$,]"))
    ) |> 
  mutate(
    across(where(is.numeric), ~ na_if(.x, 0))
  )

write_rds(mls, "data/mls_subdiv.rds")

## Building Permits -------------------

cbps <- read_csv("data/raw/cbps.csv") |> 
  filter(GEOID == 51047)

write_rds(cbps, "data/cbps.rds")
