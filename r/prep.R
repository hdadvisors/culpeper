## Setup ------------------------------

library(tidyverse)
library(janitor)
library(sf)
library(mapview)

## County boundary --------------------

county <- read_sf("gis/raw/county.shp")

write_rds(parcel, "gis/county.rds")

## Parcels ----------------------------

parcel_data <- read_csv("data/raw/parcel-data.csv") |> 
  select(
    id = 3,
    street = 92,
    city = 93,
    state = 94,
    zip = 95
    )

parcel <- read_sf("gis/raw/parcel.shp") |> 
  select(2:4, 11:12, 15, 17, 30:31, 47:49, 53:56) |> 
  clean_names() |> 
  rename(
    id = av_interna,
    desc = descriptio,
    conservation = conservati
  ) |> 
  left_join(parcel_data) |> 
  select(1:16, 18:21, 19)

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
  filter(road_type != 2) |> 
  mutate(
    road_type = case_match(
      road_type,
      0 ~ "Highway",
      1 ~ "Secondary"
    )
  )

write_rds(roads, "gis/roads.rds")

## FEMA Floodplan ---------------------

fema <- read_sf("gis/raw/fema.shp") |> 
  clean_names() |> 
  select(5, 26)

write_rds(zoning, "gis/fema.rds")

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
