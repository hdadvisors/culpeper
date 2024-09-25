## Setup ------------------------------

library(tidyverse)
library(janitor)
library(sf)
library(mapview)

## Parcels ----------------------------

parcel <- read_rds("gis/parcel.rds")

fema <- read_rds("gis/fema.rds")

parcel_filter <- parcel |> 
  st_cast("MULTIPOLYGON") |> 
  filter(zoning %in% c("RA", "A1")) |> 
  filter(
    is.na(subd),
    is.na(conservation)
    ) |> 
  mutate(
    acres = as.numeric(acres),
    ag_district = case_when(
      !is.na(ag_district) ~ 1,
      .default = 0
      )
    ) |> 
  filter(acres >= 6) |> 
  st_difference(st_union(fema))

write_rds(parcel_filter, "gis/parcel_filter.rds")

## Current ----------------------------

current <- parcel_filter |> 
  st_drop_geometry() |>
  filter(
    (zoning == "A1" & acres >= 10) | (zoning == "RA" & acres >= 6)
  ) |> 
  mutate(
    potential_lots = 
      case_when(
        zoning == "A1" ~ floor(acres/5),
        zoning == "RA" ~ floor(acres/3)
    ),
    potential_lots = pmax(potential_lots - 1, 0)
  )

current_sum <- current |> 
  summarise(
    parcels = n(),
    lots = sum(potential_lots),
    .by = "zoning"
  ) |> 
  adorn_totals()

current |> 
  mutate(
    bin = fct_case_when(
      potential_lots == 1 ~ "1",
      potential_lots == 2 ~ "2",
      potential_lots == 3 ~ "3",
      potential_lots == 4 ~ "4",
      potential_lots == 5 ~ "5",
      potential_lots >= 6 & potential_lots <= 9 ~ "6-9",
      potential_lots >= 10 & potential_lots <= 19 ~ "10-19",
      potential_lots >= 20 ~ "20+"
    )
  ) |> 
  ggplot(aes(x = bin, fill = zoning)) +
  facet_wrap(~zoning) +
  geom_bar() +
  scale_fill_hda() +
  add_zero_line() +
  labs(
    title = "Most eligible parcels can create 1 or 2 new lots",
    subtitle = "Under current regulations",
    x = "New lots per parcel",
    y = "Parcels"
  ) +
  theme_hda() +
  theme(axis.title = element_text())


## Option 2 ---------------------------

opt2 <- parcel_filter |> 
  st_drop_geometry() |>
  filter(
    (zoning == "A1" & acres >= 20) | (zoning == "RA" & acres >= 12)
  ) |> 
  mutate(
    potential_lots = 
      case_when(
        zoning == "A1" ~ floor(acres/10),
        zoning == "RA" ~ floor(acres/6)
      ),
    potential_lots = pmax(potential_lots - 1, 0)
  )

opt2_sum <- opt2 |> 
  summarise(
    parcels = n(),
    lots = sum(potential_lots),
    .by = "zoning"
  ) |> 
  adorn_totals()

## Option 3 ---------------------------

opt3 <- parcel_filter |> 
  st_drop_geometry() |>
  filter(
    zoning == "A1" & acres >= 22
  ) |> 
  mutate(
    potential_lots = pmax(floor((acres - 20) / 2), 0)
  )

opt3_sum <- opt3 |> 
  summarise(
    parcels = n(),
    lots = sum(potential_lots),
    .by = "zoning"
  )

## Option 4 ---------------------------

opt4 <- parcel_filter |> 
  st_drop_geometry() |>
