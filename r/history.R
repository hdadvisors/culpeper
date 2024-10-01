## Setup ------------------------------

library(tidyverse)
library(janitor)
library(hdatools)


## Parcels ----------------------------

parcel_subdiv <- read_rds("data/parcel_subdiv.rds") |> 
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
