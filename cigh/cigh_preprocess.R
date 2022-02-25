library(tidyverse)
library(googlesheets4)
gs4_deauth()

data <-
  range_speedread("1YeYdVzfS5vpZppMUTR3xRw3XK-PDMRtvTHF21f06O1s",range="Projects") %>% 
  mutate(index = row_number())

data_projects <- data %>% 
  select(
    index,
    `Project Name`,
    `Link for additional information`,
    `Hospital/Facility name`,
    `Partner Agencies (separate with semicolons)`
  )

data_faculty <-
  range_speedread("1YeYdVzfS5vpZppMUTR3xRw3XK-PDMRtvTHF21f06O1s",range="Faculty")

data_tidy <- data %>% 
  select(
    index,
    starts_with(c(
      "Faculty",
      "Clinical/academic focus area",
      "Country"
    ))
  ) %>% 
  pivot_longer(
    starts_with("Faculty"),
    values_to = "Faculty"
  ) %>% 
  select(-name) %>% 
  filter(!is.na(Faculty)) %>% 
  pivot_longer(
    starts_with("Clinical/academic focus area"),
    values_to = "Area"
  ) %>% 
  select(-name) %>% 
  filter(!is.na(Area)) %>% 
  pivot_longer(
    starts_with("Country"),
    values_to = "Country"
  ) %>% 
  select(-name) %>% 
  filter(!is.na(Country))

save(
  data_projects,
  data_faculty,
  data_tidy,
  file = "cigh/cigh_data.rda"
)