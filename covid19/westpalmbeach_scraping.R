# for use in Github Actions workflow

library(tidyverse)
library(jsonlite)

data <- fromJSON("https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/services/Florida_Cases_Zips_COVID19/FeatureServer/0/query?where=1%3D1&outFields=ZIP,COUNTYNAME,Cases_1&returnGeometry=false&outSR=4326&f=json") %>% 
  .$features %>% 
  .$attributes %>% 
  transmute(
    ZIP,
    COUNTYNAME,
    Cases = Cases_1,
    date = Sys.Date()
  ) %>% 
  filter(ZIP %in% c(
    "33480",
    "33401",
    "33405",
    "33406",
    "33407",
    "33409",
    "33411",
    "33412",
    "33417",
    "33418"
  )) %>% 
  select(-COUNTYNAME) %>% 
  pivot_wider(
    names_from = ZIP,
    values_from = Cases
  )

write_csv(data, paste0("covid19/wpb-zip-", Sys.Date(), ".csv"))

county_data <- fromJSON("https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/services/Florida_COVID19_Cases/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json") %>% 
  .$features %>% 
  .$attributes %>% 
  mutate(
    date = Sys.Date()
  ) %>% 
  select(-c(
    OBJECTID,
    OBJECTID_12_13,
    DEPCODE,
    COUNTY,
    COUNTYNAME,
    State,
    PUIAgeRange,
    Shape__Area,
    Shape__Length,
    GlobalID
  )) %>% 
  rename(County = County_1) %>% 
  filter(County == "Palm Beach") %>% 
  mutate(date = Sys.Date()) %>% 
  select(date, everything())

write_csv(county_data, paste0("covid19/pbc-county-", Sys.Date(), ".csv"))

scrape_time_df <- data.frame(scrape_last_time_ran = Sys.time())

write_csv(scrape_time_df, "covid19/fl_scrape_last_time_ran.csv")

zip_data <- 
  ("2020-12-05" %>% as.Date()):Sys.Date() %>% 
  as.Date(origin = "1970-01-01") %>% 
  map_dfr(function(x){
    
    temp <- 
      read_csv(paste0("covid19/wpb-zip-", x, ".csv"))
    
  })

write_csv(zip_data,"covid19/wpb_zip_data.csv")

county_data <- 
  ("2020-12-05" %>% as.Date()):Sys.Date() %>% 
  as.Date(origin = "1970-01-01") %>% 
  map_dfr(function(x){
    
    tryCatch(
      temp <- 
        read_csv(paste0("covid19/pbc-county-", x, ".csv")),
      error = function(e) NULL
      
    )
    
  })

write_csv(county_data,"covid19/pbc_county_data.csv")
