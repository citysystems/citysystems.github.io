# this file includes code for scraping data from https://www.health.pa.gov/topics/disease/coronavirus/Pages/Cases.aspx
# for use in Github Actions workflow

library(esri2sf)
library(tidyverse)
library(sf)

data <- esri2sf("https://services1.arcgis.com/Nifc7wlHaBPig3Q3/ArcGIS/rest/services/ZIP_Code_PA_COVID/FeatureServer/0") %>% 
  st_set_geometry(NULL) %>% 
  select(
    ZIP_CODE,
    Positive,
    Negative,
    Probable
  ) %>% 
  mutate(
    date = Sys.Date()
  )

write_csv(data, paste0("covid19/pa-zip-", Sys.Date(), ".csv"))

scrape_time_df <- data.frame(scrape_last_time_ran = Sys.time())

write_csv(scrape_time_df, "covid19/pa_scrape_last_time_ran.csv")

prior_dates <- data.frame(
  date = c("2020-10-23","2020-10-24","2020-10-25","2020-10-26","2020-10-27"),
  ZIP_18503 = c(25, 25, 27, 29, 29),
  ZIP_18504 = c(236, 240, 244, 247, 247),
  ZIP_18505 = c(471, 479, 479, 483, 494),
  ZIP_18508 = c(207, 211, 212, 213, 218),
  ZIP_18509 = c(167, 169, 172, 172, 174),
  ZIP_18510 = c(908, 914, 942, 953, 960)
)

zips <- c(18503,18504,18505,18508,18509,18510)

zip_data <- 
  ("2020-10-28" %>% as.Date()):Sys.Date() %>% 
  as.Date(origin = "1970-01-01") %>% 
  map_dfr(function(x){
    
    temp <- 
      read_csv(paste0("covid19/pa-zip-", x, ".csv")) %>% 
      filter(ZIP_CODE %in% zips) %>% 
      mutate(across(
        c("Positive","Negative","Probable"),
        function(x){
          ifelse(
            x == -1,
            0,
            x
          )
        }
      ))
    
  })

write_csv(zip_data,"covid19/zip_data.csv")

scranton_summary <-
  zip_data %>% 
  group_by(date) %>% 
  summarize_at(
    c("Positive","Negative","Probable"),
    sum
  ) %>% 
  mutate(
    Positive_New = c(NA,diff(Positive)),
    Negative_New = c(NA,diff(Negative)),
    Probable_New = c(NA,diff(Probable)),
    date = date %>% as.character()
  )

write_csv(scranton_summary,"covid19/scranton_summary.csv")
