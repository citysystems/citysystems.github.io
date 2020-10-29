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

write_csv(scrape_time_df, "covid19/scranton_scrape_last_time_ran.csv")