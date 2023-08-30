library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(readxl)
library(htmltools)
library(htmlwidgets)

projection <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=ft +no_defs"

sf_parcels_shape <- 
  st_read("https://data.sfgov.org/api/geospatial/acdm-wktn?method=export&format=GeoJSON") %>% 
  filter(active == "true") %>% 
  select(
    apn = blklot,
    zoning = zoning_code,
    zoning_desc = zoning_district
  )

temp <- tempfile()
download.file("https://sfassessor.org/sites/default/files/uploaded/2023.7.18_SF_ASR_Secured_Roll_Data_2022-2023.xlsx",destfile = temp, mode = "wb")

sf_secured <- read_excel(temp, sheet = "Roll Data 2022-2023")
datakey <- read_excel(temp, sheet = "Data Key")
usecode <- read_excel(temp, sheet = "Class & Use Code") %>% 
  filter(!is.na(USE) & USE != "USE")

unlink(temp)

sf_parcels <-
  sf_parcels_shape %>% 
  left_join(
    sf_secured %>% 
      mutate(
        apn = RP1PRCLID %>% 
          str_replace(" ",""),
        RP1CLACDE = RP1CLACDE %>% gsub("VPU","VPUB",.)
      ) %>% 
      left_join(
        usecode,
        by = c("RP1CLACDE" = "CLASS")
      )
  )

sf_parcels_public <- sf_parcels %>% 
  filter(
    zoning == "P" |
      USE == "GOVT" |
      DESC...4 == "VPUB"
  ) %>% 
  filter(!st_is_empty(.)) %>% 
  st_transform(projection) %>% 
  mutate(
    PARCELAREA = as.numeric(st_area(.))
  ) %>% 
  st_transform(4326)

pal_sqft <- colorNumeric(
  "RdYlGn",
  sf_parcels_public$SQFT
)

pal <- colorNumeric(
  "RdYlGn",
  sf_parcels_public$LAREA
)

pal <- colorNumeric(
  "RdYlGn",
  c(0:250000)
)

map <- leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addPolygons(
    data = sf_parcels_public,
    color = ~pal(PARCELAREA),
    fillOpacity = 0.5,
    opacity = 0.75,
    weight = 1,
    label = ~paste0(
        PROPLOC,
        "<br>",
        "APN: ", apn,
        "<br>",
        "ZONING: ", zoning_desc,
        "<br>",
        "USE: ", DESC...4,
        "<br>",
        "PARCEL AREA: ", round(PARCELAREA),
        "<br>",
        "YEAR STRUCTURE BUILT: ", YRBLT,
        "<br>",
        "NUMBER OF STORIES: ", STOREYNO,
        "<br>",
        "NUMBER OF UNITS: ", UNITS
      ) %>% lapply(HTML),
    highlightOptions = highlightOptions(
      weight = 3
    )
  ) %>% 
  addLegend(
    pal = pal,
    values = c(0:250000)
  )

map

saveWidget(map, "sf_public_parcels.html")