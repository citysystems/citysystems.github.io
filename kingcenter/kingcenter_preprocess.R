library(tidyverse)
library(googlesheets4)

gs4_deauth()


data_news <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="News!A:L") %>%
  mutate(Date = Date %>% paste(.,"1") %>% as.Date("%B %Y %d")) %>%
  rename(Subcategory = `Sub-category (Title)`) %>% arrange(desc(Date))

data_initiatives <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Initiatives!A:P") %>%
  rename(Subcategory = `Sub-category (Title)`) %>%
  mutate(Date = NA) %>%
  .[,c(1:13,17,14:16)]

data_faculty <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Faculty Research Support!A:L") %>%
  mutate(
    Date = case_when(
      grepl("Fall",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-09-01") %>% as.Date(),
      grepl("Winter",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-01-01")%>% as.Date(),
      grepl("Spring",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-03-01")%>% as.Date(),
      grepl("Summer",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-06-01")%>% as.Date()
    ),
    Category = "Faculty Research Support") %>%
  rename(Subcategory = `Sub-category (Title)`) %>%
  mutate(Subcategory = paste0(Subcategory,"; ",`Sub-category (Name)`)) %>%
  dplyr::select(-`Sub-category (Name)`) %>%
  dplyr::select(-Quarter,-AY) %>%
  .[,c(1:5,9:10,6:8)] %>%
  dplyr::select(-`Old Project Title`)

data_graduate <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Graduate Student Research Support!A:O") %>%
  mutate(
    Date = case_when(
      grepl("Fall",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-09-01") %>% as.Date(),
      grepl("Winter",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-01-01")%>% as.Date(),
      grepl("Spring",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-03-01")%>% as.Date(),
      grepl("Summer",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-06-01")%>% as.Date()
    ),
    Category = "Graduate Student Research Support") %>%
  rename(Subcategory = `Sub-category (Title)`) %>%
  mutate(Subcategory = paste0(Subcategory,"; ",`Sub-category (Name)`)) %>%
  dplyr::select(-`Sub-category (Name)`) %>%
  dplyr::select(-Quarter,-AY) %>%
  .[,c(1:8,12:13,9:11)] %>%
  dplyr::select(-`Old Project Title`)

data_undergraduate <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Undergraduate Summer Programs!A:O") %>%
  mutate(
    Date = case_when(
      grepl("Fall",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-09-01") %>% as.Date(),
      grepl("Winter",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-01-01")%>% as.Date(),
      grepl("Spring",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-03-01")%>% as.Date(),
      grepl("Summer",Quarter) ~ paste0("20",substr(Quarter,nchar(Quarter) - 1,nchar(Quarter)),"-06-01")%>% as.Date()
    ),
    Category = "Undergraduate Summer Programs") %>%
  rename(Subcategory = `Sub-category (Title)`) %>%
  mutate(Subcategory = paste0(Subcategory,"; ",`Sub-category (Name)`)) %>%
  dplyr::select(-`Sub-category (Name)`) %>%
  dplyr::select(-Quarter,-AY) %>%
  .[,c(1:8,12:13,9:11)] %>%
  dplyr::select(-`Old Project Title`)

data_people <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="People!A:AF") %>%
  rename(
    Subcategory = `Sub-category (Name)`,
    Category = `Category (Affiliation)`
  ) %>%
  mutate(
    Date = NA,
    Category = "People"
  ) %>%
  .[,c(1:23,33,24:32)]

data_events <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Events!A:L") %>%
  mutate(Date = Date %>% paste(.,"1") %>% as.Date("%B %Y %d")) %>%
  rename(Subcategory = `Sub-category (Title)`)

data_papers <-
  range_speedread("1eMowQcLh12vq0jAOjMbnE0FlLoh7ocHg5me9j-2Lrlo",range="Working Papers!A:L") %>%
  mutate(Date = Date %>% paste(.,"1") %>% as.Date("%B %Y %d")) %>%
  rename(Subcategory = `Sub-category (Title)`)


#Takes all of the country columns and creates individual rows with duplicates of relevant article names/links/themes
reorder_data <- function(df,num_country_col,num_theme_col){
  
  all_hold <- NULL
  
  for(i in 1:nrow(df)){
    row_hold <- df[i,]
    single_hold <- NULL
    for(j in 1:num_country_col){
      if(!is.na(row_hold[1,j])){
        
        name <- row_hold[1,j] %>% unlist() %>% .[[1]]
        
        country_df <-
          row_hold[1,j] %>%
          cbind(row_hold[1,c((num_country_col + 1):ncol(row_hold))]) %>%
          mutate(
            Country = name
          ) %>%
          .[,-1]
      }else{next}
      
      single_hold <- single_hold %>% rbind(country_df)
    }
    all_hold <- all_hold %>% rbind(single_hold)
  }
  
  theme_hold <- NULL
  for(i in 1:nrow(all_hold)){
    row_hold <- all_hold[i,]
    
    test <- row_hold[1,(ncol(row_hold) - num_theme_col):(ncol(row_hold) - 1)]
    test1 <- transpose(test) %>% unlist() %>% as.data.frame() %>% na.omit() %>% rename("Theme" = ".")
    
    rownames(test1) <- seq(length=nrow(test1))
    
    
    test2 <- test1 %>% cbind(row_hold[,c(1:(ncol(row_hold) - num_theme_col-1),ncol(row_hold))])
    theme_hold <- theme_hold %>% rbind(test2)
  }
  
  
  
  
  return(theme_hold)
}

reorder_news <- reorder_data(data_news,5,3)
reorder_initiatives <- reorder_data(data_initiatives,10,3)
reorder_faculty <- reorder_data(data_faculty,2,3)
reorder_graduate <- reorder_data(data_graduate,5,3)
reorder_undergraduate <- reorder_data(data_undergraduate,5,3)
reorder_people <- reorder_data(data_people,20,9)
reorder_events <- reorder_data(data_events,5,3)
reorder_papers <- reorder_data(data_papers,5,3)

all_cleaned <-
  rbind(reorder_news) %>%
  rbind(reorder_initiatives) %>%
  rbind(reorder_faculty) %>%
  rbind(reorder_graduate) %>%
  rbind(reorder_undergraduate) %>%
  rbind(reorder_people) %>%
  rbind(reorder_events) %>%
  rbind(reorder_papers)



# all_countries <- all_cleaned$Country %>% unique()

# data <- all_cleaned

save(
  all_cleaned,
  data_news,
  data_events,
  data_faculty,
  data_graduate,
  data_initiatives,
  data_papers,
  data_people,
  data_undergraduate,
  file = "kingcenter/kingcenter_cleaned_data.rda"
)

#write_csv(all_cleaned,"kingcenter/kingcenter_cleaned_data.csv")

#saveRDS(all_cleaned,"kingcenter_cleaned_data.rds")

