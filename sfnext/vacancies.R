library(tidyverse)

# labels <- read_csv("https://data.sfgov.org/api/views/j2hz-23ps/rows.csv?date=20231023&accessType=DOWNLOAD")

data <- read_csv("G:/Shared drives/City Systems/SFNext/vacancies.csv") %>% 
  mutate(
    funded_ftes = round(funded_ftes),
    filled_ftes = pmin(funded_ftes,round(filled_ftes)),
    vacant_ftes = funded_ftes - filled_ftes
  ) %>% 
  arrange(desc(funded_ftes)) %>% 
  # arrange(desc(vacant_ftes)) %>% 
  mutate(
    Name =  ifelse(
      row_number() < 14,
      Name,
      "Z_Other"
    )
  ) %>% 
  group_by(Name) %>% 
  select(-Department) %>% 
  summarize_all(sum) %>% 
  mutate(
    perc_vacant = vacant_ftes/funded_ftes
  ) %>% 
  arrange(desc(funded_ftes)) %>% 
  # arrange(desc(perc_vacant)) %>% 
  .[c(1:2,4:10,3),] %>% 
  mutate(
    perc = funded_ftes/sum(funded_ftes),
    cum_perc_deg = cumsum(perc)*360,
    cum_perc = cumsum(perc)*pi*2,
    deg = tan(cum_perc)
  )

saveRDS(data,"data1.rds")
saveRDS(data,"data2.rds")

DF <- expand.grid(-90:90, -90:90) %>% 
  mutate(R =sqrt(Var1^2 + Var2^2)) %>% 
  filter(R <= 90) %>% 
  mutate(
    group = case_when(
      (Var1 > 0 & Var2 >= 0) & (Var2 < 3.128232e-01 * Var1) ~ "Public Works",
      (Var1 > 0 & Var2 >= 0) & (Var2 < 3.760999e-01 * Var1) ~ "Port",
      (Var1 > 0 & Var2 >= 0) & (Var2 < 8.103003e-01 * Var1) ~ "Airport",
      (Var1 > 0 & Var2 >= 0) & (Var2 < 2.568145e+00 * Var1) ~ "Police",
      (Var1 > 0 & Var2 >= 0) | (Var1 <=0 & Var2 > 0 & Var2 > -6.939101e-01 * Var1) ~ "Public Health",
      (Var1 <=0 & Var2 > 0) & Var2 > -4.601155e-01 * Var1 ~ "Administrator",
      (Var1 <=0 & Var2 > 0) | (Var1 < 0 & Var2 <= 0 & Var2 > 8.633976e-01 * Var1) ~ "Transportation",
      (Var1 < 0 & Var2 <= 0) & Var2 > 1.201122 * Var1 ~ "Recreation & Parks",
      (Var1 < 0 & Var2 <= 0) & Var2 > 3.182197 * Var1 ~ "Human Services",
      TRUE ~ "Other"
    )
  ) %>% 
  group_by(group) %>% 
  mutate(
    Status = case_when(
      group == "Public Works" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.31995277,1-0.31995277)),
      group == "Port" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.26582278,1-0.26582278)),
      group == "Airport" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.17660167,1-0.17660167)),
      group == "Police" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.14601312,1-0.14601312)),
      group == "Public Health" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.12851782,1-0.12851782)),
      group == "Administrator" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.11020408,1-0.11020408)),
      group == "Transportation" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.10815464,1-0.10815464)),
      group == "Recreation & Parks" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.10239651,1-0.10239651)),
      group == "Human Services" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.06703398,1-0.06703398)),
      group == "Other" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.08560794,1-0.08560794))
    ),
    group = ifelse(
      Status == "Vacant",
      Status,
      group
    )
  )

sort(unique(DF$group))

ggplot(DF, aes(x = Var1, y = Var2, color = group)) + 
  geom_point(size = 0.3) + 
  scale_color_manual(values=c("gray60","gray80", "gray80", "grey60","gray60", "gray60", "gray80", "gray80", "gray60", "gray80", "red")) +
  coord_fixed()




DF <- expand.grid(-102:102, -102:102) %>% 
  mutate(R =sqrt(Var1^2 + Var2^2)) %>% 
  filter(R <= 101.2) %>% 
  mutate(
    group = case_when(
      (Var1 > 0 & Var2 >= 0) & (Var2 < 8.73 * Var1) ~ "Public Health",
      (Var1 > 0 & Var2 >= 0) | (Var1 <= 0 & Var2 > 0 & Var2 > -4.68e-1 * Var1) ~ "Transportation",
      (Var1 <= 0 & Var2 > 0) | (Var1 < 0 & Var2 <= 0 & Var2 > 1.29e-1 * Var1) ~ "Police",
      (Var1 < 0 & Var2 <= 0 & Var2 > 6.17e-1 * Var1) ~ "Human Services",
      (Var1 < 0 & Var2 <= 0 & Var2 > 1.41 * Var1) ~ "Public Utilities",
      (Var1 < 0 & Var2 <= 0 & Var2 > 3.89 * Var1) ~ "Fire",
      (Var1 < 0 & Var2 <= 0) | (Var1 >= 0 & Var2 <= 0 & Var2 < -10.1 * Var1) ~ "Airport",
      (Var1 >= 0 & Var2 <= 0 & Var2 < -2.18 * Var1) ~ "Public Works",
      (Var1 >= 0 & Var2 <= 0 & Var2 < -1.39 * Var1) ~ "Sheriff",
      TRUE ~ "Other"
    )
  ) %>% 
  group_by(group) %>% 
  mutate(
    Status = case_when(
      group == "Public Health" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.129,1-0.129)),
      group == "Transportation" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.108,1-0.108)),
      group == "Police" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.146,1-0.146)),
      group == "Human Services" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.0670,1-0.0670)),
      group == "Public Utilities" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.0298,1-0.0298)),
      group == "Fire" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.0330,1-0.0330)),
      group == "Airport" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.177,1-0.177)),
      group == "Public Works" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.320,1-0.320)),
      group == "Sheriff" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.0728,1-0.0728)),
      group == "Other" ~ sample(c("Vacant","Filled"), n(), replace = T, prob = c(0.153,1-0.153))
    ),
    group = ifelse(
      Status == "Vacant",
      Status,
      group
    )
  )

sort(unique(DF$group))

ggplot(DF, aes(x = Var1, y = Var2, color = group)) + 
  geom_point(size = 1) + 
  scale_color_manual(values=c("gray90","gray70", "gray70", "gray70","gray90", "gray90", "gray90", "gray70", "gray90", "gray70", "red")) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position="none"
  )
