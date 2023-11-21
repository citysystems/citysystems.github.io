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

# simpler version

data <- read_csv("G:/Shared drives/City Systems/SFNext/vacancies.csv") %>% 
  mutate(
    funded_ftes = round(funded_ftes),
    `Filled FTEs` = pmin(funded_ftes,round(filled_ftes)),
    `Vacant FTEs` = funded_ftes - `Filled FTEs`
  ) %>% 
  arrange(desc(`Vacant FTEs`)) %>% 
  head(15) %>%
  mutate(
    Name = factor(Name, levels = rev(unique(Name))),
    # Label = paste0(`Vacant FTEs`,"/",funded_ftes,"\n(",round(`Vacant FTEs`/funded_ftes*100),"%)")
    Label = paste0(round(`Vacant FTEs`/funded_ftes*100),"%")
  ) %>% 
  select(-funded_ftes,-filled_ftes,-vacant_ftes,-Department) %>% 
  pivot_longer(
    -c(Name,Label),
    names_to = "names",
    values_to = "values"
  ) %>% 
  mutate(
    Label = ifelse(
      names == "Vacant FTEs",
      Label,
      ""
    )
  )

data %>% 
  ggplot() +
  geom_bar(
    aes(
      x = Name,
      y = values,
      fill = names
    ),
    stat = "identity",
    position = "stack"
  ) +
  geom_text(aes(x = Name, y = values, label = Label), hjust = -0.1, size = 2.5, color = "white") +
  coord_flip() +
  labs(
    x = "Department",
    y = "# of full-time equivalent positions (FTEs)",
    fill = "Type"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8)
  )
  

  
pdf("G:/Shared drives/City Systems/SFNext/vacancies_tall.pdf", width = 5, height = 5)
data %>% 
  ggplot() +
  geom_bar(
    aes(
      x = Name,
      y = values,
      fill = names
    ),
    stat = "identity",
    position = "stack"
  ) +
  geom_text(aes(x = Name, y = -250, label = Label), size = 2.5, color = "red") +
  coord_flip() +
  labs(
    x = "",
    y = "Number of full-time equivalent positions (FTEs)",
    fill = ""
  ) +
  scale_fill_manual(values = c("gray","red")) +
  ylim(-250,NA) +
  guides(color = "none") +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.2)
  )
dev.off()

pdf("G:/Shared drives/City Systems/SFNext/vacancies_wide.pdf", width = 8, height = 5)
data %>% 
  ggplot() +
  geom_bar(
    aes(
      x = Name,
      y = values,
      fill = names
    ),
    stat = "identity",
    position = "stack"
  ) +
  geom_text(aes(x = Name, y = -250, label = Label), size = 2.5, color = "red") +
  coord_flip() +
  labs(
    x = "",
    y = "Number of full-time equivalent positions (FTEs)",
    fill = ""
  ) +
  scale_fill_manual(values = c("gray","red")) +
  ylim(-250,NA) +
  guides(color = "none") +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.2)
  )
dev.off()
