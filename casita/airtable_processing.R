library(airtabler)
library(tidyverse)
library(qdapRegex)
library(RColorBrewer)

basekey_issues = Sys.getenv("AIRTABLE_BASEKEY_ISSUES")
basekey_projects = Sys.getenv("AIRTABLE_BASEKEY_PROJECTS")
tablekey_issues = Sys.getenv("AIRTABLE_TABLEKEY_ISSUES")
tablekey_interpretations = Sys.getenv("AIRTABLE_TABLEKEY_INTERPRETATIONS")



issue_airtable <- airtable(
  base = basekey_issues,
  tables = Sys.getenv("AIRTABLE_TABLEKEY_ISSUES")
)

issue_table <- issue_airtable$tbl106h1MXjyBVrN9$select() %>%
  mutate(across(where(is.list), ~ sapply(., unlist)))

data1 <- issue_table
offset <- get_offset(issue_table)

while(!is.null(offset)) {
  
  issue_table <- issue_airtable$tbl106h1MXjyBVrN9$select(offset = offset) %>%
    mutate(across(where(is.list), ~ sapply(., unlist)))
  
  
  
  data1 <- 
    tryCatch({
      data1 %>% 
        dplyr::bind_rows(issue_table)
    }, error = function(e) {
      print(e)
      return(data1)
    }
    )
  offset <- get_offset(issue_table)
}

issue_table <-
  data1 %>% 
  filter(Reviewed == TRUE) %>% 
  dplyr::select(Issue,`Interpretation (from Interpretation)`,createdTime) %>% 
  rename(
    "Interpretation" = "Interpretation (from Interpretation)"
  ) %>% 
  mutate(Created = as.Date(createdTime)) %>% 
  dplyr::select(-createdTime)

######

interpretation_airtable <- airtable(
  base = basekey_issues,
  tables = Sys.getenv("AIRTABLE_TABLEKEY_INTERPRETATIONS")
)

interpretation_table <- interpretation_airtable$tblv0mPHXsqYBK2Ws$select() %>%
  mutate(across(where(is.list), ~ sapply(., unlist)))


data <- interpretation_table
offset <- get_offset(interpretation_table)

while(!is.null(offset)) {
  
  interpretation_table <- interpretation_airtable$tblv0mPHXsqYBK2Ws$select(offset = offset) %>%
    mutate(across(where(is.list), ~ sapply(., unlist)))
  
  data <- 
    tryCatch({
      data %>% 
        dplyr::bind_rows(interpretation_table)
    }, error = function(e) {
      print(e)
      return(data)
    }
    )
  offset <- get_offset(interpretation_table)
}


interpretation_table <-
  data %>% 
  dplyr::select(`Interpretation`,`State Law Relatedness` = `State Grab Text`,Source,Topic,`Last Modified`) %>%
  mutate(
    `Last Modified` = as.Date(`Last Modified`)
  ) %>% 
  arrange(desc(Interpretation))

interp_mod <-
  interpretation_table %>% 
  mutate(
    `Topic 1` = "",
    `Topic 2` = "",
    `Topic 3` = "",
    `Topic 4` = "",
    `Topic 5` = ""
  )

for(i in 1:nrow(interpretation_table)){
  
  #interpretation_table$topic_mod[i] <- paste(unlist(interpretation_table$Topic[i]), collapse=', ')
  
  temp <- interpretation_table$Topic[i][[1]]
  
  
  if(!is.null(temp)){
    for(j in 1:length(temp)){
      if(j == 1){
        interp_mod$`Topic 1`[i] <- temp[j]
      }else if(j == 2){
        interp_mod$`Topic 2`[i] <- temp[j]
      }else if(j == 3){
        interp_mod$`Topic 3`[i] <- temp[j]
      }else if(j == 4){
        interp_mod$`Topic 4`[i] <- temp[j]
      }else if(j == 5){
        interp_mod$`Topic 5`[i] <- temp[j]
      }
      
    }
  }
}


####color processing


n <- 33
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) %>% as.data.frame()


keyword_list1 <- unique(c(unique(interp_mod$`Topic 1`),unique(interp_mod$`Topic 2`),unique(interp_mod$`Topic 3`),unique(interp_mod$`Topic 4`),unique(interp_mod$`Topic 5`))) %>% 
  na.omit() %>% 
  as.data.frame() %>% 
  rename(keywords = ".") %>% 
  filter(keywords != "") 

keyword_list <-
  keyword_list1 %>% 
  cbind(head(col_vector,nrow(keyword_list1))) %>% 
  rename("hex" = ".")


interp_mod2 <-
  interp_mod %>% 
  left_join(
    keyword_list %>% 
      rename(color1 = "hex"),
    by = c("Topic 1" = "keywords")
  ) %>% 
  left_join(
    keyword_list %>% 
      rename(color2 = "hex"),
    by = c("Topic 2" = "keywords")
  ) %>% 
  left_join(
    keyword_list %>% 
      rename(color3 = "hex"),
    by = c("Topic 3" = "keywords")
  ) %>% 
  left_join(
    keyword_list %>% 
      rename(color4 = "hex"),
    by = c("Topic 4" = "keywords")
  ) %>% 
  left_join(
    keyword_list %>% 
      rename(color5 = "hex"),
    by = c("Topic 5" = "keywords")
  ) 

interp_mod <- 
  interp_mod2 

###########

for(i in 1:nrow(interp_mod)){
  if(is.null(unlist(interp_mod$`State Law Relatedness`[i]))){
    interp_mod$`State Law Relatedness`[i] <- c("","")
  }
}

for(i in 1:nrow(issue_table)){
  if(is.null(unlist(issue_table$Interpretation[i]))){
    issue_table$Interpretation[i] <- c("","")
  }
}


code_display_data <- interp_mod %>%
  dplyr::select(Interpretation,`Last Modified`,`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`) %>% 
  filter(substr(Interpretation,1,4) %in% c("312.","310.","6585","311.")) %>%
  na.omit() %>% 
  rename("Code" = "Interpretation") %>% 
  mutate(
    Code = case_when(
      substr(Code,1,8) == "65852.2("  ~ paste0(substr(Code,1,7),"0",substr(Code,8,nchar(Code))),
      grepl("310.|311.|312.",Code) ~ paste0("CPC ",Code),
      TRUE ~ Code
    )
  ) #%>% 
# arrange(Code) %>% 
# mutate(
#   `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0("Keywords: ",`Topic 1`),""),
#   `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
#   `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
#   `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
#   `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
#   Code = paste0(Code,"</br></br><i><small>Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
# ) %>%
# dplyr::select(Code)

interpretation_display_data <- interp_mod %>%
  dplyr::select(Interpretation,`Last Modified`,`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`) %>% 
  filter(!substr(Interpretation,1,4) %in% c("312.","310.","6585","311.")) %>% 
  na.omit() %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A #",as.character(substr(Interpretation,6,7)),substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  )

issue_display_data <- issue_table %>%
  dplyr::select(Issue) 

all_hold_issue <- NULL

for(i in 1:nrow(issue_table)){
  hold_explode <- NULL
  if(length(issue_table$Interpretation[i][[1]]) > 1){
    for(j in 1:length(issue_table$Interpretation[i][[1]])){
      temp <- data.frame(
        "Issue" = issue_table$Issue[i],
        "Interpretation" = issue_table$Interpretation[i][[1]][j],
        "Created" = issue_table$Created[i])
      
      hold_explode <-
        hold_explode %>% 
        rbind(temp)
    }
  }else{
    hold_explode <- data.frame(
      "Issue" = issue_table$Issue[i],
      "Interpretation" = issue_table$Interpretation[i][[1]][1],
      "Created" = issue_table$Created[i])
    
  }
  
  all_hold_issue <-
    all_hold_issue %>% 
    rbind(hold_explode)
}

all_hold_issue <-
  all_hold_issue %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A #",as.character(substr(Interpretation,6,7)),substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      substr(Interpretation,1,8) == "65852.2("  ~ paste0(substr(Interpretation,1,7),"0",substr(Interpretation,8,nchar(Interpretation))),
      grepl("310.|311.|312.",Interpretation) ~ paste0("CPC ",Interpretation),
      TRUE ~ Interpretation
    )
  ) %>% 
  dplyr::select(-Created) %>% 
  left_join(
    interpretation_display_data %>% 
      rbind(code_display_data %>% rename("Interpretation" = "Code")),
    by = "Interpretation"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"</br></br><i><small> Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Issue,Interpretation)


all_hold_interp <- NULL

for(i in 1:nrow(interpretation_table)){
  hold_explode <- NULL
  if(length(interpretation_table$`State Law Relatedness`[i][[1]]) > 1){
    for(j in 1:length(interpretation_table$`State Law Relatedness`[i][[1]])){
      temp <- data.frame(
        "Interpretation" = interpretation_table$Interpretation[i],
        "Code" = interpretation_table$`State Law Relatedness`[i][[1]][j],
        "Last Modified" = interpretation_table$`Last Modified`[i])
      
      hold_explode <-
        hold_explode %>% 
        rbind(temp)
    }
  }else if(length(interpretation_table$`State Law Relatedness`[i][[1]]) == 1){
    hold_explode <- data.frame(
      "Interpretation" = interpretation_table$Interpretation[i],
      "Code" = interpretation_table$`State Law Relatedness`[i][[1]][1],
      "Last Modified" = interpretation_table$`Last Modified`[i])
    
  }
  
  all_hold_interp <-
    all_hold_interp %>% 
    rbind(hold_explode)
}

all_hold_interp <-
  all_hold_interp %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A #",as.character(substr(Interpretation,6,7)),substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Code = case_when(
      substr(Code,1,8) == "65852.2("  ~ paste0(substr(Code,1,7),"0",substr(Code,8,nchar(Code))),
      grepl("310.|311.|312.",Code) ~ paste0("CPC ",Code),
      TRUE ~ Code
    )
  ) %>% 
  left_join(
    interpretation_display_data,
    by = "Interpretation"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"</br></br><i><small> Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation,Code) %>% 
  left_join(
    code_display_data,
    by = "Code"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Code = paste0(Code,"</br></br><i><small> Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation,Code)

interpretation_display_data <-
  interpretation_display_data %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"</br></br><i><small> Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation) %>% 
  arrange(Interpretation)

code_display_data <-
  code_display_data %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Code = paste0(Code,"</br></br><i><small> Last Modified: ",`Last Modified`,"</br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Code) %>% 
  arrange(Code)

interpretation_display_data$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = interpretation_display_data$Interpretation)

interpretation_display_data$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = interpretation_display_data$Interpretation)

interpretation_display_data$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = interpretation_display_data$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = all_hold_issue$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = all_hold_issue$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = all_hold_issue$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = all_hold_interp$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = all_hold_interp$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = all_hold_interp$Interpretation)


save(
  issue_table,
  interpretation_table,
  interp_mod,
  all_hold_interp,
  all_hold_issue,
  interpretation_display_data,
  issue_display_data,
  code_display_data,
  file = "casita/airtable_data.rda")


