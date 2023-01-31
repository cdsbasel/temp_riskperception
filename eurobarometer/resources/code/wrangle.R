## -------------------------------------------------------------------------- ##
## Script name: Eurobarometer: Most Important Issues
## Script description:
## 
## -------------------------------------------------------------------------- ##
## Author: Gayoung Son
## Email:  gayoungson0@gmail.com
## Date: 2022-12-29
##
## --------------------------------------------*
## set working directory to desktop
setwd("C:/Users/HOME/Desktop/CDS/eurobarometer")

## --------------------------------------------*
## set options 
options(scipen = 6, digits = 3)

## --------------------------------------------*
## load packages
# source("somefile/somemainrfile.R")
library(tidyverse)
library(haven)  # read sav files
library(readxl)
library(sjlabelled)




# Read in data ------------------------------------------------------------
## item list of important issues
issue_items <- read_excel("description/issue_items.xlsx")

## data sets
# get a list of all .sav files in the folder
sav_files <- list.files(path = "datasets", pattern = "*.sav", full.names = TRUE)

# make an empty list for all data frames
df_list <- list()

# Iterate over the list of .sav files
#create a loop to read each of the files
for(i in 1:length(sav_files)){
  # read the file
  read_file <- read_sav(sav_files[i])
  # rename the file
  file_name <- gsub("_.*", "", sav_files[i])
  file_name <- sub("datasets/", "", file_name)
  # print file name
  print(file_name)
  # assign the file
  assign(file_name, read_file)
  # append the data frame to the list
  df_list[[file_name]] <- read_file
}

# save data and read 
# saveRDS(df_list, file = "C:/Users/HOME/Desktop/df_list.RDS")
# df_list <- readRDS("C:/Users/HOME/Desktop/df_list.RDS")

# Read in overview of waves -----------------------------------------------
library(docxtractr)
overview_waves <- read_docx("description/standardEB_overview.docx") %>% 
  # extract table
  docx_extract_tbl() %>% 
  # clean colnames
  janitor::clean_names()

# Read in codebook ---------------------------------------------------------
# Codebook with all items
codebook_all <- read_csv("codebook/codebook_all.csv",
                           col_types = cols(item_level = col_character(),
                                            item_new_code = col_character(),
                                            item_type = col_character(),
                                            trends_issues = col_character(),
                                            trends_threats = col_character(),
                                            standard_report = col_character(),
                                            comment = col_character()))
# Codebook with only Important Issues Items
# NOTE: waves before 2002 are incomplete due to differing item questions and answers - need manual coding. 
codebook_filtered <- read_csv("codebook/codebook_filtered.csv", 
                              col_types = cols(item_level = col_character(), 
                                               item_new_code = col_character(), 
                                               item_type = col_character(), trends_issues = col_character(), 
                                               trends_threats = col_character(), 
                                               special_topic = col_character(), 
                                               comment = col_character()))

# Filter datasets by "important issues" ----------------------------------
# # define function and loop
# filter_items <-  function(data_list, codebook_filtered){
#   df_list_filtered <- list()
#   for (data_name in names(data_list)) {
#     print(data_name)
#     filtered_dataset <- data_list[[data_name]]
#     codebook_filtered <- codebook_filtered
#     # select those items which contain the prefix in the item_list$item
#     df_list_filtered[[data_name]] <- filtered_dataset %>%
#       select(contains(codebook_filtered$item_variable[codebook_filtered$study_number == data_name]))
#   }
#   return(df_list_filtered)
# }
# 
# df_list_filtered <- filter_items(df_list, codebook_filtered)
# # NOTE: only filters waves from ZA5567 onwards - the earlier waves have different item codes. 


# Alternative: filter by those items in the filtered codebook ----
filter_items <- function(data_list, codebook_filtered) {
  df_list_filtered <- list()
  for (data_name in names(data_list)) {
    print(data_name)
    filtered_codebook <- codebook_filtered %>% 
      filter(study_number == data_name)
    df_list_filtered[[data_name]] <-
      data_list[[data_name]][, filtered_codebook$item_variable]
  }
  return(df_list_filtered)
}

df_list_filtered <- filter_items(df_list, codebook_df_filtered)
# AGAIN NOTE: waves before 2002 are incomplete due to differing item questions and answers - need manual coding. 

# combine split columns ---------------------------------------------------
# in certain waves (ZA7601), samples were split into two groups A and B, which is why questions are duplicated in the data sets.
# need to coalesce
# filter and exclude TCC
ZA7601_namechange <- df_list_filtered$ZA7601 %>% 
  select(-contains(c("c.", "d.")))

ZA7601_namechange <- ZA7601_namechange %>% 
  mutate(qa3a.1_new = coalesce(qa3a.1, qa3b.1)) %>% 
  mutate(qa3a.2_new = coalesce(qa3a.2, qa3b.2)) %>%
  mutate(qa3a.3_new = coalesce(qa3a.3, qa3b.3)) %>%
  mutate(qa3a.4_new = coalesce(qa3a.4, qa3b.4)) %>%
  mutate(qa3a.5_new = coalesce(qa3a.5, qa3b.5)) %>%
  mutate(qa3a.6_new = coalesce(qa3a.6, qa3b.6)) %>%
  mutate(qa3a.7_new = coalesce(qa3a.7, qa3b.7)) %>%
  mutate(qa3a.8_new = coalesce(qa3a.8, qa3b.8)) %>%
  mutate(qa3a.9_new = coalesce(qa3a.9, qa3b.9)) %>%
  mutate(qa3a.10_new = coalesce(qa3a.10, qa3b.10)) %>%
  mutate(qa3a.11_new = coalesce(qa3a.11, qa3b.11)) %>%
  mutate(qa3a.12_new = coalesce(qa3a.12, qa3b.12)) %>%
  mutate(qa3a.13_new = coalesce(qa3a.13, qa3b_t)) %>%
  mutate(qa3a.14_new = coalesce(qa3a.14, qa3b.15)) %>%
  mutate(qa3b.14_new = qa3b.14) %>% 
  mutate(qa3a.15_new = coalesce(qa3a.15, qa3b.16)) %>%
  mutate(qa3a.16_new = coalesce(qa3a.16, qa3b.17)) %>%
  mutate(qa4a.1_new = coalesce(qa4a.1, qa4b.1)) %>%
  mutate(qa4a.2_new = coalesce(qa4a.2, qa4b.2)) %>%
  mutate(qa4a.3_new = coalesce(qa4a.3, qa4b.3)) %>%
  mutate(qa4a.4_new = coalesce(qa4a.4, qa4b.4)) %>%
  mutate(qa4a.5_new = coalesce(qa4a.5, qa4b.5)) %>%
  mutate(qa4a.6_new = coalesce(qa4a.6, qa4b.6)) %>%
  mutate(qa4a.7_new = coalesce(qa4a.7, qa4b.7)) %>%
  mutate(qa4a.8_new = coalesce(qa4a.8, qa4b.8)) %>%
  mutate(qa4a.9_new = coalesce(qa4a.9, qa4b.9)) %>%
  mutate(qa4a.10_new = coalesce(qa4a.10, qa4b.10)) %>%
  mutate(qa4a.11_new = coalesce(qa4a.11, qa4b.11)) %>%
  mutate(qa4a.12_new = coalesce(qa4a.12, qa4b.12)) %>%
  mutate(qa4a.13_new = coalesce(qa4a.13, qa4b.13)) %>%
  mutate(qa4a.14_new = coalesce(qa4a.14, qa4b.14)) %>%
  mutate(qa4a.15_new = coalesce(qa4a.15, qa4b.15)) %>%
  mutate(qa4a.16_new = coalesce(qa4a.16, qa4b.16)) %>%
  mutate(qa4a.17_new = coalesce(qa4a.17, qa4b.17)) %>%
  mutate(qa4a.18_new = coalesce(qa4a.18, qa4b.18)) %>%
  mutate(qa5a.1_new = coalesce(qa5a.1, qa5b.1)) %>%
  mutate(qa5a.2_new = coalesce(qa5a.2, qa5b.2)) %>%
  mutate(qa5a.3_new = coalesce(qa5a.3, qa5b.3)) %>%
  mutate(qa5a.4_new = coalesce(qa5a.4, qa5b.4)) %>%
  mutate(qa5a.5_new = coalesce(qa5a.5, qa5b.5)) %>%
  mutate(qa5a.6_new = coalesce(qa5a.6, qa5b.6)) %>%
  mutate(qa5a.7_new = coalesce(qa5a.7, qa5b.7)) %>%
  mutate(qa5a.8_new = coalesce(qa5a.8, qa5b.8)) %>%
  mutate(qa5a.9_new = coalesce(qa5a.9, qa5b.9)) %>%
  mutate(qa5a.10_new = coalesce(qa5a.10, qa5b.10)) %>%
  mutate(qa5a.11_new = coalesce(qa5a_t, qa5b.11)) %>%
  mutate(qa5a.13_new = coalesce(qa5a.13, qa5b.12)) %>%
  mutate(qa5a.14_new = coalesce(qa5a.14, qa5b.13)) %>%
  mutate(qa5a.15_new = coalesce(qa5a.15, qa5b.14)) %>%
  mutate(qa5a.16_new = coalesce(qa5a.16, qa5b.15)) 

# select items of interest and drop suffixes
ZA7601_namechange_clean <- ZA7601_namechange %>% 
  select(d10, d11, isocntry, ends_with("_new")) %>% 
  rename_all(~gsub("_new", "", .))

# copy originals labels into new dataset
ZA7601_namechange_clean <- copy_labels(ZA7601_namechange_clean, ZA7601_namechange)

names(ZA7601_namechange_clean)

# reassign into df_list_filtered
df_list_filtered[["ZA7601"]] <- ZA7601_namechange_clean


# rename variables in list ------------------------------------------------
# NOTE: the renaming part is not finished!!!

df_name_changed_list <- list()

for (data_name in names(df_list_filtered)) {
  current_df <- df_list_filtered[[data_name]]
  print(data_name)
  # set value labels as variable names
  colnames(current_df) <- colnames(label_to_colnames(current_df))
  # exclude any variables containing "TCC" (Turkish Cypriot Community)
  current_df <- select(current_df, -contains("TCC"))
  # simplify colnames
  colnames(current_df) <- sub("IMPORTANT ISSUES ", "", colnames(current_df))
  colnames(current_df) <- gsub(": ", "_", colnames(current_df))
  colnames(current_df) <- sub(" \\(.*", "", colnames(current_df))

  # rename variables
  current_df <- current_df %>% 
   rename_at(vars(contains("3166")), ~"isocntry") %>% 
   rename_at(vars(contains("CNTRY_ECONOMIC")), ~"CNTRY_ECONOMIC") %>% 
   rename_at(vars(contains("CNTRY_RISING")), ~"CNTRY_INFLATION") %>%
   rename_at(vars(contains("CNTRY_HEALTH")), ~"CNTRY_HEALTH") %>%
   rename_at(vars(contains("CNTRY_CYPRUS")), ~"CNTRY_CYPRUS") %>%
   rename_at(vars(contains("CNTRY_GOVERNMENT")), ~"CNTRY_GOV_DEBT") %>%
   rename_at(vars(contains("CNTRY_EDUCATION")), ~"CNTRY_EDUCATION") %>%
   rename_at(vars(contains("CNTRY_THE ENVIRONMENT")), ~"CNTRY_ENVIRONMENT") %>%
   rename_at(vars(contains("CNTRY_ENERGY")), ~"CNTRY_ENERGY") %>%
   rename_at(vars(contains("PERS_ECONOMIC")), ~"PERS_ECONOMIC") %>%
   rename_at(vars(contains("PERS_RISING")), ~"PERS_INFLATION") %>%
   rename_at(vars(contains("PERS_CYPRUS")), ~"PERS_CYPRUS") %>%
   rename_at(vars(contains("PERS_FINANCIAL")), ~"PERS_FINANCIAL") %>% 
   rename_at(vars(contains("PERS_EDUCATION")), ~"PERS_EDUCATION") %>%
   rename_at(vars(contains("PERS_THE ENVIRONMENT")), ~"PERS_ENVIRONMENT ") %>%
   rename_at(vars(contains("PERS_WORKING")), ~"PERS_WORKING") %>%
   rename_at(vars(contains("PERS_LIVING")), ~"PERS_LIVING") %>%
   rename_at(vars(contains("EU_ECONOMIC")), ~"EU_ECONOMIC") %>%
   rename_at(vars(contains("EU_RISING")), ~"EU_INFLATION") %>%
   rename_at(vars(contains("EU_INFLUENCE")), ~"EU_INFLUENCE") %>%
   rename_at(vars(contains("EU_MEMBER")), ~"EU_MEMBER") %>%
   rename_at(vars(contains("EU_ENERGY")), ~"EU_ENERGY") %>%
   rename_at(vars(contains("EU_THE ENVIRONMENT")), ~"EU_ENVIRONMENT") 

  # lower case
  colnames(current_df) <- tolower(colnames(current_df))

  # save to new list
  
  df_name_changed_list[[data_name]] <- current_df
}


# Calculate percentages of "yes" in each variable by country ------------
calculate_percentages <- function(data_list){
  perc_list <- list()
  
  # Iterate over the data frames
  for (data_name in names(data_list)) {
    print(data_name)
    clean_dataset <- data_list[[data_name]]
    
    # add n by country
    cntry_size <- clean_dataset %>%
      group_by(isocntry) %>%
      summarise(n = n())
    cntry_dataset <- left_join(cntry_size, clean_dataset, by = "isocntry")
    
    # calculate percentages of "yes" for each variable
    perc_dataset <- cntry_dataset %>%
      group_by(isocntry) %>%
      summarize_at(vars(starts_with("cntry")), ~ sum(.x == 1) / n())
    
    # Append the result to the results list
    perc_list[[data_name]] <- perc_dataset
  }
  return(perc_list)
}

perc_list <- calculate_percentages(df_name_changed_list)


# aggregate by wave -----------------------------------------------------
create_aggregate_df <- function(data_list, item_list){
  new_df <- data.frame()
  for(data_name in names(data_list)){
    print(data_name)
    
    # calculate percentages of "yes" for each variable
    temp_df <- data_list[[data_name]] %>%
      summarize_at(vars(starts_with("cntry")), ~ sum(.x == 1, na.rm = TRUE) / n()) %>%
      mutate(study_number = data_name) 
    
    # add data frame together
    new_df <- bind_rows(new_df, temp_df)
  }
  
  # Select only the "study_number" and "fieldwork_year" columns from the issue_items data frame
  item_list_select <- item_list %>% distinct(., study_number, .keep_all = TRUE) %>% select(study_number, fieldwork_year)
  
  # Join the current data frame with the selected columns of issue_items data frame
  merged_df <- merge(new_df, item_list_select, by = "study_number", all = TRUE) %>% 
    relocate(study_number, fieldwork_year)
  
  return(merged_df)
}
df_aggregate <- create_aggregate_df(df_name_changed_list, issue_items)


