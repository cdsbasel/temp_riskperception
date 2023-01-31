## -------------------------------------------------------------------------- ##
## Script name: Make Eurobarometer Codebook
## Script description: Do not run
## 
## -------------------------------------------------------------------------- ##
## Author: Gayoung Son
## Email:  gayoungson0@gmail.com
## Date: 2023-01-24
##
## --------------------------------------------*
## set working directory to desktop
setwd("C:/Users/HOME/Desktop/CDS/eurobarometer")

## --------------------------------------------*
## set options 
options(scipen = 6, digits = 4)

## --------------------------------------------*
## load packages
# source("somefile/somemainrfile.R")
library(tidyverse)
library(writexl)
library(readxl)
library(sjlabelled)


# Read item overview of important issues ----------------------------------
issue_items <- read_excel("description/issue_items.xlsx")

# Read in overview of waves -----------------------------------------------
library(docxtractr)
overview_waves <- read_docx("description/standardEB_overview.docx") %>% 
  # extract table
  docx_extract_tbl() %>% 
  # clean colnames
  janitor::clean_names()

# Read in all waves -------------------------------------------------------

# get a list of all .sav files in the folder
sav_files <- list.files(path = "datasets", pattern = "*.sav", full.names = TRUE)

# make an empty list for all data frames
df_list <- list()

# Iterate over the list of .sav files 
# NOTE: this takes a few minutes
# create a loop to read each of the files
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

# Make codebook ----------------------------------------------------------
codebook_df <- data.frame()

for (i in 1:length(df_list)) {
  # get dataset names
  data_name <- names(df_list)[i]
  dataframe_codebook <- df_list[[i]]
  # get labels
  value_labels <- enframe(get_label(dataframe_codebook))
  # get levels
  value_levels <- enframe(get_labels(dataframe_codebook, values = "n")) %>% rename("levels" = "value") 
  codebook <- left_join(value_labels, value_levels, by = "name") %>% 
    mutate(study_number = data_name) 
  # join all data frames together
  codebook_df <- bind_rows(codebook_df, codebook)
}

# merge with overview of waves
overview_waves_select <- overview_waves %>%
  rename(study_number = gesis_study_id,
         eurobarometer = standard_and_eu_trend_surveys)

merged_codebook <- merge(codebook_df, overview_waves_select, by = "study_number", all = TRUE)

# clean variables, add new ones
codebook_df0 <-  merged_codebook %>% 
  # separate fieldwork by month and year
  separate(fieldwork, sep = " ", into = c("fieldwork_month", "fieldwork_year")) %>%
  mutate(fieldwork_year = as.numeric(fieldwork_year)) %>% 
  # specify survey type
  mutate(survey_type = ifelse(standard_report == "X", "standard", 
                              ifelse(special_topic == "X", "special", "subset"))) %>% 
  # rename columns
  rename(item_variable = name,
         item_label = value,
         item_level = levels) 

# fill in missing eurobarometer number and dates from issue_items
issue_items_select <- issue_items %>% 
  select(-c("item", "variable"))

cleaned_codebook <- left_join(codebook_df0, issue_items_select, by = "study_number", all = TRUE) %>% 
  mutate(eurobarometer = coalesce(eurobarometer.x, eurobarometer.y),
         fieldwork_month = coalesce(fieldwork_month.x, fieldwork_month.y),
         fieldwork_year = coalesce(fieldwork_year.x, fieldwork_year.y)) %>% 
  select(-c("eurobarometer.x", "eurobarometer.y", "fieldwork_month.x", "fieldwork_month.y", "fieldwork_year.x", "fieldwork_year.y")) %>% 
  # remove duplicate columns
  distinct(study_number, item_variable, .keep_all = TRUE)

# add columns and reorder
cleaned_codebook <-  cleaned_codebook %>% 
  add_column(item_code = NA,
             item_new_code = NA,
             item_type = NA,
             trends_issues = NA,
             trends_threats = NA,
             comment = NA) %>% 
  # extract study number
  mutate(study_digits = as.numeric(str_extract(study_number, "\\d+"))) %>% 
  relocate(c("study_number", "study_digits", "eurobarometer", "fieldwork_month", "fieldwork_year", "item_variable", "item_code", "item_label", "item_level", "item_new_code", "item_type", "trends_issues", "trends_threats", "standard_report", "subset", "special_topic", "survey_type", "comment")) 

names(cleaned_codebook)


# organize variables --------------------------------------------------------------

codebook_df1 <- cleaned_codebook 

# Extract first alphanumeric string from "item_label" and store in "item_code" only for studies equal to or ess than ZA5481.
codebook_df1$item_code <- ifelse(codebook_df1$study_digits <= 5481, 
                                 str_extract(codebook_df1$item_label, "[A-Z]{1,2}\\d+[^\\s]*"), 
                                 codebook_df1$item_variable)

# If blank, fill with values from item_variable
codebook_df1$item_code <- ifelse(codebook_df1$study_digits <= 5481 & is.na(codebook_df1$item_code), 
                                 codebook_df1$item_variable, 
                                 codebook_df1$item_code)


# Unnest levels column ----------------------------------------------------
# Use the `sapply()` function to apply the `paste0()` function to each element of the list column
codebook_df2 <- codebook_df1

codebook_df2$item_level <- sapply(codebook_df1$item_level, function(x) {
  if(is.null(x)) return(x)
  option <- paste0(names(x), " = ")
  x <- map2(option, x, paste0)
  as.character(unlist(x, use.names = FALSE))
})

# make a new df with unnested columns
codebook_df2_select <- codebook_df2 %>% 
  unnest_longer(item_level) %>% 
  group_by(study_number, item_variable) %>% 
  summarise(item_new_level = str_c(item_level, collapse = " "), .groups = "keep")

# join data frames
codebook_draft <- left_join(codebook_df2, codebook_df2_select, by = c("study_number", "item_variable")) %>% 
  relocate(item_new_level, .after = item_new_code)

# export as excel
# write_csv(codebook_draft, file = "codebook/codebook_draft.csv")

# reread in codebook 
# codebook_draft <- read_csv("codebook/codebook_draft.csv", 
#                            col_types = cols(item_level = col_character(), 
#                                             item_new_code = col_character(), 
#                                             item_type = col_character(), 
#                                             trends_issues = col_character(), 
#                                             trends_threats = col_character(), 
#                                             standard_report = col_character(), 
#                                             comment = col_character()))


# Filter codebook by "important issues" and ISO country code ------------------
# make codebook into list
make_codebook_list <- function(data_list, codebook_dataframe){
  codebook_list <- list()
  for (data_name in names(data_list)) {
    print(data_name)
    codebook_list[[data_name]] <- codebook_dataframe %>% 
      filter(study_number == data_name)
  }
  return(codebook_list)
}
codebook_list <- make_codebook_list(df_list, codebook_final)

# define function and loop
filter_codebook <-  function(codebook_list) {
  codebook_list_filtered <- list()
  for (data_name in names(codebook_list)) {
    print(data_name)
    filtered_codebook <- codebook_list[[data_name]] %>%
      # filter items which contain the following strings in the labels
      filter(str_detect(item_label, "ISO 3166") |
               str_detect(item_label, "IMPORTANT ISSUES") |
               str_detect(item_label, "IMPORTANT NAT ISSUES") |
               str_detect(item_label, "IMPORT ISSUES") | 
               str_detect(item_label, "^SPLIT"))
    codebook_list_filtered[[data_name]] <- filtered_codebook
  }
  return(codebook_list_filtered)
}

codebook_list_filtered <- filter_codebook(codebook_list)
# NOTE: items before 2002 have different questions and answer options, and therefore are not included in this filtered codebook. Manual categorization is needed.

# make into data frame
codebook_df_filtered <- bind_rows(codebook_list_filtered)

# write_csv(codebook_df_filtered, "codebook/codebook_filtered_draft.csv")
