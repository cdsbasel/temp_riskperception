###last author: Arzie 
### last date: 14.11.23

#####Prep ------------------
#load packages 
#install.packages("dplyr")
#install.packages("here")
library(dplyr)
library(here)
library(readr)

#set working directory
setwd(here())
getwd()


#####import data set-----------------
#import data set
df <- read_csv("Scoping_review/data/raw/raw_data.csv")

#####rename column names ----------------------
#look at variable names
names(df)

#create name list with the old and new names
name_file <- read_csv("Scoping_review/scripts/name_list.csv")

#create column new/old names list
numbers = 1:15
variables = 1:34
new_name_list <- vector(mode='list', length=length(numbers)* length(variables))
old_name_list <- vector(mode='list', length=length(numbers)* length(variables))

for (i in 1:length(numbers)) {
  for (j in 1:length(variables)) {
    order=(j-1) * 15 + i
    new_name_list[[order]] = paste0(name_file[j, 1][[1]], "_", numbers[i])
    old_name_list[[order]] = paste(name_file[j, 2][[1]], numbers[i])
  }
}

#rename the columns with new names
df_rename <- df
for (x in 1:length(new_name_list)) {
  new_name = new_name_list[[x]]
  old_name = old_name_list[[x]]
  names(df_rename)[names(df_rename) == old_name] <- new_name
}

#check the column new names
names(df_rename)


#rename the rest of the columns
df_rename <- df_rename %>%
  rename(
    covidence_number = `Covidence #`,
    study_id = `Study ID`,
    title = `Title`,
    reviewer_name = `Reviewer Name`,
    author = `Last name of the first author`,
    paper_title = `Title of the paper`,
    publication_year = `Year when article was published`,
    aim_study = `Aim of study`,
    data_availability = `Data availability (osf, panel, etc.)`,
    study_design = `What type of design was used?`,
    domain = "What domain is being analyzed?",
    to_be_deleted_1 = `:...11`,
    to_be_deleted_2 = ":...223",
    to_be_deleted_3 = ":...359")

#check if step above worked
names(df_rename)


# Write the data frame to a CSV file
write.csv(df_rename, file = "Scoping_review/data/secondary/df_rename.csv")


####edit columns ------------------ 

# Identify column indices to be removed: to be deleted, every variable form _6 to _15, covidence number, reviewer name, study id and covidence created title
columns_to_remove <- c(grep("_[6-9]|_1[0-5]$", names(df_rename)), 
                       grep("^SE_age_[1-9]|^SE_age_1[0-5]$", names(df_rename)),
                       grep("^intervention_interval_[1-9]|^intervention_interval_[0-5]$", names(df_rename)),
                       grep("^intervention_duration_[1-9]|^intervention_duration_[0-5]$", names(df_rename)),
                       grep("^exposure_interval_[1-9]|^exposure_interval_[0-5]$", names(df_rename)),
                       grep("^exposure_duration_[1-9]|^exposure_duration_[0-5]$", names(df_rename)),
                       which(names(df_rename) %in% c("to_be_deleted_1", "to_be_deleted_2",
                                                     "to_be_deleted_3", "covidence_number",
                                                     "reviewer_name", "study_id", "title")))
##not sure if we should remove intevention_duration_intervention_interval, exposure_duration, exposure_interval???????????

# Remove the identified columns
df_edit <- df_rename[, -columns_to_remove]


#add missing values in "author", "title", "publication_year"
df_edit [5, 1] <- "Malnar"
df_edit [159, 2] <- "Burnout among hospital staff during the COVID-19 pandemic: Longitudinal results from the international Cope-Corona survey study"
df_edit [119, 3] <- 2021

###aim of study was missing for the following papers. This was originally extracted but not selected in consensus.
### add it in df_edit
#30Yarahmandi: This study aimed to develop and validate Health Care Workers’ Concerns in Infectious Outbreaks Scale (HCWCIOS)
df_edit [30, 4] <- "This study aimed to develop and validate Health Care Workers’ Concerns in Infectious Outbreaks Scale (HCWCIOS)"

#59 Martin: The present study explores the change sensitivity of the two constructs of worry and risk perception, and how the two constructs are differentially associated with objective risk factors such as family history of dementia.
df_edit [59, 4] <- "The present study explores the change sensitivity of the two constructs of worry and risk perception, and how the two constructs are differentially associated with objective risk factors such as family history of dementia."


#78 van Genderer: the aim to study trends in KAP of travel risk groups toward prevention of hepatitis A.
df_edit [78, 4] <- "the aim to study trends in KAP of travel risk groups toward prevention of hepatitis A."

#186 Kang: This study investigated the reliability and validity of the Korean version of the Penn State Worry Questionnaire for Children (PSWQ-CK).
df_edit [186, 4] <- "This study investigated the reliability and validity of the Korean version of the Penn State Worry Questionnaire for Children (PSWQ-CK)."


#convert all character columns to lowercase, excluding "author" and "title"
df_edit <- df_edit %>%
  mutate(across(where(is.character) & !matches(c("author", "title")), tolower))


# Write the data frame to a CSV file
write.csv(df_edit, file = "Scoping_review/data/secondary/df_edit.csv")



