# Description -------------------------------------------------------------

# In this script we preprocess the extracted data.

# Author(s): Amanda Holzer(1), Arzie Bajrami(1), Rui Mata(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

#####Prep ------------------
#load packages 
#install.packages("dplyr")
#install.packages("here")
#install.packages("tidyverse")
library(dplyr)
library(here)
library(readr)
library(tidyverse)


#set working directory
setwd(here())
getwd()


#####import data set-----------------
#import data set
df <- read_csv("data/raw.csv")

#####rename column names ----------------------
#look at variable names
names(df)

#create name list with the old and new names
name_file <- read_csv("analysis/variable_name_list.csv")

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


#add design
df_edit$study_design[is.na(df_edit$study_design) & df$`Covidence #` == "230"] <- "serial-cross sectional"

df_edit$study_design[is.na(df_edit$study_design) & df$`Covidence #` == "534"] <- "longitudinal"

df_edit$study_design[is.na(df_edit$study_design) & df$`Covidence #` == "86"] <- "longitudinal"


#change how often was is measured ("daily" value)
df_edit$times_measured_1[171] <- 540

#Add the new columns for the items numbers

# Install and load the stringr package
#install.packages("stringr")
library(stringr)


df_edit$how_computed_1 <- as.character(df_edit$how_computed_1)
df_edit$how_computed_2 <- as.character(df_edit$how_computed_2)
df_edit$how_computed_3 <- as.character(df_edit$how_computed_3)
df_edit$how_computed_4 <- as.character(df_edit$how_computed_4)
df_edit$how_computed_5 <- as.character(df_edit$how_computed_5)


# Extract the item count and create a new column named 'item_number'
df_edit$item_number_1 <- as.numeric(str_extract(df_edit$how_computed_1, "\\d+"))
df_edit$item_number_2 <- as.numeric(str_extract(df_edit$how_computed_2, "\\d+"))
df_edit$item_number_3 <- as.numeric(str_extract(df_edit$how_computed_3, "\\d+"))
df_edit$item_number_4 <- as.numeric(str_extract(df_edit$how_computed_4, "\\d+"))
df_edit$item_number_5 <- as.numeric(str_extract(df_edit$how_computed_5, "\\d+"))

#Add all the missing item numbers
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Bish"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) &  df_edit$author == "Smith"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Concern and Risk Perception: Effects on Osteoprotective Behaviour "] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Carere"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Accuracy of HIV Risk Perception in East Zimbabwe 2003–2013 "] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Franceschinis"] <- 5
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Schulz"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Maheu"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Stuijfzand"] <- 10
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Pieterse"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Vornanen"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Moustafa"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Kuk"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == " Fear of COVID-19 predicts vaccination willingness 14 months later "] <- 8
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Wagoner"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author ==  "Malt"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Chuo"] <- 4
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Francis"] <- 12
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Kollmann"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Helweg-Larsen"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Helleringer"] <- 3
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Wambua"] <- 9
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Kobbeltved"] <- 3
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Sapp"] <- 4
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Eggers"] <- 4
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Concern and risk perception of osteoporosis and fracture among post-menopausal Australian women: results from the Global Longitudinal Study of Osteoporosis in Women (GLOW) cohort "] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Poulus"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "de Graaff"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Betsch"] <- 4
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Bearth"] <- 3
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$how_computed_1 == "measured using two sub scales, cognitive and affective perception "] <- 8
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == " Rimal"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == " Schumpe"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Millman"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Bränström"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Moodie"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Grant"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Lehto"] <- 25
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Cox"] <- 11
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Lindgren"] <- 14
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Quinn"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Kim"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Canetti"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Johnson"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Armstrong-Carter"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Johnson"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Rockliffe"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Contreras- Yáñez"] <- ""
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Kim"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Thomas"] <- 7
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Sorvali"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Shoots- Reinhard"] <- 5
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Risk perception and smoking behavior in medically ill smokers: a prospective study "] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Older marijuana users’ marijuana risk perceptions: associations with marijuana use patterns and marijuana and other substance use disorders "] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == " Relationships between changes in HIV risk perception and condom use in East Zimbabwe 2003–2013: population-based longitudinal analyses "] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Cooper"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Salloum"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Lin"] <- 16
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Oyenubi"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Social Networks and HIV/AIDS Risk Perceptions "] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Measuring risk perceptions of skin cancer: Reliability and validity of different operationalizations "] <- 14
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "Relationships between risk-taking behaviour and subsequent risk perceptions "] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$paper_title == "How a Nuclear Power Plant Accident Influences Acceptance of Nuclear Power: Results of a Longitudinal Study Before and After the Fukushima Disaster "] <- 3
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Ullrich-Kleinmanns"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Elad-Strenger"] <- 6
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Tenkorang"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Walthouwer"] <- 12
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Russo"] <- 1
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Suzuki"] <- 2
df_edit$item_number_1[is.na(df_edit$item_number_1) & df_edit$author == "Grevenstein"] <- 3

df_edit <- df_edit %>%
  mutate(item_number_1 = ifelse(item_number_1 == 0, NA, item_number_1))

library(dplyr)

df_edit <- df_edit %>%
  mutate(
    item_number_1 = ifelse(grepl("single item", measured_1, ignore.case = TRUE), 1, item_number_1),
    item_number_2 = ifelse(grepl("single item", measured_2, ignore.case = TRUE), 1, item_number_2),
    item_number_3 = ifelse(grepl("single item", measured_3, ignore.case = TRUE), 1, item_number_3),
    item_number_4 = ifelse(grepl("single item", measured_4, ignore.case = TRUE), 1, item_number_4),
    item_number_5 = ifelse(grepl("single item", measured_5, ignore.case = TRUE), 1, item_number_5)
  )


#new column for intervention yes/no (1/2)
df_edit$intervention_yesno_1 <- ifelse(!is.na(df_edit$intervention_1) & df_edit$intervention_1 != "", 1, 0)
df_edit$intervention_yesno_2 <- ifelse(!is.na(df_edit$intervention_2) & df_edit$intervention_2 != "", 1, 0)
df_edit$intervention_yesno_3 <- ifelse(!is.na(df_edit$intervention_3) & df_edit$intervention_3 != "", 1, 0)
df_edit$intervention_yesno_4 <- ifelse(!is.na(df_edit$intervention_4) & df_edit$intervention_4 != "", 1, 0)
df_edit$intervention_yesno_5 <- ifelse(!is.na(df_edit$intervention_5) & df_edit$intervention_5 != "", 1, 0)


#new column for exposure yes/no (1/2)
df_edit$exposure_yesno_1 <- ifelse(!is.na(df_edit$exposure_1) & df_edit$exposure_1 != "", 1, 0)
df_edit$exposure_yesno_2 <- ifelse(!is.na(df_edit$exposure_2) & df_edit$exposure_2 != "", 1, 0)
df_edit$exposure_yesno_3 <- ifelse(!is.na(df_edit$exposure_3) & df_edit$exposure_3 != "", 1, 0)
df_edit$exposure_yesno_4 <- ifelse(!is.na(df_edit$exposure_4) & df_edit$exposure_4 != "", 1, 0)
df_edit$exposure_yesno_5 <- ifelse(!is.na(df_edit$exposure_5) & df_edit$exposure_5 != "", 1, 0)

#change the column temporal analysis 1-5 to 1 and 0
df_edit$temporal_analysis_1 <- ifelse(df_edit$temporal_analysis_1 == "yes", 1, 0)
df_edit$temporal_analysis_2 <- ifelse(df_edit$temporal_analysis_2 == "yes", 1, 0)
df_edit$temporal_analysis_3 <- ifelse(df_edit$temporal_analysis_3 == "yes", 1, 0)
df_edit$temporal_analysis_4 <- ifelse(df_edit$temporal_analysis_4 == "yes", 1, 0)
df_edit$temporal_analysis_5 <- ifelse(df_edit$temporal_analysis_5 == "yes", 1, 0)





##### add the new domains health, finance, nature, crime and nuclear

# Create a new column 'health' based on conditions in 'domain'
df_edit$health <- as.integer(grepl("\\b(health|cancer|drugs|cigarettes)\\b", df_edit$domain, ignore.case = TRUE))


######data quality---------------------------
###y#spellings, right values

##prop_female
columns_to_check <- c("prop_female_1", "prop_female_2", "prop_female_3", "prop_female_4", "prop_female_5")

out_of_range <- apply(df_edit[, columns_to_check], 2, function(x) any(x < 0 | x > 1))

cat(ifelse(any(out_of_range), paste("Columns", names(out_of_range[out_of_range]), "have values outside [0, 1]"), "No columns with values outside [0, 1]"), "\n")

#no values over 0 and 1 in prop_female

##risk
columns_to_check_risk <- c("risk_1", "risk_2", "risk_3", "risk_4", "risk_5")

unique_spellings <- lapply(df_edit[, columns_to_check_risk], unique)

# Display unique spellings for each column
for (i in seq_along(unique_spellings)) {
  cat("Unique spellings in", columns_to_check_risk[i], ":", toString(unique_spellings[[i]]), "\n")
}

#Unique spellings in risk_1 : risk, worry, concern, threat
#Unique spellings in risk_2 : worry, NA, concern, risk, threat
#Unique spellings in risk_3 : NA, risk, worry
#Unique spellings in risk_4 : NA, risk
#Unique spellings in risk_5 : NA, worry

#seems to be alright 

##country
columns_to_check_country <- c("country_1", "country_2", "country_3", "country_4", "country_5")

unique_countries <- lapply(df_edit[, columns_to_check_country], unique)

# Display unique values for each column
for (i in seq_along(unique_countries)) {
  cat("Unique values in", columns_to_check_country[i], ":", toString(unique_countries[[i]]), "\n")
}

# Replace specified values with "uk"
df_edit <- df_edit %>%
  mutate_at(vars(columns_to_check_country), ~ ifelse(. %in% c("united kingdom", "uk", "the united kingdom"), "uk", .))

# Replace specified values with "usa"
df_edit <- df_edit %>%
  mutate_at(vars(columns_to_check_country), ~ ifelse(. %in% c("usa", "united states of america", "the united states of america"), "usa", .))

# Replace specified values with "netherlands"
df_edit <- df_edit %>%
  mutate_at(vars(columns_to_check_country), ~ ifelse(. %in% c("netherlands", "the netherlands"), "netherlands", .))

df_edit$country_1[52] <- NA

###
#columns_to_check_country <- c("country_1", "country_2", "country_3", "country_4", "country_5")

#unique_countries <- unique(unlist(df_edit[, columns_to_check_country]))

# Display unique values
#cat("Unique values in columns", paste(columns_to_check_country, collapse = ", "), ":", toString(unique_countries), "\n")


#remove column comparison_significant 
columns_to_remove_cs <- c("comparison_significant_1", "comparison_significant_2", "comparison_significant_3", "comparison_significant_4", "comparison_significant_5")

# Remove specified columns
df_edit <- df_edit[, !(names(df_edit) %in% columns_to_remove_cs), drop = FALSE]

##study design 
# Display unique values in the "study_design" variable
unique_study_design <- unique(df_edit$study_design)
cat("Unique values in 'study_design':", toString(unique_study_design), "\n")

df_edit <- df_edit %>%
  mutate_at(vars(study_design), ~ ifelse(. %in% c("serial-cross sectional", "serial cross-sectional"), "serial cross-sectional", .))

##measured
# Specify the columns to check
columns_to_check_measured <- c("measured_1", "measured_2", "measured_3", "measured_4", "measured_5")

# Display unique values for each column
for (column in columns_to_check_measured) {
  unique_values <- unique(df_edit[[column]])
  cat("Unique values in", column, ":", toString(unique_values), "\n")
}

#set "questionnaire" to "scale" 
df_edit[, columns_to_check_measured] <- lapply(df_edit[, columns_to_check_measured], function(x) ifelse(x == "questionnaire", "scale", x))

#every value is now single item, scale or NA with the exeption of one value: audiotaped, hypothetical stories

##study

# Specify the columns to check
columns_to_check_study <- c("study_1", "study_2", "study_3", "study_4", "study_5")

# Display unique values for each column
for (column in columns_to_check_study) {
  unique_values <- unique(df_edit[[column]])
  cat("Unique values in", column, ":", toString(unique_values), "\n")
}


##how_analyzed

# Specify the columns to check
columns_to_check_how_analyzed <- c("how_analyzed_1", "how_analyzed_2", "how_analyzed_3", "how_analyzed_4", "how_analyzed_5")

# Display unique values for each column
for (column in columns_to_check_how_analyzed) {
  unique_values <- unique(df_edit[[column]])
  cat("Unique values in", column, ":", toString(unique_values), "\n")
}


#mean difference
df_edit <- df_edit %>%
  mutate_at(vars(columns_to_check_how_analyzed), ~ ifelse(. %in% c("mean difference", "mean differences"), "mean difference", .))

#temporal trend
df_edit <- df_edit %>%
  mutate_at(vars(columns_to_check_how_analyzed), ~ ifelse(. %in% c("temporal trend", "time trend"), "temporal trend", .))


##remove unwanted variables in temporal_trend_result_1
# Specify values to remove
values_to_remove <- c("0.29", "moderater stable (r= 0.5 - 0.7)")

# Replace specified values with NA in temporal_trend_result_1
df_edit$temporal_trend_result_1 <- replace(df_edit$temporal_trend_result_1, df_edit$temporal_trend_result_1 %in% values_to_remove, NA)

# Change the value to "not significant" in mean_difference_result_1 for row 7, was weird...
df_edit$mean_difference_result_1[7] <- "not significant"

##change interval to days for all the relevant columns 

# Display unique values in test-retest_interval_1
unique_values <- unique(df_edit$`test-retest_interval_1`)

cat("Unique values in 'test-retest_interval_1':", toString(unique_values), "\n")

##type_participant 
# List of column names
type_participants_columns <- c("type_participants_1", "type_participants_2", "type_participants_3", "type_participants_4", "type_participants_5")

# Display unique values in each type participant column
for (column in type_participants_columns) {
  unique_values <- unique(df_edit[, column])
  cat("Unique values in", column, ":", toString(unique_values), "\n")
}

df_edit <- df_edit %>%
  mutate_at(all_of(type_participants_columns), ~ ifelse(. %in% c("laypeople", "layeople", "159"), "laypeople", .))

df_edit$type_participants_1[235] <- "laypeople"

#age_category
# List of column names
age_category_columns <- c("age_category_1", "age_category_2", "age_category_3", "age_category_4", "age_category_5")

# Display unique values in each age category column
for (column in age_category_columns) {
  unique_values <- unique(df_edit[, column])
  cat("Unique values in", column, ":", toString(unique_values), "\n")
}

# Replace specific values in age_category_1 to age_category_5
df_edit <- df_edit %>%
  mutate_at(vars(all_of(age_category_columns)), 
            ~ ifelse(. %in% c("adult", "adults\n\n", "laypeople", "older adults and younger adults"), "adults",
                     ifelse(. %in% c("adolescent", "adolescents"), "adolescents",
                            ifelse(. == "adolescents and young adults", "young adults", .))))


####change interval columns--------------------

#weird values
df_edit$"test-retest_interval_1"[1] <- NA  
df_edit$"test-retest_interval_1"[17] <- "15 days" 
df_edit$"test-retest_interval_1"[26] <- "425 days"
#df_edit$"test-retest_interval_1"[56] <- " days"  

transform_interval <- function(df, column_name) {
  df %>%
    separate(!!sym(column_name), into = c("number", "unit"), sep = " ") %>%
    mutate(number = as.numeric(number)) %>%
    mutate(!!sym(column_name) := case_when(
      unit %in% c("days", "day") ~ number * 1,
      unit %in% c("weeks", "week") ~ number * 7,
      unit %in% c("months", "month") ~ number * 30,
      unit %in% c("years", "year") ~ number * 365
    )) %>%
    select(-number, -unit)
}

##test-retest_interval
df_edit <- df_edit %>%
  transform_interval("test-retest_interval_1") %>%
  transform_interval("test-retest_interval_2") %>%
  transform_interval("test-retest_interval_3") %>%
  transform_interval("test-retest_interval_4") %>%
  transform_interval("test-retest_interval_5")

##temporal_trend_interval
df_edit <- df_edit %>%
  transform_interval("temporal_trend_interval_1") %>%
  transform_interval("temporal_trend_interval_2") %>%
  transform_interval("temporal_trend_interval_3") %>%
  transform_interval("temporal_trend_interval_4") %>%
  transform_interval("temporal_trend_interval_5")

##mean_difference_interval
#weird values
df_edit$"mean_difference_interval_1"[117] <- "605 days" 
df_edit$"mean_difference_interval_1"[119] <- "605 days"  
df_edit$"mean_difference_interval_1"[138] <- NA  
df_edit$"mean_difference_interval_1"[154] <- "368.5 days"

df_edit <- df_edit %>%
  transform_interval("mean_difference_interval_1") %>%
  transform_interval("mean_difference_interval_2") %>%
  transform_interval("mean_difference_interval_3") %>%
  transform_interval("mean_difference_interval_4") %>%
  transform_interval("mean_difference_interval_5")

####check the warning messages to make sure everything is in place, at the end do it in df_edit and not in df_edit1

# # Check if the same rows have values for the specified sets of variables
# # Create subsets of the data frames with the relevant columns
# columns_to_check <- c(
#   paste0("test-retest_interval_", 1:5),
#   paste0("temporal_trend_interval_", 1:5),
#   paste0("mean_difference_interval_", 1:5)
# )
# 
# subset_df_edit <- df_edit[, columns_to_check]
# subset_df_edit1 <- df_edit1[, columns_to_check]
# 
# # Create a logical matrix indicating differences
# differences_matrix <- subset_df_edit != subset_df_edit1
# 
# # Find the rows where differences exist
# differing_rows <- which(rowSums(differences_matrix) > 0)
# 
# # Display differing rows
# if (length(differing_rows) == 0) {
#   print("The rows with values for the specified sets of variables are the same.")
# } else {
#   print("The following rows have differences:")
#   print(differing_rows)
# }




##### add the new domains health, finance, nature, crime and nuclear

#create a new column finance based on conditions in domain
df_edit$finance <- as.integer(grepl("\\b(financial|financial|workers|deployment,)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column nature based on conditions in domain
df_edit$nature <- as.integer(grepl("\\b(natural|pollution|floods|climate|air|pollution|cyclones| forest| water|environmental|agriculture|insect|forests|water,|wildfires|earthquakes|environment,)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column crime based on conditions in domain
df_edit$crime <- as.integer(grepl("\\b(crime|speeding|disturbance|road|terrorism,)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column nuclear based on conditions in domain
df_edit$nuclear <- as.integer(grepl("\\b(radiation|radiation,|pollution|power|nuclear|electromagnetic,)\\b", df_edit$domain, ignore.case = TRUE))


# Create a new dataframe for better editing 
new_df <- data.frame(
  test_retest_results_1 = df_edit$`test-retest_result_1`
)

new_df$ICC_results_1 <- NA
new_df$correlation_results_1 <- NA
new_df$ICC_results_1.1 <- NA
new_df[, paste0("correlation_results_1.", 1:8)] <- NA


# create new column ICC_results_1 and correlation_results_1 in new_df
new_df$ICC_results_1 <- df_edit$`test-retest_result_1`
new_df$correlation_results_1 <- df_edit$`test-retest_result_1`


# replace 'correlation_results_1'-row, with icc= or icc = or icc with NA
new_df$correlation_results_1[grep("icc[[:space:]]*=[[:space:]]*[0-9]+", new_df$correlation_results_1, ignore.case = TRUE)] <- NA


# delete in the row 'ICC_results_1'all the double numbers of the correlation row
new_df$ICC_results_1[!is.na(new_df$correlation_results_1)] <- NA

# replace
new_df$ICC_results_1[216] <- 0.42


#Clean up the column ICC_results_1 and column ICC_results_1.1
new_df$ICC_results_1.1[17] <- "0.82"
new_df$ICC_results_1[17] <- "0.78"
new_df$ICC_results_1.1[209] <- "0.705"
new_df$ICC_results_1[209] <- "0.877"
new_df$ICC_results_1.1[179] <- "0.82"
new_df$ICC_results_1[179] <- "0.75"
new_df$ICC_results_1 <- gsub("[^-0-9.,]+", "", new_df$ICC_results_1)


#Clean up the column correlation_results_1 and column correaltion_results_1.1-1.8
new_df$correlation_results_1[216] <- 0.87
new_df$correlation_results_1[179] <- "0.51"
new_df$correlation_results_1.1[179] <- "0.58"
new_df$correlation_results_1[43] <- "0.49"
new_df$correlation_results_1.1[43] <- "0.66"

new_df$correlation_results_1[163] <- "0.49"
new_df$correlation_results_1.1[163] <- "0.59"

new_df$correlation_results_1[227] <- "0.57"
new_df$correlation_results_1.1[227] <- "0.46"
new_df$correlation_results_1.2[227] <- "0.58"

new_df$correlation_results_1[176] <- "0.22"
new_df$correlation_results_1.1[176] <- "0.6"

new_df$correlation_results_1[101] <- "0.47"
new_df$correlation_results_1.1[101] <- "0.58"

new_df$correlation_results_1[32] <- "0.54"
new_df$correlation_results_1.1[32] <- "0.34"
new_df$correlation_results_1.2[32] <- "0.5"

new_df$correlation_results_1[197] <- "0.13"
new_df$correlation_results_1.1[197] <- "0.96"

new_df$correlation_results_1[66] <- "0.49"
new_df$correlation_results_1.1[66] <- "0.31"

new_df$correlation_results_1[241] <- "0.68"
new_df$correlation_results_1.1[241] <- "0.90"

new_df$correlation_results_1[127] <- NA
new_df$correlation_results_1[230] <- NA

new_df$correlation_results_1[193] <- "0.47"
new_df$correlation_results_1.1[193] <- "0.54"
new_df$correlation_results_1.2[193] <- "0.52"
new_df$correlation_results_1.3[193] <- "0.38"

new_df$correlation_results_1[63] <- "0.49"
new_df$correlation_results_1.1[63] <- "0.43"

new_df$correlation_results_1[99] <- "0.85"
new_df$correlation_results_1.1[99] <- "0.88"

new_df$correlation_results_1[220] <- "0.92"
new_df$correlation_results_1.1[220] <- "0.95"
new_df$correlation_results_1.2[220] <- "0.87"

new_df$correlation_results_1[198] <- "0.44"
new_df$correlation_results_1.1[198] <- "0.63"

new_df$correlation_results_1[240] <- "0.69"
new_df$correlation_results_1.1[240] <- "0.72"

new_df$correlation_results_1[8] <- "0.70"
new_df$correlation_results_1.1[8] <- "0.74"

new_df$correlation_results_1[83] <- "0.58"
new_df$correlation_results_1.1[83] <- "0.55"
new_df$correlation_results_1.2[83] <- "0.45"
new_df$correlation_results_1.3[83] <- "0.42"
new_df$correlation_results_1.4[83] <- "0.43"
new_df$correlation_results_1.5[83] <- "0.38"
new_df$correlation_results_1.6[83] <- "0.42"
new_df$correlation_results_1.7[83] <- "0.42"
new_df$correlation_results_1.8[83] <- "0.54"

new_df$correlation_results_1[215] <- "-0.3784"
new_df$correlation_results_1[161] <- "-0.19"

new_df$correlation_results_1[207] <- "0.52"
new_df$correlation_results_1.1[207] <- "0.57"
new_df$correlation_results_1.2[207] <- "0.54"
new_df$correlation_results_1.3[207] <- "0.26"
new_df$correlation_results_1.4[207] <- "0.61"
new_df$correlation_results_1.5[207] <- "0.68"

new_df$correlation_results_1[77] <- "0.295"
new_df$correlation_results_1.1[77] <- "0.224"

new_df$correlation_results_1[42] <- "0.55"
new_df$correlation_results_1[203] <- "0.815"
new_df$correlation_results_1[70] <- "0.74"
new_df$correlation_results_1[74] <- "0.71"
new_df$correlation_results_1[102] <- "0.68"
new_df$correlation_results_1[75] <- "0.58"
new_df$correlation_results_1[122] <- "0.42"
new_df$correlation_results_1[76] <- "0.97"
new_df$correlation_results_1[132] <- "0.88"
new_df$correlation_results_1[13] <- "0.87"
new_df$correlation_results_1[218] <- "0.86"
new_df$correlation_results_1[221] <- "0.71"
new_df$correlation_results_1[201] <- "0.68"
new_df$correlation_results_1[205] <- "0.63"
new_df$correlation_results_1[237] <- "0.97"
new_df$correlation_results_1[186] <- "0.83"
new_df$correlation_results_1[238] <- "0.91"
new_df$correlation_results_1[27] <- "-0.6188"
new_df$correlation_results_1[39] <- "0.815"



#add the dataframe back
new_df_subset <- new_df[, !grepl("test-retest", names(new_df))]

df_edit <- cbind(df_edit, new_df_subset)

#create a second new df for better editing
# Select the columns 'test-retest_results_2', 'test-retest_results_3', 'test-retest_results_4', 'test-retest_results_5'
new_dataframe <- data.frame(
  test_retest_results_1 = df_edit$`test-retest_result_2`,
  test_retest_results_1 = df_edit$`test-retest_result_3`,
  test_retest_results_1 = df_edit$`test-retest_result_4`,
  test_retest_results_1 = df_edit$`test-retest_result_5`
)

#rename the columns to correlation_results_2-5
names(new_dataframe) <- c("correlation_results_2", "correlation_results_3", "correlation_results_4", "correlation_results_5")

new_dataframe$correlation_results_2[215] <- "-0.2666"
new_dataframe$correlation_results_3[215] <- "-0.6188"
new_dataframe$correlation_results_2[23] <- NA


# Remove all non-numeric characters from the entire dataframe
new_dataframe[] <- lapply(new_dataframe, function(x) gsub("[^0-9.,-]+", "", x))

# Add the relevant columns from 'new_dataframe' back to 'df_edit'
df_edit <- cbind(df_edit, new_dataframe[, c("correlation_results_2", "correlation_results_3", "correlation_results_4", "correlation_results_5")])

df_edit$correlation_results_1[89] <- "0.78"
df_edit$ICC_results_1[89] <- "0.83"

#add all the missing sample sizes. 
df_edit$sample_size_1[24] <- 359
df_edit$sample_size_1[28] <- 138
df_edit$sample_size_1[70] <- 366
df_edit$sample_size_1[118] <- 15431
df_edit$sample_size_1[170] <- 686
df_edit$sample_size_1[177] <- 2705
df_edit$sample_size_1[235] <- 159

##check if all the sample sizes are there for relevant cases
# Count NA values in sample_size_1 to sample_size_5 when temporal_analysis_1 is 1
na_counts_temporal_1 <- sum(df_edit$temporal_analysis_1 == 1 & is.na(df_edit$sample_size_1))
na_counts_temporal_2 <- sum(df_edit$temporal_analysis_1 == 1 & is.na(df_edit$sample_size_2))
na_counts_temporal_3 <- sum(df_edit$temporal_analysis_1 == 1 & is.na(df_edit$sample_size_3))
na_counts_temporal_4 <- sum(df_edit$temporal_analysis_1 == 1 & is.na(df_edit$sample_size_4))
na_counts_temporal_5 <- sum(df_edit$temporal_analysis_1 == 1 & is.na(df_edit$sample_size_5))

# Display the counts of NA's for each column when temporal_analysis_1 is 1
print(na_counts_temporal_1)
print(na_counts_temporal_2)
print(na_counts_temporal_3)
print(na_counts_temporal_4)
print(na_counts_temporal_5)

##DOMAINS
# Replace 'domain' with the actual column name in your data frame
unique_values <- unique(df_edit$domain)

# Print or use the unique values as needed
print(unique_values)


values_with_other <- df_edit$domain[grep("other", df_edit$domain, ignore.case = TRUE)]

# Print or inspect the values
print(values_with_other)

#edit domains 
#df_edit <- df_edit %>%
#  mutate(
#    health = ifelse(grepl("road safety intervention|affect/cognition|technological (cell site deployment)|risk communication and decision-making during emergencies", tolower(domain)), 1, 0),
#    nature = ifelse( grepl("climate change|forests", tolower(domain)),1, 0),
#    crime = ifelse( grepl("speeding|sexual assault", tolower(domain)),1, 0),
#    nuclear = ifelse(grepl("hazardous waste site", tolower(domain)),1, 0))

#DOMAIN: SOCIAL
#df_edit <- df_edit %>%
#  mutate(
#    social = ifelse(grepl("social anxiety|occupational|social media and online privacy #attitudes|social risk and prosocial tendencies|aggressive intergroup action", tolower(domain)),1,0))


#DOMAIN: POLITICAL AND VALIDATION
#df_edit <- df_edit %>%
#  mutate(
#    political = ifelse(
#      grepl("trust in politics|political", tolower(domain)),1,0))


df_edit$social <- as.integer(grepl("\\b(social anxiety|occupational|social media and online privacy attitudes|social risk and prosocial tendencies|aggressive intergroup action)\\b", df_edit$domain, ignore.case = TRUE))

# Create a new column 'health' based on conditions in 'domain'
df_edit$health <- as.integer(grepl("\\b(health|cancer|drugs|cigarettes|road safety intervention|affect/cognition|technological|safety and hazard recognition|risk communication and decision-making during emergencies |validation)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column finance based on conditions in domain
df_edit$finance <- as.integer(grepl("\\b(financial|financial|workers|deployment,)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column nature based on conditions in domain
df_edit$nature <- as.integer(grepl("\\b(natural|pollution|floods|climate|air|pollution|cyclones| forest| water|environmental|agriculture|insect|forests|water,|wildfires|earthquakes|environment,)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column crime based on conditions in domain
df_edit$crime <- as.integer(grepl("\\b(crime|speeding|disturbance|road|terrorism|assault)\\b", df_edit$domain, ignore.case = TRUE))

#create a new column nuclear based on conditions in domain
df_edit$nuclear <- as.integer(grepl("\\b(radiation|radiation,|pollution|power|nuclear|electromagnetic|hazardous waste site)\\b", df_edit$domain, ignore.case = TRUE))

df_edit$political <- as.integer(grepl("trust in politics|political", df_edit$domain, ignore.case = TRUE))



####characteristics_sample:
columns_to_check <- c("characteristics_sample_1", "characteristics_sample_2", "characteristics_sample_3", "characteristics_sample_4", "characteristics_sample_5")

# Create a list to store unique values for each column
unique_values_list <- lapply(df_edit[, columns_to_check], unique)

# Print or use the unique values as needed
for (i in seq_along(columns_to_check)) {
  column_name <- columns_to_check[i]
  unique_values <- unique_values_list[[i]]
  cat("Unique values in", column_name, ":", toString(unique_values), "\n")
}


categorize_samples <- function(characteristics_sample) {
  case_when(
    grepl("cancer counselee|healthcare workers|food handlers and managers|military sailors|offshore workers|correctional officers|doctors|nurses and other medical professionals|nurses|construction workers|counselees|breast cancer genetic counselee", characteristics_sample, ignore.case = TRUE) ~ "Workers",
    grepl("students|undergraduates|high school students|female cancer survivors|rheumatoid arthritis outpatients", characteristics_sample, ignore.case = TRUE) ~ "Students",
    grepl("female students|women|only female", characteristics_sample, ignore.case = TRUE) ~ "Only Female",
    grepl("pregnant women|pregnant|pregnant women and their male partners", characteristics_sample, ignore.case = TRUE) ~ "Pregnant Women",
    grepl("people with history of breast cancer|individuals with a family history of pancreatic cancer or brca2 mutation carriers|clinical sample|older adults with the 2 diabetes mellitus|adult women with primary diagnosis of stage 0-iiia bbc|no previous cancer diagnoses|received bc surgery and in a committed relationship|patients|cardiac patients|older adults with gad|women with ductal carcinoma in situ|patients with psoriasis|breast cancer patients", characteristics_sample, ignore.case = TRUE) ~ "Clinical/Patients",
    grepl("smokers|current or former smokers|former smokers", characteristics_sample, ignore.case = TRUE) ~ "Smokers",
    grepl("several data sets|study 2007|study 2018|panel data|participants and data from a panel study|sample consisted of two case studies|one from previous study and one freshly recruited|two samples used|50 general population|25 patients", characteristics_sample, ignore.case = TRUE) ~ "Multiple Samples",
    TRUE ~ as.character(characteristics_sample)
  )
}

df_edit[paste0("sample_category_", 1:5)] <- lapply(1:5, function(i) {
  categorize_samples(df_edit[[paste0("characteristics_sample_", i)]])
})


#lower case letters for the new columns 
df_edit <- df_edit %>%
  mutate_at(
    vars(sample_category_1:sample_category_5),
    funs(tolower(.)))

##UNITS ASSESSED
columns_to_check <- c("units_assessed_1", "units_assessed_2", "units_assessed_3", "units_assessed_4", "units_assessed_5")

# Create a list to store unique values for each column
unique_values_list <- lapply(df_edit[, columns_to_check], unique)

# Print or use the unique values as needed
for (i in seq_along(columns_to_check)) {
  column_name <- columns_to_check[i]
  unique_values <- unique_values_list[[i]]
  
  cat("Unique values in", column_name, ":", toString(unique_values), "\n")}


df_edit <- df_edit %>%
  mutate_at(
    vars(units_assessed_1:units_assessed_5),
    funs(ifelse(
      grepl("likert|5- point scale|not serious at all – very serious| the responses were coded as accurate or an underestimate| 5-point categorical scale|4-point response categories| yes, no, don't know| 8-point liker scale|five-point scale:|great|moderate|small|no chance at all", ., ignore.case = TRUE),"likert scale",.)))


df_edit <- df_edit %>%
  mutate_at(
    vars(units_assessed_1:units_assessed_5),
    funs(ifelse(
      grepl("0 \\(no risk\\) to 100 \\(very high risk\\)|0% -100%|1 in 100 (1%) to inevitable (100%) --> then converted into risk categories| total scores range from 6 to 24 where a higher score indicates higher levels of\ncancer’s worry| 0 to 100%|0-100%|0% - 100%|percentage scale|0% to 100% visual scale|0%-100%|1 in 100 \\(1%\\) to inevitable \\(100%\\)|10 cm visual analogue scale \\(vas\\)|ranging from ‘not at all likely|10-point visual analog scale|0 \\(no concern\\) to 100 \\(very high concern\\)", ., ignore.case = TRUE),
      "range",.)))


#fix stubborn cases
df_edit$units_assessed_2[3] <- "range"
df_edit$units_assessed_1[168] <- "range"
df_edit$units_assessed_1[134] <- "range"
df_edit$units_assessed_1[135] <- "range"
df_edit$units_assessed_1[239] <- "range"
df_edit$units_assessed_1[67] <- "range"
df_edit$units_assessed_1[142] <- "range"
df_edit$units_assessed_1[29] <- "range"
df_edit$units_assessed_1[144] <- "range"
df_edit$units_assessed_1[15] <- "likert scale"
df_edit$units_assessed_1[107] <- "range"


#correct and add the item numbers
df_edit$item_number_1[123] <- 8
df_edit$item_number_1[124] <- 8
df_edit$item_number_1[126] <- NA
df_edit$item_number_1[131] <- 4
df_edit$item_number_1[141] <- NA
df_edit$study_design[239] <- "serial cross-sectional"
df_edit$study_design[155] <- "serial cross-sectional"
df_edit$units_assessed_1[72] <- "likert scale"
df_edit$units_assessed_1[35] <- "likert scale"
df_edit$units_assessed_1[132] <- "likert scale"
df_edit$units_assessed_1[113] <- "likert scale"

#make one final dataset for the analysis 
df_final <- df_edit[c(
  "author", "paper_title", "publication_year", "data_availability",
  "study_design", "risk_1", "risk_2", "risk_3", "risk_4", "risk_5",
  "measured_1", "measured_2", "measured_3", "measured_4", "measured_5",
  "units_assessed_1", "units_assessed_2", "units_assessed_3", "units_assessed_4", "units_assessed_5",
  "item_number_1", "item_number_2", "item_number_3", "item_number_4", "item_number_5",
  "times_measured_1", "times_measured_2", "times_measured_3", "times_measured_4", "times_measured_5",
  "study_1", "study_2", "study_3", "study_4", "study_5",
  "intervention_yesno_1", "intervention_yesno_2", "intervention_yesno_3", "intervention_yesno_4", "intervention_yesno_5",
  "exposure_yesno_1", "exposure_yesno_2", "exposure_yesno_3", "exposure_yesno_4", "exposure_yesno_5",
  "domain", "health", "nature", "crime", "finance", "nuclear", "political", "social",
  "temporal_analysis_1", "temporal_analysis_2", "temporal_analysis_3", "temporal_analysis_4", "temporal_analysis_5",
  "how_analyzed_1", "how_analyzed_2", "how_analyzed_3", "how_analyzed_4", "how_analyzed_5",
  "correlation_results_1", "correlation_results_1.1", "correlation_results_1.2", "correlation_results_1.3", "correlation_results_1.4",
  "correlation_results_1.5", "correlation_results_1.6", "correlation_results_1.7", "correlation_results_1.8",
  "correlation_results_2", "correlation_results_3", "correlation_results_4", "correlation_results_5",
  "ICC_results_1", "ICC_results_1.1",
  "temporal_trend_result_1", "temporal_trend_result_2", "temporal_trend_result_3", "temporal_trend_result_4", "temporal_trend_result_5",
  "mean_difference_result_1", "mean_difference_result_2", "mean_difference_result_3", "mean_difference_result_4", "mean_difference_result_5",
  "test-retest_interval_1", "test-retest_interval_2", "test-retest_interval_3", "test-retest_interval_4", "test-retest_interval_5",
  "temporal_trend_interval_1", "temporal_trend_interval_2", "temporal_trend_interval_3", "temporal_trend_interval_4", "temporal_trend_interval_5",
  "mean_difference_interval_1", "mean_difference_interval_2", "mean_difference_interval_3", "mean_difference_interval_4", "mean_difference_interval_5",
  "type_participants_1", "type_participants_2", "type_participants_3", "type_participants_4", "type_participants_5",
  "sample_category_1", "sample_category_2", "sample_category_3", "sample_category_4", "sample_category_5",
  "age_category_1", "age_category_2", "age_category_3", "age_category_4", "age_category_5",
  "country_1", "country_2", "country_3", "country_4", "country_5",
  "sample_size_1", "sample_size_2", "sample_size_3", "sample_size_4", "sample_size_5",
  "prop_female_1", "prop_female_2", "prop_female_3", "prop_female_4", "prop_female_5",
  "mean_age_1", "mean_age_2", "mean_age_3", "mean_age_4", "mean_age_5",
  "SD_age_1", "SD_age_2", "SD_age_3", "SD_age_4", "SD_age_5",
  "lowest_age_1", "lowest_age_2", "lowest_age_3", "lowest_age_4", "lowest_age_5",
  "highest_age_1", "highest_age_2", "highest_age_3", "highest_age_4", "highest_age_5"
)]

##check if every row has at least one value in the domains.
columns_to_check <- c('health', 'crime', 'nature', 'finance', 'nuclear', 'political', 'social')

# Convert the selected columns to numeric
df_final[columns_to_check] <- lapply(df_final[columns_to_check], as.numeric)

# Find rows where all specified columns have a value of 0
rows_with_all_zeros <- df_final[apply(df_final[columns_to_check] == 0, 1, all), ]

##it worked, every row has at least one domain
library(openxlsx)
#install.packages("openxlsx")

write.csv(df_final, file = "data/final.csv")
write.xlsx(df_final, file = "data/final.xlsx")


##merge abstract file with final df
df_final_codes <- read.xlsx("data/df_final_codes.xlsx")
df_abstracts_codes <- read_csv("data/df_abstracts_codes.csv")

merged_df <- inner_join(df_abstracts_codes, df_final_codes, by = "codes")

final <- merged_df %>%
  select(-codes, -Title, -Authors, -`Published Year`)

write.csv(final, file = "data/final.csv")

