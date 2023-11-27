###last author: Arzie 
### last date: 14.11.23

#####Prep ------------------
#load packages 
#install.packages("dplyr")
#install.packages("here")
library(dplyr)
library(here)
library(readr)
#install.packages("tidyverse")
library(tidyverse)


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
#looks alright

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
df_edit1 <- df_edit %>%
  transform_interval("test-retest_interval_1") %>%
  transform_interval("test-retest_interval_2") %>%
  transform_interval("test-retest_interval_3") %>%
  transform_interval("test-retest_interval_4") %>%
  transform_interval("test-retest_interval_5")

##temporal_trend_interval
df_edit1 <- df_edit %>%
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

df_edit1 <- df_edit %>%
  transform_interval("mean_difference_interval_1") %>%
  transform_interval("mean_difference_interval_2") %>%
  transform_interval("mean_difference_interval_3") %>%
  transform_interval("mean_difference_interval_4") %>%

####check the warning messages to make sure everything is in place, at the end do it in df_edit and not in df_edit1

