# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP). 
# We also specify the dataset used to fit the MASC model and create new variables where required.

# Author(s): Amanda Holzer(1), Arzie Bajrami(1), Rui Mata(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
#install.packages("brms")
library(brms)
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
#install.packages("loo")
library(loo)
#install.packages("bayesplot")
library(bayesplot)
#install.packages("tidybayes")
library(tidybayes)
#install.packages("posterior")
library(posterior)
library(here)
#install.packages("janitor")
library(janitor)

color_scheme_set("teal")

# FUNCTIONS ----------------------------------------------------------------

source("helper_functions.R")

#path
setwd(here())
getwd()

#data
df <- read_csv("data/final.csv")

#cleaning columns names
df <- clean_names(df)


selected_columns <- select(df, x1, author, paper_title, study_design, risk_1:risk_5, 
                           intervention_yesno_1:intervention_yesno_5, 
                           exposure_yesno_1:exposure_yesno_5, health, nature, crime, finance, 
                           nuclear, political, social, 
                           correlation_results_1_1:correlation_results_1_8, correlation_results_1:correlation_results_5, 
                           icc_results_1_1, test_retest_interval_1:test_retest_interval_5, 
                           type_participants_1:type_participants_5, sample_category_1:sample_category_5, 
                           age_category_1:age_category_5, country_1:country_5, sample_size_1:sample_size_5, 
                           prop_female_1:prop_female_5, mean_age_1:mean_age_5, sd_age_1:sd_age_5)

  
# Attempt to pivot the selected columns
df_pivoted <- selected_columns %>%
  mutate_all(as.character()) %>%
  mutate_if(is.logical, as.character) %>%
  mutate_if(is.double, as.character) %>%
  pivot_longer(-c(x1, author, paper_title, study_design),  # Columns to exclude from pivot
               names_to = "var",
               values_to = "val")

# Filter out rows with NA values in the val column
df_filtered <- df_pivoted %>%
  filter(!is.na(val))

# Separate only the rows with "correlation_results" prefix in the var column
df_filtered_separated <- df_filtered %>%
  filter(str_detect(var, "correlation_results")) %>%
  separate(var, into = c("correlation_name", "correlation_value"), sep = ":")

# Print the first few rows of the separated dataframe to check the result
head(df_filtered_separated)

df_filtered <- df_pivoted %>%
  filter(!is.na(val))

######################################################################
# Separate the var column into correlation_name and correlation_value
df_cor_results <- df_filtered %>%
  filter(str_detect(var, "correlation_results")) %>%
  separate(var, into = c("cor_name"), sep = ":")%>%
  rename(cor_val = val)

df_risk <- df_filtered %>%
  filter(str_detect(var, "risk")) %>%
  separate(var, into = c("risk_name"), sep = ":") %>%
  rename(risk_val = val)%>%
  select(-author, -paper_title, -study_design)

df_interval <- df_filtered %>%
  filter(str_detect(var, "test_retest_interval")) %>%
  separate(var, into = c("interval_name"), sep = ":") %>%
  rename(interval_val = val)%>%
  select(-author, -paper_title, -study_design)

df_size <- df_filtered %>%
  filter(str_detect(var, "sample_size")) %>%
  separate(var, into = c("sample_name"), sep = ":") %>%
  rename(sample_val = val)%>%
  select(-author, -paper_title, -study_design)

df_study <- df_filtered %>%
  filter(str_detect(var, "study")) %>%
  separate(var, into = c("study_name"), sep = ":") %>%
  rename(study_val = val)%>%
  select(-author, -paper_title)


merged_df <- left_join(df_cor_results, df_risk, by = "x1")

merged_df <- left_join(merged_df, df_interval, by = "x1")

merged_df <- left_join(merged_df, df_size, by = "x1")

#this looks weird-----------------

###### do it again like previously and try to follow alexandras comments

# Attempt to pivot the selected columns
df_1 <- selected_columns %>%  mutate_all(as.character()) %>%
  mutate_if(is.logical, as.character) %>%
  mutate_if(is.double, as.character) %>%
  pivot_longer(-c(x1, author, paper_title, study_design),  # Columns to exclude from pivot
               names_to = "var",
               values_to = "val")

# Print out the first few rows of df_pivoted to check the result
head(df_pivoted)

# Filter out rows with NA values in the val column
df_2 <- df_1 %>%
  filter(!is.na(val))

#Alexandra: So already based on this data structure you can run a very simple (intercept only without weights) version of MASC.

#######



