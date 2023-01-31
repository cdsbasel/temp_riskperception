## -------------------------------------------------------------------------- ##
## Script name: Eurobarometer Important Issues
## Script description: Analyze trend lines 
## 
## -------------------------------------------------------------------------- ##
## Author: Gayoung Son
## Email:  gayoungson0@gmail.com
## Date: 2023-01-30
##
## --------------------------------------------*
## set working directory
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


# national: read in data ------------------------------------------------------------
import_issues_de <- read_excel("trend_data/MainIssuesNat.xlsx", sheet = 7, na = "-", skip = 5)


# make pretty data --------------------------------------------------------
import_issues_de <- import_issues_de %>% 
  rename(Date = ...1)

# set variable names as labels
col_names <- names(import_issues_de)
import_issues_de <- set_label(import_issues_de, label = col_names)

# rename variable names
import_issues_de <- import_issues_de %>% 
  rename_with(tolower) %>% 
  rename(transport = `public transport`,
         economic = `economic situation`,
         inflation = `rising prices/ inflation`,
         unemployment = `un-employment`, 
         influence = `(our country)'s external influence`,
         govdebt = `government debt`,
         defence = `defence/ foreign affairs`,
         cyprus = `cyprus issue`, 
         health_security = `health and social security`,
         education = `the education system`,
         environment = `the environment, climate and energy issues`,
         other = `other (sp.)`,
         none = `none (sp.)`,
         dk = `don't know`)

# separate date column
import_issues_de <- import_issues_de %>%
  separate(date, sep = "-", into = c("season", "year"), remove = FALSE)

# change variable structure
import_issues_de$season <- as.factor(import_issues_de$season) %>% 
  factor(c("Spr", "Sum", "Aut", "Win"))

import_issues_de$year[import_issues_de$year == "20/21"] <- "20"
import_issues_de$year[import_issues_de$year == "21/22"] <- "21"
import_issues_de$year <- as.numeric(import_issues_de$year)

# add wave numbers
import_issues_de <- import_issues_de %>% 
  mutate(wave = c(1:35)) %>% 
  relocate(wave)

# save as excel
# write_xlsx(import_issues_de, "trend_data/import_issues_nat_de.xlsx")


# personal: read in data ------------------------------------------------------------
import_issues_de <- read_excel("trend_data/MainIssuesPerso.xlsx", sheet = 7, na = "-", skip = 5)


# make pretty data --------------------------------------------------------
import_issues_de <- import_issues_de %>% 
  rename(Date = ...1)

# set variable names as labels
col_names <- names(import_issues_de)
import_issues_de <- set_label(import_issues_de, label = col_names)

# rename variable names
import_issues_de <- import_issues_de %>% 
  rename_with(tolower) %>% 
  rename(economic = `the economic situation in (our country)`,
         inflation = `rising prices/ inflation/ cost of living`,
         unemployment = `un-employment`, 
         finance_household = `the financial situation of your household`,
         health_security = `health and social security`,
         education = `the education system`,
         living_cond = `living conditions`,
         work_cond = `working conditions`,
         defence = `defence/ foreign affairs`,
         environment = `the environment, climate and energy issues`,
         other = `other\r\n(sp.)`,
         none = `none \r\n(sp.)`,
         dk = `don't \r\nknow`)

# separate date column
import_issues_de <- import_issues_de %>%
  separate(date, sep = "-", into = c("season", "year"), remove = FALSE)

# change variable structure
import_issues_de$season <- as.factor(import_issues_de$season) %>% 
  factor(c("Jan", "Feb", "Mar", "Spr", "Sum", "Aut", "Win"))

import_issues_de$year[import_issues_de$year == "20/21"] <- "20"
import_issues_de$date[import_issues_de$date == "Win-20/21"] <- "Win-20"
import_issues_de$year <- as.numeric(import_issues_de$year)

# add wave numbers
import_issues_de <- import_issues_de %>% 
  mutate(wave = c(1:24)) %>% 
  relocate(wave)

# save as excel
# write_xlsx(import_issues_de, "trend_data/import_issues_pers_de.xlsx")

