## -------------------------------------------------------------------------- ##
## Script name: Make excel of important issues
## Script description:
## 
## -------------------------------------------------------------------------- ##
## Author: Gayoung Son
## Email:  gayoungson0@gmail.com
## Date: 2023-01-24
##
## --------------------------------------------*
## set working directory
setwd("C:/Users/HOME/Desktop/CDS/eurobarometer")

## --------------------------------------------*
## set options 
options(scipen = 6, digits = 4)

## --------------------------------------------*
## load packages
# source("somefile/somemainrfile.R")
library(tidyverse)



# Prepare List ------------------------------------------------------------
# I fed text-davinci the text from the table in the link below, and easily wrote the code for the issue_items data frame, so that I didnt have to type everything manually.

# make list of items in most important issues
# make data frame with study number, wave number, year and question numbers (https://www.gesis.org/en/eurobarometer-data-service/search-data-access/eb-trends-trend-files/list-of-trends/polit-issues-national#%281%29)
issue_items <- data.frame(study_number = c("0628", "1318", "1715", "1750", "1751", "2491", "3640", "3904", "3938", "4056", "4229", "4411", "4414", "4506", "4507", "4526", "4528", "4530", "4565", "4744", "4819", "4971", "4973", "4994", "5234", "5449", "5481", "5567", "5612", "5685", "5689", "5876", "5913", "5928", "5932", "5964", "5998", "6643", "6694", "6788", "6863", "6928", "6963", "7489", "7562", "7576", "7601", "7649", "7780", "7783", "7848"),
                          eurobarometer = c("ECS73", "19", "30", "31", "31A", "41.1", "57.2", "59.1", "60.1", "61", "62.0", "63.4", "64.2", "65.2", "65.3", "66.1", "66.3", "67.2", "68.1", "69.2", "70.1", "71.1", "71.3", "72.4", "73.4", "74.2", "75.3", "76.3", "77.3", "78.1", "79.3", "80.1", "81.2", "81.4", "82.3", "83.1", "83.3", "84.3", "85.2", "86.2", "87.3", "88.3", "89.1", "90.3", "91.2", "91.5", "92.3", "93.1", "94.3", "95.3", "96.3"),
                          fieldwork_month= c("9-10", "3-4", "10-11", "3-4", "7", "6-7", "4-6", "3-4", "10-11", "2-3", "10-11", "5-6", "10-11", "3-5", "5-6", "9-10", "11-12", "4-5", "9-11", "3-5", "10-11", "1-2", "6-7", "10-11", "5", "11-12", "5", "11", "5", "11", "5", "11", "3", "5-6", "11", "2-3", "5", "11", "5", "11", "5", "11", "3", "11", "3", "6-7", "11-12", "7-8", "2-3", "6-7", "1-2"),
                          fieldwork_year = c(1973, 1983, 1988, 1989, 1989, 1994, 2002, 2003, 2003, 2004, 2004, 2005, 2005, 2006, 2006, 2006, 2006, 2007, 2007, 2008, 2008, 2009, 2009, 2009, 2010, 2010, 2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019, 2019, 2019, 2020, 2021, 2021, 2022),
                          item = c("Q1", "Q271B_2", "Q123", "Q413/416/419", "Q216/218", "Q47", "Q2", "Q5", "Q26", "Q27", "Q33", "QA26", "QA30", "QA28", "QD1", "QA23", "QA26a/b", "QA18", "QA6", "QA6", "QA8/A9", "QA5/A6", "QA4/A5", "QA5/A6", "QA7/A8", "QA6/A7/A8", "QA6/A7/A8", "QA6/A7/A8", "QA7/A8/A9", "QA5/A6/A7", "QA6/A7/A8", "QA4/A5/A6", "QA4/A5/A6", "QA4/A5/A6", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA1/QA2", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5", "QA3/A4/A5"),
                          variable = c("V20-V30", "V200", "", "V300-V311", "V303-V326", "V173-V175", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                          stringsAsFactors = FALSE)

# split items into separate rows
issue_items <- issue_items %>% 
  mutate(item = strsplit(as.character(item), "/")) %>% 
  unnest(item)

# add strings to splitted items
issue_items[5, "item"] <- "Q416"
issue_items[6, "item"] <- "Q419"
issue_items[8, "item"] <- "Q218"
issue_items[21, "item"] <- "QA26b"
issue_items <- issue_items %>%
  mutate(item = case_when(str_detect(item, "^A") ~ paste0("Q", item), TRUE ~ item))

# make items lowercase
issue_items$item <- tolower(issue_items$item)

# add prefix "ZA" to study numbers
issue_items$study_number <- paste0("ZA", issue_items$study_number)

# write to excel
# write_xlsx(issue_items, "description/issue_items.xlsx")