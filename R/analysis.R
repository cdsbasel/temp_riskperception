


#####Prep ------------------
#load packages 
#install.packages("dplyr")
#install.packages("here")
library(dplyr)
library(here)
library(readr)
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#set working directory
setwd(here())
getwd()


#####import data set-----------------
#import data set
df_final <- read_csv("data/final.csv")


###### analysis -----------------

###plot domain with study design--------

# Function to create a single horizontal bar plot for counts of 1
create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Arrange levels of 'variable' by count in ascending order
  df_long$variable <- factor(df_long$variable, levels = names(sort(table(df_long$variable), decreasing = FALSE)))
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = value, y = variable, fill = study_design)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = NA) +  # Reverse the stacking order
    scale_fill_manual(values = c("serial cross-sectional" = "red", "longitudinal" = "blue")) +
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank()) +  # Remove grid lines and borders
    labs(x = "Number of Studies", y = NULL, title = NULL) +  # Remove y-axis label and title
    xlab("Number of Studies")  # Set x-axis label
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)



###plot domain with measurement points-----
#create measurement category with 2, 3, and 3+

# Assuming df_final already exists and you want to modify it
df_final <- df_final %>%
  mutate(measure = ifelse(times_measured_1 == 2, "2",
                          ifelse(times_measured_1 == 3, "3", "3+")))

# Function to create a single horizontal bar plot for counts of 1
create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Arrange levels of 'variable' by count in ascending order
  df_long$variable <- factor(df_long$variable, levels = names(sort(table(df_long$variable), decreasing = FALSE)))
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = value, y = variable, fill = measure)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = NA) +  # Reverse the stacking order
    scale_fill_manual(values = c("2" = "darkgreen", "3" = "seagreen", "3+" = "lightgreen")) +
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank()) +  # Remove grid lines and borders
    labs(x = "Number of Studies", y = NULL, title = NULL) +  # Remove y-axis label and title
    xlab("Number of Studies")  # Set x-axis label
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)




##make the bars a 100 percent. And not count anymore

create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Set the order of the levels for the 'measure' column
  df_long$measure <- factor(df_long$measure, levels = c("3+", "3", "2"))
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = as.factor(measure))) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c("2" = "darkgreen", "3" = "seagreen", "3+" = "lightgreen"), name = "Measurement Points")  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)


