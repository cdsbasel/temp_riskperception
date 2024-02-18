


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


# analysis visual -----------------

###plot domains with study design--------

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

## to do list other plots 
#kind of risk (risk, concern, worry)
create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = as.factor(risk_1))) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c(
      "risk" = "darkgreen", "worry" = "seagreen", "concern" = "lightgreen", "threat" = "green"
    ), name = "Risk Categories")  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)




#### how measured (scale, single item, etc.)-----
create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = as.factor(measured_1))) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c(
      "scale" = "darkgreen", "single item" = "seagreen"
    ), name = "Measurement Categories")  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)


#exposure and intervention
create_combined_horizontal_bar <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Create a new factor combining intervention and exposure columns
  df_long$factor_combined <- interaction(df_long$intervention_yesno_1, df_long$exposure_yesno_1)
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = factor_combined)) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c(
      "0.0" = "darkgreen", 
      "0.1" = "seagreen",
      "1.0" = "pink",
      "1.1" = "lightgreen"
    ), name = "Intervention and Exposure (Yes/No)", labels = c(
      "No Intervention, No Exposure",
      "No Intervention, Exposure",
      "Intervention, No Exposure",
      "Intervention, Exposure"
    ))  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar(df_final)



# how analyzed 
# significance and not signifcance. are they domain dependend? 
# country
# sd of age
# highest age and lowest age????
# temporal analysis 
create_combined_horizontal_bar_temporal <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Create a new factor for temporal analysis column
  df_long$factor_combined <- as.factor(df_long$temporal_analysis_1)
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = factor_combined)) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c(
      "0" = "darkgreen", 
      "1" = "seagreen"
    ), name = "Temporal Analysis", labels = c(
      "No Analysis",
      "Analyzed"
    ))  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar_temporal(df_final)

# data availability
# Create a new column data_availability_1
df_final$data_availability_1 <- as.integer(!is.na(df_final$data_availability) & df_final$data_availability != "")

create_combined_horizontal_bar_data <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the horizontal bar plot using ggplot2
  ggplot(df_long, aes(x = variable, fill = as.factor(data_availability_1))) +
    geom_bar(position = "fill", stat = "count", color = "white") +
    coord_flip() +  # Flip the coordinates for a horizontal bar plot
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    labs(x = NULL, y = NULL, title = NULL) +  # Remove axis labels and change title
    scale_fill_manual(values = c(
      "0" = "darkgreen", 
      "1" = "seagreen"
    ), name = "Data Availability", labels = c(
      "No Data",
      "Data Available"
    ))  # Customize fill colors and legend title
}

# Apply the function to df_final
create_combined_horizontal_bar_data(df_final)



####correlation plotting with time interval 

# Create a scatter plot with a correlation line
ggplot(df_final, aes(x = `test-retest_interval_1`, y = correlation_results_1, color = as.factor(health))) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  scale_color_manual(values = c("0" = "black", "1" = "red"), name = "Health") +  # Specify color for health values and set legend title
  theme_minimal() +  # Minimal theme
  labs(x = "Time Interval in days", y = "Correlation Results", title = NULL)


# Create a scatter plot with a correlation line
ggplot(df_final, aes(x = `test-retest_interval_1`, y = correlation_results_1, color = as.factor(health))) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  scale_color_manual(values = c("0" = "black", "1" = "red"), name = "Health") +  # Specify color for health values and set legend title
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Set axis line color
    legend.position = "bottom"  # Position legend at the bottom
  ) +
  labs(x = "Time Interval in days", y = "Correlation Results", title = NULL)



##to do list correlations
# correlation x publishing year
# correlation x time interval
# correlation x domain (look into the subdomains)
# correlation x (group size)
# correlation x data availability (put the da into another plot to look how they influence the points. or to look if the correlation line changes if we only include these ones.) 
# correlation with age. 
#do all the things above also with the icc  




##density plots----- 
###number of studies----

# Function to create separate density plots for each category
create_separate_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the separate density plots using ggplot2
  ggplot(df_long, aes(x = times_measured_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_wrap(~category, scales = "free_y") +  # Separate plots for each category
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank()) +  # Remove grid lines and borders
    labs(x = "Times Measured", y = "Density", title = NULL) +  # Set labels
    xlim(0, 50)  # Set x-axis limits
}

# Apply the function to df_final
create_separate_density_plots(df_final)



###measurment points ----
# Function to create stacked density plots for each category
create_stacked_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = times_measured_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank()) +  # Remove grid lines and borders
    labs(x = "Times Measured", y = "Density", title = NULL) +  # Set labels
    xlim(0, 50)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots(df_final)




# Function to create stacked density plots for each category
create_stacked_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = times_measured_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "Times Measured", y = "Density", title = NULL) +  # Set labels
    xlim(2, 10)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots(df_final)







# Function to create stacked density plots for each category
create_stacked_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = sample_size_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "sample size", y = "Density", title = NULL) +  # Set labels
    xlim(0, 1000)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots(df_final)


###create a density plot for days apart between the measurement------
# Create a new column by combining values from the three columns
df_final <- df_final %>%
  mutate(combined_column = paste(`test-retest_interval_1`, `temporal_trend_interval_1`, `mean_difference_interval_1`, sep = "_"))
# Remove non-numeric characters from the combined_column
df_final$combined_column <- gsub("[^0-9.]", "", df_final$combined_column)

# Convert the combined_column to numeric
df_final$combined_column <- as.numeric(df_final$combined_column)


# Function to create stacked density plots for each category
create_stacked_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = combined_column, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "Time interval", y = "Density", title = NULL) +  # Set labels
    xlim(2, 1000)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots(df_final)


### publishing year---
create_stacked_density_plots <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = publication_year, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "Publication Year", y = "Density", title = NULL)  # Set labels
}

# Apply the function to df_final
create_stacked_density_plots(df_final)



###item number----
create_stacked_density_plots_item <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = item_number_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "Item Number", y = "Density", title = NULL) +  # Set labels
    xlim(1, 10)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots_item(df_final)



###time measured----
create_stacked_density_plots_item <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = times_measured_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "times measured", y = "Density", title = NULL) +  # Set labels
    xlim(1, 10)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots_item(df_final)



###female male----
create_stacked_density_plots_item <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = prop_female_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "female percentage", y = "Density", title = NULL) +  # Set labels
    xlim(0, 1)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots_item(df_final)




###age (age and age category)----
# Convert the column to numeric
df_final$mean_age_1 <- as.numeric(df_final$mean_age_1)
create_stacked_density_plots_item <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "category", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the stacked density plots using ggplot2
  ggplot(df_long, aes(x = mean_age_1, fill = category)) +
    geom_density(alpha = 0.5) +  # Density plot with transparency
    facet_grid(category ~ ., scales = "free_x", switch = "y") +  # Stacked plots with category names on the y-axis
    scale_fill_manual(values = c(
      "health" = "darkgreen", "finance" = "blue", "political" = "purple",
      "crime" = "orange", "nature" = "red", "nuclear" = "brown", "social" = "cyan"
    )) +  # Specify colors for each category
    theme_minimal() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.y = element_blank()) +  # Remove grid lines, borders, and y-axis text
    labs(x = "mean age", y = "Density", title = NULL) +  # Set labels
    xlim(4, 70)  # Set x-axis limits
}

# Apply the function to df_final
create_stacked_density_plots_item(df_final)


#correlation plot (correlation&time interval)----

##create a dataframe with all the correlations for a better overview.---- 


# Combine the correlation results columns into one
correlation_results <- c(df_final$correlation_results_1, df_final$correlation_results_1.1,
                         df_final$correlation_results_1.2, df_final$correlation_results_1.3,
                         df_final$correlation_results_1.4, df_final$correlation_results_1.5,
                         df_final$correlation_results_1.6, df_final$correlation_results_1.7,
                         df_final$correlation_results_1.8)

# Create a dataframe with two columns: correlation_results and test_retest_interval_1
correlation_1 <- data.frame(correlation_results, `test-retest_interval_1` = df_final$`test-retest_interval_1`)

colnames(correlation_1) <- c("correlation_results", "correlation_interval_1")

# Remove rows with NAs from correlation dataframe
correlation_1 <- na.omit(correlation_1)



# Combine the correlation_2 results columns into one
correlation_results <- c(df_final$correlation_results_2)

# Create a dataframe with two columns: correlation_results and test_retest_interval_1
correlation_2 <- data.frame(correlation_results, `test-retest_interval_2` = df_final$`test-retest_interval_2`)

colnames(correlation_2) <- c("correlation_results", "correlation_interval_1")

correlation_2 <- na.omit(correlation_2)



# Combine the correlation_3 results columns into one
correlation_results <- c(df_final$correlation_results_3)

# Create a dataframe with two columns: correlation_results and test_retest_interval_1
correlation_3 <- data.frame(correlation_results, `test-retest_interval_3` = df_final$`test-retest_interval_3`)

colnames(correlation_3) <- c("correlation_results", "correlation_interval_1")

correlation_3 <- na.omit(correlation_3)



# Combine the correlation_4 results columns into one
correlation_results <- c(df_final$correlation_results_4)

# Create a dataframe with two columns: correlation_results and test_retest_interval_1
correlation_4 <- data.frame(correlation_results, `test-retest_interval_4` = df_final$`test-retest_interval_4`)

colnames(correlation_4) <- c("correlation_results", "correlation_interval_1")

correlation_4 <- na.omit(correlation_4)



# Combine the correlation_5 results columns into one
correlation_results <- c(df_final$correlation_results_5)

# Create a dataframe with two columns: correlation_results and test_retest_interval_1
correlation_5 <- data.frame(correlation_results, `test-retest_interval_5` = df_final$`test-retest_interval_5`)

colnames(correlation_5) <- c("correlation_results", "correlation_interval_1")

correlation_5 <- na.omit(correlation_5)



# Assuming correlation_2 has the same structure as correlation (two columns: correlation_results and correlation_interval_1)
correlation <- rbind(correlation_1, correlation_2, correlation_3, correlation_4, correlation_5 )


###plot----

# Plot the two columns against each other with x-axis limited to 800 and a correlation line
ggplot(correlation, aes(x = correlation_interval_1, y = correlation_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add correlation line
  labs(x = "Correlation Interval", y = "Correlation Results") +
  xlim(0, 800)


library(ggplot2)

# Calculate correlation coefficient
correlation_coefficient <- cor(correlation$correlation_interval_1, correlation$correlation_results)

# Plot the two columns against each other with x-axis limited to 800 and a correlation line
ggplot(correlation, aes(x = correlation_interval_1, y = correlation_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add correlation line
  annotate("text", x = 400, y = max(correlation$correlation_results) * 0.9, 
           label = paste("Correlation coefficient:", round(correlation_coefficient, 2)), 
           color = "blue", size = 4) +  # Add correlation coefficient annotation
  labs(x = "Correlation Interval", y = "Correlation Results") +
  xlim(0, 800)


####minimal indication that over time the correlation coefficient decreases. 


















