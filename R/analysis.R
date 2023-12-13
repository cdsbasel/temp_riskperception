


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

# Boxplot
# Convert 'study_design' to factor
df_final$study_design <- factor(df_final$study_design)



ggplot(df_final, aes(x = factor(study_design))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Study Design", x = "2 designs", y = "Count")

ggplot(df_final, aes(x = factor(type_participants_1))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Study Design", x = "2 designs", y = "Count")


# Filter the dataframe for 'experts' and 'laypeople'
df_filtered <- df_final %>%
  filter(type_participants_1 %in% c("experts", "laypeople"))

# Create a bar plot
ggplot(df_filtered, aes(x = factor(type_participants_1))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Study Design", x = "Participant Type", y = "Count")


ggplot(df_final, aes(x = factor(age_category_1))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Study Design", x = "2 designs", y = "Count")

# Filter the dataframe for 'experts' and 'laypeople'
df_filtered1 <- df_final %>%
  filter(age_category_1 %in% c("adults", "adolescents", "children", "older adults"))

# Create a bar plot
ggplot(df_filtered1, aes(x = factor(age_category_1))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "age groups", x = "type", y = "Count")


##### age ------------


df_final$mean_age_1 <- as.numeric(df_final$mean_age_1)


df_final$mean_age_1 <- as.numeric(df_final$mean_age_1)

plot(df_final$mean_age_1, main="Scatter Plot of mean_age_1", xlab="Index", ylab="mean_age_1")

# Order the dataframe by the values in the 'mean_age_1' column
df_final <- df_final[order(df_final$mean_age_1), ]

# Plot a scatter plot
plot(df_final$mean_age_1, main="Scatter Plot of mean_age_1", xlab="Index", ylab="mean_age_1")




# Order the dataframe by the values in the 'mean_age_1' column
df_final <- df_final %>% arrange(mean_age_1)

ggplot(df_final, aes(x = seq_along(mean_age_1), y = mean_age_1)) +
  geom_point() +
  labs(title = "Scatter Plot of mean_age_1", x = "Index", y = "mean_age_1") +
  scale_x_continuous(breaks = seq(1, 100, by = 10), labels = seq(1, 100, by = 10), limits = c(1, 100))

library(viridis)


#####Age plot---------
# Create 10 bins for the x-axis
df_final$bins <- cut(seq_along(df_final$mean_age_1), breaks = 10, labels = FALSE)

ggplot(df_final, aes(x = mean_age_1)) +
  geom_histogram(bins = 10, fill = viridis(10)) +
  scale_fill_viridis_c() +
  labs(title = "Age", x = "mean age", y = "Frequency")



# Calculate median and mean, excluding NA values
median_value <- median(df_final$mean_age_1, na.rm = TRUE)
mean_value <- mean(df_final$mean_age_1, na.rm = TRUE)

ggplot(df_final, aes(x = mean_age_1)) +
  geom_histogram(bins = 10, fill = viridis(10), color = "white", boundary = 0.2) +
  scale_fill_viridis_c() +
  labs(title = "Age", x = "Mean Age", y = "Frequency") +
  # Add vertical lines for median and mean
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +  # Median line
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  # Annotate median and mean values with adjusted y-coordinate
  annotate("text", x = median_value, y = 20, 
           label = paste("Median = ", round(median_value, 2)),
           color = "red", size = 3, vjust = 0) +
  annotate("text", x = mean_value, y = 22, 
           label = paste("Mean = ", round(mean_value, 2)),
           color = "blue", size = 3, vjust = 0) +
  # Limit the y-axis to 25
  coord_cartesian(ylim = c(0, 22))




######### puplation -------------


# Filter out rows with NA in the 'sample_category_1' column
df_filtered <- df_final %>% filter(!is.na(sample_category_1))

# Get counts for each category
category_counts <- df_filtered %>% count(sample_category_1)

# Plot using ggplot with a bar plot for categorical data
ggplot(df_filtered, aes(x = reorder(sample_category_1, -category_counts$n), fill = sample_category_1)) +
  geom_bar() +
  scale_fill_viridis_d() +
  labs(title = "Bar Plot of sample_category_1", x = "Sample Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Filter out rows with NA and counts <= 2 in the 'sample_category_1' column
df_filtered <- df_final %>% 
  filter(!is.na(sample_category_1)) %>%
  group_by(sample_category_1) %>%
  filter(n() > 2) %>%
  ungroup()

# Get counts for each category and reorder the factor levels
category_counts <- df_filtered %>% count(sample_category_1)
df_filtered$sample_category_1 <- factor(df_filtered$sample_category_1, levels = category_counts$sample_category_1[order(-category_counts$n)])

# Plot using ggplot with a bar plot for categorical data
ggplot(df_filtered, aes(x = sample_category_1)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Categories", x = "Sample Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




#### domains-------------


#####How was risk perception measured? -----------
###Look at the measured_1 column

# Convert 'measured_1' to a character vector
df_final$measured_1 <- as.character(df_final$measured_1)

# Remove rows with NAs in 'measured_1'
df_final <- df_final %>% filter(!is.na(measured_1))

# Now, check the structure again
str(df_final$measured_1)

# Count occurrences of each unique row in 'measured_1'
category_counts <- df_final %>%
  count(measured_1) %>%
  arrange(desc(n))

# Plot using ggplot with a bar plot for category counts
ggplot(category_counts, aes(x = reorder(measured_1, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Category Counts", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())  # Remove x-axis title for better appearance

####look at units_assessed_1
# Convert 'units_assessed_1' to a character vector
df_final$units_assessed_1 <- as.character(df_final$units_assessed_1)

# Remove rows with NAs in 'units_assessed_1'
df_final <- df_final %>% filter(!is.na(units_assessed_1))

# Now, check the structure again
str(df_final$units_assessed_1)

# Count occurrences of each unique row in 'units_assessed_1'
units_counts <- df_final %>%
  count(units_assessed_1) %>%
  arrange(desc(n))

# Plot using ggplot with a bar plot for units counts
ggplot(units_counts, aes(x = reorder(units_assessed_1, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Units Assessed Counts", x = "Units Assessed", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())  # Remove x-axis title for better appearance



###item number--------- 

# Convert 'item_number_1' to numeric, handling non-numeric values gracefully
df_final <- df_final %>%
  mutate(item_number_numeric = as.numeric(item_number_1),
         item_number_numeric = ifelse(is.na(item_number_numeric), NA, item_number_numeric))

# Now, check the data type of the new 'item_number_numeric' column
str(df_final$item_number_numeric)

# Calculate median and mean, excluding NA values
median_value <- median(df_final$item_number_numeric, na.rm = TRUE)
mean_value <- mean(df_final$item_number_numeric, na.rm = TRUE)

# Plot
ggplot(df_final, aes(x = item_number_numeric)) +
  geom_histogram(bins = 10, fill = viridis(10), color = "white", boundary = 0.2) +
  scale_fill_viridis_c() +
  labs(title = "Item Number", x = "Item Number", y = "Frequency") +
  # Add vertical lines for median and mean
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +  # Median line
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  # Annotate median and mean values with adjusted y-coordinate
  annotate("text", x = median_value, y = 20, 
           label = paste("Median = ", round(median_value, 2)),
           color = "red", size = 3, vjust = 0) +
  annotate("text", x = mean_value, y = 22, 
           label = paste("Mean = ", round(mean_value, 2)),
           color = "blue", size = 3, vjust = 0) +
  # Limit the y-axis to 25
  coord_cartesian(ylim = c(0, 22))



#with blue bars
ggplot(df_final, aes(x = item_number_numeric)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "white", boundary = 0.2) +
  labs(title = "Item Number", x = "Item Number", y = "Frequency") +
  # Add vertical lines for median and mean
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +  # Median line
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  # Annotate median and mean values with adjusted y-coordinate
  annotate("text", x = median_value, y = 20, 
           label = paste("Median = ", round(median_value, 2)),
           color = "red", size = 3, vjust = 0) +
  annotate("text", x = mean_value, y = 22, 
           label = paste("Mean = ", round(mean_value, 2)),
           color = "blue", size = 3, vjust = 0) +
  # Limit the y-axis to 25
  coord_cartesian(ylim = c(0, 80))

##with points instead of bars
# Assuming df_final is your data frame
summary_df <- df_final %>% 
  group_by(item_number_numeric) %>% 
  summarise(count = n())

# Create a point plot
ggplot(summary_df, aes(x = item_number_numeric, y = count)) +
  geom_point(color = "black") +
  labs(title = "Item Number", x = "Item Number", y = "Frequency") +
  # Add vertical lines for median and mean
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +  # Median line
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  # Annotate median and mean values with adjusted y-coordinate
  annotate("text", x = median_value, y = 20, 
           label = paste("Median = ", round(median_value, 2)),
           color = "red", size = 3, vjust = 0) +
  annotate("text", x = mean_value, y = 22, 
           label = paste("Mean = ", round(mean_value, 2)),
           color = "blue", size = 3, vjust = 0) +
  # Limit the y-axis to 80
  coord_cartesian(ylim = c(0, 60))


summary_df <- df_final %>% 
  group_by(item_number_numeric) %>% 
  summarise(count = n())

# Create a point plot with more details in the description of the x-axis
ggplot(summary_df, aes(x = item_number_numeric, y = count)) +
  geom_point(color = "black") +
  labs(title = "Item Number", x = "Item Number", y = "Frequency") +
  # Add vertical lines for median and mean
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +  # Median line
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  # Annotate median and mean values with adjusted y-coordinate
  annotate("text", x = median_value, y = 20, 
           label = paste("Median = ", round(median_value, 2)),
           color = "red", size = 3, vjust = 0) +
  annotate("text", x = mean_value, y = 22, 
           label = paste("Mean = ", round(mean_value, 2)),
           color = "blue", size = 3, vjust = 0) +
  # Limit the y-axis to 80
  coord_cartesian(ylim = c(0, 60)) +
  # Display every number on the x-axis
  scale_x_continuous(breaks = unique(summary_df$item_number_numeric))


##create one column for time interval------


# Combine columns into a new column
df_final <- df_final %>%
  mutate(time_interval = paste(df_final$`test-retest_interval_1`, df_final$temporal_trend_interval_1, df_final$mean_difference_interval_1, sep = "_"))


# Assuming df_final is your data frame
df_final$time_interval <- as.numeric(gsub("[^0-9]", "", df_final$time_interval))

# Replace non-numeric values with NA
df_final$time_interval[is.na(df_final$time_interval)] <- NA

### create a plot for it. 

# Convert 'time_interval' column to numeric
df_final$time_interval <- as.numeric(df_final$time_interval)

# Create a scatterplot with ggplot2
ggplot(df_final, aes(x = rownames(df_final), y = time_interval)) +
  geom_point(col = "blue", pch = 16) +
  labs(title = "Scatterplot of Time Interval by Row",
       x = "Row Indices", y = "Time Interval")

plot(df_final$time_interval)

print(df_final$time_interval)

# Assuming df_final is your data frame
# Assuming time_interval column is loaded as character and contains numeric values or NAs

library(ggplot2)

# Convert 'time_interval' column to numeric
df_final$time_interval <- as.numeric(df_final$time_interval)

# Order the dataframe by the 'time_interval' column
df_final_ordered <- df_final[order(df_final$time_interval), ]

# Create a scatterplot with ggplot2
ggplot(df_final_ordered, aes(x = seq_along(time_interval), y = time_interval, color = is.na(time_interval))) +
  geom_point(pch = 16) +
  labs(title = "Scatterplot of Time Interval by Row",
       x = "Row Indices", y = "Time Interval") +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  ylim(1, 1100) + xlim(1, 120)






##############Questions----------


####How can the variation in domains be characterized, and what research designs were implemented for studies within each domain, drawing comparisons with papers that investigated temporal trends?

#In the studies exploring temporal trends, what is the observed variation in domains, and how does the choice of research designs for each domain compare to those employed in papers specifically analyzing temporal trends?
  
#To what degree do variations in domains influence the selection of research designs in studies, and how does this compare to the research designs utilized in papers investigating temporal trends?
  


# Function to create a single histogram for counts of 1
create_combined_histogram <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'validation', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    pivot_longer(cols = columns_of_interest, names_to = "variable", values_to = "value") %>%
    group_by(variable, study_design, temporal_analysis_1) %>%
    summarize(count = sum(value == 1))
  
  # Plotting the histogram using ggplot2
  ggplot(df_long, aes(x = variable, y = count, fill = interaction(as.factor(temporal_analysis_1), study_design))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Categories", y = "Counts", title = "Domains") +
    scale_fill_manual(values = c("0.serial cross-sectional" = "lightpink", 
                                 "0.longitudinal" = "pink",  # Same color as the first group
                                 "1.serial cross-sectional" = "blue",
                                 "1.longitudinal" = "darkblue"),  # Same color as the first group
                      name = "Temporal Analysis and Study Design")
}

# Apply the function to df_final
create_combined_histogram(df_final)


# List of columns to process
columns_of_interest <- c('health', 'finance', 'political', 'validation', 'crime', 'nature', 'nuclear', 'social')

# Iterate through each column to create a new column
for (col in columns_of_interest) {
  df_final[[paste0(col, "_1")]] <- ifelse(df_final[[col]] == 1 & df_final$temporal_analysis_1 == 1, 1, 0)
}




# Function to create a single histogram for counts of 1
create_combined_histogram <- function(df) {
  columns_of_interest <- c('health', 'health_1', 'finance','finance_1', 'political', 'political_1', 'validation', 'validation_1','crime', 'crime_1', 'nature', 'nature_1','nuclear', 'nuclear_1','social', 'social_1')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the histogram using ggplot2
  ggplot(df_long, aes(x = variable, fill = study_design)) +
    geom_bar(stat = "count", position = "stack", color = "black") +
    labs(x = "Categories", y = "Counts", title = "Domains") +
    scale_fill_manual(values = c("serial cross-sectional" = "lightpink", "longitudinal" = "lightblue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle and hjust as needed
}

# Apply the function to df_final
create_combined_histogram(df_final)


#####How does the researched population look like and is there a difference between the papers which analyzed the temporal trend?

#To what extent do variations exist in the demographic composition of the researched population, and are discernible differences apparent among papers analyzing temporal trends?
  
#maybe one plot where slide 16 and 17 are combined. Next to each bar is the bar which only includes studies where the temporal trend was analized. 


# Filter out rows with NA and counts <= 2 in the 'sample_category_1' column
df_filtered <- df_final %>% 
  filter(!is.na(sample_category_1)) %>%
  group_by(sample_category_1) %>%
  filter(n() > 2) %>%
  ungroup()

# Get counts for each category and reorder the factor levels
category_counts <- df_filtered %>% count(sample_category_1)
df_filtered$sample_category_1 <- factor(df_filtered$sample_category_1, levels = category_counts$sample_category_1[order(-category_counts$n)])

# Plot using ggplot with a bar plot for categorical data
ggplot(df_filtered, aes(x = sample_category_1)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Categories", x = "Sample Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability






# Filter out rows with NA and counts <= 2 in the 'sample_category_1' column
df_filtered <- df_final %>% 
  filter(!is.na(sample_category_1)) %>%
  group_by(sample_category_1, type_participants_1) %>%
  filter(n() > 2) %>%
  ungroup()

# Get counts for each category and reorder the factor levels
category_counts <- df_filtered %>% count(sample_category_1)
df_filtered$sample_category_1 <- factor(df_filtered$sample_category_1, levels = category_counts$sample_category_1[order(-category_counts$n)])

# Plot using ggplot with a stacked bar plot for categorical data and fill by 'type_participants_1'
ggplot(df_filtered, aes(x = sample_category_1, fill = type_participants_1)) +
  geom_bar(stat = "count", color = "black") +
  labs(title = "Categories", x = "Sample Category", y = "Count") +
  scale_fill_manual(values = c("laypeople" = "lightpink", "experts" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



####From the available data, what indications or patterns emerge regarding the overall trend?
print(df_final$correlation_results_1)

library(ggplot2)

# Assuming df_final is your data frame
# Replace "correlation_results_1" with the actual column name from your data frame

ggplot(df_final, aes(x = correlation_results_1, y = ..density..)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue") +
  labs(title = "Histogram with Density Curve",
       x = "correlation_results_1",
       y = "Density") +
  theme_minimal()

