


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
df <- read_csv("Scoping_review/data/secondary/df_finale.csv")


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


df_final$mean_age_1 <- as.numeric(df_final$mean_age_1)
# Create a scatterplot
ggplot(df_final, aes(x = mean_age_1, y = 0)) +
  geom_point(position = position_jitter(width = 0.1), color = "blue") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Scatterplot of Mean Age", x = "Mean Age", y = "Range")

typeof(df_final$mean_age_1)

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





########Characteristic of Sample. Absolutly useless-----------------

# Filter out rows with NA and counts <= 2 in the 'sample_category_1' column
df_filtered <- df_final %>% 
  filter(!is.na(sample_category_1)) %>%
  group_by(sample_category_1) %>%
  filter(n() > 2) %>%
  ungroup()

# Get counts for each category and reorder the factor levels
category_counts <- df_filtered %>% count(sample_category_1)
df_filtered$sample_category_1 <- factor(df_filtered$sample_category_1, levels = category_counts$sample_category_1[order(-category_counts$n)])

# Calculate median and mean based on counts
median_value <- median(category_counts$n)
mean_value <- mean(category_counts$n)

# Plot using ggplot with a bar plot for categorical data and rainbow color scheme
plot <- ggplot(df_filtered, aes(x = sample_category_1, fill = sample_category_1)) +
  geom_bar() +
  labs(title = "Bar Plot of sample_category_1", x = "Sample Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Add horizontal lines for median and mean
plot <- plot + geom_hline(yintercept = median_value, color = "red", linetype = "dashed", size = 1)  # Median line
plot <- plot + geom_hline(yintercept = mean_value, color = "blue", linetype = "dashed", size = 1)  # Mean line

# Annotate median and mean values
plot <- plot + annotate("text", x = 0.5, y = c(1.1 * median_value, 1.1 * mean_value),
                        label = c(paste("Median = ", round(median_value, 2)), paste("Mean = ", round(mean_value, 2))),
                        color = c("red", "blue"), size = 3, hjust = 0)

# Print the plot
print(plot)








# Filter out rows with NA and counts <= 2 in the 'sample_category_1' column
df_filtered <- df_final %>% 
  filter(!is.na(sample_category_1)) %>%
  group_by(sample_category_1) %>%
  filter(n() > 2) %>%
  ungroup()

# Get counts for each category and reorder the factor levels
category_counts <- df_filtered %>% count(sample_category_1)
df_filtered$sample_category_1 <- factor(df_filtered$sample_category_1, levels = category_counts$sample_category_1[order(-category_counts$n)])

# Plot using ggplot with a bar plot for categorical data and rainbow color scheme
ggplot(df_filtered, aes(x = sample_category_1)) +
  geom_bar(fill = viridis(10, option = "D")) +
  labs(title = "Bar Plot of sample_category_1", x = "Sample Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

library(ggplot2)
library(tidyr)

####domains plot
# Function to create a single histogram for counts of 1
create_combined_histogram <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'validation', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the histogram using ggplot2
  ggplot(df_long, aes(x = variable)) +
    geom_bar(stat = "count", fill = "skyblue", color = "black") +
    labs(x = "Categories", y = "Counts", title = "domains")
}

# Apply the function to df_final
create_combined_histogram(df_final)


##ad to the domain the study_design 
# Function to create a single histogram for counts of 1
create_combined_histogram <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'validation', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the histogram using ggplot2
  ggplot(df_long, aes(x = variable, fill = study_design)) +
    geom_bar(stat = "count", position = "stack", color = "black") +
    labs(x = "Categories", y = "Counts", title = "Domains") +
    scale_fill_manual(values = c("serial cross-sectional" = "lightpink", "longitudinal" = "lightblue"))
}

# Apply the function to df_final
create_combined_histogram(df_final)





# Function to create a single histogram for counts of 1 with rainbow colors
create_combined_rainbow_histogram <- function(df) {
  columns_of_interest <- c('health', 'finance', 'political', 'validation', 'crime', 'nature', 'nuclear', 'social')
  
  # Create a long-format data frame for ggplot
  df_long <- df %>%
    gather(key = "variable", value = "value", columns_of_interest) %>%
    filter(value == 1)  # Filter only rows where the value is 1
  
  # Plotting the histogram using ggplot2 with rainbow colors
  ggplot(df_long, aes(x = variable)) +
    geom_bar(stat = "count", fill = rainbow(length(unique(df_long$variable))), color = "black") +
    labs(x = "Categories", y = "Count of 1", title = "Combined Histogram of 1 Counts in Different Categories (Rainbow Colors)")
}

# Apply the function to df_final
create_combined_rainbow_histogram(df_final)




#####How was risk perception measured? 
###Look at the measured_1 column
library(tidytext)
library(ggplot2)

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



###look at the item number. 
library(tidyverse)

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


library(dplyr)

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
  coord_cartesian(ylim = c(0, 60)) +
  # Display every number on the x-axis
  scale_x_continuous(breaks = unique(summary_df$item_number_numeric))

