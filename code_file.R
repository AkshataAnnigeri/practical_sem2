library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(readxl)      # For reading Excel files

sheets <- excel_sheets("C:/Users/aksha/OneDrive/Desktop/survey.xlsx")
print(sheets)

# Load the Data
data1 <- read_excel("C:/Users/aksha/OneDrive/Desktop/survey.xlsx", sheet = "Sheet1")
print(data1)

data2 <- read_excel("C:/Users/aksha/OneDrive/Desktop/survey.xlsx", sheet = "Sheet 2")  
print(data2)

# Convert the Timestamp column in data2 to POSIXct
data2$Timestamp <- as.POSIXct(as.Date("1899-12-30") + data2$Timestamp * 86400, origin = "1970-01-01")

# Combine the datasets into one
data <- bind_rows(data1, data2, .id = "source")

# Calculate the percentage of missing values in each column
missing_percentages <- colMeans(is.na(data)) * 100
print(missing_percentages)
#No columns with significant percentage of missing values

# Function to categorize gender
categorize_gender <- function(gender) {
  gender <- tolower(gender)
  if (gender %in% c("male", "m")) {
    return("Male")
  } else if (gender %in% c("female", "f")) {
    return("Female")
  } else {
    return("Other")
  }
}

# Apply categorization function to Gender column
data$Gender <- sapply(data$Gender, categorize_gender)

# Remove rows with tech_company entry as "No"
cleaned_data <- data %>% 
  filter(tech_company == "Yes") %>%
  select(-tech_company)  # Remove the tech_company column

# Print summary of cleaned data
summary(cleaned_data)

# Define the allowed values
allowed_values <- c("26-100", "100-500", "500-1000", "More than 1000")

# Replace values not in the allowed list with NA
cleaned_data <- data %>%
  mutate(no_employees = ifelse(no_employees %in% allowed_values, no_employees, NA_character_))

# Print the updated column
print(cleaned_data$no_employees)

# Remove rows with Age below 15 or negative values.
cleaned_data <- data %>% 
  filter(Age >= 15)

# Replace values with NA
cleaned_data <-cleaned_data %>%
  mutate(seek_help = ifelse(seek_help %in% c("not sure", "Not sure", "Don't know"), NA, seek_help))

# Check the unique values in seek_help column after replacement
unique(cleaned_data$seek_help)

# Print summary of cleaned data
summary(cleaned_data)

#Data Visualization
# Bar plot for gender distribution
gender_plot <- ggplot(cleaned_data, aes(x = Gender)) +
  geom_bar(fill = "skyblue", alpha = 0.8) +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  theme_minimal()

# Display the plot
print(gender_plot)

#understanding people who are seeking help

# Pie chart for seek_help distribution
seek_help_plot <- ggplot(cleaned_data, aes(x = "", fill = seek_help)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Seek Help Distribution",
       fill = "Seek Help") +
  theme_minimal()

# Display the plot
print(seek_help_plot)

#understanding family_history
ggplot(data, aes(x = family_history)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Family History",
       x = "Family History",
       y = "Count") +
  theme_minimal()


