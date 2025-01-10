# Load the data (replace with the actual file path on your system)
data <- read.csv("C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/fall_risk_T2.csv")

# View the first few rows and column names
head(data)
str(data)
# Load necessary libraries
library(dplyr)
library(lubridate)

# Convert date columns to Date format
#data <- data %>%
# mutate(
 #   DateofBirth = as.Date(DateOfBirth, format = "%m/%d/%Y"),
  #  FirstDiagnosisStartDate = as.Date(FirstDiagnosisStartDate, format = "%m/%d/%Y"),
   # FallDate = as.Date(FallDate, format = "%m/%d/%Y")
 # )#
## Verify the conversion
#str(data %>% select(DateofBirth, FirstDiagnosisStartDate, FallDate))
# set missing values to NA explicitly for clarity. 
# Impute missing FallDate by keeping NA where HistoryofFall indicates a fall history
data <- data %>%
  mutate(FallDate = ifelse(is.na(FallDate) & HistoryOfFall %in% c(1, 2, 3), as.Date(NA), FallDate))

# Verify the changes again
table(is.na(data$FallDate), data$HistoryOfFall, useNA = "ifany")
#Censored Data
# Remove rows where RacePrimary has less than 200 entries to protect patient privacy
data <- data %>%
  group_by(RacePrimary) %>%
  filter(n() >= 200) %>%
  ungroup()

# Verify the filtering
table(data$RacePrimary)
# Impute missing values and handle outliers for Average Calculated BMI
data <- data %>%
  mutate(
    AverageCalculatedBMI = ifelse(is.na(AverageCalculatedBMI), mean(AverageCalculatedBMI, na.rm = TRUE), AverageCalculatedBMI)
  ) %>%
  filter(AverageCalculatedBMI <= 50) # Remove outliers for BMI

# Impute missing values and handle outliers for A1C Lab Result
data <- data %>%
  mutate(
    A1CLabResult = ifelse(is.na(A1CLabResult), mean(A1CLabResult, na.rm = TRUE), A1CLabResult)
  ) %>%
  filter(A1CLabResult > 0 & A1CLabResult < 20) # Remove impossible values for A1C

# Verify the changes
summary(data %>% select(AverageCalculatedBMI, A1CLabResult))

# Load Amelia for missing data visualization
library(Amelia)

# Visualize missing data using a heatmap
missmap(data, main = "Missing Data Heatmap", col = c("yellow", "black"), legend = TRUE)

# Impute missing values for Ethnicity with "Unknown"
data$Ethnicity[is.na(data$Ethnicity)] <- "Unknown"

# Verify the changes
table(data$Ethnicity, useNA = "ifany")

# Export the cleaned data to a CSV file
write.csv(data, "C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/cleaned_fall_risk_data3.csv", row.names = FALSE)


# Replace blanks in Ethnicity with NA
data$Ethnicity[data$Ethnicity == ""] <- NA

# Replace "Unknown" in RacePrimary with NA
data$RacePrimary[data$RacePrimary == "Unknown"] <- NA

# Convert Ethnicity and RacePrimary to factors (required for categorical imputation)
data$Ethnicity <- as.factor(data$Ethnicity)
data$RacePrimary <- as.factor(data$RacePrimary)

# Load necessary package
if (!require(mice)) install.packages("mice")
library(mice)

# Convert other categorical columns to factors
data$HistoryOfFall <- as.factor(data$HistoryOfFall)
data$CurrentlyOnPainMedication <- as.factor(data$CurrentlyOnPainMedication)
data$HistoryofStroke <- as.factor(data$HistoryofStroke)
data$HistoryofOsteoporosis <- as.factor(data$HistoryofOsteoporosis)
data$HistoryofArthritis <- as.factor(data$HistoryofArthritis)
data$DiabeticComplications <- as.factor(data$DiabeticComplications)

# Optional: Convert DateOfBirth to Age (numeric) if it’s relevant for imputation#
#data$Age <- as.numeric(difftime(Sys.Date(), as.Date(data$DateOfBirth, format = "%m/%d/%Y"), units = "weeks")) / 52.25
#data$DateOfBirth <- NULL # Remove DateOfBirth column if using Age

# Run multiple imputation using mice with random forest method
imputed_data <- mice(data, method = "rf", m = 5, maxit = 10, seed = 42)

# Extract the completed dataset with imputed values
data_imputed <- complete(imputed_data)

# Verify imputed results for RacePrimary and Ethnicity
table(data_imputed$RacePrimary, useNA = "ifany")
table(data_imputed$Ethnicity, useNA = "ifany")

# Save the imputed dataset to a CSV file
write.csv(data_imputed, "C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/cleaned_fall_risk_data_final9.csv", row.names = FALSE)

# Install and load Amelia if not already installed
if (!require(Amelia)) install.packages("Amelia")
library(Amelia)

# Plot heatmap of missing data
missmap(data_imputed, main = "Missing Data Heatmap", col = c("yellow", "black"), legend = TRUE)



data <- data %>%
  mutate(FallDate = if_else(is.na(FallDate) & HistoryOfFall %in% c(1, 2, 3), as.Date(NA), FallDate))
