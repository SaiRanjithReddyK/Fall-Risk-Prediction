# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(randomForest)
library(cluster)

# Load the dataset
file_path <- "C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/cleaned_fall_risk_data_final9v1.csv"
fall_risk_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Define complication categories
neurological <- c(
  "Type 2 diabetes mellitus with diabetic amyotrophy",
  "Type 2 diabetes mellitus with diabetic autonomic (poly)neuropathy",
  "Type 2 diabetes mellitus with diabetic mononeuropathy",
  "Type 2 diabetes mellitus with diabetic neuropathic arthropathy",
  "Type 2 diabetes mellitus with diabetic neuropathy, unspecified",
  "Type 2 diabetes mellitus with diabetic polyneuropathy",
  "Type 2 diabetes mellitus with other diabetic neurological complication"
)

ophthalmic <- c(
  "Type 2 diabetes mellitus with diabetic cataract",
  "Type 2 diabetes mellitus with mild nonproliferative diabetic retinopathy with macular edema, bilateral",
  "Type 2 diabetes mellitus with mild nonproliferative diabetic retinopathy with macular edema, unspecified eye",
  "Type 2 diabetes mellitus with moderate nonproliferative diabetic retinopathy with macular edema, bilateral",
  "Type 2 diabetes mellitus with proliferative diabetic retinopathy with macular edema, bilateral",
  "Type 2 diabetes mellitus with proliferative diabetic retinopathy with macular edema, left eye",
  "Type 2 diabetes mellitus with proliferative diabetic retinopathy with macular edema, right eye",
  "Type 2 diabetes mellitus with proliferative diabetic retinopathy with traction retinal detachment not involving the macula, right eye",
  "Type 2 diabetes mellitus with severe nonproliferative diabetic retinopathy with macular edema, bilateral",
  "Type 2 diabetes mellitus with unspecified diabetic retinopathy with macular edema",
  "Type 2 diabetes mellitus with other diabetic ophthalmic complication"
)

renal <- c(
  "Type 2 diabetes mellitus with diabetic chronic kidney disease",
  "Type 2 diabetes mellitus with diabetic nephropathy",
  "Type 2 diabetes mellitus with other diabetic kidney complication"
)

circulatory <- c(
  "Type 2 diabetes mellitus with diabetic peripheral angiopathy with gangrene",
  "Type 2 diabetes mellitus with other circulatory complications"
)

dermatological <- c(
  "Type 2 diabetes mellitus with diabetic dermatitis",
  "Type 2 diabetes mellitus with foot ulcer",
  "Type 2 diabetes mellitus with other skin complications",
  "Type 2 diabetes mellitus with other skin ulcer"
)

metabolic <- c(
  "Type 2 diabetes mellitus with hyperglycemia",
  "Type 2 diabetes mellitus with hyperosmolarity with coma",
  "Type 2 diabetes mellitus with ketoacidosis with coma"
)

musculoskeletal <- c("Type 2 diabetes mellitus with other diabetic arthropathy")

other <- c(
  "Type 2 diabetes mellitus with other specified complication",
  "Type 2 diabetes mellitus with unspecified complications"
)

# Add a new column for complication categories
fall_risk_data <- fall_risk_data %>%
  mutate(
    ComplicationCategory = case_when(
      DiabeticComplicationType %in% neurological ~ "Neurological Complications",
      DiabeticComplicationType %in% ophthalmic ~ "Ophthalmic Complications",
      DiabeticComplicationType %in% renal ~ "Renal Complications",
      DiabeticComplicationType %in% circulatory ~ "Circulatory Complications",
      DiabeticComplicationType %in% dermatological ~ "Dermatological Complications",
      DiabeticComplicationType %in% metabolic ~ "Metabolic Complications",
      DiabeticComplicationType %in% musculoskeletal ~ "Musculoskeletal Complications",
      DiabeticComplicationType %in% other ~ "Other Complications",
      TRUE ~ "Unknown"
    )
  )

# View the dataset with the new column
head(fall_risk_data)

# Save the updated dataset
write.csv(fall_risk_data, "C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/fall_risk_data_final9v2.csv", row.names = FALSE)
####

colnames(fall_risk_data)


