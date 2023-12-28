# Install and load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")

library(tidyverse)
library(readr)

# Set the file path to your NetLogo BehaviorSpace table file
file_path <- "C:/Users/eduar/OneDrive/Documentos/GitHub/reciprocity-token-money/BehaviorSpace/population 100 cost2 benefit 5 replacement 0.01 (100 runs)/sucker-p100-c2b5-e1%-table.csv"

# Step 1: Import the data
behaviorspace_data <- read_csv(file_path)
problems(behaviorspace_data)
# Step 2: Clean up the data
# (You may need to customize this based on the structure of your specific data)

# Drop unnecessary columns
columns_to_drop <- c("run.number", "table_run_number", "step", "name")
behaviorspace_data <- behaviorspace_data %>%
  select(-one_of(columns_to_drop))

# Rename columns for better clarity
behaviorspace_data <- behaviorspace_data %>%
  rename(
    Replication = replication,
    Independent_Variable_1 = "independent variable 1",
    Independent_Variable_2 = "independent variable 2",
    Dependent_Variable_1 = "dependent variable 1",
    Dependent_Variable_2 = "dependent variable 2"
    # Add more columns as needed
  )

# Convert columns to appropriate data types
behaviorspace_data$Replication <- as.factor(behaviorspace_data$Replication)
behaviorspace_data$Independent_Variable_1 <- as.numeric(behaviorspace_data$Independent_Variable_1)
behaviorspace_data$Independent_Variable_2 <- as.numeric(behaviorspace_data$Independent_Variable_2)
behaviorspace_data$Dependent_Variable_1 <- as.numeric(behaviorspace_data$Dependent_Variable_1)
behaviorspace_data$Dependent_Variable_2 <- as.numeric(behaviorspace_data$Dependent_Variable_2)
# Convert more columns as needed

# View the cleaned data
print(head(behaviorspace_data))

# Now, you can proceed with further analysis using the cleaned data