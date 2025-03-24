#Loading library
library(car)
library(leaps)
library(tidyverse)
library(broom)

# Read the CSV file
data <- read.csv("data/colorectal_cancer_dataset.csv")

# Select the data in USA
usa_data <- data %>%
  filter(Country == "USA")

# Remove Patient_ID and Country
colorectal_usa <- usa_data %>%
  select(-Patient_ID, -Country)
