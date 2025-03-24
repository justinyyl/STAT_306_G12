#Loading library
library(car)
library(leaps)
library(tidyverse)
library(broom)
library(GGally)

# Read the CSV file
data <- read.csv("data/colorectal_cancer_dataset.csv")

# Select the data in USA
usa_data <- data %>%
  filter(Country == "USA")

# Remove Patient_ID, Country, Tumor_Size_mm
colorectal_usa <- usa_data %>%
  select(-Patient_ID, -Country, -Tumor_Size_mm)

# Sampling 2,000 observations
set.seed(42)  # for reproducibility
colorectal_usa <- colorectal_usa[sample(nrow(colorectal_usa), 1000), ]

# Set response variable
resp <- "Mortality_Rate_per_100K"

# Initialize containers
continuous <- c()
categorical <- c()

# Loop through all variables
for (colname in names(colorectal_usa)) {
  if (colname == resp) next  # Skip the response variable itself
  
  column <- colorectal_usa[[colname]]
  
  if (is.numeric(column)) {
    continuous <- c(continuous, colname)
  } else if (is.character(column) || is.factor(column)) {
    categorical <- c(categorical, colname)
  }
}

# Convert all categorical variables to factors
for (colname in categorical) {
  colorectal_usa[[colname]] <- as.factor(colorectal_usa[[colname]])
}

# Boxplots for categorical variables
for (colname in categorical) {
  cat("Boxplot:", resp, "by", colname, "\n")
    
  xvar <- colorectal_usa[[colname]]
  yvar <- colorectal_usa[[resp]]
    
  p <- ggplot(colorectal_usa, aes(x = xvar, y = yvar)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Boxplot of", resp, "by", colname),
          x = colname, y = resp) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  print(p)
}

# Scatter plots for continuous variables
for (colname in continuous) {
  cat("Scatter plot:", resp, "vs", colname, "\n")
  
  xvar <- colorectal_usa[[colname]]
  yvar <- colorectal_usa[[resp]]
  
  p <- ggplot(colorectal_usa, aes(x = xvar, y = yvar)) +
    geom_point(alpha = 0.6, color = "tomato") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    labs(title = paste("Scatter Plot of", resp, "vs", colname),
         x = colname, y = resp)
  
  print(p)
}

# Select only numeric variables
numeric_data <- colorectal_usa[sapply(colorectal_usa, is.numeric)]

# Drop rows with missing in response
resp <- "Mortality_Rate_per_100K"
numeric_data <- numeric_data[!is.na(numeric_data[[resp]]), ]

subset_data <- colorectal_usa[, names(numeric_data)]

# Plot
ggpairs(subset_data,
        title = "Pairwise Plot of Selected Variables",
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.3)),
        diag = list(continuous = wrap("barDiag", fill = "steelblue")))
