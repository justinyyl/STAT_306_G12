#Loading library
library(car)
library(leaps)
library(tidyverse)
library(broom)
library(GGally)
library(patchwork)
library(gridExtra)
library(grid)
library(ggplotify)

# Read the CSV file
data <- read.csv("data/colorectal_cancer_dataset.csv")

# Select the data in USA
usa_data <- data %>%
  filter(Country == "USA")

# Remove Patient_ID, Country, Country-level features and other response features
colorectal_usa <- usa_data %>%
  select(
    -Patient_ID,                     # Not relevant
    -Country,                        # Focus on USA only
    -Incidence_Rate_per_100K,        # Country-level
    -Mortality_Rate_per_100K,        # Country-level
    -Economic_Classification,        # Country-level
    -Healthcare_Access,              # Country-level
    -Survival_Prediction,            # Same effect as response variable: Mortality
    -Survival_5_years                # Same effect as response variable: Mortality
  )

# Set response variable 
resp <- "Mortality"
# Set response variable into factor type
colorectal_usa[[resp]] <- as.factor(colorectal_usa[[resp]])

# Set the unit of Healthcare_Costs in $1,000
colorectal_usa[["Healthcare_Costs"]] <- colorectal_usa[["Healthcare_Costs"]]/1000

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

# Bar plot of Mortality in original dataset
plot_not_balance <- ggplot(colorectal_usa, aes(x = Mortality, fill = Mortality)) +
  geom_bar(alpha = 0.6) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Alive", "Dead")) +
  labs(
    title = "Class Distribution of Mortality (Imbalance)",
    x = "Mortality Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

# Count how many in minority class
min_class_size <- min(table(colorectal_usa$Mortality))

# Sample from each class equally
set.seed(123)
balanced_data <- colorectal_usa %>%
  group_by(Mortality) %>%
  sample_n(min_class_size) %>%
  ungroup()

# Bar plot of Mortality in balanced dataset
plot_balance <- ggplot(balanced_data, aes(x = Mortality, fill = Mortality)) +
  geom_bar(alpha = 0.6) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Alive", "Dead")) +
  labs(
    title = "Class Distribution of Mortality (Balance)",
    x = "Mortality Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

my_list <- list(plot_not_balance, plot_balance)

combined_plot <- do.call(arrangeGrob, c(my_list, ncol = 2))

caption_grob <- textGrob("Figure 1: Class Distribution of the Mortality Variable Before and After Balancing",
                         gp = gpar(fontface = "bold.italic", fontsize = 15), 
                         hjust = 0.5,
                         vjust = 0.3)

final_plot <- arrangeGrob(combined_plot, bottom = caption_grob)

as.ggplot(final_plot)

# Create Histogram plots for Continuous Covariates of Binary Response
numeric_cols <- c("Age", "Tumor Size (mm)", "Healthcare Costs ($K)")
my_list <- list()

for (i in seq_along(continuous)) {
  feat <- continuous[i]
  feat_plot <- ggplot(balanced_data, aes_string(x = feat, fill = resp)) + 
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    labs(
      title = paste("Histogram of", numeric_cols[i]), 
      x = numeric_cols[i], 
      y = "Frequency"
    ) +
    scale_fill_manual(values = c("blue", "red"), labels = c("Alive", "Dead")) +
    guides(fill = guide_legend(title = "Mortality")) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 14)
    )
  my_list[[i]] <- feat_plot
}

combined_plot <- do.call(arrangeGrob, c(my_list, ncol = 2))

caption_grob <- textGrob("Figure 2: Distribution of continuous variables by mortality status",
                         gp = gpar(fontface = "bold.italic", fontsize = 24), 
                         hjust = 0.5,
                         vjust = 0.3)

final_plot <- arrangeGrob(combined_plot, bottom = caption_grob)

as.ggplot(final_plot)

# Create Proportion Bar plots for Categorical Covariates of Binary Response
categorical_cols <- c("Gender", "Cancer Stage", "Family History", "Smoking History", "Alcohol Consumption", "Obesity BMI", "Diet Risk", "Physical Activity", "Diabetes", "Inflammatory Bowel Disease", "Genetic Mutation", " Screening History", "Early Detection", "Treatment Type", "Urban or Rural", "Insurance Status")
my_list <- list()
for (i in seq_along(categorical)) {
  feat <- categorical[i]
  feat_plot <- ggplot(balanced_data, aes_string(x = feat, fill = resp)) +
    geom_bar(position = "fill", , alpha = 0.6) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste("Mortality Proportion by", categorical_cols[i]),
      x = categorical_cols[i],
      y = "Proportion"
    ) +
    scale_fill_manual(values = c("blue", "red"), labels = c("Alive", "Dead")) +
    guides(fill = guide_legend(title = "Mortality")) +
    theme_minimal(base_size = 32) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 36),
      axis.title = element_text(size = 32),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 28),
      axis.text.y = element_text(size = 28),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 28)
    )
  
  my_list[[i]] <- feat_plot
}

combined_plot <- do.call(arrangeGrob, c(my_list, ncol = 4))

caption_grob <- textGrob("Figure 3: Proportion Bar plots for Categorical variables by mortality status",
                         gp = gpar(fontface = "bold.italic", fontsize = 62), 
                         hjust = 0.5,
                         vjust = 0.3)

final_plot <- arrangeGrob(combined_plot, bottom = caption_grob)

as.ggplot(final_plot)

# Fit the full logistic regression model with all predictors
full_model <- glm(Mortality ~ ., data = balanced_data, family = binomial)

# Check for duplicate rows
dup_check <- any(duplicated(balanced_data)) 

# Compute predicted probabilities
fitted_probs <- fitted(full_model)

# Compute binomial variances
binomial_var <- fitted_probs * (1 - fitted_probs)

# Create the base plot
variance_plot <- ggplot(data.frame(fitted_probs, binomial_var), aes(x = fitted_probs, y = binomial_var)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  labs(
    x = expression("Predicted Probability (" * hat(pi) * ")"),
    y = expression("Binomial Variance " * hat(pi) * " (1 - " * hat(pi) * ")")
  ) +
 scale_x_continuous(
  breaks = seq(0.45, 0.55, by = 0.01), limits = c(0.44, 0.56)) +
  theme_minimal(base_size = 16)

caption_grob <- textGrob("Figure 4: Variance peaks at predicted probability = 0.5.",
                         gp = gpar(fontface = "bold.italic", fontsize = 24), 
                         hjust = 0.5, vjust = 0.3)

final_plot <- arrangeGrob(variance_plot, bottom = caption_grob)

as.ggplot(final_plot)

# Get the logit (link) values
logit_vals <- predict(full_model, type = "link")

# Create list to store plots
logit_plot_list <- list()

# Loop over each continuous variable
for (i in seq_along(continuous)) {
  feat <- continuous[i]
  label <- numeric_cols[i]

  p <- ggplot(balanced_data, aes_string(x = feat, y = logit_vals)) +
    geom_point(alpha = 0.4, color = "darkblue") +
    geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "solid") +
    labs(
      title = paste("Logit vs", label),
      x = label,
      y = expression("Logit")
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )

  logit_plot_list[[i]] <- p
}

combined_plot <- do.call(arrangeGrob, c(logit_plot_list, ncol = 2))

caption_grob <- textGrob(
  "Figure 5: Linearity check — logit of mortality plotted against continuous variables",
  gp = gpar(fontface = "bold.italic", fontsize = 16), 
  hjust = 0.5,
  vjust = 0.3
)

final_plot <- arrangeGrob(combined_plot, bottom = caption_grob)

as.ggplot(final_plot)


# Start with full model
selected_vars <- setdiff(names(balanced_data), resp)
current_formula <- as.formula(paste(resp, "~", paste(selected_vars, collapse = " + ")))
current_model <- glm(current_formula, data = balanced_data, family = binomial)
current_aic <- AIC(current_model)

# Store best models
best_models <- data.frame(
  Num_Covariates = length(selected_vars),
  AIC = current_aic,
  Variables = paste(selected_vars, collapse = " + "),
  stringsAsFactors = FALSE
)

step <- 1
improvement <- TRUE

while (improvement && length(selected_vars) > 1) {
  aic_list <- c()

  # Try removing each variable one at a time
  for (var in selected_vars) {
    test_vars <- setdiff(selected_vars, var)
    test_formula <- as.formula(paste(resp, "~", paste(test_vars, collapse = " + ")))
    test_model <- glm(test_formula, data = balanced_data, family = binomial)
    aic_list[var] <- AIC(test_model)
  }

  best_var_to_remove <- names(which.min(aic_list))
  best_aic <- min(aic_list)

  if (best_aic < current_aic) {
    selected_vars <- setdiff(selected_vars, best_var_to_remove)
    current_aic <- best_aic
    current_formula <- as.formula(paste(resp, "~", paste(selected_vars, collapse = " + ")))
    current_model <- glm(current_formula, data = balanced_data, family = binomial)

    # Log best model at this level
    best_models <- rbind(best_models, data.frame(
      Num_Covariates = length(selected_vars),
      AIC = best_aic,
      Variables = paste(selected_vars, collapse = " + ")
    ))
    step <- step + 1
  } else {
    improvement <- FALSE
  }
}

# Add null model into best_models
best_models <- rbind(best_models, data.frame(
      Num_Covariates = 0,
      AIC = AIC(glm(Mortality ~ 1, data = balanced_data, family = binomial)),
      Variables = 1
    ))

aic_plot <- ggplot(best_models, aes(x = Num_Covariates, y = AIC)) +
  geom_line(color = "black") +
  geom_point(color = "red") +
  labs(
    x = "Number of Covariates",
    y = "AIC"
  ) +
  theme_minimal()

# Caption as a grob (bold + italic)
caption_grob <- textGrob("Figure 6: AIC vs Number of Covariates – demonstrating backward feature selection.",
                         gp = gpar(fontface = "bold.italic", fontsize = 8),
                         hjust = 0.5, vjust = 0.3)

# Combine plot and caption
final_plot <- arrangeGrob(aic_plot, bottom = caption_grob)

# Convert to ggplot object
as.ggplot(final_plot)
