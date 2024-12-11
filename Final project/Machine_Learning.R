  ## 0. Authors Information
  ## 1. Data Preparation and Robust Analysis
  

packages <- c("tidyverse", "haven", "table1", "purrr", "MASS", "stargazer")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Create a folder to store all the data
if (!dir.exists("data")) { # check if folder already exists
  dir.create("data")
}

# Setup function to download the data
download_and_save <- function(url, filename) {
  filepath <- file.path("data", filename)
  
  if (!file.exists(filepath)) { # Check if file already exists
    message(paste("Downloading", filename, "..."))
    dataset <- read_xpt(url)
    saveRDS(dataset, filepath)
  } else {
    message(paste(filename, "already exists. Loading from cache."))
  }
  
  readRDS(filepath) # Load the dataset
}

# Function to clean individual datasets
clean_dataset <- function(data, vars) {
  data %>% dplyr::select(all_of(vars))
}

# Selecting data
datasets <- list(
  demo = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DEMO_L.xpt",
    file = "demo_data.rds",
    vars = c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH3")
  ),
  diet = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DR1TOT_L.xpt",
    file = "diet_data.rds",
    vars = c("SEQN", "DR1TSODI", "DR1TKCAL")
  ),
  bp = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BPXO_L.xpt",
    file = "bp_data.rds",
    vars = c("SEQN", "BPXOSY1", "BPXOSY2", "BPXOSY3", "BPXODI1", "BPXODI2", "BPXODI3")
  ),
  body = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BMX_L.xpt",
    file = "body_data.rds",
    vars = c("SEQN", "BMXBMI")
  ),
  lab = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/TCHOL_L.xpt",
    file = "lab_data.rds",
    vars = c("SEQN", "LBXTC")
  ),
  ques = list(
    url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BPQ_L.xpt",
    file = "ques_data.rds",
    vars = c("SEQN", "BPQ150")
  )
)

# Downloading, saving, and cleaning datasets
nhanes_data <- purrr::map(datasets, function(info) {
  raw_data <- download_and_save(info$url, info$file)
  clean_dataset(raw_data, info$vars)
})

# Merging into single dataframe
merged_data <- purrr::reduce(nhanes_data, dplyr::left_join, by = "SEQN")

# Function to preprocess data
process_data <- function(data) {
  data %>%
    dplyr::mutate(
      RIAGENDR = dplyr::recode_factor(RIAGENDR, `1` = "Male", `2` = "Female"),
      RIDRETH3 = dplyr::recode_factor(
        RIDRETH3,
        `1` = "Mexican American",
        `2` = "Other Hispanic",
        `3` = "Non-Hispanic White",
        `4` = "Non-Hispanic Black",
        `6` = "Other"
      ),
      BPQ150 = dplyr::recode_factor(BPQ150, `1` = "Yes", `2` = "No"),
      sybp = rowMeans(dplyr::select(., BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE),
      dibp = rowMeans(dplyr::select(., BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE)
    )
}

# Create Table 1 function
create_table1 <- function(data) {
  table1(~ RIDAGEYR + RIAGENDR + RIDRETH3 + DR1TSODI + BMXBMI + sybp + dibp + 
           BPQ150, data = data)
}

# Labeling data
set_labels_and_units <- function(data, labels, units = NULL) {
  for (var in names(labels)) {
    label(data[[var]]) <- labels[[var]]
  }
  if (!is.null(units)) {
    for (var in names(units)) {
      units(data[[var]]) <- units[[var]]
    }
  }
  return(data)
}

## Define labels and units as named lists
labels <- list(
  RIDAGEYR = "Age at Screening",
  RIAGENDR = "Gender",
  RIDRETH3 = "Race",
  BPQ150 = "High Blood Pressure Medication",
  sybp = "Systolic Blood Pressure",
  dibp = "Diastolic Blood Pressure",
  BMXBMI = "Body Mass Index"
)

units <- list(
  RIDAGEYR = "Year",
  sybp = "mmHg",
  dibp = "mmHg",
  BMXBMI = "kg/m\u00B2"
)

# Apply the data processing function
cleaned_data <- process_data(merged_data)

# Apply labels and units
cleaned_data <- set_labels_and_units(cleaned_data, labels, units)

# Generate Table 1
create_table1(cleaned_data)

# Save cleaned data
readr::write_csv(cleaned_data, "data/NHANES_cleaned_data.csv")


# Stratification function setup
stratified_analysis <- function(data, group_var, analysis_func) {
  # Split data into groups
  split_data <- split(data, data[[group_var]])
  
  # Apply the analysis function and add group label
  summary_list <- map(split_data, ~ {
    analysis_func(.x) %>%
      mutate(Gender = unique(.x[[group_var]])) # Add group label
  })
  
  # Combine all group summaries into a single table
  combined_summary <- bind_rows(summary_list)
  
  # Return combined table
  combined_summary
}

summarize_data <- function(data) {
  data %>%
    summarize(
      mean_age = mean(RIDAGEYR, na.rm = TRUE),
      mean_sodium = mean(DR1TSODI, na.rm = TRUE),
      mean_sbp = mean(sybp, na.rm = TRUE),
      mean_dbp = mean(dibp, na.rm = TRUE),
      n = n()
    )
}

# stratified analysis by gender
stratified_summary <- stratified_analysis(cleaned_data, "RIAGENDR", summarize_data)
knitr::kable(stratified_summary, 
             caption = "Table 2: Summary of each variables stratified by gender", 
             format = "html")


## 2. Using S3 to Conduct Object-Oriented Programming


# Extend the S3 methods to cover all seven models

# Define an S3 class for blood pressure analysis
BPAnalysis <- function(data) {
  structure(
    list(data = data, summaries = list()), # Store the dataset and summaries in the object
    class = "BPAnalysis"
  )
}

# S3 method for cleaning data
clean.BPAnalysis <- function(obj) {
  obj$data <- obj$data %>%
    mutate(
      BP_Category = case_when(
        sybp < 90 ~ "Low",
        sybp >= 90 & sybp <= 130 ~ "Normal",
        sybp > 130 ~ "High"
      ) %>% factor(levels = c("Low", "Normal", "High")),
      BP_Binary = ifelse(sybp > 130, 1, 0)
    )
  obj
}

# S3 method for modeling with condition-based formulas
model.BPAnalysis <- function(obj, type = "linear", variables = c("DR1TSODI")) {
  # Base formula
  response <- if (type == "logistic") "BP_Binary" else if (type == "ordinal") "BP_Category" else "sybp"
  formula <- as.formula(paste(response, "~", paste(variables, collapse = " + ")))
  
  if (type == "linear") {
    obj$model <- lm(formula, data = obj$data)
  } else if (type == "logistic") {
    obj$model <- glm(formula, data = obj$data, family = binomial)
  } else if (type == "ordinal") {
    obj$model <- MASS::polr(formula, data = obj$data, Hess = TRUE)
  } else {
    stop("Invalid model type.")
  }
  obj
}

# S3 method for summarizing results
summary.BPAnalysis <- function(obj, model_description) {
  if (is.null(obj$model)) {
    stop("No model has been created yet.")
  }
  summary_text <- capture.output(summary(obj$model))
  obj$summaries[[model_description]] <- summary_text
  obj
}

# Extend S3 to handle all seven models using conditions
run_models.BPAnalysis <- function(obj) {
  models <- list(
    list(type = "linear", variables = c("DR1TSODI"), description = "Model 1: Linear Model (Sodium Intake and Blood Pressure)"),
    list(type = "linear", variables = c("DR1TSODI", "RIDAGEYR", "RIAGENDR", "RIDRETH3"), description = "Model 2: Linear Model (Sodium Intake, Age, Gender, Race)"),
    list(type = "linear", variables = c("DR1TSODI", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "BMXBMI", "LBXTC", "BPQ150", "DR1TKCAL"), description = "Model 3: Linear Model (Sodium Intake, Age, Gender, Race, BMI, Total Cholesterol, High Blood Pressure Medication, Calories Intake)"),
    list(type = "logistic", variables = c("DR1TSODI"), description = "Model 4: Logistic Regression (Sodium Intake and Blood Pressure)"),
    list(type = "ordinal", variables = c("DR1TSODI"), description = "Model 5: Ordinal Logistic Regression (Sodium Intake and Blood Pressure)"),
    list(type = "ordinal", variables = c("DR1TSODI", "RIDAGEYR", "RIAGENDR", "RIDRETH3"), description = "Model 6: Ordinal Logistic Regression (Sodium Intake, Age, Gender, Race)"),
    list(type = "ordinal", variables = c("DR1TSODI", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "BMXBMI", "LBXTC", "BPQ150", "DR1TKCAL"), description = "Model 7: Ordinal Logistic Regression (Sodium Intake, Age, Gender, Race, BMI, Total Cholesterol, High Blood Pressure Medication, Calories Intake)")
  )
  
  for (model in models) {
    obj <- model.BPAnalysis(obj, type = model$type, variables = model$variables)
    obj <- summary.BPAnalysis(obj, model$description)
  }
  
  obj
}

# Function to output all summaries as HTML
output_summaries <- function(obj, output_file = "model_summaries.html") {
  html_content <- "<html><head><title>Model Summaries</title></head><body><h1>Model Summaries</h1>"
  
  for (model_name in names(obj$summaries)) {
    html_content <- paste0(
      html_content,
      "<h2>", model_name, "</h2>",
      "<pre>", paste(obj$summaries[[model_name]], collapse = "\n"), "</pre>"
    )
  }
  
  html_content <- paste0(html_content, "</body></html>")
  writeLines(html_content, output_file)
  message("Summaries written to ", output_file)
}

# Create an S3 object and run all models
bp_obj <- BPAnalysis(cleaned_data)
bp_obj <- clean.BPAnalysis(bp_obj)
bp_obj <- run_models.BPAnalysis(bp_obj)

# Output summaries to an HTML file
output_summaries(bp_obj)

  

# Visualization 1: Distribution of Systolic Blood Pressure (sybp) by Gender
ggplot(cleaned_data, aes(x = sybp, fill = RIAGENDR)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  labs(
    title = "Distribution of Systolic Blood Pressure by Gender",
    x = "Systolic Blood Pressure (mmHg)",
    y = "Count",
    fill = "Gender"
  ) +
  theme_classic()



# Visualization 2: Boxplot of Sodium Intake (DR1TSODI) by Gender
ggplot(cleaned_data, aes(x = RIAGENDR, y = DR1TSODI, fill = RIAGENDR)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  labs(
    title = "Sodium Intake by Gender",
    x = "Gender",
    y = "Sodium Intake (mg)"
  ) +
  theme_classic()


# Visualization 3: Relationship between BMI (BMXBMI) and Systolic Blood Pressure (sybp)
ggplot(cleaned_data, aes(x = BMXBMI, y = sybp, color = RIDRETH3)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3")) +
  labs(
    title = "Relationship between BMI and Systolic Blood Pressure by Race",
    x = "Body Mass Index (kg/m²)",
    y = "Systolic Blood Pressure (mmHg)",
    color = "Race"
  ) +
  theme_minimal()


# Visualization 4: Model Performance Comparison using Residuals
residuals_data <- do.call(rbind, lapply(names(bp_obj$summaries), function(model_name) {
  model <- bp_obj$summaries[[model_name]]
  if (inherits(model, "lm") || inherits(model, "polr")) {
    data.frame(
      Model = model_name,
      Residuals = residuals(model),
      Fitted = fitted(model)
    )
  }
}))

ggplot(residuals_data, aes(x = Fitted, y = Residuals, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Residuals vs Fitted Values for All Models",
    x = "Fitted Values",
    y = "Residuals",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


