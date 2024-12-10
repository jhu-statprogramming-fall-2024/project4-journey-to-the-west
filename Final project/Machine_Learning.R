# Split data into training and testing sets
set.seed(237461)
train_index <- createDataPartition(cleaned_data$BPQ150, p = 0.8, list = FALSE)
train_data <- cleaned_data[train_index, ]
test_data <- cleaned_data[-train_index, ]

# Impute missing values in training data
imputed_train_data <- mice(train_data, m = 1, method = "pmm", maxit = 50, seed = 500)
train_data <- complete(imputed_train_data)

# Impute missing values in test data
imputed_test_data <- mice(test_data, m = 1, method = "pmm", maxit = 50, seed = 500)
test_data <- complete(imputed_test_data)

# Define training control
train_control <- trainControl(method = "cv", number = 10)

# Model 1: Base Model
model1 <- train(
  sybp ~ RIDAGEYR + DR1TSODI + BMXBMI, # Base model
  data = train_data,
  method = "lm",
  trControl = train_control
)

# Model 2: Adding Race
model2 <- train(
  sybp ~ RIDAGEYR + DR1TSODI + BMXBMI + RIDRETH3, # Adding race
  data = train_data,
  method = "lm",
  trControl = train_control
)

# Model 3: Interaction Term (DR1TSODI × BMXBMI)
model3 <- train(
  sybp ~ RIDAGEYR + DR1TSODI + BMXBMI + I(DR1TSODI * BMXBMI), # Interaction term
  data = train_data,
  method = "lm",
  trControl = train_control
)

# Model 4: Polynomial Terms
model4 <- train(
  sybp ~ poly(RIDAGEYR, 2) + DR1TSODI + poly(BMXBMI, 2), # Polynomial terms for age and BMI
  data = train_data,
  method = "lm",
  trControl = train_control
)

# Model 5: Interaction and Polynomial Terms
model5 <- train(
  sybp ~ poly(RIDAGEYR, 2) + DR1TSODI + poly(BMXBMI, 2) + I(DR1TSODI * BMXBMI), # Combined terms
  data = train_data,
  method = "lm",
  trControl = train_control
)


cl <- makeCluster(detectCores() - 1) # Use all cores except one
registerDoParallel(cl)

model_rf <- train(
  sybp ~ RIDAGEYR + DR1TSODI + BMXBMI + RIDRETH3,
  data = train_data,
  method = "rf",
  tuneGrid = expand.grid(.mtry = c(2, 3)),
  trControl = trainControl(method = "cv", number = 10),
  ntree = 200
)

stopCluster(cl) 

# Evaluate Models on Test Data
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, test_data)
  data.frame(
    RMSE = RMSE(predictions, test_data$sybp),
    R2 = R2(predictions, test_data$sybp)
  )
}

# Evaluate all models
results <- list(
  # Model1 = evaluate_model(model1, test_data),
  # Model2 = evaluate_model(model2, test_data),
  # Model3 = evaluate_model(model3, test_data),
  # Model4 = evaluate_model(model4, test_data),
  # Model5 = evaluate_model(model5, test_data),
  Model_rf = evaluate_model(model_rf, test_data)
)

# Combine results into a single dataframe
results_df <- bind_rows(results, .id = "Model")

# Print results
print(results_df)

# Visualize Results
ggplot(results_df, aes(x = Model, y = RMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "RMSE Comparison Across Models",
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal()

ggplot(results_df, aes(x = Model, y = R2)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(
    title = "R² Comparison Across Models",
    x = "Model",
    y = "R²"
  ) +
  theme_minimal()
