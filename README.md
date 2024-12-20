# Project 4: Dietary Nutrient Intake and Blood Pressure Prediction

## Authors
- **Yishan Lin**: ylin183@jh.edu, JHED ID: ylin183
- **Yimin Ding**: yding75@jh.edu, JHED ID: yding75
- **Yingqi Wang**: ywang911@jh.edu, JHED ID: ywang911
- **Ruiqing Cai**: rcai9@jh.edu, JHED ID: rcai9

## Overview
This project investigates the relationships between dietary nutrient intake and blood pressure levels using data from the NHANES (2021–2023) dataset. Hypertension is a significant risk factor for cardiovascular diseases, and understanding dietary influences can help guide public health strategies.

### Objectives
- Statistical Analysis: Explore associations between sodium, potassium, calcium, magnesium, fiber, and blood pressure (systolic and diastolic).
- Machine Learning: Build a predictive model to identify hypertension risk based on demographic and dietary factors.

## Methods
### Data Preparation
- Utilized NHANES datasets for demographic, dietary, and laboratory data.
- Imputed missing values using Predictive Mean Matching (PMM) to ensure completeness.
- Stratified analysis by demographic factors to provide subgroup insights.
### Statistical Models
- Regression models to identify significant nutrient-blood pressure relationships.
- Functional programming approaches (e.g., reusable functions for cleaning and processing) were implemented for reproducibility.
### Machine Learning
- Random Forest models trained on demographic and dietary variables to predict:
  - Systolic Blood Pressure (SYBP)
  - Diastolic Blood Pressure (DIBP)
  - Combined Blood Pressure Category (Normal/High)
- Parallel processing was used to improve computational efficiency.

## Key Results
1. **Statistical Analysis:**
   - Sodium intake showed a weak negative association with blood pressure.
2. **Machine Learning:**
   - Predictive performance for binary blood pressure outcomes:
     - Sensitivity for "Normal" classification was high (~96%).
     - Specificity for "High" classification was limited (~10–39%).
   - Random Forest models highlighted age and BMI as the most critical predictors.
3. **Dashboard Insights:**
   - Dynamic visualization options (e.g., violin plots, scatterplots) provided granular insights into nutrient-blood pressure interactions.

## Technical Highlights
- **Programming Paradigms:** Functional and object-oriented approaches streamlined workflows.
- **Parallel Processing:** Boosted computational efficiency for machine learning.
- **Visualization:** An interactive dashboard enhanced data accessibility and usability.

## Requirements
- **R version 4.0 or higher**
- **Required R packages:**
  - `tidyverse`
  - `randomForest`
  - `caret`
  - `doParallel`
  - `mice`
  - `quarto`
- **Running the Code**
  - Clone this repository.
  - Open the `Final_project.qmd` file to explore data preparation, statistical models, and machine learning implementations.
  - Launch the dashboard with `Interactive Dashboard.qmd`.
- **Accessing Output**
  - Summaries and visualizations are available in the `Final project.html` files
  - The `Final Project Write up.pdf` contains a comprehensive report.

## License
This project is for educational purposes and uses NHANES data, which is publicly available under the CDC guidelines for non-commercial use. '

