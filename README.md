# Project 4: Association Between Dietary Sodium Intake and Blood Pressure in Adults Using NHANES Data

## Overview
This project examines the correlation between dietary sodium intake and blood pressure levels in U.S. adults using the NHANES dataset (2017–2018). It investigates the role of sodium as a modifiable factor in managing hypertension and its associated risks, such as cardiovascular diseases.

The study involves the following:
1. Data preprocessing and integration using shell scripting and R.
2. Statistical analysis using linear regression models to explore the relationship between sodium intake and blood pressure.
3. Interactive visualization using Shiny for exploring demographic variations in the association.

## Requirements
- R version 4.0 or higher
- Required R packages:
  - `tidyverse`
  - `labelled`
  - `tableone`
  - `shiny`
  - `quarto`
- A terminal with basic shell utilities (`curl`, `unzip`, `awk`, `sed`)

## Installation
To install the required R packages, run the following command in R:

```r
install.packages(c("tidyverse", "labelled", "tableone", "shiny", "quarto"))
```

## Authors
- **Yishan Lin**: ylin183@jh.edu, JHED ID: ylin183
- **Yimin Ding**: yding75@jh.edu, JHED ID: yding75
- **Yingqi Wang**: ywang911@jh.edu, JHED ID: ywang911
- **Ruiqing Cai**: rcai9@jh.edu, JHED ID: rcai9

## Files
- `project4_analysis.qmd`: Quarto Markdown file containing the analysis code.
- `rendered_output.htm`l: HTML file summarizing the results and findings.
- `shiny_app/`: Directory containing the Shiny app files for interactive visualization.
- `scripts/`:Shell scripts for downloading and preprocessing NHANES datasets.
- `data/`: Directory containing the processed data.

## Analysis Workflow
- Data Download and Preprocessing:
Shell scripts automate downloading NHANES datasets and filtering for key variables.

- Data is merged and cleaned using R.

## Statistical Analysis:
- Linear regression models are used to explore associations between dietary sodium intake and blood pressure, adjusting for covariates such as age, sex, race, BMI, and cholesterol levels.

## Visualization:
- Shiny dashboard provides an interactive interface for exploring the results by demographic groups.
- Histogram of sodium intake and other plots summarize key findings.

## Deliverables

- Quarto Website: Comprehensive report with visualizations and results.
Interactive Dashboard: Shiny app for exploring sodium and blood pressure associations.
- GitHub Repository: All code, data, and documentation.

##License
This project is for educational purposes and uses NHANES data, which is publicly available under the CDC guidelines for non-commercial use. '

