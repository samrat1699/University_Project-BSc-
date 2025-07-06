# Prevalence and Determinants of Malnutrition Among Women of Reproductive Age in Urban and Rural Bangladesh

This repository contains the full research project for the B.Sc. (Honors) in Statistics program at Jagannath University. The study investigates the prevalence and determinants of malnutrition—including underweight, overweight, and obesity—among women of reproductive age (15–49 years) in both urban and rural areas of Bangladesh using BDHS survey data.

## 📁 Project Structure

malnutrition-wra-bangladesh/
│
├── data/
│ ├── raw/ # Original BDHS data files (DTA, CSV, etc.)
│ └── processed/ # Cleaned and transformed data
│
├── scripts/ # R scripts for analysis
│ ├── data_cleaning.R
│ ├── descriptive_stats.R
│ └── regression_model.R
│
├── notebooks/ # RMarkdown notebooks for analysis
│ ├── 01_data_cleaning.Rmd
│ ├── 02_descriptive_analysis.Rmd
│ └── 03_multinomial_logit.Rmd
│
├── results/
│ ├── tables/ # Output tables from analysis
│ └── figures/ # Plots and graphs
│
├── report/
│ ├── final_report.pdf # Final project paper
│ └── presentation.pptx # Project presentation slides
│
├── requirements.R # R package dependencies
├── .gitignore # Files/folders to ignore in Git
└── README.md # This file



##  Objective

To examine the nutritional status of women of reproductive age in urban and rural Bangladesh and identify key socioeconomic and demographic determinants of malnutrition using descriptive and multivariate analysis.

##  Methods

- **Descriptive Statistics**
- **Chi-square Tests**
- **Multinomial Logistic Regression**
- **Urban-Rural Stratified Analysis**

##  Requirements

To install all required R packages, run:
```r
source("requirements.R")
