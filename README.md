
# Prevalence and Determinants of Malnutrition Among Women of Reproductive Age in Urban and Rural Bangladesh

This repository contains the full research project conducted as part of the B.Sc. (Honors) in Statistics program at Jagannath University.
 The study investigates the prevalence and determinants of malnutrition—including underweight, overweight, and obesity—among women of reproductive age (15–49 years) in both urban and rural areas of Bangladesh using BDHS survey data.

---

# Objective

The main objectives of this study are:

- To examine the nutritional status of women of reproductive age (15–49 years) in urban and rural Bangladesh.
- To identify key socioeconomic and demographic determinants of malnutrition.
- To perform urban-rural stratified analysis to understand differences in determinants and prevalence.

---

# Methods

The study employed the following statistical methods:

1. Descriptive Statistics
2. Chi-square Tests
3. Multinomial Logistic Regression
4. Urban-Rural Stratified Analysis

---

# Repository Structure

```text
malnutrition-bdhs/
│
├── README.md                 # This file
├── LICENSE                   # License file (MIT recommended)
├── requirements.R            # R script to install all required packages
├── data/
│   ├── raw/                  # Raw BDHS survey data (not uploaded due to privacy)
│   └── processed/            # Cleaned and preprocessed data
├── scripts/
│   ├── 01_data_cleaning.R         # Data cleaning and preprocessing
│   ├── 02_descriptive.R           # Descriptive statistics and chi-square tests
│   ├── 03_multinomial.R           # Multinomial logistic regression analysis
│   └── 04_stratified_analysis.R   # Urban-rural stratified analysis
├── results/
│   ├── tables/               # Tables for manuscript
│   └── figures/              # Graphs and visualizations
└── reports/
    └── manuscript.md         # Draft manuscript or final report
````



## Requirements

To install all required R packages, run:

```R
source("requirements.R")
```

Typical packages included in `requirements.R`:

```R
install.packages(c("tidyverse", "dplyr", "ggplot2", "nnet", "foreign", "broom", "readr"))
```

---

##  How to Run the Project

1. Place the BDHS survey dataset inside the `data/raw/` folder.
2. Run the scripts sequentially:

```bash
Rscript scripts/01_data_cleaning.R
Rscript scripts/02_descriptive.R
Rscript scripts/03_multinomial.R
Rscript scripts/04_stratified_analysis.R
```

3. Output tables and figures will be saved in `results/tables/` and `results/figures/`.

---

## 📊 Results

* Prevalence of underweight, overweight, and obesity among urban and rural women.
* Socioeconomic and demographic determinants.
* Stratified insights for urban vs. rural populations.
* Tables and figures ready for manuscript or presentation use.

---

## 📝 Citation

> Sharma, S.K.D., et al. (2025). Prevalence and Determinants of Malnutrition Among Women of Reproductive Age in Urban and Rural Bangladesh. B.Sc. (Honors) Thesis, Department of Statistics, Jagannath University, Dhaka.

---

## 📜 License

This project is licensed under the MIT License. See `LICENSE` for details.

---

## 💡 Notes

* Raw BDHS data cannot be shared publicly. Request access via the [BDHS website](https://dhsprogram.com/).
* Ensure all dependencies in `requirements.R` are installed before running scripts.
* Figures and tables are automatically saved in the `results/` folder.

```


```
