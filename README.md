Perfect! Here’s a **copy-paste ready `README.md`** for your project. You can directly save it as `README.md` in your GitHub repository:

---

```markdown
# Prevalence and Determinants of Malnutrition Among Women of Reproductive Age in Urban and Rural Bangladesh

This repository contains the full research project conducted as part of the **B.Sc. (Honors) in Statistics** program at **Jagannath University**. The study investigates the prevalence and determinants of malnutrition—including **underweight, overweight, and obesity**—among women of reproductive age (15–49 years) in both **urban and rural areas of Bangladesh** using **BDHS survey data**.

---

## 📌 Objective

The main objectives of this study are:

- To examine the nutritional status of women of reproductive age (15–49 years) in urban and rural Bangladesh.
- To identify key **socioeconomic and demographic determinants** of malnutrition.
- To perform **urban-rural stratified analysis** to understand differences in determinants and prevalence.

---

## 🧠 Methods

The study employed the following statistical methods:

1. **Descriptive Statistics**
   - Summary statistics for demographic and socioeconomic variables.
   - Prevalence of underweight, overweight, and obesity.

2. **Chi-square Tests**
   - To examine associations between categorical variables and nutritional status.

3. **Multinomial Logistic Regression**
   - To model determinants of malnutrition (underweight, normal, overweight/obese).
   - Adjusted odds ratios (AOR) with 95% confidence intervals reported.

4. **Urban-Rural Stratified Analysis**
   - Separate analysis for urban and rural women to identify context-specific determinants.

---

## 📁 Repository Structure

```

malnutrition-bdhs/
│
├── README.md                 # This file
├── LICENSE                   # License file (MIT recommended)
├── requirements.R            # R script to install all required packages
├── data/
│   ├── raw/                  # Raw BDHS survey data (not uploaded due to privacy)
│   └── processed/            # Cleaned and preprocessed data
├── scripts/
│   ├── 01_data_cleaning.R    # Data cleaning and preprocessing
│   ├── 02_descriptive.R      # Descriptive statistics and chi-square tests
│   ├── 03_multinomial.R      # Multinomial logistic regression analysis
│   └── 04_stratified_analysis.R # Urban-rural stratified analysis
├── results/
│   ├── tables/               # Tables for manuscript
│   └── figures/              # Graphs and visualizations
└── reports/
└── manuscript.md         # Draft manuscript or final report

````

---

## 📦 Requirements

To install all required R packages, run:

```R
source("requirements.R")
````

**Typical packages included in `requirements.R`:**

```R
install.packages(c("tidyverse", "dplyr", "ggplot2", "nnet", "foreign", "broom", "readr"))
```

---

## 🔹 How to Run the Project

1. Place the **BDHS survey dataset** inside the `data/raw/` folder.
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

The project includes:

* Prevalence of underweight, overweight, and obesity among urban and rural women.
* Socioeconomic and demographic determinants (age, education, wealth index, marital status, etc.).
* Stratified insights showing differences in urban vs. rural populations.
* Tables and figures ready for inclusion in manuscripts or presentations.

---

## 📝 Citation

If you use this project in your research, please cite:

> Sharma, S.K.D., et al. (2025). Prevalence and Determinants of Malnutrition Among Women of Reproductive Age in Urban and Rural Bangladesh. B.Sc. (Honors) Thesis, Department of Statistics, Jagannath University, Dhaka.

---

## 📜 License

This project is licensed under the **MIT License**. See `LICENSE` for details.

---

## 💡 Notes

* Data privacy: Raw BDHS data **cannot be shared publicly**. Please request access through the official [BDHS website](https://dhsprogram.com/).
* All scripts are reproducible. Ensure that all dependencies in `requirements.R` are installed before running scripts.
* Figures and tables are saved automatically in the `results/` folder.

```

---

If you want, I can also **create a ready-to-upload GitHub folder with `scripts/`, `data/`, `results/`, and this README.md**, so you can just push it directly.  

Do you want me to do that next?
```
