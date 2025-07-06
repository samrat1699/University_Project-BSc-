# Set working directory
setwd("E:\\download")






# Load required libraries
library(nnet)    # multinom()
library(car)     # vif()
library(dplyr)   # data manipulation

# Example: assume df is already loaded and cleaned
# Convert outcome and predictors to factors if not done
df$bmi <- factor(df$bmi, levels = c("Normal", "Underweight", "Overweight", "Obese"))

# Convert predictors to factors (replace with your variables)
factor_vars <- c('area', 'age_cat', 'religion', 'edu', 'occup', 'marriage_age', 'parity',
                 'births_last5', 'breastfeeding', 'contraceptive', 'autonomy', 'media',
                 'husb_edu', 'hh_size', 'wealth', 'division')

for (v in factor_vars) {
    df[[v]] <- as.factor(df[[v]])
}

# 1. Univariate multinomial logistic regression to select variables with p < 0.25

significant_vars <- c()

for (var in factor_vars) {
    formula_uni <- as.formula(paste("bmi ~", var))
    uni_model <- multinom(formula_uni, data = df, trace = FALSE)
    sum_uni <- summary(uni_model)
    
    # Calculate z-values and p-values
    z_vals <- sum_uni$coefficients / sum_uni$standard.errors
    p_vals <- 2 * (1 - pnorm(abs(z_vals)))
    
    if (any(p_vals < 0.25)) {
        significant_vars <- c(significant_vars, var)
        cat(paste("Variable selected:", var, " (min p =", signif(min(p_vals),3), ")\n"))
    }
}

cat("Variables included in final model:", significant_vars, "\n")

# 2. Fit final multinomial logistic regression with selected variables

formula_final <- as.formula(paste("bmi ~", paste(significant_vars, collapse = " + ")))
final_model <- multinom(formula_final, data = df, trace = FALSE)
summary_final <- summary(final_model)

# 3. Calculate AOR, 95% CI and p-values

coef_mat <- coef(final_model)
se_mat <- summary_final$standard.errors

z_vals <- coef_mat / se_mat
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

AOR <- exp(coef_mat)
lower_CI <- exp(coef_mat - 1.96 * se_mat)
upper_CI <- exp(coef_mat + 1.96 * se_mat)

# Create a tidy results table

results <- data.frame(
    Outcome = rep(rownames(coef_mat), each = ncol(coef_mat)),
    Predictor = rep(colnames(coef_mat), times = nrow(coef_mat)),
    Coef = as.vector(coef_mat),
    SE = as.vector(se_mat),
    z = as.vector(z_vals),
    p_value = as.vector(p_vals),
    AOR = as.vector(AOR),
    lower_95CI = as.vector(lower_CI),
    upper_95CI = as.vector(upper_CI)
)

# 4. Filter significant predictors only (p < 0.05)

results_significant <- results %>% filter(p_value < 0.05)

# Print results
print(results_significant)


# === Extract only "Underweight" outcome summary ===
underweight_summary <- results_significant[results_significant$Outcome == "Underweight", ]
print(underweight_summary)


# === Extract only "Underweight" outcome summary ===
underweight <- results[results$Outcome == "Underweight", ]
print(underweight)













































#---------Urban__________




# Load required libraries
library(nnet)    # multinom()
library(car)     # vif()
library(dplyr)   # data manipulation

# Step 0: Filter Urban women only
df_urban <- df %>% filter(area == "Urban")

# Convert outcome and predictors to factors in df_urban
df_urban$bmi <- factor(df_urban$bmi, levels = c("Normal", "Underweight", "Overweight", "Obese"))

# Remove 'area' from predictors because df_urban$area is constant ("Urban")
factor_vars <- c('age_cat', 'religion', 'edu', 'occup', 'marriage_age', 'parity',
                 'births_last5', 'breastfeeding', 'contraceptive', 'autonomy', 'media',
                 'husb_edu', 'hh_size', 'wealth', 'division')

for (v in factor_vars) {
    df_urban[[v]] <- as.factor(df_urban[[v]])
}

# 1. Univariate multinomial logistic regression on Urban subset
significant_vars <- c()

for (var in factor_vars) {
    formula_uni <- as.formula(paste("bmi ~", var))
    uni_model <- multinom(formula_uni, data = df_urban, trace = FALSE)
    sum_uni <- summary(uni_model)
    
    # Calculate z-values and p-values
    z_vals <- sum_uni$coefficients / sum_uni$standard.errors
    p_vals <- 2 * (1 - pnorm(abs(z_vals)))
    
    # Check if any p-value for this predictor < 0.25
    if (any(p_vals < 0.25)) {
        significant_vars <- c(significant_vars, var)
        cat(paste("Variable selected:", var, " (min p =", signif(min(p_vals),3), ")\n"))
    }
}

cat("Variables included in final model (Urban only):", significant_vars, "\n")

# 2. Fit final multinomial logistic regression on Urban subset
formula_final <- as.formula(paste("bmi ~", paste(significant_vars, collapse = " + ")))
final_model <- multinom(formula_final, data = df_urban, trace = FALSE)
summary_final <- summary(final_model)

# 3. Calculate AOR, 95% CI, and p-values
coef_mat <- coef(final_model)
se_mat <- summary_final$standard.errors
z_vals <- coef_mat / se_mat
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

AOR <- exp(coef_mat)
lower_CI <- exp(coef_mat - 1.96 * se_mat)
upper_CI <- exp(coef_mat + 1.96 * se_mat)

# Create results dataframe
results1 <- data.frame(
    Outcome = rep(rownames(coef_mat), each = ncol(coef_mat)),
    Predictor = rep(colnames(coef_mat), times = nrow(coef_mat)),
    Coef = as.vector(coef_mat),
    SE = as.vector(se_mat),
    z = as.vector(z_vals),
    p_value = as.vector(p_vals),
    AOR = as.vector(AOR),
    lower_95CI = as.vector(lower_CI),
    upper_95CI = as.vector(upper_CI)
)

# 4. Filter significant predictors only (p < 0.05)
results_significant1 <- results1 %>% filter(p_value < 0.05)

# Print all significant results
print(results_significant1)

# 5. Extract significant results for Underweight outcome only
underweight_summary <- results_significant1 %>% filter(Outcome == "Underweight")
print(underweight_summary)

# Optional: Also print all results for Underweight (significant and non-significant)
underweight_all <- results1 %>% filter(Outcome == "Underweight")
print(underweight_all)

# Function to add significance stars based on p-value
add_signif_stars <- function(p) {
    if (is.na(p)) {
        return("")
    } else if (p < 0.001) {
        return("***")
    } else if (p < 0.01) {
        return("**")
    } else if (p < 0.05) {
        return("*")
    } else {
        return("")
    }
}

# Apply function to add a new column 'signif' in underweight_all
underweight_all$signif <- sapply(underweight_all$p_value, add_signif_stars)

# Optionally, create a combined p-value with stars for printing
underweight_all$p_value_with_signif <- paste0(
    round(underweight_all$p_value, 4), 
    underweight_all$signif
)

# Print the table with stars showing significance
print(underweight_all[, c("Outcome", "Predictor", "Coef", "AOR", "p_value_with_signif", "lower_95CI", "upper_95CI")])






























#-----------Rural----------


# Load required libraries
library(nnet)    # multinom()
library(car)     # vif()
library(dplyr)   # data manipulation

# Step 0: Filter Rural women only
df_rural <- df %>% filter(area == "Rural")

# Convert outcome and predictors to factors in df_rural
df_rural$bmi <- factor(df_rural$bmi, levels = c("Normal", "Underweight", "Overweight", "Obese"))

# Remove 'area' from predictors because df_rural$area is constant ("Rural")
factor_vars <- c('age_cat', 'religion', 'edu', 'occup', 'marriage_age', 'parity',
                 'births_last5', 'breastfeeding', 'contraceptive', 'autonomy', 'media',
                 'husb_edu', 'hh_size', 'wealth', 'division')

for (v in factor_vars) {
    df_rural[[v]] <- as.factor(df_rural[[v]])
}

# 1. Univariate multinomial logistic regression on Rural subset
significant_vars_rural <- c()

for (var in factor_vars) {
    formula_uni <- as.formula(paste("bmi ~", var))
    uni_model <- multinom(formula_uni, data = df_rural, trace = FALSE)
    sum_uni <- summary(uni_model)
    
    # Calculate z-values and p-values
    z_vals <- sum_uni$coefficients / sum_uni$standard.errors
    p_vals <- 2 * (1 - pnorm(abs(z_vals)))
    
    # Check if any p-value for this predictor < 0.25
    if (any(p_vals < 0.25)) {
        significant_vars_rural <- c(significant_vars_rural, var)
        cat(paste("Variable selected:", var, " (min p =", signif(min(p_vals),3), ")\n"))
    }
}

cat("Variables included in final model (Rural only):", significant_vars_rural, "\n")

# 2. Fit final multinomial logistic regression on Rural subset
formula_final_rural <- as.formula(paste("bmi ~", paste(significant_vars_rural, collapse = " + ")))
final_model_rural <- multinom(formula_final_rural, data = df_rural, trace = FALSE)
summary_final_rural <- summary(final_model_rural)

# 3. Calculate AOR, 95% CI, and p-values
coef_mat_rural <- coef(final_model_rural)
se_mat_rural <- summary_final_rural$standard.errors
z_vals_rural <- coef_mat_rural / se_mat_rural
p_vals_rural <- 2 * (1 - pnorm(abs(z_vals_rural)))

AOR_rural <- exp(coef_mat_rural)
lower_CI_rural <- exp(coef_mat_rural - 1.96 * se_mat_rural)
upper_CI_rural <- exp(coef_mat_rural + 1.96 * se_mat_rural)

# Create results dataframe
results2 <- data.frame(
    Outcome = rep(rownames(coef_mat_rural), each = ncol(coef_mat_rural)),
    Predictor = rep(colnames(coef_mat_rural), times = nrow(coef_mat_rural)),
    Coef = as.vector(coef_mat_rural),
    SE = as.vector(se_mat_rural),
    z = as.vector(z_vals_rural),
    p_value = as.vector(p_vals_rural),
    AOR = as.vector(AOR_rural),
    lower_95CI = as.vector(lower_CI_rural),
    upper_95CI = as.vector(upper_CI_rural)
)

# 4. Filter significant predictors only (p < 0.05)
results_significant2 <- results2 %>% filter(p_value < 0.05)
print(results_significant2)

# 5. Extract significant results for Underweight outcome only
underweight_summary_rural <- results_significant2 %>% filter(Outcome == "Underweight")
print(underweight_summary_rural)

# Optional: Also print all results for Underweight (significant and non-significant)
underweight_all_rural <- results2 %>% filter(Outcome == "Underweight")
print(underweight_all_rural)

# Function to add significance stars
add_signif_stars <- function(p) {
    if (is.na(p)) {
        return("")
    } else if (p < 0.001) {
        return("***")
    } else if (p < 0.01) {
        return("**")
    } else if (p < 0.05) {
        return("*")
    } else {
        return("")
    }
}

# Add stars
underweight_all_rural$signif <- sapply(underweight_all_rural$p_value, add_signif_stars)
underweight_all_rural$p_value_with_signif <- paste0(
    round(underweight_all_rural$p_value, 4), 
    underweight_all_rural$signif
)

# Print final table
print(underweight_all_rural[, c("Outcome", "Predictor", "Coef", "AOR", "p_value_with_signif", "lower_95CI", "upper_95CI")])


library(ggplot2)
library(dplyr)
library(tidyr)

# Your updated BMI data with detailed labels and actual values
bmi_table <- data.frame(
    BMI_Category = c(
        "Underweight (BMI < 18.5)",
        "Normal (BMI 18.5 to < 23)",
        "Overweight (BMI 23 to < 27.5)",
        "Obese (BMI ≥ 27.5)"
    ),
    Overall = c(13.30, 37.20, 33.23, 16.27),
    Rural   = c(15.28, 39.07, 32.48, 13.18),
    Urban   = c(9.40, 33.78, 34.77, 22.05)
)

# Convert to long format for ggplot
bmi_long <- bmi_table %>%
    pivot_longer(cols = c("Overall", "Rural", "Urban"),
                 names_to = "Residence", values_to = "Prevalence")

# Ensure BMI_Category is ordered correctly
bmi_long$BMI_Category <- factor(bmi_long$BMI_Category,
                                levels = c("Underweight (BMI < 18.5)",
                                           "Normal (BMI 18.5 to < 23)",
                                           "Overweight (BMI 23 to < 27.5)",
                                           "Obese (BMI ≥ 27.5)"))

bmi_long$Residence <- factor(bmi_long$Residence, levels = c("Overall", "Rural", "Urban"))

# Plot with bar values above
ggplot(bmi_long, aes(x = BMI_Category, y = Prevalence, fill = Residence)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.75) +
    geom_text(aes(label = sprintf("%.1f", Prevalence)),
              position = position_dodge(width = 0.9),
              vjust = -0.4,
              size = 4,
              fontface = "bold") +
    labs(title = "Prevalence of BMI Categories by Residence",
         x = "BMI Category", y = "Percentage (%)") +
    scale_fill_manual(values = c("Overall" = "#4F81BD",   # Blue
                                 "Rural" = "#C0504D",     # Red
                                 "Urban" = "#9BBB59")) +  # Green
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.title = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylim(0, 45)


















































install.packages("stringr")
library(dplyr)
library(tidyr)
library(DescTools)

# Function to add significance stars
significance_star <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else return("")
}

# Final robust function
calc_bmi_prevalence_with_p <- function(df_urban, df_rural, group_var, bmi_cat) {
    # Summarize urban and rural counts
    urban <- df_urban %>%
        group_by(across(all_of(group_var))) %>%
        summarise(n_urban = n(),
                  count_urban = sum(bmi == bmi_cat), .groups = "drop")
    
    rural <- df_rural %>%
        group_by(across(all_of(group_var))) %>%
        summarise(n_rural = n(),
                  count_rural = sum(bmi == bmi_cat), .groups = "drop")
    
    # Merge and process row-by-row
    merged <- full_join(urban, rural, by = group_var) %>%
        rowwise() %>%
        mutate(
            n_urban = ifelse(is.na(n_urban), 0, n_urban),
            count_urban = ifelse(is.na(count_urban), 0, count_urban),
            n_rural = ifelse(is.na(n_rural), 0, n_rural),
            count_rural = ifelse(is.na(count_rural), 0, count_rural),
            total = n_urban + n_rural,
            count_total = count_urban + count_rural,
            
            # Urban CI
            urban_ci_lwr = BinomCI(count_urban, n_urban, method = "wilson")[, 2],
            urban_ci_upr = BinomCI(count_urban, n_urban, method = "wilson")[, 3],
            urban_percent = round(count_urban / n_urban * 100, 1),
            urban_lower = round(urban_ci_lwr * 100, 1),
            urban_upper = round(urban_ci_upr * 100, 1),
            
            # Rural CI
            rural_ci_lwr = BinomCI(count_rural, n_rural, method = "wilson")[, 2],
            rural_ci_upr = BinomCI(count_rural, n_rural, method = "wilson")[, 3],
            rural_percent = round(count_rural / n_rural * 100, 1),
            rural_lower = round(rural_ci_lwr * 100, 1),
            rural_upper = round(rural_ci_upr * 100, 1),
            
            # Overall CI
            overall_ci_lwr = BinomCI(count_total, total, method = "wilson")[, 2],
            overall_ci_upr = BinomCI(count_total, total, method = "wilson")[, 3],
            overall_percent = round(count_total / total * 100, 1),
            overall_lower = round(overall_ci_lwr * 100, 1),
            overall_upper = round(overall_ci_upr * 100, 1),
            
            # p-value
            p_value = tryCatch({
                suppressWarnings(prop.test(c(count_urban, count_rural),
                                           c(n_urban, n_rural))$p.value)
            }, error = function(e) NA),
            
            stars = significance_star(p_value),
            
            # Final columns with stars
            Urban = paste0(urban_percent, " [", urban_lower, ", ", urban_upper, "]", stars),
            Rural = paste0(rural_percent, " [", rural_lower, ", ", rural_upper, "]", stars),
            Overall = paste0(overall_percent, " [", overall_lower, ", ", overall_upper, "]"),
            BMI = bmi_cat
        ) %>%
        ungroup() %>%
        select(all_of(group_var), BMI, Urban, Rural, Overall)
    
    return(merged)
}
# Define grouping variable
group_var <- "age_cat"  # change to "edu", "wealth", etc.

# BMI categories
bmi_cats <- c("Underweight", "Overweight", "Obese")

# Loop through each BMI category
table2_list <- lapply(bmi_cats, function(b) {
    calc_bmi_prevalence_with_p(df_urban, df_rural, group_var, b)
})

# Combine and reshape to wide format
table2 <- bind_rows(table2_list) %>%
    pivot_wider(names_from = BMI, values_from = c(Urban, Rural, Overall))

# View or export
print(table2)
# write.csv(table2, "Table2_BMI_by_Age_with_CI_and_Stars.csv", row.names = FALSE)





















library(ggplot2)
library(dplyr)
library(tidyr)

# Data (already provided)
bmi_table <- data.frame(
    BMI_Category = c("Underweight (BMI < 18.5)",
                     "Normal (BMI 18.5 to < 23)",
                     "Overweight (BMI 23 to < 27.5)",
                     "Obese (BMI ≥ 27.5)"),
    Overall = c(13.30, 37.20, 33.23, 16.27),
    Rural = c(15.28, 39.07, 32.48, 13.18),
    Urban = c(9.40, 33.78, 34.77, 22.05)
)

# Prepare for plot
bmi_long <- bmi_table %>%
    pivot_longer(cols = c("Overall", "Rural", "Urban"),
                 names_to = "Residence", values_to = "Prevalence") %>%
    mutate(BMI_Category = factor(BMI_Category, levels = bmi_table$BMI_Category),
           Residence = factor(Residence, levels = c("Overall", "Rural", "Urban")))

# Plot
p <- ggplot(bmi_long, aes(x = BMI_Category, y = Prevalence, fill = Residence)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.75) +
    geom_text(aes(label = sprintf("%.1f", Prevalence)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3.8, fontface = "bold") +
    labs(title = "Prevalence of BMI Categories by Residence",
         x = "BMI Category", y = "Percentage (%)") +
    scale_fill_manual(values = c("Overall" = "#4F81BD", "Rural" = "#C0504D", "Urban" = "#9BBB59")) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1),
          legend.title = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.grid.major.x = element_blank())
p
# Save as high-quality image
ggsave("bmi_prevalence_residence.png", p, width = 9, height = 6, dpi = 600)



# Example: underweight_summary dataframe is assumed available
library(forcats)


forest_data <- underweight_summary %>%
    mutate(Predictor = fct_reorder(Predictor, AOR))

forest_plot <- ggplot(forest_data, aes(x = AOR, y = Predictor)) +
    geom_point(size = 3, color = "#E69F00") +
    geom_errorbarh(aes(xmin = lower_95CI, xmax = upper_95CI), height = 0.2, color = "#999999") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    labs(title = "Adjusted Odds Ratios (AOR) for Underweight - Urban Women",
         x = "Adjusted Odds Ratio (95% CI)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
forest_plot
ggsave("AOR_forest_underweight_urban.png", forest_plot, width = 8, height = 5, dpi = 600)









install.packages("GGally")



library(GGally)
df_subset <- df %>% select(age_cat, parity, wealth, bmi)
ggpairs(df_subset, mapping = aes(color = bmi))





df %>%
    group_by(division, wealth, bmi) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(division, wealth) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ggplot(aes(x = wealth, y = division, fill = percent)) +
    geom_tile() +
    facet_wrap(~bmi) +
    scale_fill_gradient(low = "white", high = "darkblue") +
    labs(title = "Prevalence of BMI Categories by Region and Wealth",
         x = "Wealth", y = "Division", fill = "Percent") +
    theme_minimal()














library(ggplot2)
library(dplyr)
library(tidyr)

# Data
bmi_table <- data.frame(
    BMI_Category = c(
        "Underweight (BMI < 18.5)",
        "Normal (BMI 18.5–22.9)",
        "Overweight (BMI 23–27.4)",
        "Obese (BMI ≥ 27.5)"
    ),
    Overall = c(13.30, 37.20, 33.23, 16.27),
    Rural   = c(15.28, 39.07, 32.48, 13.18),
    Urban   = c(9.40, 33.78, 34.77, 22.05)
)

# Long format
bmi_long <- bmi_table %>%
    pivot_longer(cols = c("Overall", "Rural", "Urban"),
                 names_to = "Residence", values_to = "Prevalence")

bmi_long$BMI_Category <- factor(bmi_long$BMI_Category,
                                levels = c("Underweight (BMI < 18.5)",
                                           "Normal (BMI 18.5–22.9)",
                                           "Overweight (BMI 23–27.4)",
                                           "Obese (BMI ≥ 27.5)"))

bmi_long$Residence <- factor(bmi_long$Residence, levels = c("Overall", "Rural", "Urban"))

# Custom colors (Colorblind-friendly)
cb_palette <- c("Overall" = "#0072B2",   # Blue
                "Rural"   = "#E69F00",   # Orange
                "Urban"   = "#009E73")   # Green

# Plot
ggplot(bmi_long, aes(x = BMI_Category, y = Prevalence, fill = Residence)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.65) +
    geom_text(aes(label = sprintf("%.1f", Prevalence)),
              position = position_dodge(width = 0.8),
              vjust = -0.5,
              size = 3.8,
              color = "black",
              family = "sans") +
    scale_fill_manual(values = cb_palette) +
    labs(
        title = "Distribution of BMI Categories Among Women by Place of Residence",
        x = "BMI Category",
        y = "Prevalence (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 0, face = "bold"),
        axis.text.y = element_text(face = "plain"),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.box.background = element_rect(color = "gray80"),
        legend.key = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank()
    ) +
    ylim(0, 45)

