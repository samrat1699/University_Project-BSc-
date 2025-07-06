library(dplyr)
library(tidyr)
library(DescTools)
library(nnet)   
library(car)     
library(dplyr)   


df <- read.csv("WomenNutrition.csv")

## Table 1. Background Characteristics of the Study Participants by Area of Residence

generate_table1 <- function(df, group_var = "area") {
    library(dplyr)
    
    # Variables to include in Table 1
    vars_to_tabulate <- c("age_cat", "religion", "edu", "occup", "marriage_age", "parity",
                          "births_last5", "breastfeeding", "contraceptive", "autonomy", "media",
                          "husb_edu", "hh_size", "wealth", "division")
    
    # Get overall total
    total_all <- nrow(df)
    
    table1 <- data.frame()
    
    for (var in vars_to_tabulate) {
        levels <- levels(as.factor(df[[var]]))  # or predefined order if you have
        
        for (lvl in levels) {
            row <- data.frame(
                Variable = var,
                Category = lvl,
                stringsAsFactors = FALSE
            )
            
            for (grp in c("Urban", "Rural")) {
                sub <- df %>% filter(!!sym(group_var) == grp)
                total_grp <- nrow(sub)
                count_grp <- sum(sub[[var]] == lvl, na.rm = TRUE)
                percent_grp <- round((count_grp / total_grp) * 100, 1)
                colname <- paste0(grp, " (n=", total_grp, ")")
                row[[colname]] <- paste0(count_grp, " (", percent_grp, "%)")
            }
            
            # Overall
            count_all <- sum(df[[var]] == lvl, na.rm = TRUE)
            percent_all <- round((count_all / total_all) * 100, 1)
            row[[paste0("Overall (n=", total_all, ")")]] <- paste0(count_all, " (", percent_all, "%)")
            
            table1 <- bind_rows(table1, row)
        }
    }
    
    return(table1)
}
















# Helper function to add significance stars based on p-value
significance_star <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else return("")
}

get_bmi_table_with_chisq <- function(df, group_var, bmi_cats = c("Underweight", "Overweight", "Obese", "Normal")) {
    df$bmi <- factor(df$bmi, levels = c("Normal", "Underweight", "Overweight", "Obese"))
    
    df_urban <- df %>% filter(area == "Urban")
    df_rural <- df %>% filter(area == "Rural")
    
    # Internal function for one BMI category
    calc_percent_with_chisq <- function(bmi_cat) {
        # Urban
        urban <- df_urban %>%
            group_by(across(all_of(group_var))) %>%
            summarise(n_urban = n(),
                      count_urban = sum(bmi == bmi_cat), .groups = "drop")
        
        # Rural
        rural <- df_rural %>%
            group_by(across(all_of(group_var))) %>%
            summarise(n_rural = n(),
                      count_rural = sum(bmi == bmi_cat), .groups = "drop")
        
        # Merge and compute % and chi-square
        merged <- full_join(urban, rural, by = group_var) %>%
            rowwise() %>%
            mutate(
                # Handle missing
                n_urban = ifelse(is.na(n_urban), 0, n_urban),
                count_urban = ifelse(is.na(count_urban), 0, count_urban),
                n_rural = ifelse(is.na(n_rural), 0, n_rural),
                count_rural = ifelse(is.na(count_rural), 0, count_rural),
                
                # Percentages
                urban_percent = round(count_urban / n_urban * 100, 1),
                rural_percent = round(count_rural / n_rural * 100, 1),
                overall_total = n_urban + n_rural,
                overall_count = count_urban + count_rural,
                overall_percent = round(overall_count / overall_total * 100, 1),
                
                # Chi-square
                p_value = tryCatch({
                    suppressWarnings(prop.test(c(count_urban, count_rural), c(n_urban, n_rural))$p.value)
                }, error = function(e) NA),
                star = significance_star(p_value),
                
                # Output format
                Urban = paste0(urban_percent, star),
                Rural = paste0(rural_percent, star),
                Overall = paste0(overall_percent),
                
                BMI = bmi_cat
            ) %>%
            ungroup() %>%
            select(all_of(group_var), BMI, Urban, Rural, Overall)
        
        return(merged)
    }
    
    # Loop over BMI categories
    table_list <- lapply(bmi_cats, calc_percent_with_chisq)
    
    # Combine and reshape
    table_full <- bind_rows(table_list) %>%
        pivot_wider(names_from = BMI, values_from = c(Urban, Rural, Overall))
    
    return(table_full)
}







# Significance stars helper
significance_star <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else return("")
}

# Main function: produce Table 2 style prevalence with CIs and stars
get_bmi_prevalence_table <- function(df, group_var, bmi_cats = c("Underweight", "Overweight", "Obese")) {
    # Ensure bmi factor
    df$bmi <- factor(df$bmi, levels = c("Normal", "Underweight", "Overweight", "Obese"))
    
    # Split urban and rural subsets
    df_urban <- df %>% filter(area == "Urban")
    df_rural <- df %>% filter(area == "Rural")
    
    # Internal function to calculate prevalence + CI + stars for one BMI category
    calc_prevalence <- function(bmi_cat) {
        # Summarize urban and rural
        urban <- df_urban %>%
            group_by(across(all_of(group_var))) %>%
            summarise(n_urban = n(),
                      count_urban = sum(bmi == bmi_cat), .groups = "drop")
        
        rural <- df_rural %>%
            group_by(across(all_of(group_var))) %>%
            summarise(n_rural = n(),
                      count_rural = sum(bmi == bmi_cat), .groups = "drop")
        
        # Merge and compute CIs, p-values, stars
        merged <- full_join(urban, rural, by = group_var) %>%
            rowwise() %>%
            mutate(
                n_urban = ifelse(is.na(n_urban), 0, n_urban),
                count_urban = ifelse(is.na(count_urban), 0, count_urban),
                n_rural = ifelse(is.na(n_rural), 0, n_rural),
                count_rural = ifelse(is.na(count_rural), 0, count_rural),
                total = n_urban + n_rural,
                count_total = count_urban + count_rural,
                
                urban_ci_lwr = BinomCI(count_urban, n_urban, method = "wilson")[, 2],
                urban_ci_upr = BinomCI(count_urban, n_urban, method = "wilson")[, 3],
                urban_percent = round(count_urban / n_urban * 100, 1),
                urban_lower = round(urban_ci_lwr * 100, 1),
                urban_upper = round(urban_ci_upr * 100, 1),
                
                rural_ci_lwr = BinomCI(count_rural, n_rural, method = "wilson")[, 2],
                rural_ci_upr = BinomCI(count_rural, n_rural, method = "wilson")[, 3],
                rural_percent = round(count_rural / n_rural * 100, 1),
                rural_lower = round(rural_ci_lwr * 100, 1),
                rural_upper = round(rural_ci_upr * 100, 1),
                
                overall_ci_lwr = BinomCI(count_total, total, method = "wilson")[, 2],
                overall_ci_upr = BinomCI(count_total, total, method = "wilson")[, 3],
                overall_percent = round(count_total / total * 100, 1),
                overall_lower = round(overall_ci_lwr * 100, 1),
                overall_upper = round(overall_ci_upr * 100, 1),
                
                p_value = tryCatch({
                    suppressWarnings(prop.test(c(count_urban, count_rural),
                                               c(n_urban, n_rural))$p.value)
                }, error = function(e) NA),
                
                stars = significance_star(p_value),
                
                Urban = paste0(urban_percent, " [", urban_lower, ", ", urban_upper, "]", stars),
                Rural = paste0(rural_percent, " [", rural_lower, ", ", rural_upper, "]", stars),
                Overall = paste0(overall_percent, " [", overall_lower, ", ", overall_upper, "]"),
                BMI = bmi_cat
            ) %>%
            ungroup() %>%
            select(all_of(group_var), BMI, Urban, Rural, Overall)
        
        return(merged)
    }
    
    # Loop over BMI categories and combine results
    table_list <- lapply(bmi_cats, calc_prevalence)
    
    table_full <- bind_rows(table_list) %>%
        pivot_wider(names_from = BMI, values_from = c(Urban, Rural, Overall))
    
    return(table_full)
}
















