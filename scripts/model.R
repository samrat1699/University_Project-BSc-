run_multinom_rural <- function(df, factor_vars, outcome_var = "bmi", ref_level = "Normal") {
    # Filter rural and set factor levels
    df_rural <- df %>% filter(area == "Rural")
    df_rural[[outcome_var]] <- factor(df_rural[[outcome_var]], levels = c(ref_level, "Underweight", "Overweight", "Obese"))
    
    # Convert predictors to factor
    for (v in factor_vars) {
        df_rural[[v]] <- as.factor(df_rural[[v]])
    }
    
    # 1. Univariate multinomial logistic regression for variable selection
    significant_vars <- c()
    for (var in factor_vars) {
        formula_uni <- as.formula(paste(outcome_var, "~", var))
        uni_model <- multinom(formula_uni, data = df_rural, trace = FALSE)
        sum_uni <- summary(uni_model)
        
        z_vals <- sum_uni$coefficients / sum_uni$standard.errors
        p_vals <- 2 * (1 - pnorm(abs(z_vals)))
        
        if (any(p_vals < 0.25)) {
            significant_vars <- c(significant_vars, var)
        }
    }
    
    # 2. Final multinomial logistic regression
    formula_final <- as.formula(paste(outcome_var, "~", paste(significant_vars, collapse = " + ")))
    final_model <- multinom(formula_final, data = df_rural, trace = FALSE)
    summary_final <- summary(final_model)
    
    coef_mat <- coef(final_model)
    se_mat <- summary_final$standard.errors
    z_vals <- coef_mat / se_mat
    p_vals <- 2 * (1 - pnorm(abs(z_vals)))
    
    AOR <- exp(coef_mat)
    lower_CI <- exp(coef_mat - 1.96 * se_mat)
    upper_CI <- exp(coef_mat + 1.96 * se_mat)
    
    # Prepare output dataframe with all outcomes and predictors
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
    
    # Add significance stars
    add_signif_stars <- function(p) {
        if (is.na(p)) return("")
        else if (p < 0.001) return("***")
        else if (p < 0.01) return("**")
        else if (p < 0.05) return("*")
        else return("")
    }
    results$signif <- sapply(results$p_value, add_signif_stars)
    results$p_value_with_signif <- paste0(round(results$p_value, 4), results$signif)
    
    # Split results by Outcome
    underweight_res <- results %>% filter(Outcome == "Underweight")
    overweight_res <- results %>% filter(Outcome == "Overweight")
    obese_res <- results %>% filter(Outcome == "Obese")
    
    # Return list of dataframes
    return(list(
        all_results = results,
        underweight = underweight_res,
        overweight = overweight_res,
        obese = obese_res
    ))
}




factor_vars <- c('age_cat', 'religion', 'edu', 'occup', 'marriage_age', 'parity',
                 'births_last5', 'breastfeeding', 'contraceptive', 'autonomy', 'media',
                 'husb_edu', 'hh_size', 'wealth', 'division')

results_list <- run_multinom_rural(df, factor_vars)