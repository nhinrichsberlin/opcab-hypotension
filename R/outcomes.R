#' Estimates the desired treatment effect (odds ratio)
#' with confidence interval and p-value (two-sided test)
#' @param formula The regression formula
#' @param data The data that includes all elements of the formula
#' @param vcov The covariance estimator to be used. NULL means default estimator
#' @param clusters For cluster-robust standard error estimation, this indicates which observation belongs to which cluster
#' @param alpha The confidence level
#' @param treatment_name The name of the treatment variable
#' @return An estimate of the desired treatment effect with CI and p-value 
.estimate_odds_ratios <- function(formula,
                                  treatment_name = "treatment",
                                  data) {
  
  if (!("weights" %in% names(data))) {
    stop("data needs to contain a weights column.")
  }
  
  if (!("subclass" %in% names(data))) {
    stop("data needs to contain a subclass column.")
  }
  
  alpha <- 0.05
  formula <- formula(formula)
  family <- quasibinomial(link = "logit")
  
  reg <- stats::glm(formula = formula, 
                    family = family,
                    data = data,
                    weights = weights)
  
  test  <- lmtest::coeftest(reg,
                            vcov. = sandwich::vcovCL,
                            cluster = data$subclass)
  
  coef_treatment <- test[treatment_name, "Estimate"]
  std_coef_treatment <- test[treatment_name, "Std. Error"]
  p <- test[treatment_name, "Pr(>|z|)"]
  
  result <- list(
    estimate = exp(coef_treatment),
    lower = exp(coef_treatment - qnorm(1 - alpha / 2) * std_coef_treatment),
    upper = exp(coef_treatment + qnorm(1 - alpha / 2) * std_coef_treatment),
    p = p
  )
  
}


#' Estimates the desired treatment effect (mean difference)
#' with confidence interval and p-value (two-sided test)
#' @param formula The regression formula
#' @param data The data that includes all elements of the formula
#' @param vcov The covariance estimator to be used. NULL means default estimator
#' @param clusters For cluster-robust standard error estimation, this indicates which observation belongs to which cluster
#' @param alpha The confidence level
#' @param treatment_name The name of the treatment variable
#' @return An estimate of the desired treatment effect with CI and p-value 
.estimate_mean_difference <- function(formula,
                                      treatment_name,
                                      data) {
  
  if (!("weights" %in% names(data))) {
    stop("data needs to contain a weights column.")
  }
  
  if (!("subclass" %in% names(data))) {
    stop("data needs to contain a subclass column.")
  }
  
  alpha <- 0.05
  formula <- formula(formula)
  family <- "gaussian"
  
  reg <- stats::glm(formula = formula, 
                    family = family,
                    data = data,
                    weights = weights)
  
  test  <- lmtest::coeftest(reg,
                            vcov. = sandwich::vcovCL,
                            cluster = data$subclass)
  
  coef_treatment <- test[treatment_name, "Estimate"]
  std_coef_treatment <- test[treatment_name, "Std. Error"]
  p <- test[treatment_name, "Pr(>|z|)"]
  
  result <- list(
    estimate = coef_treatment,
    lower = coef_treatment - qnorm(1 - alpha / 2) * std_coef_treatment,
    upper = coef_treatment + qnorm(1 - alpha / 2) * std_coef_treatment,
    p = p
  )
  
}


or_table <- function(vars,
                     labels,
                     data_unmatched,
                     data_matched,
                     treatment_name = "treatment") {
  
  t_unmatched <-
    data_unmatched %>% 
    t1(numerical_vars = NULL,
       categorical_vars = vars,
       var_labels = labels,
       treatment = treatment_name,
       test = FALSE)
  
  t_matched <-
    data_matched %>% 
    t1(numerical_vars = NULL,
       categorical_vars = vars,
       var_labels = labels,
       treatment = treatment_name,
       test = FALSE)
  
  # Estimate Odds ratios per outcome
  or <- 
    purrr::cross_df(list(outcome = vars,
                         treatment = treatment_name)) %>% 
    dplyr::mutate(formula = paste0(outcome, " ~ ", treatment)) %>% 
    dplyr::mutate(or_estimation = purrr::map(formula,
                                             .estimate_odds_ratios,
                                             data = data_matched,
                                             treatment_name = treatment_name)) %>% 
    tidyr::unnest_wider(or_estimation) %>% 
    dplyr::select(outcome, estimate, lower, upper, p)
  
  or <-
    or %>% 
    # manually fix display in case of very large values
    dplyr::mutate_at(c("estimate", "lower", "upper"), ~dplyr::case_when(. > 1000 ~ Inf,
                                                                        TRUE ~ .)) %>% 
    dplyr::mutate("CI" = paste0("[", lower %>% round(3), "; ", upper %>% round(3), "]")) %>% 
    dplyr::rename(" " = outcome, 
                  "OR" = estimate) %>% 
    dplyr::select(c(" ", "OR", "CI", "p")) %>% 
    dplyr::mutate(" " = map_to_named_list(!!rlang::sym(" "), labels))
  
  t <- 
    t_unmatched %>% 
    dplyr::left_join(t_matched, by = " ") %>% 
    dplyr::left_join(or, by = " ") %>% 
    # round and turn into characters so we an add a matching status column
    dplyr::mutate_at(c("OR", "p"), .funs = ~round(., 3)) %>% 
    dplyr::mutate(p = dplyr::case_when(p < 0.001 ~ "<0.001",
                                       TRUE ~ as.character(p))) %>% 
    dplyr::mutate_all(.funs = ~as.character(.)) %>% 
    dplyr::rename(Control = Control.x,
                  "Control " = Control.y,
                  Treatment = Treatment.x,
                  "Treatment " = Treatment.y) %>% 
    dplyr::add_row(" " = "Matching status",
                   Control = "unmatched",
                   Treatment = "unmatched",
                   "Control " = "matched",
                   "Treatment " = "matched",
                   OR = "matched",
                   CI = "matched",
                   p = "matched",
                   .before = 1)
  
  return(t)
  
}


mean_diff_table <- function(vars,
                            mean_std = NULL,
                            labels,
                            data_unmatched,
                            data_matched,
                            treatment_name) {
  
  t_unmatched <-
    data_unmatched %>% 
    t1(numerical_vars = vars,
       numerical_vars_mean_std = mean_std,
       categorical_vars = NULL,
       var_labels = labels,
       treatment = treatment_name,
       test = FALSE)
  
  t_matched <-
    data_matched %>% 
    t1(numerical_vars = vars,
       numerical_vars_mean_std = mean_std,
       categorical_vars = NULL,
       var_labels = labels,
       treatment = treatment_name,
       test = FALSE)
  
  # Estimate Odds ratios per outcome
  mean_diff <- 
    purrr::cross_df(list(outcome = vars,
                         treatment = treatment_name))%>% 
    dplyr::mutate(formula = paste0(outcome, " ~ ", treatment)) %>% 
    dplyr::mutate(mean_diff_estimation = purrr::map(formula,
                                                    .estimate_mean_difference,
                                                    data = data_matched,
                                                    treatment_name = treatment_name)) %>% 
    tidyr::unnest_wider(mean_diff_estimation) %>% 
    dplyr::select(outcome, estimate, lower, upper, p)
  
  mean_diff <-
    mean_diff %>% 
    dplyr::mutate("CI" = paste0("[", lower %>% round(3), " ; ", upper %>% round(3), "]")) %>% 
    dplyr::rename(" " = outcome, 
                  "Mean Difference" = estimate) %>% 
    dplyr::select(c(" ", "Mean Difference", "CI", "p")) %>% 
    dplyr::mutate(" " = map_to_named_list(!!rlang::sym(" "), labels))
  
  t <- 
    t_unmatched %>% 
    dplyr::left_join(t_matched, by = " ") %>% 
    dplyr::left_join(mean_diff, by = " ") %>% 
    # round and turn into characters so we an add a matching status column
    dplyr::mutate_at(c("Mean Difference", "p"), .funs = ~round(., 3)) %>% 
    dplyr::mutate(p = dplyr::case_when(p < 0.001 ~ "<0.001",
                                       TRUE ~ as.character(p))) %>% 
    dplyr::mutate_all(.funs = ~as.character(.)) %>% 
    dplyr::rename(Control = Control.x,
                  "Control " = Control.y,
                  Treatment = Treatment.x,
                  "Treatment " = Treatment.y) %>% 
    dplyr::add_row(" " = "Matching status",
                   Control = "unmatched",
                   Treatment = "unmatched",
                   "Control " = "matched",
                   "Treatment " = "matched",
                   "Mean Difference" = "matched",
                   CI = "matched",
                   p = "matched",
                   .before = 1)
  
  return(t)
  
}
