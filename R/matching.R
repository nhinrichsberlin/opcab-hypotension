map_to_named_list <- function(char_vector, named_list) {
  
  mapped_vector <- 
    char_vector %>% 
    purrr::modify(~purrr::pluck(named_list, ., .default = .))
  
}

#' Performs matching using the MatchIt package
#' @param formula A formula: treatment ~ confounders
#' @param data A data frame containing treatment and confounders
#' @param distance The distance measure. "glm" will run propensity score matching. Could be changed e.g. to randomforest or nnet
#' @param link The link function of the glm. logit for logistic regression, probit for the probit model.
#' @param method The matching method to be used
#' @return A named list containing outputs of the matching procedure
.match <- function(data, 
                   formula, 
                   distance = "glm",
                   link = "logit",
                   method = "nearest",
                   caliper = NA,
                   ratio = 1,
                   m.order = "largest",
                   estimand = "ATT") {
  
  # set seed for reproducibility
  set.seed(123)
  
  # matchit expects caliper to be NULL if none is to be used
  # But we need to pass it as NA because NULL does not work
  # with purrr::cross_df(...)
  caliper <- if (caliper %>% is.na()) NULL else caliper
  
  # Run the matching procedure
  match  <- MatchIt::matchit(formula = formula,
                             data = data,
                             distance = distance,
                             link = link,
                             method = method,
                             caliper = caliper,
                             std.caliper = FALSE,
                             ratio = ratio,
                             replace = FALSE,
                             estimand = estimand,
                             m.order = m.order)
  
  # Summarize the output
  match_summary <- 
    match %>% 
    summary(standardize = TRUE, interactions = FALSE)
  
  # Calculate some "goodness of fit" summary statistics
  std_mean_diff <- 
    match_summary %>% 
    purrr::pluck("sum.matched") %>% 
    dplyr::as_tibble() %>% 
    # drop interactions and squares
    dplyr::mutate(confounder_name = match_summary %>% purrr::pluck("sum.matched") %>% rownames()) %>% 
    dplyr::filter(confounder_name %in% names(data)) %>% 
    purrr::pluck("Std. Mean Diff.")
  
  avg_abs_std_mean_diff <- 
    std_mean_diff %>% 
    abs() %>% 
    mean()
  
  max_abs_std_mean_diff <- 
    std_mean_diff %>% 
    abs() %>% 
    max()
  
  # Get the resulting matched data
  data_matched <- 
    match %>% 
    MatchIt::match.data()
  
  # Put it all together
  match_result <- list(
    match = match,
    data_matched = data_matched,
    match_summary = match_summary,
    avg_abs_std_mean_diff = avg_abs_std_mean_diff,
    max_abs_std_mean_diff = max_abs_std_mean_diff
  )
  
  return(match_result)
  
}


#' Tries different matching specifications and chooses the one optimising a user-defined criterion
#' @param data The data for matching, which includes the treatment variable and confounders
#' @param formula The regression formula
#' @param distance The MatchIt distance parameter, indicating which model to use for PS estimation
#' @param link The MatchIt link parameter, indicating the desired link function
#' @param method The MatchIt method parameter, indicating the matching method to be used
#' @param caliper The MatchIt caliper parameter. Cannot be used if method="full" and max_k_to_one != Inf
#' @param criterion The criterion based on which the best matching specification is to be chosen (options max_abs_std_mean_diff, avg_abs_std_mean_diff)
#' @param criterion_secondary The criterion on which to sort in case of ties in the primary criterion
#' @param estimand Whether the desired treatment effect is an ATT or ATE
#' @param n_cores Number of cores to use in the computation (> 1 will automatically activate multi-processing)
#' @return A call to .match using the optimal matching specification
optimize_matching <- function(data, 
                              formula, 
                              distance = c("glm"),
                              link = c("logit"),
                              method = c("nearest"),
                              caliper = NA,
                              ratio = c(1),
                              m.order = c("largest"),
                              criterion = "max_abs_std_mean_diff",
                              criterion_secondary = "avg_abs_std_mean_diff",
                              estimand = "ATT",
                              n_cores = 1) {
  
  matching_quality <- tidyr::expand_grid(distance,
                                         link,
                                         method,
                                         ratio,
                                         caliper,
                                         m.order)
  
  # set up multiprocessing
  if (n_cores > 1) {
    
    cluster <- 
      multidplyr::new_cluster(n_cores) %>% 
      multidplyr::cluster_library("dplyr") %>% 
      multidplyr::cluster_library("purrr") %>% 
      multidplyr::cluster_library("MatchIt") %>% 
      multidplyr::cluster_copy(".match") %>% 
      multidplyr::cluster_copy("data") %>% 
      multidplyr::cluster_copy("formula") %>% 
      multidplyr::cluster_copy("estimand")
    
    matching_quality <-
      matching_quality %>% 
      dplyr::group_by(distance, link, method, ratio, caliper, m.order) %>% 
      multidplyr::partition(cluster)
    
  }
  
  matches <-
    matching_quality %>% 
    dplyr::mutate(match = purrr::pmap(list(distance,
                                           link,
                                           method,
                                           caliper,
                                           ratio,
                                           m.order),
                                      .match,
                                      data = data,
                                      formula = formula,
                                      estimand = estimand)) %>% 
    dplyr::collect() %>% 
    dplyr::ungroup()
  
  # unnest and sort by the desired criterion
  matches <- 
    matches %>% 
    tidyr::unnest_wider(match, strict = TRUE) %>% 
    dplyr::arrange(!!rlang::sym(criterion), !!rlang::sym(criterion_secondary))
  
  # display the caliper only if it is not NA
  cols_to_show <- c("distance",
                    "link",
                    "caliper",
                    "ratio",
                    "max_abs_std_mean_diff",
                    "avg_abs_std_mean_diff")
  
  print(matches %>% 
          dplyr::select(dplyr::all_of(cols_to_show)) %>% 
          dplyr::slice(1:10))
  
  return(.match(data = data,
                formula = formula,
                distance = matches %>% purrr::pluck("distance") %>% purrr::pluck(1),
                link = matches %>% purrr::pluck("link") %>% purrr::pluck(1),
                method = matches %>% purrr::pluck("method") %>% purrr::pluck(1),
                ratio = matches %>% purrr::pluck("ratio") %>% purrr::pluck(1),
                caliper = matches %>% purrr::pluck("caliper") %>% purrr::pluck(1),
                m.order = matches %>% purrr::pluck("m.order") %>% purrr::pluck(1),
                estimand = estimand))
  
}


smd_from_matched_data <- function(data_matched,
                                  data_unmatched, 
                                  confounders,
                                  confounder_labels,
                                  treatment = "treatment") {
  
  # calculate means per treatment_group in the unmatched data
  confounder_means_unmatched <-
    data_unmatched %>% 
    dplyr::group_by(!!rlang::sym(treatment)) %>% 
    dplyr::summarise_at(confounders, mean) %>% 
    dplyr::select(dplyr::all_of(confounders)) %>% 
    sjmisc::rotate_df() %>% 
    dplyr::rename(mean_0 = V1,
                  mean_1 = V2) %>% 
    dplyr::mutate(confounders = confounders)
  
  # calculate weighted means per treatment group in the matched data
  confounders_means_weighted <- 
    data_matched %>% 
    dplyr::mutate_at(confounders, ~.*weights) %>% 
    dplyr::group_by(!!rlang::sym(treatment)) %>% 
    dplyr::summarise_at(confounders, mean) %>% 
    dplyr::select(dplyr::all_of(confounders)) %>% 
    sjmisc::rotate_df() %>% 
    dplyr::rename(mean_w_0 = V1,
                  mean_w_1 = V2) %>% 
    dplyr::mutate(confounders = confounders)
  
  # calculate standard errors per treatment group in the unmatched data
  confounder_std <-
    data_unmatched %>% 
    dplyr::group_by(!!rlang::sym(treatment)) %>% 
    dplyr::summarise_at(confounders, ~sqrt(var(.))) %>% 
    dplyr::select(dplyr::all_of(confounders)) %>% 
    sjmisc::rotate_df() %>% 
    dplyr::rename(std_0 = V1,
                  std_1 = V2) %>% 
    dplyr::mutate(confounders = confounders)
  
  # calculate pre-matching SMD
  smd_pre <-
    confounder_means_unmatched %>% 
    dplyr::left_join(confounder_std, by = "confounders") %>% 
    dplyr::mutate(smd_pre_matching = (mean_1 - mean_0) / std_1)
  
  # calculate post-matching SMD
  smd_post <-
    confounders_means_weighted %>% 
    dplyr::left_join(confounder_std, by = "confounders") %>% 
    dplyr::mutate(smd_post_matching = (mean_w_1 - mean_w_0) / std_1)
  
  # join SMDs
  smd <-
    smd_pre %>% 
    dplyr::select(confounders, smd_pre_matching) %>% 
    dplyr::left_join(smd_post %>% dplyr::select(confounders, smd_post_matching), by = "confounders") %>% 
    dplyr::mutate_at(c("confounders"), .funs = map_to_named_list, named_list = confounder_labels)
  
  return(smd)
  
}


love_plot <- function(data_matched,
                      data_unmatched,
                      confounders,
                      confounder_labels,
                      output_path,
                      treatment = "treatment") {
  
  # calculate SMDs and absSMDs
  smd <- smd_from_matched_data(data_matched,
                               data_unmatched,
                               confounders,
                               confounder_labels,
                               treatment)
  
  p <- 
    smd %>% 
    dplyr::rename(Matched = smd_post_matching,
                  Unmatched = smd_pre_matching) %>% 
    dplyr::arrange(-abs(Unmatched)) %>% 
    # hack to keep the order
    dplyr::mutate(confounders = factor(confounders, levels = rev(confounders))) %>% 
    tidyr::pivot_longer(c("Matched", "Unmatched"),
                        values_to = "smd",
                        names_to = "matching_status") %>% 
    dplyr::select(confounders, matching_status, smd) %>% 
    dplyr::filter(confounders != "n") %>% 
    dplyr::group_by(matching_status) %>% 
    ggplot2::ggplot(aes(x = abs(smd),
                        y = confounders,
                        color = matching_status,
                        shape = matching_status)) +
    ggplot2::geom_point(size = 1.5) + 
    ggplot2::scale_color_manual(values = c("darkred", "blue")) + 
    ggplot2::scale_shape_manual(values = c(17, 16)) + 
    ggplot2::xlab("Absolute standardized mean difference") + 
    ggplot2::ylab("") + 
    # ggplot2::xlim(-1.05, 1.05) + 
    ggplot2::scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                                limits = c(-0.0025, 0.605)) + 
    ggplot2::geom_vline(xintercept = 0.1,
                        alpha = 0.5) + 
    ggplot2::theme_light() + 
    ggplot2::theme(panel.grid.major.x = element_line(linewidth = 0.1,
                                                     color = alpha("black", 0.1)),
                   panel.grid.major.y = element_line(linewidth = 0.1,
                                                     color = alpha("black", 0.1))) + 
    ggplot2::theme(legend.title = element_blank(), 
                   legend.box = "rect",
                   legend.position = c(0.55, 0.55))
  
  ggplot2::ggsave(output_path,
                  dpi = 500,
                  height = 7,
                  width = 9)
  
  return(p)
  
}
