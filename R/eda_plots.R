.deciles <- function(x) {
  
  m <- 
    x %>% 
    median(na.rm = TRUE)
  
  x <- 
    x %>% 
    dplyr::coalesce(m)
  
  q <- 
    x %>% 
    quantile(seq(0.1, 1, 0.1), type = 1) %>% 
    unique()
  
  cuts <- 
    x %>% 
    cut(q) %>% 
    dplyr::coalesce(paste0("[", min(x), ",", q[1], "]")) %>% 
    factor()
  
  # fix the level order
  lvls <- levels(cuts)
  lvls_first_int <- as.integer(gsub(",", "", substr(lvls, 2, 3)))
  new_lvls <- lvls[order(match(lvls_first_int, sort(lvls_first_int)))]
  
  cuts <- 
    cuts %>% 
    factor(levels = new_lvls)
  
  return(cuts)
}


.xlab <- function(time_column) {
  
  xlab <- NULL
  
  if (time_column == "max_consec_time_below_60") {
    xlab <- "Longest consecutive stretch (minutes) below BPM 60 mmHg"
  }
  if (time_column == "max_consec_time_below_50") {
    xlab <- "Longest consecutive stretch (minutes) below BPM 50 mmHg"
  }
  if (time_column == "total_time_below_60") {
    xlab <- "Total time (minutes) below BPM 60 mmHg"
  }
  if (time_column == "total_time_below_50") {
    xlab <- "Total time (minutes) below BPM 50 mmHg"
  }
  if (time_column == "bpm_lowest") {
    xlab <- "Lowest BPM (mmHg)"
  }
  
  if (xlab %>% is.null()) {
    stop("Invalid time_column: ", time_column)
  }
  
  return(xlab)
  
}


outcome_per_decile <- function(data,
                               time_column, 
                               out_file) {
  
  x_var <- paste0(time_column, "_decile")
  
  p <-
    data %>% 
    dplyr::group_by(!!rlang::sym(x_var)) %>% 
    dplyr::summarise(outcome_composite_mean = 100 * mean(outcome_composite),
                     n = n()) %>% 
    ggplot2::ggplot(aes(x = !!rlang::sym(x_var),
                        y = outcome_composite_mean,
                        label = paste0("n = ", n))) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "blue") +
    ggplot2::geom_text(vjust = -0.15) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.y = element_text(size = 10)) +
    ggplot2::xlab(.xlab(time_column)) +
    ggplot2::ylab("Stroke, myocardial infarction, \nrenal complications, or delirium (%)")
  
  ggplot2::ggsave(plot = p, out_file)
  
  return(p)
  
}


outcome_per_decile_fancy <- function(data,
                                     time_column, 
                                     out_file) {
  
  x_var <- paste0(time_column, "_decile")
  
  p <- 
    data %>% 
    dplyr::group_by(!!rlang::sym(x_var)) %>% 
    dplyr::summarise(outcome_composite_mean = 100 * mean(outcome_composite),
                     stroke_macce_mean = 100 * mean(stroke_macce), 
                     myo_infarction_macce_mean = 100 * mean(myo_infarction_macce), 
                     renal_complications_mean = 100 * mean(renal_complications),
                     delirium_neuro_mean = 100 * mean(delirium_neuro),
                     n = n()) %>% 
    tidyr::pivot_longer(cols = c("outcome_composite_mean", 
                                 "stroke_macce_mean",
                                 "myo_infarction_macce_mean",
                                 "renal_complications_mean",
                                 "delirium_neuro_mean"),
                        names_to = "outcome") %>% 
    dplyr::mutate(outcome = factor(outcome, 
                                   levels = c("myo_infarction_macce_mean",
                                              "stroke_macce_mean",
                                              "renal_complications_mean",
                                              "delirium_neuro_mean",
                                              "outcome_composite_mean"),
                                   labels = c("Myocardial infarction",
                                              "Stroke",
                                              "Renal complications",
                                              "Delirium",
                                              "Composite outcome"))) %>% 
    ggplot2::ggplot(aes(x = !!rlang::sym(x_var),
                        fill = outcome,
                        y = value,
                        width = ifelse(outcome == "Composite outcome", 0.55, 0.5))) +  # Adjust width for "Any of the above"
    ggplot2::geom_bar(stat = "identity",
                      position = "dodge") +
    ggplot2::geom_text(data = . %>% dplyr::filter(outcome == "Composite outcome"),  # Filter for "Any of the above"
                       aes(label = paste0("n = ", n), y = value + 0.325),  # Display `n` slightly above the bar
                       position = ggplot2::position_dodge(width = 0.55),  # Align text with wider bar
                       size = 2.5) +  # Adjust text size
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.y = element_text(size = 12),
                   legend.position = c(0.02, 0.98),  # Position legend in upper left corner
                   legend.justification = c(0, 1),  # Align top-left of legend box
                   legend.title = element_blank()) +  # Remove legend title
    ggplot2::xlab("Total minutes below BPM 60 mmHg") +
    ggplot2::ylab("Frequency of myocardial infarction, stroke, \nacute renal failure, or delirium (%)") +
    ggplot2::scale_fill_brewer(palette = "Accent") +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, by = 5),   # Major grid lines every 5%
      minor_breaks = seq(0, 100, by = 1)  # Minor grid lines every 1%
    )
  
  ggplot2::ggsave(plot = p, out_file)
  
  return(p)
  
}
