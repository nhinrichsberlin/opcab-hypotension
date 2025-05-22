#' Extracts the SMD (unmatched and matched) per confounder from 
#' a summary.MatchIt 
#' @param matchit_summary The return of a summary(matchit) call
#' @param var_labels A named list mapping colnames to desired label
#' @retun A tibble with three columns: names, SMD before and after matching
.smd_from_matchit_summary <- function(matchit_summary, var_labels) {
  
  matched <- 
    matchit_summary %>% 
    purrr::pluck("sum.matched")
  
  unmatched <- 
    matchit_summary %>% 
    purrr::pluck("sum.all")
  
  names <- 
    matched %>% 
    dimnames() %>% 
    purrr::pluck(1) %>% 
    purrr::modify(~purrr::pluck(var_labels, ., .default = .))
  
  smd_matched <- 
    matched %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(v = names) %>% 
    dplyr::filter(v != "distance")%>% 
    dplyr::select("v", "Std. Mean Diff.") %>% 
    dplyr::rename("SMD (matched)" = "Std. Mean Diff.")
  
  smd_unmatched <- 
    unmatched %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(v = names) %>% 
    dplyr::filter(v != "distance")%>% 
    dplyr::select("v", "Std. Mean Diff.") %>% 
    dplyr::rename("SMD (unmatched)" = "Std. Mean Diff.")
  
  smd <-
    smd_unmatched %>% 
    dplyr::left_join(smd_matched, on = "v") %>% 
    dplyr::rename(" " = "v")
  
  return(smd)
  
}


#' Wrapper for the CreateTableOne and print.TableOne functions
#' @param data A data frame
#' @param numerical_vars A list of numerical vars to appear
#' @param categorical_vars A list of categorical vars to appear
#' @param test A boolean indicating whether or not to display p-vals
#' @param treatment A string indicating the name of the treatment var
#' @param matching_summary Optional, if you wish to add SMDs before and after matching
#' @param var_labels Optional, only used if matching_summary is True. 
#' @return  
t1 <- function(data,
               numerical_vars,
               categorical_vars,
               numerical_vars_mean_std = NULL,
               test = TRUE,
               treatment = "treatment",
               matching_summary = NULL,
               var_labels = NULL,
               name_untreated = "Control",
               name_treated = "Treatment") {
  
  vars <- c(numerical_vars, categorical_vars)
  
  table <- 
    vars %>% 
    tableone::CreateTableOne(data = data,
                             strata = treatment,
                             factorVars = categorical_vars,
                             testNonNormal = wilcox.test)
  
  nnormal <- numerical_vars
  if (!is.null(numerical_vars_mean_std)) {
    nnormal <- setdiff(numerical_vars, numerical_vars_mean_std)
  }
  
  # Transform into a chr matrix with desired columns
  table_chr <-
    table %>% 
    print(nonnormal = nnormal,
          smd = FALSE,
          test = test,
          varLabels = if (is.null(var_labels)) FALSE else TRUE,
          explain = FALSE,
          showAllLevels = FALSE,
          dropEqual = TRUE)
  
  rownames <- dimnames(table_chr)[[1]]
  
  # Transform into a tibble
  table_tibble <- 
    table_chr %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(v = rownames) %>% 
    dplyr::select(if (test) c("v", "1", "0", "p") else c("v", "1", "0")) %>% 
    dplyr::rename(" " = "v",
                  !!name_untreated := "0",
                  !!name_treated := "1")
  
  if (!is.null(matching_summary)) {
    
    # Extract the SMDs from the matching summary
    smd <- .smd_from_matchit_summary(matching_summary, var_labels)
    
    # Join the SMDs
    table_tibble <-
      table_tibble %>%
      dplyr::left_join(smd, on = " ")
  }
  
  return(table_tibble)
  
}
