#' Reads the RedCap data and runs necessary preprocessing
#' @param  path_to_data Filepath to the RedCap export file
#' @returns A tibble containing the relevant data for further analysis
load_data <- function(path_to_data = "data/OPCABHypotonieProjek-OPCABHypotonieProjek_DATA_2025-02-05_1105.csv") {
  
  # read the data
  data <- readr::read_delim(path_to_data, show_col_types = FALSE)
  
  # add the euroscore
  es <- readr::read_delim('data/euroscores.csv', delim = ";")
  
  # add info on preoperative myocardial infarction
  mi <- readr::read_delim('data/patients_with_mi.csv', delim=';')
  
  data <- 
    data %>% 
    dplyr::left_join(es, by = "record_id") %>% 
    dplyr::mutate(myocardial_inf_preop = record_id %in% mi$record_id)
  
  # store original columns
  cols_original <- names(data)
  
  data <-
    data %>% 
    # create new columns
    dplyr::mutate(# confounders
                  diabetes_oral = (dm == 2),
                  diabetes_insulin = (dm == 3),
                  neurovascular_disease_history = ((tia == 1) | (stroke == 1)),
                  smoking = (smoke %in% c(1, 2)),
                  chronic_lung_disease = (chronic_lung_disease_sts_d %in% c(1, 2, 3)),
                  nyha_1 = (pre_op_nyha == 1),
                  nyha_2 = (pre_op_nyha == 2),
                  nyha_3 = (pre_op_nyha == 3),
                  nyha_4 = (pre_op_nyha == 4),
                  ccs_1 = (preop_ccs == 1),
                  ccs_2 = (preop_ccs == 2),
                  ccs_3 = (preop_ccs == 3),
                  ccs_4 = (preop_ccs == 4),
                  atrial_fibrillation = (preop_rhythm_disorder == 2),
                  carotid_stenosis = (carotid_stenosis_50 == 1),
                  pad = (peripheral_artery_disease == 1),
                  pulm_hypertension = (pulmonary_hypertension %in% c(1, 2)),
                  cardiomyo = (cardiomyopathy %in% c(1, 2, 3)),
                  cad_1 = (cad == 1),
                  cad_2 = (cad == 2),
                  cad_3 = (cad == 3),
                  lvef = (lvef_pre),
                  dementia = (neuro_disease___4 == 1),
                  depression = (neuro_disease___8 == 1),
                  # outcomes
                  myo_infarction_macce = (macce_myo_infarction == 1),
                  stroke_macce = (macce_stroke == 1 | tia_postop == 1 | early_stroke == 1),
                  pci_revasc = (re_revascularisation_pci == 1),
                  cabg_revasc = (re_revascularisation_cabg == 1),
                  new_afib_postop = (postop_new_atrial_fibrillation == 1),
                  lcos = (low_cardiac_output_syndrom == 1),
                  ppm = (peri_op_pm_implantation == 1),
                  resp_complications = ((respiratory_post_op_compl___1 + 
                                         respiratory_post_op_compl___5 +
                                         respiratory_post_op_compl___6 +
                                         respiratory_post_op_compl___7) > 0),
                  pneumonia = (respiratory_post_op_compl___1 == 1),
                  tracheotomy = (respiratory_post_op_compl___5 == 1),
                  re_intubation = (respiratory_post_op_compl___6 == 1),
                  type_2_resp_failure = (respiratory_post_op_compl___7 == 1),
                  gastrointestinal_complications = ((gastrointestinal_complicat___2 +
                                                     gastrointestinal_complicat___3 +
                                                     gastrointestinal_complicat___4 +
                                                     gastrointestinal_complicat___5) > 0),
                  renal_complications = (renal_complications___1 == 1),
                  wound_infection_superficial = (sup_wound_infection___5 == 0),
                  wound_infection_deep = (deep_wound_infection___5 == 0),
                  reop_tamponade_or_bleeding = ((reason_for_return_to_theat___6 == 1) | (return_for_bleeding == 1)),
                  stroke_early = (early_stroke == 1),
                  # blood products
                  red_cells_intraop = dplyr::coalesce(spec_rcc, 0),
                  platelets_intraop = dplyr::coalesce(spec_plt, 0),
                  plasma_intraop = dplyr::coalesce(spec_ffp, 0),
                  red_cells_postop = dplyr::coalesce(red_cells_post, 0),
                  platelets_postop = dplyr::coalesce(platelets_units_post, 0),
                  plasma_postop = dplyr::coalesce(ffp_units_post, 0),
                  blood_products_total = (dplyr::coalesce(spec_rcc, 0) +
                                          dplyr::coalesce(spec_plt, 0) +
                                          dplyr::coalesce(spec_ffp, 0) +
                                          dplyr::coalesce(red_cells_post, 0) +
                                          dplyr::coalesce(platelets_units_post, 0) +
                                          dplyr::coalesce(ffp_units_post, 0)),
                  delirium_emergence = (emergence_delirium == 1),
                  rass = dplyr::coalesce(as.integer(richmond_agitation_sedation_scale), 0)) %>% 
    # define the treatment
    dplyr::mutate(max_10_total_below_60 = (total_time_below_60 <= 10)) %>% 
    # add additional outcomes
    dplyr::mutate(haemoglobin_diff = haemoglobin_postop_within_48h - haemoglobin_preop,
                  creatinine_clearance_diff = creatinine_clearance_postop_within_48h - creatinine_clearance_preop,
                  ckmb_diff = ckmb_postop_within_48h - ckmb_preop,
                  rass_abs = abs(rass)) %>% 
    # change units of outcomes
    dplyr::mutate(ventilation_hours = ventilation_minutes / 60,
                  length_of_icu_stay_hours = length_of_icu_stay_minutes / 60)
  
  # coalesce new columns to zero
  data <- 
    data %>% 
    dplyr::mutate_at(setdiff(names(data), cols_original), ~dplyr::coalesce(as.integer(.), 0))
  
  # create a composite outcome
  data <-
    data %>% 
    dplyr::mutate(outcome_composite = as.integer(stroke_macce + 
                                                 myo_infarction_macce + 
                                                 renal_complications +
                                                 delirium_neuro > 0))
  
  data <- 
    data %>% 
    dplyr::select(dplyr::all_of(c("record_id",
                                  "bpm_lowest",
                                  "total_time_below_60",
                                  "total_time_below_50",
                                  "max_consec_time_below_60",
                                  "max_consec_time_below_50",
                                  treatment,
                                  confounders_numerical, 
                                  confounders_binary,
                                  outcomes_numerical,
                                  outcomes_binary,
                                  cols_t1_additional)))
  
  # save as csv
  data %>% readr::write_delim("data/opcab_analysis.csv", delim = ";")
  
  return(data)
  
}
