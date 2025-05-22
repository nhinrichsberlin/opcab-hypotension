library(dplyr)
library(Hmisc)
library(multidplyr)
library(readr)
library(sandwich)
library(tableone)
library(sjmisc)
library(MatchIt)
library(ggplot2)
library(ggExtra)
library(kableExtra)
library(lmtest)

source("R/prep_data.R")
source("R/eda_plots.R")
source("R/tables.R")
source("R/outcomes.R")
source("R/matching.R")

# set a random seed
set.seed(123)

# define the treatment variable
treatment <- "max_10_total_below_60"

# define numerical confounders
confounders_numerical <- c(
  "age",
  "bmi",
  "lvef",
  "haemoglobin_preop",
  "creatinine_clearance_preop",
  "ckmb_preop"
)

# define binary confounders
confounders_binary <- c(
  "gender",
  "depression",
  "diabetes_oral",
  "diabetes_insulin",
  "hypertension",
  "dyslipidemia",
  "family",
  "poor_mobility",
  "neurovascular_disease_history",
  "smoking",
  "preop_dialysis",
  "nyha_2",
  "nyha_3",
  "nyha_4",
  "ccs_1",
  "ccs_2",
  "ccs_3",
  "ccs_4",
  "cad_3",
  "atrial_fibrillation",
  "carotid_stenosis",
  "pad",
  "pulm_hypertension",
  "cardiomyo",
  "drugs_use",
  "high_aortic_calc_yn",
  "no_touch_aorta",
  "heartstring_aorta",
  "partiell_clamping_aorta",
  "analgesics_remi",
  "analgesics_sufenta"
)

# define numerical outcomes
outcomes_numerical <- c(
  "length_of_icu_stay_hours",
  "ventilation_hours",
  "hospitalization_time",
  "hospitalizationtime_total",
  "blood_products_total",
  "red_cells_intraop",
  "platelets_intraop",
  "plasma_intraop",
  "red_cells_postop",
  "platelets_postop",
  "plasma_postop",
  "haemoglobin_diff",
  "creatinine_clearance_diff",
  "ckmb_diff",
  "haemoglobin_postop_within_48h",
  "creatinine_clearance_postop_within_48h",
  "creatinine_clearance_postop_after_48h",
  "ckmb_postop_within_48h",
  "ckmb_postop_after_48h"
)

# define binary outcomes
outcomes_binary <- c(
  "outcome_composite",
  "stroke_macce",
  "myo_infarction_macce",
  "renal_complications",
  "delirium_neuro",
  "delirium_emergence",
  "incomplete_revasc",
  "d_mortality",
  "pci_revasc",
  "new_afib_postop",
  "lcos",
  "ppm",
  "resp_complications",
  "pneumonia",
  "tracheotomy",
  "re_intubation",
  "type_2_resp_failure",
  "gastrointestinal_complications",
  "wound_infection_superficial",
  "wound_infection_deep",
  "reop_tamponade_or_bleeding"
)

cols_t1_additional <- c("es", "myocardial_inf_preop")

# assign labels to all confunders and outcomes
labels <- list(
  # treatment
  "max_10_total_below_60"                  = "Max. 10 total minutes below BPM 60 mmHg",
  # outcomes
  "outcome_composite"                      = "Composite outcome",
  "stroke_macce"                           = "··· Stroke",
  "myo_infarction_macce"                   = "··· Myocardial infarction",
  "renal_complications"                    = "··· Acute renal failure",
  "delirium_neuro"                         = "··· Delirium",
  "delirium_emergence"                     = "Emergence agitation",
  "incomplete_revasc"                      = "Incomplete revascularization",
  "resp_complications"                     = "Respiratory complications",
  "pneumonia"                              = "··· Pneumonia",
  "tracheotomy"                            = "··· Tracheotomy",
  "re_intubation"                          = "··· Re-intubation",
  "type_2_resp_failure"                    = "··· Type II respiratory failure",
  "length_of_icu_stay_hours"               = "Length of ICU stay (h)",
  "ventilation_hours"                      = "Ventilation time (h)",
  "hospitalization_time"                   = "Length of hospital stay at DHZC (days)",
  "hospitalizationtime_total"              = "Length of hospital stay at any institution (days)",
  "blood_products_total"                   = "Blood products (total intra- and postoperative units)",
  "red_cells_intraop"                      = "··· Red cells (intraoperative units)",
  "platelets_intraop"                      = "··· Platelets (intraoperative units)",
  "plasma_intraop"                         = "··· Fresh frozen plasma (intraoperative units)",
  "red_cells_postop"                       = "··· Red cells (postoperative units)",
  "platelets_postop"                       = "··· Platelets (postoperative units)",  
  "plasma_postop"                          = "··· Fresh frozen plasma (postoperative units)",
  "haemoglobin_diff"                       = "Post/pre-operative difference in Hemoglobin (g/dL)",
  "creatinine_clearance_diff"              = "Post/pre-operative difference in Creatinine Clearance (mL/min)",
  "ckmb_diff"                              = "Post/pre-operative difference in CKMB (U/L)",
  "haemoglobin_postop_within_48h"          = "Hemoglobin <= 48h post operation (g/dL)",
  "creatinine_clearance_postop_within_48h" = "Creatinine clearance <= 48h post operation (mL/min)",
  "creatinine_clearance_postop_after_48h"  = "Creatinine clearance > 48h post operation (mL/min)",
  "ckmb_postop_within_48h"                 = "CKMB <= 48h post operation (U/L)",
  "ckmb_postop_after_48h"                  = "CKMB > 48h post operation (U/L)",
  "d_mortality"                            = "30-day mortality",
  "pci_revasc"                             = "Re-revascularization",
  "new_afib_postop"                        = "Newly developed atrial fibrillation",
  "lcos"                                   = "Low cardiac output syndrome",
  "ppm"                                    = "PPM implantation",
  "gastrointestinal_complications"         = "Gastrointestinal complications",
  "wound_infection_superficial"            = "Superficial wound infection",
  "wound_infection_deep"                   = "Deep wound infection",
  "reop_tamponade_or_bleeding"             = "Return to theatre due to tamponade or bleeding",
  # confounders
  "gender"                                 = "Sex (female)",
  "depression"                             = "Depression",
  "age"                                    = "Age (years)",
  "bmi"                                    = "Body mass index",
  "diabetes_oral"                          = "Diabetes mellitus (oral)",
  "diabetes_insulin"                       = "Diabetes mellitus (Insulin)",
  "hypertension"                           = "Hypertension",
  "dyslipidemia"                           = "Dyslipidemia",
  "family"                                 = "Family history",
  "poor_mobility"                          = "Cognitive impairment or frailty",
  "neurovascular_disease_history"          = "History of neurovascular disease",
  "smoking"                                = "Current or ex smoker",
  "preop_dialysis"                         = "Dialysis",
  "nyha_2"                                 = "NYHA class II",
  "nyha_3"                                 = "NYHA class III",
  "nyha_4"                                 = "NYHA class IV",
  "ccs_1"                                  = "CCS class I",
  "ccs_2"                                  = "CCS class II",
  "ccs_3"                                  = "CCS class III",
  "ccs_4"                                  = "CCS class IV",
  "cad_3"                                  = "CAD level III",
  "analgesics_remi"                        = "Remifentanil",
  "analgesics_sufenta"                     = "Sufentanil",
  "atrial_fibrillation"                    = "Atrial fibrillation",
  "carotid_stenosis"                       = "Carotid stenosis > 50 %",
  "pad"                                    = "Peripheral artery disease",
  "pulm_hypertension"                      = "Pulmonary hypertension",
  "cardiomyo"                              = "Cardiomyopathy",
  "lvef"                                   = "Left valve ejection fraction (%)",
  "haemoglobin_preop"                      = "Hemoglobin",
  "creatinine_clearance_preop"             = "Creatinine clearance",
  "ckmb_preop"                             = "CKMB",
  "drugs_use"                              = "Drug abuse",
  "high_aortic_calc_yn"                    = "High aortic calcification",
  "no_touch_aorta"                         = "Aortic no touch OPCAB",
  "heartstring_aorta"                      = "Heart string proximal anastomosis",
  "partiell_clamping_aorta"                = "Partial aortic clamping",
  # some additional columns used for table 1
  "es"                                     = "EuroScore II",
  "myocardial_inf_preop"                   = "Recent MI (48 h)"
)

# check that all variables have a label
vars <- c(treatment,
          confounders_numerical, 
          confounders_binary, 
          outcomes_binary,
          outcomes_numerical,
          cols_t1_additional)

diff <- setdiff(vars, names(labels)) 
if (length(diff) > 0) {
  stop("Variables without label: ", diff)
}

diff <- setdiff(names(labels), vars)
if (length(diff) > 0) {
  stop("Unused labels: ", diff)
}
