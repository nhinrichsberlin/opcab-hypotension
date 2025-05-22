import pandas as pd
import numpy as np

DATA = pd.read_csv('data/opcab_analysis.csv', sep=';')
TREATMENT = 'total_time_below_60'

# add column of transformed treatment for nicer statistical properties
TREATMENT_TRANSFORMED = f'{TREATMENT}_transformed'
DATA[TREATMENT_TRANSFORMED] = np.log(DATA[TREATMENT] + 1)

OUTCOME = 'outcome_composite'

CONFOUNDERS = [
    'age',
    'bmi',
    'lvef',
    'haemoglobin_preop',
    'creatinine_clearance_preop',
    'ckmb_preop',
    'gender',
    'diabetes_oral',
    'diabetes_insulin',
    'hypertension',
    'dyslipidemia',
    'family',
    'poor_mobility',
    'neurovascular_disease_history',
    'smoking',
    'preop_dialysis',
    'depression',
    'nyha_2',
    'nyha_3',
    'nyha_4',
    'ccs_1',
    'ccs_2',
    'ccs_3',
    'ccs_4',
    'cad_3',
    'atrial_fibrillation',
    'carotid_stenosis',
    'pad',
    'pulm_hypertension',
    'cardiomyo',
    'drugs_use',
    'high_aortic_calc_yn',
    'no_touch_aorta',
    'heartstring_aorta',
    'partiell_clamping_aorta',
    'analgesics_remi',
    'analgesics_sufenta'
]

CONFOUNDERS_NUMERICAL = [c for c in CONFOUNDERS if len(DATA[c].unique()) > 2]

# define points where the dose response function is to be evaluated
TREATMENT_INDEX = np.linspace(0, DATA[TREATMENT].max(), DATA[TREATMENT].max() + 1)
TREATMENT_INDEX_TRANSFORMED = np.log(TREATMENT_INDEX + 1)

CONFOUNDER_LABELS = {
  'gender': 'Sex (female)',
  'age': 'Age (years)',
  'bmi': 'Body mass index',
  'diabetes_oral': 'Diabetes mellitus (oral)',
  'diabetes_insulin': 'Diabetes mellitus (Insulin)',
  'hypertension': 'Hypertension',
  'dyslipidemia': 'Dyslipidaemia',
  'family': 'Family history',
  'poor_mobility': 'Cognitive impairment or frailty',
  'neurovascular_disease_history': 'History of neurovascular disease',
  'smoking': 'Current or ex-smoker',
  'preop_dialysis': 'Dialysis',
  'nyha_2': 'NYHA class II',
  'nyha_3': 'NYHA class III',
  'nyha_4': 'NYHA class IV',
  'depression': 'Depression',
  'ccs_1': 'CCS class I',
  'ccs_2': 'CCS class II',
  'ccs_3': 'CCS class III',
  'ccs_4': 'CCS class IV',
  'cad_3': 'CAD level 3',
  'blood_products_needed': 'Blood products (intra- or post-op)',
  'analgesics_remi': 'Remifentanil',
  'analgesics_sufenta': 'Sufentanil',
  'atrial_fibrillation': 'Atrial fibrillation',
  'carotid_stenosis': 'Carotid stenosis > 50 %',
  'pad': 'Peripheral artery disease',
  'pulm_hypertension': 'Pulmonary hypertension',
  'cardiomyo': 'Cardiomyopathy',
  'lvef': 'Left valve ejection fraction (%)',
  'haemoglobin_preop': 'Haemoglobin',
  'creatinine_clearance_preop': 'Creatinine clearance',
  'ckmb_preop': 'CKMB',
  'drugs_use': 'Drug abuse',
  'high_aortic_calc_yn': 'High aortic calcification',
  'no_touch_aorta': 'Aortic no touch OPCAB',
  'heartstring_aorta': 'Heart string proximal anastomosis',
  'partiell_clamping_aorta': 'Partial aortic clamping'
}

assert all(confounder in CONFOUNDER_LABELS.keys() for confounder in CONFOUNDERS), 'Found confounder without label!'
