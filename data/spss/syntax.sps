* Encoding: UTF-8.
GET 
  FILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/clustr.sav'. 
DATASET NAME Clusters WINDOW=FRONT.
  
  
DISCRIMINANT 
  /GROUPS=cluster_z15(1 15) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  SHEET='cluster_z15'.
  
OUTPUT CLOSE ALL.

DISCRIMINANT 
  /GROUPS=cluster_old13(1 13) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='o13'.

OUTPUT CLOSE ALL.


DISCRIMINANT 
  /GROUPS=cluster_z5(1 5) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='z5'.

OUTPUT CLOSE ALL.


DISCRIMINANT 
  /GROUPS=cluster_m6(1 6) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='m6'.

OUTPUT CLOSE ALL.


DISCRIMINANT 
  /GROUPS=cluster_m13(1 13) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='m13'.

OUTPUT CLOSE ALL.


DISCRIMINANT 
  /GROUPS=cluster_m16(1 16) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='m16'.

OUTPUT CLOSE ALL.


DISCRIMINANT 
  /GROUPS=cluster_z12(1 12) 
  /VARIABLES=age land_use_mix r1 st_cap gmp_mfg_lq chrasts flights transp hist_prop med_value_c 
    home_oldr1940_c vacan_c prop_crp1k inf_mr pct_smoke pct_obese pop_mmntm decds_snc_peak 
    decl_snc_peak poverty gini bad_bridge sfund h2o_viol below_dipl bach_plus civ_lfpr pct_undr18 
    pct_65ovr low_inc_hsing hsng_stock_age density immigrant 
  /ANALYSIS ALL 
  /SAVE=CLASS SCORES PROBS 
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

OUTPUT EXPORT 
  /XLSX DOCUMENTFILE='/Users/andrew/Public/Google Drive/School/Research/1-Ned/0-Legacy Cities/manuscript/legacyR/data/spss/output'
  OPERATION=CREATESHEET SHEET='z12'.

OUTPUT CLOSE ALL.
