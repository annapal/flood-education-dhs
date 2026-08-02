
# List containing information for all models run

model_specs <- list(
  
  ## Weighting scheme
  
  # Full model weighted by sample size in each country
  main = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Full model weighted by population size in each country
  pop_weight = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight_pop,
    data = "flood_56_age_10"
  ),
  
  ## Model specification
  
  # Model without cluster fixed effects
  no_cluster = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model without any covariates
  no_covariates = list(
    formula = no_education ~ flooded_10km_age_5_or_6 | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with just wealth status
  only_wealth = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with just sex
  only_sex = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with all covariates & hh covariates
  hh_covariates = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric + 
      no_education_hh_head + hh_head_sex | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  ## Strata
  
  # Model with country-level coefficients
  country_level = list(
    formula = no_education ~ i(country_id, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with region-level coefficients
  region_level = list(
    formula = no_education ~ i(region23_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with region by flood cause coefficients
  region_cause_level = list(
    formula = no_education ~ i(region_cause_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with rural/urban coefficients
  rural_level = list(
    formula = no_education ~ i(hv025, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with sex coefficients
  sex_level = list(
    formula = no_education ~ i(hv104, flooded_10km_age_5_or_6) + hv270_numeric | clustid_sex + country_year_sex,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with frequency of flooding coefficients
  flood_level = list(
    formula = no_education ~ i(flood_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with severity of flooding coefficients
  severity_level = list(
    formula = no_education ~ i(dfo_severity_age_5_or_6, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with baseline level of schooling coefficients
  school_level = list(
    formula = no_education ~ i(school_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Model with wealth status coefficients
  wealth_level = list(
    formula = no_education ~ i(wealth_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with household head education coefficients (main effect varies within cluster)
  hh_head_level = list(
    formula = no_education ~ i(hh_head_educ_cat, flooded_10km_age_5_or_6) + hh_head_educ_cat +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with number dead coefficients (unexposed children are the reference)
  dead_level = list(
    formula = no_education ~ i(flood_dead_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with number displaced coefficients (unexposed children are the reference)
  affected_level = list(
    formula = no_education ~ i(flood_displaced_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  # Model with decade coefficients (main effect absorbed by country_year FE)
  decade_level = list(
    formula = no_education ~ i(decade_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),

  ## Different flood distances
  
  buffer = list(
    formula = no_education ~ i(flooded_dist, flooded_00km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  ## Exposure ages
  
  age_exp = list(
    formula = no_education ~ flooded_10km_age_5 + 
      flooded_10km_age_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  ## Flood characteristics
  
  # Main cause
  main_cause = list(
    formula = no_education ~ i(flood_storm_cause, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  # Flood length
  flood_length = list(
    formula = no_education ~ i(flood_duration_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10"
  ),
  
  ## Effect within different samples
  
  # Aged 12 or older
  main_12 = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_12"
  ),
  
  # Aged 15 or older
  main_15 = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  ## Effect of flood exposure at 12 and 13
  
  exp_12_13 = list(
    formula = no_education ~ flooded_10km_age_12_or_13 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_1213_age_15"
  ),
  
  ## Effect on completing primary
  
  main_primary = list(
    formula = no_primary ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with rural/urban coefficients
  rural_level_primary = list(
    formula = no_primary ~ i(hv025, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with sex coefficients
  sex_level_primary = list(
    formula = no_primary ~ i(hv104, flooded_10km_age_5_or_6) + hv270_numeric | clustid_sex + country_year_sex,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with severity of flooding coefficients
  severity_level_primary = list(
    formula = no_primary ~ i(dfo_severity_age_5_or_6, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with wealth status coefficients
  wealth_level_primary = list(
    formula = no_primary ~ i(wealth_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Main cause
  main_cause_primary = list(
    formula = no_primary ~ i(flood_storm_cause, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Flood length
  flood_length_primary = list(
    formula = no_primary ~ i(flood_duration_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Distance to flood
  buffer_primary = list(
    formula = no_primary ~ i(flooded_dist, flooded_00km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with baseline level of schooling coefficients
  school_level_primary = list(
    formula = no_primary ~ i(school_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with frequency of flooding coefficients
  flood_level_primary = list(
    formula = no_primary ~ i(flood_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with number dead coefficients (unexposed children are the reference)
  dead_level_primary = list(
    formula = no_primary ~ i(flood_dead_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with number displaced coefficients (unexposed children are the reference)
  affected_level_primary = list(
    formula = no_primary ~ i(flood_displaced_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with household head education coefficients (main effect varies within cluster)
  hh_head_level_primary = list(
    formula = no_primary ~ i(hh_head_educ_cat, flooded_10km_age_5_or_6) + hh_head_educ_cat +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with decade coefficients (main effect absorbed by country_year FE)
  decade_level_primary = list(
    formula = no_primary ~ i(decade_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  ## Effect on years of primary education completed
  
  main_attain = list(
    formula = years_primary ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with rural/urban coefficients
  rural_level_attain = list(
    formula = years_primary ~ i(hv025, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with sex coefficients
  sex_level_attain = list(
    formula = years_primary ~ i(hv104, flooded_10km_age_5_or_6) + hv270_numeric | clustid_sex + country_year_sex,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with severity of flooding coefficients
  severity_level_attain = list(
    formula = years_primary ~ i(dfo_severity_age_5_or_6, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with wealth status coefficients
  wealth_level_attain = list(
    formula = years_primary ~ i(wealth_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Main cause
  main_cause_attain = list(
    formula = years_primary ~ i(flood_storm_cause, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Flood length
  flood_length_attain = list(
    formula = years_primary ~ i(flood_duration_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Distance to flood
  buffer_attain = list(
    formula = years_primary ~ i(flooded_dist, flooded_00km_age_5_or_6) + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with baseline level of schooling coefficients
  school_level_attain = list(
    formula = years_primary ~ i(school_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric | 
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  # Model with frequency of flooding coefficients
  flood_level_attain = list(
    formula = years_primary ~ i(flood_baseline_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with number dead coefficients (unexposed children are the reference)
  dead_level_attain = list(
    formula = years_primary ~ i(flood_dead_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with number displaced coefficients (unexposed children are the reference)
  affected_level_attain = list(
    formula = years_primary ~ i(flood_displaced_cat, flooded_10km_age_5_or_6, ref = "not flooded") +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with household head education coefficients (main effect varies within cluster)
  hh_head_level_attain = list(
    formula = years_primary ~ i(hh_head_educ_cat, flooded_10km_age_5_or_6) + hh_head_educ_cat +
      hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),

  # Model with decade coefficients (main effect absorbed by country_year FE)
  decade_level_attain = list(
    formula = years_primary ~ i(decade_cat, flooded_10km_age_5_or_6) + hv104 + hv270_numeric |
      clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_15"
  ),
  
  ## Run without clusters that are completely flooded
  
  main_flood_rm = list(
    formula = no_education ~ flooded_10km_age_5_or_6 + hv104 + hv270_numeric | clustid + country_year,
    weights = ~weight,
    data = "flood_56_age_10_flood_rm"
  )
  
)
