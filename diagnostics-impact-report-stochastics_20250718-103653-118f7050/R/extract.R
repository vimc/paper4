## extract
extract_data <- function(pars){
  ## source for coverage
  con <- orderly.db::orderly_db_connection("source")
  
  ## annex for burden
  annex <- orderly.db::orderly_db_connection("annex")
  
  ## only 202310 and 202409 touchstones
  touchstone <- ifelse(pars$disease == "Malaria", "202409malaria", "202310gavi")
  
  ## YoV window
  y_max <- ifelse(pars$disease %in% c("Cholera"), 2040, 2030)
  
  ## country dictionary for matching names
  country_dict <- DBI::dbReadTable(con, "country")
  
  ## test report with minimal countries
  if (pars$test_report){
    country_list <- country_dict %>% filter(id %in% c("PAK", "IND", "NGA", "ETH", "COD", "CHN"))
  } else {
    country_list <- country_dict
  }
  
  ## locate burden
  meta <- pull_meta(pars, annex, touchstone)
  tab_cohort_view <- paste0("stochastic_", meta$id[meta$is_cohort])
  tab_calendar_view <- paste0("stochastic_", meta$id[!meta$is_cohort])
  
  b_cohort_view_2030 <- pull_burden(annex, tab_cohort_view, "cohort", country_list, cohort_max = 2030) # routine age = 0 i.e. most routine vaccines & also used for the alternative disease level cohort approach
  b_cohort_view_2029 <- pull_burden(annex, tab_cohort_view, "cohort", country_list, cohort_max = 2029) # routine age = 1 i.e. Malaria routine
  b_cohort_view_2028 <- pull_burden(annex, tab_cohort_view, "cohort", country_list, cohort_max = 2028) # routine age = 2, i.e. MCV2 and RCV2
  b_cohort_view_2021 <- pull_burden(annex, tab_cohort_view, "cohort", country_list, cohort_max = 2021) # routine age = 9, i.e.g HPV
  b_calendar_view <- pull_burden(annex, tab_calendar_view, "calendar", country_list) # for campaigns
  
  ## pull fvps
  all_types <- c("default", "ia2030", "bluesky","default_severe", "bluesky_severe","default_transition_hpv_1d", "sear_s1", "sear_s2")
  fvps <- vimpact::extract_vaccination_history(con, 
                                               touchstone_cov = touchstone, 
                                               disease_to_extract = pars$disease, year_min = 2000, year_max = y_max, 
                                               countries_to_extract = country_list$id, scenario_type = all_types,
                                               demographic_source = "dds-202208")   %>%
    mutate(vaccine = paste(vaccine, activity_type, sep = "_"))
  
  ### some disease specific operations on fvps
  if(pars$disease == "Measles"){
    fvps <- fvps %>%
      filter(scenario_type != "default") %>%
      filter(scenario_type != "sear_s2")
    t1 <- fvps %>% filter(activity_type == "campaign" & age > 4 & scenario_type == "sear_s1") %>% 
      mutate(vaccine = "Measles_>=5s_campaign", scenario_type = "default_update")
    t2 <- fvps %>% filter(activity_type == "campaign" & age <= 4 & scenario_type == "sear_s1") %>% 
      mutate(vaccine = "Measles_under5s_campaign", scenario_type = "default_under5sia")
    t3 <-  fvps %>% filter(scenario_type == "sear_s1") %>% 
      mutate(scenario_type = case_when(vaccine %in% c("MCV1_routine", "MCV2_routine") ~ "default",
                                       TRUE ~ "default_update"))
    fvps <- bind_rows(fvps %>% filter(!(scenario_type == "sear_s1")), t1, t2, t3)
  }
  if(pars$disease == "HPV"){
    fvps$vaccine <- gsub("1D_", "", fvps$vaccine) ## this is HPV transition to 1D scenario, call it HPV
  }
  if(pars$disease == "MenA"){
    fvps1 <- fvps
    fvps1$vaccine <- gsub("CWYX", "", fvps1$vaccine)
    fvps2 <- fvps %>% filter(grepl("CWYX", vaccine))
    fvps2$vaccine <- gsub("MenA", "Men", fvps2$vaccine)
    fvps <- bind_rows(fvps1, fvps2)
  }
  if(pars$disease == "Malaria"){
    t1 <- fvps
    t1$vaccine <- gsub("routine", "all", t1$vaccine)
    fvps <- bind_rows(fvps, t1) %>% filter(!grepl("3", vaccine))
  }
  if(pars$disease == "Cholera"){
    t1 <- fvps %>% filter(grepl("OCV2", vaccine)) %>% mutate(vaccine = "Cholera_campaign") 
    fvps <- bind_rows(fvps, t1)
  }
  ## sia fvps
  calendar_fvps <- fvps %>%
    group_by(scenario_type, country, vaccine)  %>%
    summarise(fvps = sum(fvps_adjusted), .groups = "drop")
  
  ### fvps on cohorts 2000-2030
  cohort_fvps <- fvps %>%
    filter(year - age >= 2000 & year - age <= 2030) %>%
    group_by(scenario_type, country, vaccine)  %>%
    summarise(fvps = sum(fvps_adjusted), .groups = "drop")
  
  if(pars$disease == "MenA"){
    t <- bind_rows(cohort_fvps %>%
      filter(grepl("CWYX", vaccine)) %>%
      group_by(scenario_type, country) %>%
      summarise(fvps = sum(fvps), .groups = "drop") %>%
      mutate(vaccine = "MenCWYX_all"),
      cohort_fvps %>%
        filter(!grepl("CWYX", vaccine)) %>%
        group_by(scenario_type, country) %>%
        summarise(fvps = sum(fvps), .groups = "drop") %>%
        mutate(vaccine = "MenA_all"))
  } else {
    t <- cohort_fvps %>%
      group_by(scenario_type, country) %>%
      summarise(fvps = sum(fvps), .groups = "drop") %>%
      mutate(vaccine = paste0(pars$disease, "_all"))    
  }

  cohort_fvps <- bind_rows(cohort_fvps, t) %>%
    filter(vaccine != "Malaria_all")
  
  ## annex numbers - pull estimates with run_id = 1, and compare with what this report produces
  if(pars$disease != "Malaria"){
    annex_numbers <- DBI::dbGetQuery(annex, "SELECT * FROM intervention_all_2023 
                                     WHERE run_id = '1' 
                                     AND disease = $1 
                                     AND modelling_group = $2", 
                                     list(pars$disease, pars$modelling_group))
  } else {
    annex_numbers <- NULL
  }
  
  return(list(disease = pars$disease, #annex meta info
              modelling_group = pars$modelling_group,
              recipe = read.csv("meta/recipe.csv") %>% filter(disease == pars$disease), ## impact recipe
              cohort_fvps = cohort_fvps,
              calendar_fvps = calendar_fvps, 
              cohort_burden_2030 = b_cohort_view_2030,
              cohort_burden_2029 = b_cohort_view_2029,
              cohort_burden_2028 = b_cohort_view_2028,
              cohort_burden_2021 = b_cohort_view_2021,
              calendar_burden = b_calendar_view,
              annex_numbers = annex_numbers))
}

## test extract
test_extract <- function(extracted_data){
## generic tests
  disease <- extracted_data$disease
  cohort_fvps <- extracted_data$cohort_fvps
  calendar_fvps <- extracted_data$calendar_fvps
  cohort_burden_2030 <- extracted_data$cohort_burden_2030
  cohort_burden_2029 <- extracted_data$cohort_burden_2029
  cohort_burden_2021 <- extracted_data$cohort_burden_2021
  calendar_burden <- extracted_data$calendar_burden
  test_extract_burden(cohort_burden_2030)
  test_extract_burden(cohort_burden_2029)
  test_extract_burden(cohort_burden_2021)
  test_extract_burden(calendar_burden)
}

test_extract_burden <- function(d){
  ## uniqueness
  testthat::expect_false(any(duplicated(d)))
  ## 200 runs per country
  n_country <- length(unique(d$country))
  testthat::expect_equal(nrow(d), n_country*200L)  
}

