####################### functions for extract
sum_exprs <- function(sum_cols){
  sum_cols2 <- gsub("-", "_", sum_cols)
  paste0("SUM(\"", sum_cols, "\") AS ", sum_cols2, collapse = ",\n  ")
}

## pull meta
pull_meta <- function(pars, annex, touchstone){
  ## this report is currently designed for paper 4;
  ## feel free to move touchstone as a parameter in the future.
  
  meta <- DBI::dbGetQuery(annex, "SELECT * FROM stochastic_file") %>% 
    filter(modelling_group == pars$modelling_group & 
             disease == pars$disease & 
             grepl(!!touchstone, touchstone) &
             !is_under5)
  
  return(meta)  
}

## pull burden
pull_burden <- function(annex, tab, view, country_list, cohort_max = NULL){
  cols <- DBI::dbListFields(annex, tab)
  index <- ifelse(view == "cohort", "cohort", "year")
  country_constrain_cohort <- ifelse(nrow(country_list) > 10L, "", sprintf("AND country IN %s", vimpact:::sql_in(country_list$nid)))
  country_constrain_calendar <- ifelse(nrow(country_list) > 10L, "", sprintf("WHERE country IN %s", vimpact:::sql_in(country_list$nid)))
  
  sum_exprs1 <- sum_exprs(setdiff(cols, c("country", "run_id", index)))
  sql_cohort_view <- sprintf("SELECT run_id, country, %s 
                             FROM %s WHERE cohort BETWEEN %s and %s
                             %s
                             GROUP BY run_id, country",
                             sum_exprs1, 
                             tab,
                             2000-(2030-cohort_max),
                             cohort_max,
                             country_constrain_cohort) ## age 0 routine vaccines
  
  sql_calendar_view <- sprintf("SELECT run_id, country, %s 
                               FROM %s 
                               %s
                               GROUP BY run_id, country",
                               sum_exprs1, 
                               tab,
                               country_constrain_calendar) 
  
  if (view == "cohort"){
    d <- DBI::dbGetQuery(annex, sql_cohort_view) %>% mutate(country = country_list$id[match(country, country_list$nid)])    
  } else {
    d <- DBI::dbGetQuery(annex, sql_calendar_view) %>% mutate(country = country_list$id[match(country, country_list$nid)])
  }
  
  return(d)
}

####################### functions for transform
## impact recipe
generic_recipe <- function(extracted_data){
  ## recipe
  recipe1 <- extracted_data$recipe
  recipe1$focal <- gsub("-", "_", recipe1$focal)
  recipe1$baseline <- gsub("-", "_", recipe1$baseline)
  
  if (extracted_data$disease == "COVID"){
    m <- data.frame(data.frame(index = c("covid_no_vaccination", "covid_no_vaccination_severe"), scenario_type = c("novac", "novac_severe")))
  } else {
    m <- data.frame(data.frame(index = tolower(paste0(extracted_data$disease, "_no_vaccination")), scenario_type = "novac"))
  }
  if (extracted_data$disease == "MenA"){
    o <- data.frame(outcome   = c("deaths", "cases", "dalys", "yll", "deaths_cwyx", "cases_cwyx", "dalys_cwyx", "yll_cwyx"))
  } else {
    o <- data.frame(outcome   = c("deaths", "cases", "dalys", "yll"))
  }
  
  
  ### standardize approach
  meta1 <- recipe1 %>% select(focal, scenario_type) %>% rename(index = focal) %>%
    bind_rows(m) %>% distinct() %>%
    cross_join(o)
  cohort_burden_2030 <- reshape_burden(extracted_data$cohort_burden_2030, meta1)
  cohort_burden_2029 <- reshape_burden(extracted_data$cohort_burden_2029, meta1)
  cohort_burden_2028 <- reshape_burden(extracted_data$cohort_burden_2028, meta1)
  cohort_burden_2021 <- reshape_burden(extracted_data$cohort_burden_2021, meta1)
  calendar_burden <- reshape_burden(extracted_data$calendar_burden, meta1)
  if(extracted_data$modelling_group[1L] == "JHU-Lee"){
    cohort_burden_2030 <- adjust_burden_jhu_cholera(cohort_burden_2030)
    cohort_burden_2029 <- adjust_burden_jhu_cholera(cohort_burden_2029)
    cohort_burden_2028 <- adjust_burden_jhu_cholera(cohort_burden_2028)
    cohort_burden_2021 <- adjust_burden_jhu_cholera(cohort_burden_2021)
    calendar_burden <- adjust_burden_jhu_cholera(calendar_burden)
  }
  return(list(recipe = recipe1,
              cohort_burden_2030 = cohort_burden_2030,
              cohort_burden_2029 = cohort_burden_2029,
              cohort_burden_2028 = cohort_burden_2028,
              cohort_burden_2021 = cohort_burden_2021,
              calendar_burden = calendar_burden,
              cohort_fvps = extracted_data$cohort_fvps,
              calendar_fvps = extracted_data$calendar_fvps)
  )
}

## attribute burden cols - sort of wide to long
reshape_burden <- function(d, meta1){
  dat <- list()
  for(i in seq_along(meta1$index)){
    j <- paste(meta1$outcome[i], meta1$index[i], sep = "_") == names(d)
    
    if (sum(j) == 1L){
      dat[[i]] <- d[, c("run_id", "country", names(d)[j])]
      names(dat[[i]])[3L] <- "value"
      dat[[i]] <- dat[[i]] %>% cross_join(meta1[i, ])
    } else {
      stop()
    } 
  }
  dat <- bind_rows(dat)
  return(dat)
}

adjust_burden_jhu_cholera <- function(dat){
  case_multiplier <- 1/.328
  death_multiplier <- 3.87
  yll_multiplier <- death_multiplier
  d1 <- dat %>%
    pivot_wider(names_from = "outcome", values_from = "value")  %>%
    mutate(dalys = (dalys-yll)*case_multiplier + yll*yll_multiplier) %>%
    mutate(deaths = deaths*death_multiplier,
           yll = yll*yll_multiplier,
           cases = cases* case_multiplier) %>%
    pivot_longer(cols = c("deaths", "cases", "dalys", "yll"), values_to = "value", names_to = "outcome") 
  
  ### test
  testthat::expect_equal(sum(d1$value[d1$outcome == "deaths"])/sum(dat$value[dat$outcome == "deaths"]), death_multiplier)
  testthat::expect_equal(sum(d1$value[d1$outcome == "cases"])/sum(dat$value[dat$outcome == "cases"]), case_multiplier)
  testthat::expect_equal(sum(d1$value[d1$outcome == "yll"])/sum(dat$value[dat$outcome == "yll"]), yll_multiplier)
  testthat::expect_false(sum(d1$value[d1$outcome == "dalys"])/sum(dat$value[dat$outcome == "dalys"]) > death_multiplier)
  
  return(d1)
}

cal_ratios <- function(recipe){
  cohort_fvps <- recipe$cohort_fvps
  calendar_fvps <- recipe$calendar_fvps
  calendar_burden <- recipe$calendar_burden
  cohort_burden_2030 <- recipe$cohort_burden_2030
  cohort_burden_2029 <- recipe$cohort_burden_2029
  cohort_burden_2028 <- recipe$cohort_burden_2028
  cohort_burden_2021 <- recipe$cohort_burden_2021
  
  recipe <- recipe$recipe %>% mutate(vaccine = paste(vaccine, activity_type, sep = "_"))
  
  dat <- list()
  for(i in seq_along(recipe$focal)){
    
    if(recipe$activity_type[i] == "all"){
      burden <- cohort_burden_2030
      fvps <- cohort_fvps
    } else  if(recipe$activity_type[i] == "routine" & grepl("HPV", recipe$vaccine[i])){
      burden <- cohort_burden_2021
      fvps <- calendar_fvps
    } else  if(recipe$activity_type[i] == "routine" & grepl("2", recipe$vaccine[i])){ # mcv2 and rcv2
      burden <- cohort_burden_2028
      fvps <- calendar_fvps
    } else  if(recipe$activity_type[i] == "routine" & grepl("4", recipe$vaccine[i])){ # r4 and rts4
      burden <- cohort_burden_2029
      fvps <- calendar_fvps
    } else  if(recipe$activity_type[i] == "routine" & grepl("COVID", recipe$vaccine[i])){ # r4 and rts4
      burden <- cohort_burden_2029
      fvps <- calendar_fvps
    }else if (recipe$activity_type[i] == "campaign"){
      burden <- calendar_burden
      fvps <- calendar_fvps
    } else {
      burden <- cohort_burden_2030
      fvps <- cohort_fvps
    }
    if(grepl("CWYX", recipe$vaccine[i])){
      burden <- burden %>% filter(grepl("cwyx", outcome))
    } else {
      burden <- burden %>% filter(!grepl("cwyx", outcome))
    }
    b_baseline <- burden %>%
      filter(index == recipe$baseline[i] ) %>%
      rename(baseline = value) %>% select(-index, -scenario_type)
    b_focal <- burden %>%
      filter(index == recipe$focal[i]) %>%
      rename(focal = value) %>% select(-index, -scenario_type)
    stopifnot(nrow(b_baseline) == nrow(b_focal))
    stopifnot(nrow(b_baseline) > 0)
    
    dat[[i]] <- vimpact:::merge_by_common_cols(b_baseline, b_focal) %>% 
      mutate(vaccine = recipe$vaccine[i], scenario_type = recipe$scenario_type[i]) %>%
      vimpact:::merge_by_common_cols(fvps, all.x = TRUE) %>%
      mutate(impact = baseline - focal) %>%
      mutate(impact_ratio = impact / fvps) %>%
      select(run_id, country, scenario_type, vaccine, outcome, impact_ratio, fvps) %>% mutate(method = recipe$method[i])
  }
  dat <- bind_rows(dat)
  return(dat)
}

summarize_stats <- function(d){
  stats <- d %>% 
    group_by(method, scenario_type, vaccine, country, outcome) %>%
    summarise(upper = quantile(impact_ratio, probs = 0.975, na.rm = TRUE),
              mid = mean(impact_ratio, na.rm = TRUE),
              median = quantile(impact_ratio, probs = 0.5, na.rm = TRUE),
              low = quantile(impact_ratio, probs = 0.025, na.rm = TRUE), .groups = "drop")
  return(stats)
}
