## transform
transform_data <- function(extracted_data){
  recipe <- generic_recipe(extracted_data)
  d <- cal_ratios(recipe) %>% mutate(disease = extracted_data$disease,
                                     modelling_group = extracted_data$modelling_group)
  d$outcome <- gsub("_cwyx", "", d$outcome)
  ## get disease level ratio for standard approach
  d1 <- d %>%
    filter(method == "standard") %>%
    group_by(run_id, country, scenario_type, outcome) %>%
    summarise(imp = sum(impact_ratio*fvps,na.rm = TRUE),
              fvps = sum(fvps, na.rm=TRUE), .groups = "drop") %>%
    mutate(impact_ratio = imp/fvps) %>%
    select(-imp) %>%
    mutate(disease = d$disease[1],
           modelling_group = d$modelling_group[1],
           method = "standard",
           vaccine = paste0(d$disease[1],"_all"))
  d <- bind_rows(d, d1) %>%
    arrange(run_id, country, scenario_type, vaccine, method, outcome)
  stats <- summarize_stats(d) %>% mutate(disease = extracted_data$disease,
                                         modelling_group = extracted_data$modelling_group)
  
  saveRDS(d, "raw_impact_ratios.rds")
  saveRDS(stats, "summary_statistics.rds")
  return(list(recipe = recipe$recipe,
              impact = d,
              summary = stats,
              annex_numbers = extracted_data$annex_numbers))
}

## test transform
test_transform <- function(transformed_data){
  recipe <- transformed_data$recipe
  impact <- transformed_data$impact %>% filter(!((grepl("all", vaccine) & method == "standard")))
  stats <- transformed_data$summary %>% filter(!((grepl("all", vaccine) & method == "standard")))
  n_outcome <- 4L #dontworry about menacwxy, it is captured by vaccine and recipe
  n_country <- length(unique(impact$country))
  
  ## test recipe match impact
  testthat::expect_equal(nrow(recipe)*n_outcome*n_country*200L, nrow(impact))
  testthat::expect_equal(nrow(recipe)*n_outcome*n_country, nrow(stats))
  
}


