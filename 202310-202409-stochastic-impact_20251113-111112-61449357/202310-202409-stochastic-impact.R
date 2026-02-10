orderly::orderly_description(
  display = "Stochastic impact estimate from 202310gavi and 202409malaria model runs",
  long = "Stochastic impact estimate from 202310gavi and 202409malaria model runs; 
          also include Hib/PCV/Rota/JE from 202110gavi model runs.",
  custom = list(requester = "VIMC", author = "Anna-Maria"))

library(dplyr)

orderly::orderly_shared_resource("run_groups.csv" = "metadata/touchstone_disease_model.csv")

dependency_inputs <- read.csv("run_groups.csv", stringsAsFactors = FALSE) %>%
  filter(comment == "paper4") %>% select(disease, modelling_group)

dat <- list()
for (i in seq_len(nrow(dependency_inputs))) {
  d <- dependency_inputs[i, ]
  orderly::orderly_dependency(name = "diagnostics-impact-report-stochastics", 
                               query = sprintf("latest(parameter:disease == '%s' && parameter:modelling_group == '%s'  && parameter:test_report == FALSE)", d$disease, d$modelling_group),
                               files = setNames("raw_impact_ratios.rds", sprintf("dat%s.rds", i))
  )
  
  dat[[i]] <- readRDS(sprintf("dat%s.rds", i))
  
}

orderly::orderly_artefact(
  files = c("stats.rds", "all_impact_ratios.rds"),
  description = "Impact ratios")


dat <- bind_rows(dat)

stats <- dat %>% 
  group_by(disease, method, scenario_type, vaccine, country, outcome) %>%
  summarise(upper = quantile(impact_ratio, probs = 0.975, na.rm = TRUE),
            mid = mean(impact_ratio, na.rm = TRUE),
            median = quantile(impact_ratio, probs = 0.5, na.rm = TRUE),
            low = quantile(impact_ratio, probs = 0.025, na.rm = TRUE), .groups = "drop")

saveRDS(stats, "stats.rds")

saveRDS(dat, "all_impact_ratios.rds")

