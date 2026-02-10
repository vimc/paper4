orderly::orderly_description(
  custom = list(requester = "VIMC", author = "Katy"))

orderly::orderly_artefact(files = "stochastic_impact_ratios.rds", 
                           description = "Compiled stochastic impact ratios")

orderly::orderly_dependency("202310-202409-stochastic-impact", "latest", "all_impact_ratios.rds")

library(dplyr)
library(tidyr)

options(dplyr.summarise.inform = FALSE)

# 202310 & 202409 & 202110------------------------------------------------------
df_202310 <-  readRDS("all_impact_ratios.rds")

df_202310 <- df_202310 %>% 
  filter(scenario_type %in% c("default")) %>%
  pivot_wider(names_from = outcome, values_from = impact_ratio) %>%
  rename(cases_averted_rate=cases, deaths_averted_rate = deaths, yll_averted_rate =yll, dalys_averted_rate=dalys) %>%
  mutate(activity_type = ifelse(grepl("routine", vaccine), "routine", 
                                ifelse(grepl("campaign", vaccine), "campaign", "all"))) %>%
  mutate(vaccine = gsub("_routine|_campaign|_all", "", vaccine)) %>%
  filter(!(vaccine %in% c("Measles_under5s", "Measles_>=5s")))

#-------------------------------------------------------------------------------
df <- df_202310

saveRDS(df, "stochastic_impact_ratios.rds") 

