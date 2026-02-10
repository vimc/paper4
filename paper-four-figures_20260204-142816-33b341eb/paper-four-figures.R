# orderly dependencies, outputs and parameters
orderly::orderly_strict_mode()
orderly::orderly_description(
  custom = list(requester = "VIMC", author = "VIMC"))

pars <- orderly::orderly_parameters(exclude_covid = FALSE,
                                    n_samp = 1000)

orderly::orderly_shared_resource("R/disease_palette.R" = "disease_palette.R", who_sub_regions.csv = "who_sub_regions.csv")

orderly::orderly_resource(c("report.Rmd", "R/all_fun.R", "R/data_viz_processing_funs.R", "R/map_fvp_fun.R"))

orderly::orderly_dependency("paper-four-stochastic-impact-ratios", "latest", 
                            c(stochastic_impact_ratios.rds = "stochastic_impact_ratios.rds"))
orderly::orderly_dependency("stochastic-202310-MenAfriVac-MMCV-impact-ratio", "latest",
                            c(men_impact_ratios.rds = "impact_ratios.rds"))

orderly::orderly_artefact(files = "report.html", description = "All figures")
orderly::orderly_artefact(
  files = c("figures/simple_ridges_disease-1.eps",                   "figures/simple_ridges_disease-1.png", 
            "figures/simple_ridges_disease_large-1.eps",             "figures/simple_ridges_disease_large-1.png", 
            "figures/simple_ridges_disease_dalys-1.eps",             "figures/simple_ridges_disease_dalys-1.png",
            "figures/simple_ridges_disease_both-1.eps",              "figures/simple_ridges_disease_both-1.png", 
            
            "figures/ladder_subregion-1.eps", "figures/ladder_subregion-1.png",
            "figures/ladder_subregion_dalys-1.eps", "figures/ladder_subregion_dalys-1.png",
            
            "figures/ladder_subregion_activity-1.eps", "figures/ladder_subregion_activity-1.png",
            "figures/ladder_subregion_activity_dalys-1.eps", "figures/ladder_subregion_activity_dalys-1.png",
            
            "figures/ladder_disease-1.eps", "figures/ladder_disease-1.png", 
            "figures/ladder_disease_dalys-1.eps", "figures/ladder_disease_dalys-1.png", 
            "figures/ladder_disease_activity-1.eps", "figures/ladder_disease_activity-1.png", 
            "figures/ladder_disease_activity_dalys-1.eps", "figures/ladder_disease_activity_dalys-1.png",
          
            "figures/jitter_dose-1.eps",           "figures/jitter_dose-1.png",           
            "figures/jitter_dose_hepb-1.eps", "figures/jitter_dose_hepb-1.png", 
            "figures/jitter_dose_men-1.eps", "figures/jitter_dose_men-1.png",
            "figures/jitter_dose_men_methods-1.eps", "figures/jitter_dose_men_methods-1.png",
            
            "figures/modelling_group_variation-1.eps","figures/modelling_group_variation-1.png",
            "figures/modelling_group_variation_dalys-1.eps","figures/modelling_group_variation_dalys-1.png",
            
            "figures/cholera_fvp_map-1.eps", "figures/cholera_fvp_map-1.png",
            "figures/hib_fvp_map-1.eps", "figures/hib_fvp_map-1.png",
            "figures/pcv_fvp_map-1.eps", "figures/pcv_fvp_map-1.png",
            "figures/je_fvp_map-1.eps", "figures/je_fvp_map-1.png",
            "figures/rota_fvp_map-1.eps", "figures/rota_fvp_map-1.png",
            "figures/typhoid_fvp_map-1.eps", "figures/typhoid_fvp_map-1.png",
            "figures/measles_fvp_map-1.eps", "figures/measles_fvp_map-1.png",
            "figures/hepb_fvp_map-1.eps", "figures/hepb_fvp_map-1.png",
            "figures/covid19_fvp_map-1.eps", "figures/covid19_fvp_map-1.png",
            "figures/men_fvp_map-1.eps", "figures/men_fvp_map-1.png",
            "figures/hpv_fvp_map-1.eps", "figures/hpv_fvp_map-1.png",
            "figures/rubella_fvp_map-1.eps", "figures/rubella_fvp_map-1.png",
            "figures/yf_fvp_map-1.eps", "figures/yf_fvp_map-1.png",
            
            "figures/world_subregion_map-1.eps", "figures/world_subregion_map-1.png"
  ),
  description = "All figures")
orderly::orderly_artefact(files = c( "table_overall.csv", "table_overall_weighted.csv", "run_ids_per_group.rds"),
                          description = "All tables and run IDs")
orderly::orderly_artefact(files = "dataviz.zip", 
                          description="Output for vaxviz tool")
orderly::orderly_artefact(files = c('country_report/summary_table_dalys_vaccine_subregion_activity_type_country.csv',
                                    'country_report/summary_table_dalys_vaccine_subregion_country.csv',
                                    'country_report/summary_table_deaths_vaccine_subregion_activity_type_country.csv',
                                    'country_report/summary_table_deaths_vaccine_subregion_country.csv'),
                          description="Output tables for country report")


library(dplyr)
library(ggplot2)
library(tidyr)
library(ggridges)
library(ggbump)
library(MetBrewer)
library(patchwork)
library(knitr)
library(kableExtra)
library(scales)
library(sf)
library(whomapper)


R.utils::sourceDirectory("R", modifiedOnly=FALSE)
options(dplyr.summarise.inform = FALSE)

subregions <- read.csv("who_sub_regions.csv", stringsAsFactors = FALSE)

df <-  readRDS("stochastic_impact_ratios.rds")
if (pars$exclude_covid){
  df <- df %>% filter(disease != "COVID")
}

## remove 0 rows
df <- df %>% filter(!is.na(fvps) & fvps !=0)

## fix malaria all activity
df <- df %>% 
  filter(vaccine != "Malaria")
tmp <- df %>% 
  filter(disease == "Malaria" & method == "standard") %>%
  mutate(activity_type = "all")
df <- bind_rows(df, tmp)
# ------------------------------------------------------------------------------
# reformatting mena and menacwyx impact ratios
df_men <- readRDS("men_impact_ratios.rds")
df_men <- df_men %>% group_by(run_id, country, vaccine, activity_type, method, outcome) %>% 
  summarise(fvps = sum(fvps), impact_ratio = mean(impact_ratio))
df_men <- df_men %>% pivot_wider(names_from = outcome, values_from = impact_ratio) %>% 
  rename(dalys_averted_rate=dalys, deaths_averted_rate=deaths)
df_men <- df_men %>% left_join(subregions,by = join_by(country))
df_men <- df_men %>% mutate(subregion = factor(subregion, levels=c('Middle Africa',
                                                                   'Northern Africa and Western Asia',
                                                                   'Eastern Africa',
                                                                   'Western Africa',
                                                                   'Southern Africa',
                                                                   
                                                                   'Central and Southern Asia',
                                                                   'Eastern and South-Eastern Asia',
                                                                   
                                                                   'Eastern and Southern Europe',
                                                                   'Northern and Western Europe',
                                                                   
                                                                   'Latin America and the Caribbean',
                                                                   
                                                                   'Northern America',
                                                                   
                                                                   'Oceania')))
# ------------------------------------------------------------------------------
# tidying, renaming and filtering
df2 <- df %>% left_join(subregions,by = join_by(country))

df2 <- df2 %>% filter(modelling_group != "LSHTM-Liu") #LSHTM COVID results not available in this analysis

df2 <- df2 %>% filter(scenario_type=="default") 

df2 <- df2 %>% mutate(vaccine = ifelse(vaccine=="R4", "R21", ifelse(vaccine=="RTS4", "RTS,S", vaccine)))

df2 <- df2 %>% mutate(activity_type = factor(activity_type, levels = c("all", "routine", "campaign"))) #ordering

df2 <- df2 %>% mutate(disease = ifelse(vaccine=="MenCWYX", "MenCWYX", disease)) %>% #splitting for now 
  filter(!(method == "alternative" & vaccine == "MenCWYX"))
# this is further updated given separated approaches to menA and menCWYX
df2 <- df2 %>% mutate(vaccine = case_when(disease == "MenA" & activity_type == "all" ~ "MenA/ACWYX",
                                          TRUE ~ vaccine),
                      disease = case_when(disease == "MenA" & activity_type == "all" ~ "Meningitis",
                                          TRUE ~ disease))
# cutting MenA and MCYWX (old version)
df2 <- df2 %>% filter( !disease %in% c("MenA", "MenCWYX") ) #this still keeps the all / meningitis combination

# order subregions
df2 <- df2 %>% mutate(subregion = factor(subregion, levels=c('Middle Africa',
                                                             'Northern Africa and Western Asia',
                                                             'Eastern Africa',
                                                             'Western Africa',
                                                             'Southern Africa',
                                                             
                                                             'Central and Southern Asia',
                                                             'Eastern and South-Eastern Asia',
                                                             
                                                             'Eastern and Southern Europe',
                                                             'Northern and Western Europe',
                                                             
                                                             'Latin America and the Caribbean',
                                                             
                                                             'Northern America',
                                                             
                                                             'Oceania')))

df2[df2=="COVID"] <- "COVID-19"

#-------------------------------------------------------------------------------
# cholera subset of countries
chol_countries <- c("AFG", "CMR", "COD", "HTI", "MWI", "MOZ", "NGA", "SOM", "YEM")
df2 <- df2 %>% filter(!disease %in% "Cholera" | (disease %in% "Cholera" & country %in% chol_countries))
#-------------------------------------------------------------------------------
# improved sampling
mg <- unique(df2$modelling_group)

set.seed(1) #to retain reproducibility

get_my_run_ids <- sapply(mg, FUN=function(x)sample(1:200, pars$n_samp, replace=TRUE))

saveRDS(get_my_run_ids, "run_ids_per_group.rds")

oot <- bind_rows(lapply(1:ncol(get_my_run_ids),
                        FUN = function(x){
                          tmp8 <- df2 %>%
                            filter(modelling_group == colnames(get_my_run_ids)[x])
                          bind_rows(lapply(get_my_run_ids[,x],
                                           FUN=function(y)tmp8 %>% filter(run_id==y))) 
                        }))

df_men <- bind_rows(lapply(get_my_run_ids[,"Cambridge-Trotter"],
                           FUN=function(y)df_men %>% filter(run_id==y))) 

df_men2 <- df_men %>% 
  mutate(disease = vaccine) %>% 
  filter(method == "Cambridge") %>% 
  mutate(method="standard", modelling_group = "Cambridge-Trotter")
                        

#-------------------------------------------------------------------------------
# ADD MEN A 
oot <- oot %>% 
  bind_rows(df_men2 )

df2_mod_ave <- oot %>% group_by(disease,  country, vaccine, 
                                    activity_type, subregion, country_name, method, 
                                scenario_type, modelling_group) %>% 
  mutate(new_id = row_number()) %>% 
  group_by(disease,  country, vaccine, 
           activity_type, subregion, country_name, method, scenario_type, new_id) %>%
  summarise(deaths_averted_rate=mean(deaths_averted_rate, na.rm=TRUE), 
            cases_averted_rate=mean(cases_averted_rate, na.rm=TRUE),
            dalys_averted_rate=mean(dalys_averted_rate, na.rm=TRUE), 
            yll_averted_rate=mean(yll_averted_rate, na.rm=TRUE),
            fvps = mean(fvps, na.rm = TRUE)) %>% 
  mutate(run_id = new_id)


## Creating datatable for mean deaths/dalys averted per 1000 vaccinations + 95% CIs
combined <- df2_mod_ave %>%
  filter(method == "standard", activity_type=="all") %>%
  group_by(disease, new_id) %>%
  summarise(deaths_rate_modified = mean(deaths_averted_rate, na.rm = TRUE)*1000,
            dalys_rate_modified = mean(dalys_averted_rate, na.rm = TRUE)*1000) %>%
  group_by(disease) %>%
  summarise(
    mean_deaths = round(mean(deaths_rate_modified, na.rm = TRUE), 2),
    low_deaths = round(quantile(deaths_rate_modified, 0.025, na.rm = TRUE), 2),
    upper_deaths = round(quantile(deaths_rate_modified, 0.975, na.rm = TRUE), 2),
    mean_dalys = round(mean(dalys_rate_modified, na.rm = TRUE), 2),
    low_dalys = round(quantile(dalys_rate_modified, 0.025, na.rm = TRUE), 2),
    upper_dalys = round(quantile(dalys_rate_modified, 0.975, na.rm = TRUE), 2)) %>%
  mutate(
    deaths_formatted = paste0(
      formatC(mean_deaths, digits = 2, format = "f"), " (",
      formatC(low_deaths, digits = 2, format = "f"), ", ",
      formatC(upper_deaths, digits = 2, format = "f"), ")"
    ),
    dalys_formatted = paste0(
      formatC(mean_dalys, digits = 2, format = "f"), " (",
      formatC(low_dalys, digits = 2, format = "f"), ", ",
      formatC(upper_dalys, digits = 2, format = "f"), ")"
    )
  ) %>%
  arrange(disease) %>% 
  select(disease, deaths_formatted, dalys_formatted)

write.csv(combined, "table_overall.csv", row.names = FALSE)

combined2 <- df2_mod_ave %>%
  filter(method == "standard", activity_type=="all") %>%
  group_by(disease, new_id) %>%
  summarise(deaths_rate_modified = weighted.mean(deaths_averted_rate,fvps, na.rm = TRUE)*1000,
            dalys_rate_modified = weighted.mean(dalys_averted_rate,fvps, na.rm = TRUE)*1000) %>%
  group_by(disease) %>%
  summarise(
    mean_deaths = round(mean(deaths_rate_modified, na.rm = TRUE), 2),
    low_deaths = round(quantile(deaths_rate_modified, 0.025, na.rm = TRUE), 2),
    upper_deaths = round(quantile(deaths_rate_modified, 0.975, na.rm = TRUE), 2),
    mean_dalys = round(mean(dalys_rate_modified, na.rm = TRUE), 2),
    low_dalys = round(quantile(dalys_rate_modified, 0.025, na.rm = TRUE), 2),
    upper_dalys = round(quantile(dalys_rate_modified, 0.975, na.rm = TRUE), 2)) %>%
  mutate(
    deaths_formatted = paste0(
      formatC(mean_deaths, digits = 2, format = "f"), " (",
      formatC(low_deaths, digits = 2, format = "f"), ", ",
      formatC(upper_deaths, digits = 2, format = "f"), ")"
    ),
    dalys_formatted = paste0(
      formatC(mean_dalys, digits = 2, format = "f"), " (",
      formatC(low_dalys, digits = 2, format = "f"), ", ",
      formatC(upper_dalys, digits = 2, format = "f"), ")"
    )
  ) %>%
  arrange(disease) %>% 
  select(disease, deaths_formatted, dalys_formatted)

write.csv(combined2, "table_overall_weighted.csv", row.names = FALSE)


#-------------------------------------------------------------------------------

# Dataset for FVP maps

## Load and read shp files using prepackaged files from whomapper
who_adm0 <- whomapper::pull_sfs(adm_level = 0, query_server = FALSE)
world_map <- who_adm0$adm0 %>%
  mutate(iso_3_code = ifelse(iso_3_code == "XKX", "XK", iso_3_code))

# Prepare dataset for subregional map
subregion_map_1 <- world_map %>% 
  select(iso_3_code, geometry) %>% 
  left_join(subregions, by = c("iso_3_code" = "country")) %>% 
  filter(!is.na(subregion) & subregion != 0)

## Filter main dataset to include fvps
df4 <- df2 %>% 
  filter(method == "standard", activity_type == "all") %>% 
  group_by(disease, country, country_name) %>% 
  summarise(fvps_all = mean(fvps, na.rm=TRUE)) %>% 
  mutate(fvps_all = fvps_all/1e6) %>% 
  rename(iso_a3 = country)

#-------------------------------------------------------------------------------
# creating output for data viz tool: vaxviz
data_viz_df_in <- df2_mod_ave %>% ungroup() %>% filter(method == "standard") %>% select(-c(method, scenario_type))

grps_ls <-list(c("disease"),
               c("disease", "subregion"),
               c("disease", "country"),
               c("disease", "activity_type"),
               c("disease", "subregion", "activity_type"),
               c("disease", "activity_type", "country"))

dir.create("dataviz", showWarnings = FALSE)
lapply(1:length(grps_ls), FUN=function(x)fun_slice_data_viz(data_viz_df_in, outc="dalys", grps_in=grps_ls[[x]]))
lapply(1:length(grps_ls), FUN=function(x)fun_slice_data_viz(data_viz_df_in, outc="deaths", grps_in=grps_ls[[x]]))

lapply(1:length(grps_ls), FUN=function(x)fun_slice_data_viz(data_viz_df_in, outc="dalys", grps_in=grps_ls[[x]],log=TRUE))
lapply(1:length(grps_ls), FUN=function(x)fun_slice_data_viz(data_viz_df_in, outc="deaths", grps_in=grps_ls[[x]], log=TRUE))

lapply(1:length(grps_ls), FUN=function(x)fun_slice_summary_table(data_viz_df_in, outc="dalys", grps_in=grps_ls[[x]]))
lapply(1:length(grps_ls), FUN=function(x)fun_slice_summary_table(data_viz_df_in, outc="deaths", grps_in=grps_ls[[x]]))

zip("dataviz", "dataviz")

#-------------------------------------------------------------------------------
# creating output for internal country report
grps_ls <-list(c("vaccine", "subregion", "activity_type", "country"),
               c("vaccine", "subregion", "country"))

dir.create("country_report", showWarnings = FALSE)
lapply(1:length(grps_ls), FUN=function(x)fun_slice_summary_table(data_viz_df_in, outc="dalys", 
                                                                 grps_in=grps_ls[[x]], head_dir = "country_report", 
                                                                 filter_out_malaria = FALSE))
lapply(1:length(grps_ls), FUN=function(x)fun_slice_summary_table(data_viz_df_in, outc="deaths", 
                                                                 grps_in=grps_ls[[x]], head_dir = "country_report",
                                                                 filter_out_malaria = FALSE))

#-------------------------------------------------------------------------------
# report
rmarkdown::render("report.Rmd")


