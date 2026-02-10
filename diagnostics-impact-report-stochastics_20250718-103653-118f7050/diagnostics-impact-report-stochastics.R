## (A) report set up

## strict mode
orderly2::orderly_strict_mode()

## report description
orderly2::orderly_description(display = "VIMC 2.0 Stochastic Impact Ratios",
                              long = "Processing Stoachstic Impact Ratios for VIMC 2.0 model runs - 202310gavi and 202409malaria")
## define parameters
pars <- orderly2::orderly_parameters(test_report = FALSE,
                                     disease = "YF",
                                     modelling_group = "IC-Garske")
## pull recipe
file_recipe <- ifelse(pars$disease == "Malaria", 
                      "impact-recipes/recipe_202409malaria_stochastics.csv", 
                      "impact-recipes/recipe_202310gavi_stochastics.csv")
orderly2::orderly_shared_resource("meta/recipe.csv" = file_recipe)

## declare resources
orderly2::orderly_resource(c("readme.md",
                             "report.Rmd",
                             "R/extract.R",
                             "R/transform.R",
                             "R/util.R"))
## declare artefacts
orderly2::orderly_artefact(description = "Diagnostics Report",
                           "report.html")
orderly2::orderly_artefact(description = "Processed data",
                           c("raw_impact_ratios.rds",
                             "summary_statistics.rds"))


## (B) the report
## load tools
library("dplyr")
library("tidyr")
source("R/extract.R")
source("R/transform.R")
source("R/util.R")

### extract data
extracted_data <- extract_data(pars)
## test extracted data
test_extract(extracted_data)

### transform data
transformed_data <- transform_data(extracted_data)
## test transformed data
test_transform(transformed_data)

### knit report
rmarkdown::render("report.Rmd")
