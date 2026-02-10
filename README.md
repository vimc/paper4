# Introduction
Outputs corresponding to the paper "Quantifying relative health impact across Gavi, the Vaccine Alliance's portfolio in 117 countries at the subregional level: a modelling study".  

# Standardised inputs
Demographic and vaccination inputs used by all modelling groups can be found in the 'VIMC_standardised_inputs' folder. 
For each disease, the 'default' with-vaccination scenario is included and, in the situation where there are historical activities in the 'no-vaccination' scenario, the no-vaccination scenario is also included. Demographic inputs, derived from the United Nations World Population Prospects are provided for the interpolated population, crude birth rate, and life expectancy by age as these were the most common inputs used by all groups.

# Outputs
Outputs are provided as [orderly](https://mrc-ide.github.io/orderly/) packits where all inputs, dependencies and outputs can be traced. The included packits are ordered as:

* diagnostics-impact-report-stochastics...
* 202310-202409-stochastic-impact...
* paper-four-stochastic-impact-ratios...
* paper-four-figures...

Where each folder has a brief readme about the aim of the packit and folder contents. Some files exceed the GitHub limit, and some outputs are sensitive due to data restrictions, but final processed outputs are contained in paper-four-figures... or the corresponding data visualisation tool.

# Please note
A data visualisation tool is in development which will complement this repository- this can be accessed here https://vaxviz.vaccineimpact.org/

