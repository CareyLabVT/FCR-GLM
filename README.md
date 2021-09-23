# FCR-GLM

This repository contains the files for calibrating and running the General Lake Model-Aquatic EcoDynamics (GLM-AED) model for Falling Creek Reservoir (FCR; located in Vinton, VA, USA), analyzing the model output, and generating figures for the manuscript on the effects of anoxia on reservoir biogeochemistry by Carey et al. Each of the directories below detail the workflow in order.

## field_data
This directory contains the field data files needed for GLM-AED model driver data, the model sensitivity analysis and parameter calibration, and comparison of model output vs. observations. The "FieldDataPrep.R" script will pull in published datasets from the Environmental Data Initiative (EDI)'s repository and format the data as needed for model analyses as CSV (comma-separated value) files. A "CTD_YSI_comp.R" script provides an easy way to compare FCR depth profiles of temperature and dissolved oxygen from different sensors. Among the CSV files in this directory, two of particular note are: 1) "Calc_HOX_flow_DO_20190916.csv", which provides the oxygenation schedule for FCR from 2013-2019, and 2) "SedimentChambersFluxes.csv", which provides the ancillary data of carbon, nitrogen, and phosphorus concentrations measured by Krueger et al. 2020 (DOI: 10.1016/j.watres.2020.116003) in sediment chambers in FCR.

## inputs
This directory contains R scripts to make the model driver data files to run GLM-AED for FCR, as well as the actual driver data files. The "WetlandInflowPrep.R" script calculates the water budget for Tunnel Branch and Falling Creek using EDI published datasets. The water budget is then used in the "StreamInflowPrep.R" file to make the inflow files for the two streams (which includes their individual water temperature, discharge, and chemistry). Finally, the "SSSInflowPrep.R" script makes the inflow file for the submerged inflow into FCR that simulates the hypolimnetic oxygenation system. The three inflow files, as well as the outflow file and meteorological driver data file (hourly NLDAS-2 weather data), are included in the directory as CSV files.

## aed2
## modeling
## output
## sensitivity
## analysis
## figures
