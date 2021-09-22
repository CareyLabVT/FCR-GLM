# FCR-GLM

This repository contains the files for running GLM-AED calibrated for Falling Creek Reservoir (FCR; located in Vinton, VA, USA), analyzing the model output, and generating figures for the manuscript on the effects of anoxia by Carey et al. Each of the directories below detail the workflow in order.

## field_data
This directory contains the field data files needed for GLM-AED model driver data, the model sensitivity analysis and parameter calibration, and comparison of model output vs. observations. The "FieldDataPrep.R" script will pull in published datasets via the Environmental Data Initiative's API and format the data as needed for model analyses as CSV (comma-separated value) files. A "CTD_YSI_comp.R" script provides an easy way to compare FCR depth profiles of temperature and dissolved oxygen from different sensors. Among the CSV files in this directory, two of particular note are: 1) "Calc_HOX_flow_DO_20190916.csv", which provides the oxygenation schedule for FCR from 2013-2019, and 2) "SedimentChambersFluxes.csv", which provides the ancillary data of carbon, nitrogen, and phosphorus concentrations measured by Krueger et al. 2020 (DOI: 10.1016/j.watres.2020.116003) in sediment chambers in FCR.

## inputs
This directory includes the scripts needed to make the model driver data for GLM-AED. The "WetlandInflowPrep.R" script model driver data needed to run the GLM-AED model: 1) meteorological data 

## aed2
## modeling
## output
## sensitivity
## analysis
## figures
