# FCR-GLM

This repository contains the files for calibrating and running the General Lake Model-Aquatic EcoDynamics (GLM-AED) model for Falling Creek Reservoir (FCR; located in Vinton, VA, USA), analyzing the model output, and generating figures for the manuscript on the effects of anoxia on reservoir biogeochemistry by Carey et al. Each of the directories below detail the workflow in order, which are contained in the "FCR_2013_2019GLMHistoricalRun_GLMv3beta" subdirectory.

## field_data
This directory contains the field data files needed for GLM-AED model driver data, the model sensitivity analysis and parameter calibration, and comparison of model output vs. observations. The "FieldDataPrep.R" script will pull in published datasets from the Environmental Data Initiative (EDI)'s repository and format the data as needed for model analyses as CSV (comma-separated value) files. A "CTD_YSI_comp.R" script provides an easy way to compare FCR depth profiles of temperature and dissolved oxygen from different sensors. Among the CSV files in this directory, two of particular note are: 1) "Calc_HOX_flow_DO_20190916.csv", which provides the oxygenation schedule for FCR from 2013-2019, and 2) "SedimentChambersFluxes.csv", which provides the ancillary data of carbon, nitrogen, and phosphorus concentrations measured by Krueger et al. 2020 (DOI: 10.1016/j.watres.2020.116003) in sediment chambers in FCR.

## inputs
This directory contains R scripts to make the model driver data files to run GLM-AED for FCR, as well as the actual driver data files. The "WetlandInflowPrep.R" script calculates the water budget for Tunnel Branch and Falling Creek using EDI published datasets. The stream water budgets are then used in the "StreamInflowPrep.R" file to make the inflow model driver files for the two streams (which includes their individual water temperature, water budget, and chemistry). Finally, the "SSSInflowPrep.R" script makes the inflow file for the submerged stream inflow into FCR that simulates the hypolimnetic oxygenation system. The three stream inflow files, as well as the outflow driver file and meteorological driver data file (hourly NLDAS-2 weather data), are included in the directory as CSV files.

## modeling
This directory contains R scripts needed to run the GLM-AED sensitivity analysis and automated calibration ("SensitivityCalibration.R") for FCR, which use functions provided in the "functions-glm.R" script. The "GLM_Run_Script_FCR.R" is used to run the model once it is calibrated and compare each of the focal state variables with observations at multiple reservoir depths.

## sensitivity
This directory contains the CSV files that serve as inputs to the model sensitivity analysis run in the "modeling/SensitivityCalibration.R" script for the focal model state variables. 

## results
This directory contains the output from the model sensitivity analysis and calibration analysis run in the "modeling/SensitivityCalibration.R" script for the focal model state variables. 

## aed2
This directory contains the two AED configuration files ("aed2_20210204_2DOCpools.nml" for the reservoir's biogeochemistry, "aed2_phyto_pars_30June2020.nml" for the reservoir's phytoplankton community) with calibrated parameters for FCR. 

## output
After the model is run, this directory contains the output netcdf files from GLM-AED. This directory also contains summarized output in CSV files needed for the analysis in Carey et al. (including t-test statistics, % reservoir retention, modeled vs. observed elemental concentrations and ratios, etc.)

## analysis
This directory contains the R scripts needed to analyze the model output for the Carey et al. analysis. This includes the script used for making the oxic/anoxic model scenarios ("ScenarioGeneration.R"), comparing observations and model simulations for the anoxic/oxic scenarios ("Analyze Observed Simulation.R"), analyzing the sediment flux chambers for comparison with calibrated parameters ("SedimentFluxCalculations.R"), and calculating the needed values used in the manuscript ("NeededNumbersForText.R" and "NIDFacts.R").

## figures
This directory contains the R script used to make data objects for visualization ("MakeFigure_OrganizeDataForFigures.R"), individual scripts used for making each of the goodness-of-fit table, data figures ("MakeFigure_FigureX..."), and the actual figures. A subdirectory called "supp_figs" contains the R scripts and output files for the supplementary figures and tables in the manuscript.

## main directory
Within the FCR-GLM directory contains the dynamic library files needed to run the GLM-AED model (v.3.2.0a3) on mac computers only, the GLM-AED binary file for this model version, and the three GLM configuration files: 1) "glm3.nml", to simulate observed conditions; 2) "glm3_anoxic.nml", the anoxic model scenario; and 3) "glm3_oxic.nml", the oxic model scenario. To run either scenario, the "_anoxic" or "_oxic" suffix would need to be removed first.
