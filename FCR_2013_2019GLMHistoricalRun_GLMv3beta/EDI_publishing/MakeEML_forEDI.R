#*****************************************************************
#* TITLE:   Script to publish the FCR GLM-AED model in EDI using
#*          the EDI assemblyline tools           
#* AUTHORS:  C.C. Carey                                          
#* DATE:   Created 15 Sept 2021                           
#* NOTES:  Goal of this script is to develop an EDI data publication
#*         that can be cited in a manuscript for the calibrated FCR-GLM-AED
#*         model. All nml files and input files are included for running
#*         the model for FCR during 2013-2019.
#*****************************************************************
# useful ref for learning more about publishing data in EDI: 
#https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/instructions.md

# # Install & load devtools before installing updated version of EDI's EMLassemblyline package
# install.packages("devtools")
# library(devtools)
# devtools::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Step 1: Create a directory for your dataset, here "EDI_publishing" within the main repo
#setwd("./EDI_publishing")

#Step 2: Move your dataset to the directory
file.copy('glm3.nml', 'EDI_publishing/glm3.nml')
file.copy('aed2/aed2_20210204_2DOCpools.nml', 'EDI_publishing/aed2_20210204_2DOCpools.nml')
file.copy('aed2/aed2_phyto_pars_30June2020.nml', 'EDI_publishing/aed2_phyto_pars_30June2020.nml')
file.copy('inputs/FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv', 'EDI_publishing/FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv')
file.copy('inputs/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv', 'EDI_publishing/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv')
file.copy('inputs/FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv', 'EDI_publishing/FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv')
file.copy('inputs/FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv', 'EDI_publishing/FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv')
file.copy('inputs/FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv', 'EDI_publishing/FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv')
#once the nml files are copied over, I put them in a sub-directory called "model_configuration_nml_files"

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset

#Step 5: Import the core metadata templates to your local directory
import_templates(path = "/Users/cayelan/Dropbox/ComputerFiles/SCC/FCR-GLM/FCR_2013_2019GLMHistoricalRun_GLMv3beta/EDI_publishing",
                 license = "CCBY",
                 data.files = c("FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv", 
                                "FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv",
                                "FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv",
                                "FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv",
                                "FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv"))

# Edit each of these imported template files for your current data package upload, by copying and 
# pasting the relevant information from the EDI_metadata_template you prepared

# Important! Before saving, check that the contents of each .txt file do not include any 
# non-allowed characters by going to: https://pteo.paranoiaworks.mobi/diacriticsremover/, 
# pasting your text, and clicking remove diacritics. copy and paste that text back into the .txt file.

view_unit_dictionary()

# After saving each file, make sure it is closed.

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#nothing mandatory for Carey Lab in this section

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
#view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# View documentation for this function
#?define_catvars

# Run this function for your dataset
define_catvars(path = "/Users/cayelan/Dropbox/ComputerFiles/Virginia_Tech/FallingCreek/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLIce")

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file that is Carey Lab specific into your working directory

#Step 16: Make EML
# View documentation for this function
?make_eml

## Step XXX: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

## Step XXX: Make EML metadata file using the EMLassemblyline::make_eml() command ####
# For modules that contain only zip folders, modify and run the following 
# ** double-check that all files are closed before running this command! **

# You will need to modify the following lines to match your current module: 
# path: set to your computer's FULL file path!
# dataset.title: Update to current module
# zip.dir: Change the name of the module files zip folder
# temporal.coverage: Update the dates
# package.id: enter the ID you obtained in Step 6
make_eml(path = "/Users/cayelan/Dropbox/ComputerFiles/Virginia_Tech/FallingCreek/Github/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLIce",
         dataset.title = "Ice cover data for Falling Creek Reservoir, Vinton, Virginia, USA for 2013-2021",
         data.files = c('Ice_Data.csv'),
         data.files.description = c('All ice-on and ice-off data recorded for Falling Creek Reservoir during 2013-2021'),
         temporal.coverage = c("2014-01-06", "2021-02-23"),
         #geographic.description = c("Falling Creek Reservoir, Vinton, Virginia, USA"),
         #geographic.coordinates = c('37.309589', '-79.836009', '37.302660', '-79.839249'), #N, E, S, W
         maintenance.description = "ongoing", 
         user.id = "ccarey",
         affiliation = 'EDI',
         package.id = "edi.456.3") # Put your package.id here, followed by .1 (for 1st version)

#PROBLEMS WITH MAKING METATDATA! SO, COLIN SUGGESTED THAT THE FALLING CREEK SPACE IN THE PATH NAME WAS
#  PROBLEMATIC, SO I COPIED AND PASTED THE ENTIRE DIRECTORY TO MY DESKTOP AND RAN THE MAKE_EML PATH THERE. THAT SEEMED TO WORK
# ??!!! SO AM COPYING & PASTING THE .XML FILE BACK INTO THE GITHUB DIRECTORY. WORTH A TRY TO RUN IT OUT OF THERE
# NEXT TIME WE UPDATE THE MET DATA IN THE FUTURE. I ALSO DELETED THE ZIP FILES 


# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!