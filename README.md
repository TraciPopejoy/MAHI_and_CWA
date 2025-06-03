# MAHI_and_CWA
code by Traci P. DuBose, with Wendell R. Haag providing statistical input.

-------------------------------

# Purpose

This repository contains R code for organizing indicator of biotic integrity (IBI) scores provided by state water quality or natural resources department, collating Clean Water Act (CWA) 303(d) integrated report data from United State Environmental Protection Agency (EPA) Assessment, Total Maximum Daily Load Tracking and Implementation System (ATTAINS), and comparing mussel assemblage health (MAHI, Haag et al. 2024) to IBI scores and CWA 303(d) categories for 79 streams across the eastern United States. 

Code is being provided for open science and no guarantees are made by the authors. 

The associated manuscript is being submitted for peer review in July 2025. 

-------------------------------

# Files

### Folder: 303d
This folder contains data related to 303d listings for the river segment that contains each of our mussel sites. This data was primarily queried from the EPA's ATTAINS database using the (R package rATTAINS)[https://github.com/mps9506/rATTAINS]. River segments were identified using the (EPA's How's My Waterway website)[https://mywaterway.epa.gov/]. 

#### 303d_inventory.R
R script used to query data from the ATTAINS database, identify epa IR status, any known issues (not meeting standards), and designated uses for each site. 

### Folder: IBI_data
This contains all of the spreadsheets provided by the state governments that included IBI data.  

#### IBI_consolidation.R
This R script contains code that consolidates and collates all the IBI data provided by 13 different states in the eastern United States. Each state has its own section, which should be in alphabetical order. At the end, we use the provided decimal coordinates to estimate the distance between the mussel site and the IBI site in the NAD83 coordinate system (ESRI number 4269). The main output is a csv of all IBI scores within 50 km of a mussel site with associated year, mussel site, and distance to the mussel site. 

#### IBI_manuscript.R
This R script contains code that 1) ranks IBI scores and identifies the ones closest to mussel sites, 2) assesses the distribution of MAHI and IBI scores, 3) correlates MAHI, macroinvertebrate and fish IBI scores, and 4) compares MAHI scores among CWA 303(d) integrated report categories, designated uses and pollutant/source combinations. Code generating all figures is found in this script, labelled with the current figure number. 

