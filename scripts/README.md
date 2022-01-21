## lakeCoSTR_manu/scripts

This folder contains the scripts used to analyze the data from the lakeCoSTR tool in R. 

### file definitions:

#### Data Processing:
* __validation - data download and collation.Rmd__: This markdown file walks through the download and processing of the Lake Sunapee high-frequency data used for validation. This script creates the file 'insitu_temp_data_v2021-10-20.csv' located in the data folder

### Figures, Tables, Analysis
* __Fig2_sunmap.R__: Creation of Figure 2, map of Lake Sunapee.
* __Fig3_Fig4_SF1_ST4_ST5_DemingFiltersResiduals.R__: Deming analysis script that creates Figures 3 and 4 as well as supplemental figure 1 and supplemental tables 4 and 5. 
* __Fig5_Tab1_SF2_application_epi_summer_temperature.R__: Indicator variable analysis of monthly temperatures. This script creates Figure 5, Table 1, and supplemental figure 2.
* __ST2_insitu_details.R__: this script summarizes the insitu data available for Lake Sunapee, as seen in Supplemental Table 2. 
* __ST3_misc_calculations.R__: this script creates Supplemental Table 3, a comparison of Collection 1, Collection 2, and Collection 2 with Kurtosis filter scene counts.

### Appendices:
* __AppendixS2_Landsat_Filters.Rmd__
* __AppendixS3_Validation_Datasets.Rmd__