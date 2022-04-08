## ids-ne-lakes/scripts 

This folder contains the scripts used to analyze the data from the lakeCoSTR tool in R for the Herrick and Steele, et al. manuscript. The Colab scripts used to gather the Collection 1 and Collection 2 data are stored with their corresponding datasets in the folder path *ids-ne-lakes/data*.

## File Descriptions:

#### Data Processing Scripts:

* __in situ data - data download and collate.Rmd__: This markdown file walks through the download and processing of the Lake Sunapee high-frequency data used for the Landsat-*in situ* pairing process in the primary lakeCoSTR script. This script creates the file 'insitu_temp_data_v2021-10-20.csv' located in the *ids-ne-lakes/data* folder path.

#### Figures, Tables, Analysis:

* __Fig2_sunmap.R__: Creation of Figure 2, map of Lake Sunapee.
* __Fig3_Fig4_A1SF1_A1ST4_A1ST5_DemingFiltersResiduals.R__: Deming analysis script that creates Figures 3 and 4 as well as supplemental figure 1 and supplemental tables 4 and 5 found in Appendix S1. 
* __Fig5_Tab1_A1SF2_application_epi_summer_temperature.R__: Indicator variable analysis of monthly temperatures. This script creates Figure 5, Table 1, and supplemental figure 2 found in Appendix S1.
* __A1ST2-5_insitu_details.R__: this script summarizes the insitu data available for Lake Sunapee, as seen in the supplemental tables 2-5 found in Appendix S1.
* __A1ST6_misc_C1C2_calculations.R__: this script creates Supplemental Table 6 in Appendix S1, a comparison of Collection 1, Collection 2, and Collection 2 with Kurtosis filter scene counts.

### Standalone Appendix Scripts:

* __AppendixS2_Landsat_Filters.Rmd__: this appendix provides an overview of scene-level filters that we tried during the development of this project.
* __AppendixS3_InSitu_Filters.Rmd__: this appendix provides and overview of various *in situ* filters we examined to see how the paired Landsat-*in situ* data set regressions might change. 

