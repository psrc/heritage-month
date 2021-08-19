library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)
library(writexl)
library(htmlwidgets)
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census_config.R')
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census.R')

# See: https://www.census.gov/content/dam/Census/library/publications/2021/acs/acsbr-005.pdf
# https://usa.ipums.org/usa/resources/chapter5/SScplfactsheet_final.pdf


Sys.getenv("CENSUS_API_KEY")

# make some tables by region and county
region_hhs_2019<-psrc_acs_table("B09019_003", "region", 2019,'acs5')
region_opposite_sex_married<-psrc_acs_table("B09019_010", "region", 2019,'acs5')
region_same_sex_married<-psrc_acs_table("B09019_011", "region", 2019,'acs5')
region_opposite_sex_cohabit<-psrc_acs_table("B09019_012", "region", 2019,'acs5')
region_same_sex_cohabit<-psrc_acs_table("B09019_013", "region", 2019,'acs5')

#map the tracts
tract_same_sex_married<-psrc_acs_table("B09019_011", "tract", 2019,'acs5')
tract_same_sex_cohabit<-psrc_acs_table("B09019_013", "tract", 2019,'acs5')


ss_married_map<-create_tract_map(tract_same_sex_married)
ss_cohabit_map<-create_tract_map(tract_same_sex_cohabit)

saveWidget(ss_married_map, file='results/same_sex_married_map.html')
saveWidget(ss_cohabit_map, file='results/same_sex_cohabit_map.html')


#unmarried partner households:
#B11009_003 Male- male; B11009_004 Male-Female; B11009_005 Female-female; B11009_006 Female-male;
#B11009_001 Total
region_hhs_2018<-psrc_acs_table("B11009_001", "region", 2018,'acs5')
region_unmarried_2018<-psrc_acs_table("B11009_002", "region", 2018,'acs5')
region_unmarried_male_male_2018<-psrc_acs_table("B11009_003", "region", 2018,'acs5')
region_unmarried_female_female_2018<-psrc_acs_table("B11009_005", "region", 2018,'acs5')

region_hhs_2009<-psrc_acs_table("B11009_001", "region", 2009,'acs5')
region_male_male_unmarried_2009<-psrc_acs_table("B11009_003", "region", 2009, 'acs5')
region_female_female_unmarried_2009<-psrc_acs_table("B11009_005", "region", 2009, 'acs5')
region_total_unmarried_2009<-psrc_acs_table("B11009_002", "region", 2009,'acs5')

tract_male_male_unmarried_2018<-psrc_acs_table("B11009_003", "tract", 2018, 'acs5')
male_male_cohabit_map_2018<-create_tract_map(tract_male_male_unmarried_2018)
saveWidget(male_male_cohabit_map_2018, file='results/male_male_cohabit_map.html')

tract_female_female_unmarried_2018<-psrc_acs_table("B11009_005", "tract", 2018, 'acs5')
female_female_cohabit_map_2018<-create_tract_map(tract_female_female_unmarried_2018)
saveWidget(female_female_cohabit_map_2018, file='results/female_female_cohabit_map.html')

