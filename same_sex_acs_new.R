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
#2018 data



#unmarried partner households:
#B11009_003 Male- male; B11009_004 Male-Female; B11009_005 Female-female; B11009_006 Female-male;
#B11009_001 Total

region_female_female_unmarried<-psrc_acs_table("B11009_003", "region", 2010, 'acs5')
region_male_male_unmarried<-psrc_acs_table("B11009_006,", "region", 2010, 'acs5')
region_total_unmarried<-psrc_acs_table("B11009_001", "region", 2010,'acs5')


tract_male_male_unmarried_2010<-psrc_acs_table("B11009_003", "tract", 2010,'acs5')
male_male_partner_2010<-create_tract_map(tract_male_male_unmarried_2005)

tract_female_female_unmarried_2005<-psrc_acs_table("B11009_006", "tract", 2010,'acs5')female_male_partner_2005<-create_tract_map(tract_male_male_unmarried_2005)
