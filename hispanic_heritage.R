library(devtools)
library(sf)
library(dplyr)
library(psrccensus)

Sys.getenv("CENSUS_API_KEY")

tract.big.tbl <- psrccensus::get_decennial_recs(geography='tract',table_codes=c('P005'),year=c(2010))


tract.tbl <- tract.big.tbl %>%
filter(label=='Total!!Hispanic or Latino')

gdb.nm <- paste0("MSSQL:server=",
"AWS-PROD-SQL\\Sockeye",
";database=",
"ElmerGeo",
";trusted_connection=yes")

spn <-  2285

tract_layer_name <- "dbo.tract2010_nowater"

tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)
m<-create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr,  
                 legend.title='Hispanic Population', legend.subtitle='by Census Tract')


