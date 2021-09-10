library(psrccensus)
library(dplyr)
library(tidyr)
library(sf)

county_race_ex_df<- get_acs_recs(geography='county', 
                                 table.names='B02001', 
                                 years=c(2019),acs.type='acs1')

county_black_df<-county_race_ex_df %>%
                  filter(label%in% c('Estimate!!Total:', 'Estimate!!Total:!!Black or African American alone'))%>%
                  select(c(name,estimate, label)) %>%
                  pivot_wider(names_from='label',values_from='estimate')%>%
                  mutate('Black Population Share'=`Estimate!!Total:!!Black or African American alone`/`Estimate!!Total:`)%>%
                  select(-c('Estimate!!Total:'))
  

write.table(county_black_df,"clipboard", sep='\t', row.names=FALSE )

tract_black_df <- get_acs_recs('tract', table.names='B02001', years=c(2019),acs.type='acs5')

tract.tbl <- tract_black_df  %>%
  filter(label=='Estimate!!Total:!!Black or African American alone')

gdb.nm <- paste0("MSSQL:server=",
                 "AWS-PROD-SQL\\Sockeye",
                 ";database=",
                 "ElmerGeo",
                 ";trusted_connection=yes")

spn <-  2285

tract_layer_name <- "dbo.tract2010_nowater"

tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)
m<-create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr,  
                    legend.title='Black or African American Alone Population', legend.subtitle='by Census Tract')

tract.tbl_small<- tract.tbl %>% select(GEOID, estimate, moe)

write.table(head(tract.tbl_small),"clipboard", sep='\t', row.names=FALSE )


county_race_ex_df_2015 <- get_acs_recs(geography='county', 
                                 table.names='B02001', 
                                 years=c(2015),acs.type='acs1')

county_black_df_2015<-county_race_ex_df_2015 %>%
  filter(label%in% c('Estimate!!Total', 'Estimate!!Total!!Black or African American alone'))%>%
  select(c(name,estimate, label)) %>%
  pivot_wider(names_from='label',values_from='estimate')%>%
  mutate('Black Population Share'=`Estimate!!Total!!Black or African American alone`/`Estimate!!Total`)%>%
  select(-c('Estimate!!Total'))

write.table(county_black_df_2015,"clipboard", sep='\t', row.names=FALSE )


county_race_ex_df<- get_acs_recs(geography='county', 
                                 table.names='B02001', 
                                 years=c(2019),acs.type='acs1')

county_race_df<-county_race_ex_df %>%
  filter(label%in% c('Estimate!!Total:', 'Estimate!!Total:!!Black or African American alone', 'Estimate!!Total:!!Asian alone', 'Estimate!!Total:!!Two or more races:'))%>%
  select(c(name,estimate, label)) %>%
  pivot_wider(names_from='label',values_from='estimate')%>%
  mutate('Black Population Share'=`Estimate!!Total:!!Black or African American alone`/`Estimate!!Total:`)%>%
  mutate('Asian Population Share'=`Estimate!!Total:!!Asian alone`/`Estimate!!Total:`)%>%
  mutate('Two or more Races Population Share'=`Estimate!!Total:!!Two or more races:`/`Estimate!!Total:`)%>%
  select(-c('Estimate!!Total:'))


write.table(county_race_df,"clipboard", sep='\t', row.names=FALSE )
