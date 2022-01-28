library(psrccensus)
library(magrittr)
library(data.table)
library(dplyr)
library(srvyr)

# Build simplified categories ----------------------------------------
pv <- tidycensus::pums_variables %>% setDT() %>% .[survey=="acs5" & year==2019]                            
pobp_lookup <- pv[var_code=="POBP", .(val_label, val_max)]
pobp_lookup[, POBP_rev:=case_when(between(as.integer(val_max),0,99) ~"U.S.", 
                                  between(as.integer(val_max),100,199) ~ "Europe",
                                  between(as.integer(val_max),200,299) ~ "Asia",
                                  between(as.integer(val_max),300,316) ~ "Americas",
                                  between(as.integer(val_max),321,344) ~ "Caribbean",
                                  between(as.integer(val_max),360,399) ~ "Americas",
                                  (as.integer(val_max) %in% c(400,414,430,436,451,456,464)) ~ "North Africa",
                                  (as.integer(val_max) %in% c(408,420,421,423,425,427,440,444,447,454,467)) ~ "West Africa",
                                  (as.integer(val_max) %in% c(407,412,442,459,460,449,461,462,468)) ~ "Other Africa",
                                  (as.integer(val_max) %in% c(453,457,463,469)) ~ "Other East Africa",
                                  between(as.integer(val_max),500,599) ~ "Australia & Oceania",
                                  TRUE ~ val_label,)]
pobp_lookup %<>% setkey("val_label")

# Recode function - focus on Blacks, apply simplified POB ---------------
recode_race_pobp <- function(x){
  dt <- x[[7]] %>% setDT()                                                 # Copy dataframe from srvyr object
  dt[RAC1P!="Black or African American alone", RAC1P:="All other races"]   # Merge other racial categories; focus on Black heritage
  dt %<>% setkey("POBP")                                                   # psrccensus uses var_label so set that as lookup key
  dt[pobp_lookup, POBP:=i.POBP_rev]                                        # Simplify place-of-birth to categories above
  dt %<>% setDF()
  x[[7]] <- dt                                                             # Re-insert (replace) dataframe to srvyr object
  return(x)
}

# Apply to data ---------------------------------------------------------
so19_5 <- get_psrc_pums(5,2019,"p",c("RAC1P","POBP")) %>% recode_race_pobp()
count_pobp19 <- psrc_pums_count(so19_5, group_vars=c("RAC1P","POBP"), incl_counties=FALSE)

so09_5 <- get_psrc_pums(5,2009,"p",c("RAC1P","POBP")) %>% recode_race_pobp()
count_pobp09 <- psrc_pums_count(so09_5, group_vars=c("RAC1P","POBP"), incl_counties=FALSE)
