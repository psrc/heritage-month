library(psrccensus)
library(magrittr)
library(data.table)
library(dplyr)
library(srvyr)

so19_5 <- get_psrc_pums(5,2019,"p",c("RAC1P","POBP"), label=FALSE)
so19_5 %<>% mutate(RAC1P=as.character(RAC1P),
                   POBP=as.character(POBP))
so19_5 %<>% mutate(RAC1P=case_when(RAC1P=="2" ~"Black or African American alone", TRUE ~"All other races"))
so19_5 %<>% mutate(POBP=case_when(between(as.integer(POBP),0,99) ~"U.S.", 
                                  between(as.integer(POBP),100,199) ~"Europe",
                                  between(as.integer(POBP),200,299) ~"Asia",
                                  between(as.integer(POBP),300,316) ~"Americas",
                                  between(as.integer(POBP),321,344) ~"Caribbean",
                                  between(as.integer(POBP),360,399) ~"Americas",
                                  (as.integer(POBP) %in% c(400,414,430,436,451,456,464)) ~"North Africa",
                                  (as.integer(POBP) %in% c(408,420,421,423,425,427,440,444,447,454,467)) ~"West Africa",
                                  (as.integer(POBP) %in% c(407,412,442,459,460,449,461,462,468)) ~"Other Africa",
                                  POBP=="416" ~"Ethiopia",
                                  POBP=="417" ~"Eritrea",
                                  POBP=="427" ~"Kenya",
                                  POBP=="448" ~"Somalia",
                                  (as.integer(POBP) %in% c(453,457,463,469)) ~"Other East Africa",
                                  between(as.integer(POBP),500,599) ~"Australia & Oceania",
                                  TRUE ~POBP))
so19_5 %<>% mutate(RAC1P=factor(RAC1P),
                   POBP=factor(POBP))

pobp_count <- psrc_pums_count(so19_5, group_vars=c("RAC1P","POBP"), incl_counties=FALSE)
