library(psrccensus)
library(magrittr)
library(data.table)
library(dplyr)
library(srvyr)

so19_5_raw <- get_psrc_pums(5,2019,"p",c("RAC1P","POBP"), label=TRUE)

afr_codes<-read.csv('C:/Users/SChildress/Documents/GitHub/heritage-month/data/african_pums_codes.csv')

so19_5<-so19_5_raw%>% mutate(RAC1P=as.character(RAC1P),
                   POBP=as.character(POBP))



so19_coded<-so19_5%>% mutate(POBP=case_when(between(as.integer(POBP),0,99) ~"U.S.", 
                                  between(as.integer(POBP),100,199) ~"Asia, Europe, Australia & Oceania",
                                  between(as.integer(POBP),200,299) ~"Asia, Europe, Australia & Oceania",
                                  between(as.integer(POBP),300,316) ~"Americas and Caribbean",
                                  between(as.integer(POBP),321,344) ~"Americas and Caribbean",
                                  between(as.integer(POBP),360,399) ~"Americas and Caribbean",
                                  (as.integer(POBP) %in% c(400,414,430,436,451,456,464)) ~"North and Central Africa",
                                  (as.integer(POBP) %in% c(406, 407, 409, 410, 412,415, 430,442, 459)) ~"North and Central Africa",
                                  (as.integer(POBP) %in% c(411, 413, 416, 427, 431, 440, 445, 446, 448, 463, 450, 451, 453, 455, 457, 469)) ~"East Africa",
                                  (as.integer(POBP) %in% c(420, 430,434, 435, 441, 436,451,464, 467))  ~"West, Southern, and Other Africa",
                                  (as.integer(POBP) %in% c(401, 403, 418, 426, 428, 432, 436, 437, 449, 460, 461, 452)) ~"West, Southern, and Other Africa",
                                  (as.integer(POBP) %in% c(402, 405, 408, 421, 422, 423, 424, 425, 429, 433, 438, 439,443, 447, 454, 458)) ~"West, Southern, and Other Africa",
                                  (as.integer(POBP) %in% c(417, 444, 462, 468)) ~"West, Southern, and Other Africa",
                                  between(as.integer(POBP),500,599) ~"Asia, Europe, Australia & Oceania",
                                  TRUE ~POBP))



so19_coded<- so19_5%>% mutate(RAC1P=factor(RAC1P),
                   POBP=factor(POBP))

pobp_count <- psrc_pums_count(so19_5, group_vars=c("RAC1P","POBP"), incl_counties=FALSE)%>% filter(RAC1P==2)
write.table(pobp_count, "clipboard", sep="\t", row.names=FALSE)