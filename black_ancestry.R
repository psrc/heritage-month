library(psrccensus)
library(magrittr)
library(data.table)
library(dplyr)
library(srvyr)

so19_5_raw <- get_psrc_pums(5,2019,"p",c("RAC1P","ANC1P"))



so19_coded<- so19_5_raw%>% mutate(RAC1P=factor(RAC1P),
                                  ANC1P=factor(ANC1P))

anc1p_count <- psrc_pums_count(so19_coded, group_vars=c("RAC1P","ANC1P"), incl_counties=FALSE)
anc1p_count_black<-anc1p_count%>% filter(RAC1P== 'Black or African American alone')
write.table(anc1p_count_black, "clipboard", sep="\t", row.names=FALSE)


so19_5_raw <- get_psrc_pums(5,2019,"p",c("RAC1P","ANC2P"))



so19_coded<- so19_5_raw%>% mutate(RAC1P=factor(RAC1P),
                                  ANC2P=factor(ANC2P))

anc2p_count <- psrc_pums_count(so19_coded, group_vars=c("RAC1P","ANC2P"), incl_counties=FALSE)
anc2p_count_black<-anc2p_count%>% filter(RAC1P== 'Black or African American alone')
write.table(anc2p_count_black, "clipboard", sep="\t", row.names=FALSE)