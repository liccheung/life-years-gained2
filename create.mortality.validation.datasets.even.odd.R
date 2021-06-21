rm(list=ls(all=TRUE)) 
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis.imputed.1.RData")
nhis97_01 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2002_05_1.RData")
nhis02_05 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2006_09_1.RData")
nhis06_09 <- nhis

nhis97_01 <- nhis97_01[which(names(nhis97_01) %in% names(nhis02_05)==TRUE)]
nhis02_05 <- nhis02_05[which(names(nhis02_05) %in% names(nhis97_01)==TRUE)]
nhis06_09 <- nhis06_09[which(names(nhis06_09) %in% names(nhis97_01)==TRUE)]
nhis <- rbind(subset(nhis97_01,year %in% c(1997,1999,2001)),
              subset(nhis02_05,year %in% c(2003,2005)),
              subset(nhis06_09,year %in% c(2007,2009)))

nhis$strata <- ifelse(nhis$year %in% c(1997,1999,2001,2003,2005),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/7
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_1.RData")

nhis <- rbind(subset(nhis97_01,year %in% c(1998,2000)),
              subset(nhis02_05,year %in% c(2002,2004)),
              subset(nhis06_09,year %in% c(2006,2008)))

nhis$strata <- ifelse(nhis$year %in% c(1998,2000,2002,2004),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/6
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_even_1.RData")







rm(list=ls(all=TRUE)) 
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis.imputed.2.RData")
nhis97_01 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2002_05_2.RData")
nhis02_05 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2006_09_2.RData")
nhis06_09 <- nhis

nhis97_01 <- nhis97_01[which(names(nhis97_01) %in% names(nhis02_05)==TRUE)]
nhis02_05 <- nhis02_05[which(names(nhis02_05) %in% names(nhis97_01)==TRUE)]
nhis06_09 <- nhis06_09[which(names(nhis06_09) %in% names(nhis97_01)==TRUE)]
nhis <- rbind(subset(nhis97_01,year %in% c(1997,1999,2001)),
              subset(nhis02_05,year %in% c(2003,2005)),
              subset(nhis06_09,year %in% c(2007,2009)))

nhis$strata <- ifelse(nhis$year %in% c(1997,1999,2001,2003,2005),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/7
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_2.RData")

nhis <- rbind(subset(nhis97_01,year %in% c(1998,2000)),
              subset(nhis02_05,year %in% c(2002,2004)),
              subset(nhis06_09,year %in% c(2006,2008)))

nhis$strata <- ifelse(nhis$year %in% c(1998,2000,2002,2004),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/6
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_even_2.RData")






rm(list=ls(all=TRUE)) 
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis.imputed.3.RData")
nhis97_01 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2002_05_3.RData")
nhis02_05 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2006_09_3.RData")
nhis06_09 <- nhis

nhis97_01 <- nhis97_01[which(names(nhis97_01) %in% names(nhis02_05)==TRUE)]
nhis02_05 <- nhis02_05[which(names(nhis02_05) %in% names(nhis97_01)==TRUE)]
nhis06_09 <- nhis06_09[which(names(nhis06_09) %in% names(nhis97_01)==TRUE)]
nhis <- rbind(subset(nhis97_01,year %in% c(1997,1999,2001)),
              subset(nhis02_05,year %in% c(2003,2005)),
              subset(nhis06_09,year %in% c(2007,2009)))

nhis$strata <- ifelse(nhis$year %in% c(1997,1999,2001,2003,2005),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/7
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_3.RData")

nhis <- rbind(subset(nhis97_01,year %in% c(1998,2000)),
              subset(nhis02_05,year %in% c(2002,2004)),
              subset(nhis06_09,year %in% c(2006,2008)))

nhis$strata <- ifelse(nhis$year %in% c(1998,2000,2002,2004),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/6
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_even_3.RData")



rm(list=ls(all=TRUE)) 
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis.imputed.4.RData")
nhis97_01 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2002_05_4.RData")
nhis02_05 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2006_09_4.RData")
nhis06_09 <- nhis

nhis97_01 <- nhis97_01[which(names(nhis97_01) %in% names(nhis02_05)==TRUE)]
nhis02_05 <- nhis02_05[which(names(nhis02_05) %in% names(nhis97_01)==TRUE)]
nhis06_09 <- nhis06_09[which(names(nhis06_09) %in% names(nhis97_01)==TRUE)]
nhis <- rbind(subset(nhis97_01,year %in% c(1997,1999,2001)),
              subset(nhis02_05,year %in% c(2003,2005)),
              subset(nhis06_09,year %in% c(2007,2009)))

nhis$strata <- ifelse(nhis$year %in% c(1997,1999,2001,2003,2005),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/7
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_4.RData")

nhis <- rbind(subset(nhis97_01,year %in% c(1998,2000)),
              subset(nhis02_05,year %in% c(2002,2004)),
              subset(nhis06_09,year %in% c(2006,2008)))

nhis$strata <- ifelse(nhis$year %in% c(1998,2000,2002,2004),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/6
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_even_4.RData")




rm(list=ls(all=TRUE))
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis.imputed.5.RData")
nhis97_01 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2002_05_5.RData")
nhis02_05 <- nhis
load(file="~/Desktop/Lung cancer/lrisk/other/nhis2002_09/nhis_imputed_2006_09_5.RData")
nhis06_09 <- nhis

nhis97_01 <- nhis97_01[which(names(nhis97_01) %in% names(nhis02_05)==TRUE)]
nhis02_05 <- nhis02_05[which(names(nhis02_05) %in% names(nhis97_01)==TRUE)]
nhis06_09 <- nhis06_09[which(names(nhis06_09) %in% names(nhis97_01)==TRUE)]
nhis <- rbind(subset(nhis97_01,year %in% c(1997,1999,2001)),
              subset(nhis02_05,year %in% c(2003,2005)),
              subset(nhis06_09,year %in% c(2007,2009)))

nhis$strata <- ifelse(nhis$year %in% c(1997,1999,2001,2003,2005),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/7
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_5.RData")

nhis <- rbind(subset(nhis97_01,year %in% c(1998,2000)),
              subset(nhis02_05,year %in% c(2002,2004)),
              subset(nhis06_09,year %in% c(2006,2008)))

nhis$strata <- ifelse(nhis$year %in% c(1998,2000,2002,2004),1000+nhis$strata,nhis$strata)
nhis$adj.wt_mort <- nhis$wt_mort/6
save(nhis, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_even_5.RData")