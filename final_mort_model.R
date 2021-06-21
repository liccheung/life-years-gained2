#This mortality model is the best AIC model using NHIS 97-2009 odd years as training data
rm(list=ls())
library(survival)
library(survey)
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/nhis_imputed_1997_09_odd_1.RData")

nhis$deathage[nhis$deathage<=nhis$age] <- nhis$age[nhis$deathage<=nhis$age] + .125 #give some follow-up time to those whose deathage is <= current age
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
design <- subset(master, age>=40&age<=84 & analypop==1 & deathage>age) #removed prior lung cancer to match published model
#All NHIS year transformations have same AIC
#beats sqrt(cpd) by decent margin
morat <- svycoxph(Surv(age, deathage, died) ~ 
                    female + race + edu6 + year + 
                    I(bmi <= 18.5) + I(bmi > 18.5 & bmi <= 20) + I(bmi > 25 & bmi <= 30) + I(bmi > 30 & bmi <= 35) + I(bmi > 35) + 
                    emp + hypertension + chd + angina + heartattack + heartdisease + stroke + diab + bron + kidney + liver + speceq + prior.cancer + 
                    I(log(qtyears + 1)) + I(log(cpd)) + I(sqrt(pkyr.cat)),
                  design=design, data = nhis, model = TRUE, y = TRUE) 
write.table(summary(morat)$conf.int,"~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/HR_v6.csv",sep=",")

#reran with latest packages
save(morat, file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/mortality.model.v6.RData")