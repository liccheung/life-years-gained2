#need to add risk of death after x years
rm(list=ls(all=TRUE))
library(survival)
library(survey)
load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/mortality.model.v6.RData")

drest <- function(design){
  EO.hat <- svyratio(num=~death_risk, denom=~died, design)
  eo <- rbind(svytable(~died,design)[2],svytotal(~death_risk,design)[1],coef(EO.hat),vcov(EO.hat))
  names(eo) <- c("obs","exp","E/O","Var(E/O)")
  return(eo)
}

foo <- function(x){
  if (is.numeric(nhis$death_risk[[x]])==1){
    return(nhis$death_risk[[x]])
  } else {
    return(as.numeric(NA))
  }
}

load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6/nhis1997_09_even2_mod1.RData")
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 & 
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
master <- subset(master, analypop==1 & age>=40 & age <= 84 & !is.na(death_risk))
exp_by5 <- svytotal(~death_risk,master)[1]/5
#dr5yr_cp <- c(min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=4*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=3*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=2*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=1*exp_by5)]))
uspstf <- subset(master, uspstf.eligible==1)
notuspstf <- subset(master, uspstf.eligible==0)
#nhisd1 <- subset(master,deathrisk5yr<=dr5yr_cp[1])
#nhisd2 <- subset(master,deathrisk5yr>dr5yr_cp[1] & deathrisk5yr<=dr5yr_cp[2])
#nhisd3 <- subset(master,deathrisk5yr>dr5yr_cp[2] & deathrisk5yr<=dr5yr_cp[3])
#nhisd4 <- subset(master,deathrisk5yr>dr5yr_cp[3] & deathrisk5yr<=dr5yr_cp[4])
#nhisd5 <- subset(master,deathrisk5yr>dr5yr_cp[4])
#dr5yr_cp.1 <- dr5yr_cp
nhis1998 <- subset(master,year==1998)
nhis2000 <- subset(master,year==2000)
nhis2002 <- subset(master,year==2002)
nhis2004 <- subset(master,year==2004)
nhis2006 <- subset(master,year==2006)
nhis2008 <- subset(master,year==2008)
age1 <- subset(master,age<50)
age2 <- subset(master,age>=50 & age<60)
age3 <- subset(master,age>=60 & age<70)
age4 <- subset(master,age>=70 & age<80)
age5 <- subset(master,age>=80)
male <- subset(master,female==0)
female <- subset(master,female==1)
white <- subset(master,race==0)
black <- subset(master,race==1)
hisp <- subset(master,race==2)
other <- subset(master,race==3)
bmi1 <- subset(master,bmi<=18.5)
bmi2 <- subset(master,bmi>18.5 & bmi<=20)
bmi3 <- subset(master,bmi>20 & bmi<=25)
bmi4 <- subset(master,bmi>25 & bmi<=30)
bmi5 <- subset(master,bmi>30 & bmi<=35)
bmi6 <- subset(master,bmi>35)
current <- subset(master,current==1)
former <- subset(master,former==1)
cur.cpd1 <- subset(master,cpd<20 & current==1)
cur.cpd2 <- subset(master,cpd>=20 & cpd<30 & current==1)
cur.cpd3 <- subset(master,cpd>=30 & cpd<40 & current==1)
cur.cpd4 <- subset(master,cpd>=40 & current==1)
form.cpd1 <- subset(master,cpd<20 & former==1)
form.cpd2 <- subset(master,cpd>=20 & cpd<30 & former==1)
form.cpd3 <- subset(master,cpd>=30 & cpd<40 & former==1)
form.cpd4 <- subset(master,cpd>=40 & former==1)
nocomor <- subset(master,comorbidities==0)
comor1 <- subset(master,comorbidities==1)
comor2 <- subset(master,comorbidities==2)
comor3p <- subset(master,comorbidities>=3)
eo.1 <- cbind(drest(master),drest(uspstf),drest(notuspstf))
#eo.dr.1 <- cbind(drest(nhisd1),drest(nhisd2),drest(nhisd3),drest(nhisd4),drest(nhisd5))
eo.yr.1 <- cbind(drest(nhis1998),drest(nhis2000),drest(nhis2002),drest(nhis2004),drest(nhis2006),drest(nhis2008))
eo.age.1 <- cbind(drest(age1),drest(age2),drest(age3),drest(age4),drest(age5))
eo.gender.1 <- cbind(drest(male),drest(female))
eo.race.1 <- cbind(drest(white),drest(black),drest(hisp),drest(other))
eo.bmi.1 <- cbind(drest(bmi1),drest(bmi2),drest(bmi3),drest(bmi4),drest(bmi5),drest(bmi6))
eo.smkstat.1 <- cbind(drest(current),drest(former))
eo.cpd.1 <- cbind(drest(cur.cpd1),drest(cur.cpd2),drest(cur.cpd3),drest(cur.cpd4),
                  drest(form.cpd1),drest(form.cpd2),drest(form.cpd3),drest(form.cpd4))
eo.comor.1 <- cbind(drest(nocomor),drest(comor1),drest(comor2),drest(comor3p))

load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6/nhis1997_09_even2_mod2.RData")
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 & 
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
master <- subset(master, analypop==1 & age>=40 & age <= 84 & !is.na(death_risk))
uspstf <- subset(master, uspstf.eligible==1)
notuspstf <- subset(master, uspstf.eligible==0)
exp_by5 <- svytotal(~death_risk,master)[1]/5
#dr5yr_cp <- c(min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=4*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=3*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=2*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=1*exp_by5)]))
#nhisd1 <- subset(master,deathrisk5yr<=dr5yr_cp[1])
#nhisd2 <- subset(master,deathrisk5yr>dr5yr_cp[1] & deathrisk5yr<=dr5yr_cp[2])
#nhisd3 <- subset(master,deathrisk5yr>dr5yr_cp[2] & deathrisk5yr<=dr5yr_cp[3])
#nhisd4 <- subset(master,deathrisk5yr>dr5yr_cp[3] & deathrisk5yr<=dr5yr_cp[4])
#nhisd5 <- subset(master,deathrisk5yr>dr5yr_cp[4])
#dr5yr_cp.2 <- dr5yr_cp
nhis1998 <- subset(master,year==1998)
nhis2000 <- subset(master,year==2000)
nhis2002 <- subset(master,year==2002)
nhis2004 <- subset(master,year==2004)
nhis2006 <- subset(master,year==2006)
nhis2008 <- subset(master,year==2008)
age1 <- subset(master,age<50)
age2 <- subset(master,age>=50 & age<60)
age3 <- subset(master,age>=60 & age<70)
age4 <- subset(master,age>=70 & age<80)
age5 <- subset(master,age>=80)
male <- subset(master,female==0)
female <- subset(master,female==1)
white <- subset(master,race==0)
black <- subset(master,race==1)
hisp <- subset(master,race==2)
other <- subset(master,race==3)
bmi1 <- subset(master,bmi<=18.5)
bmi2 <- subset(master,bmi>18.5 & bmi<=20)
bmi3 <- subset(master,bmi>20 & bmi<=25)
bmi4 <- subset(master,bmi>25 & bmi<=30)
bmi5 <- subset(master,bmi>30 & bmi<=35)
bmi6 <- subset(master,bmi>35)
current <- subset(master,current==1)
former <- subset(master,former==1)
cur.cpd1 <- subset(master,cpd<20 & current==1)
cur.cpd2 <- subset(master,cpd>=20 & cpd<30 & current==1)
cur.cpd3 <- subset(master,cpd>=30 & cpd<40 & current==1)
cur.cpd4 <- subset(master,cpd>=40 & current==1)
form.cpd1 <- subset(master,cpd<20 & former==1)
form.cpd2 <- subset(master,cpd>=20 & cpd<30 & former==1)
form.cpd3 <- subset(master,cpd>=30 & cpd<40 & former==1)
form.cpd4 <- subset(master,cpd>=40 & former==1)
nocomor <- subset(master,comorbidities==0)
comor1 <- subset(master,comorbidities==1)
comor2 <- subset(master,comorbidities==2)
comor3p <- subset(master,comorbidities>=3)
eo.2 <- cbind(drest(master),drest(uspstf),drest(notuspstf))
#eo.dr.2 <- cbind(drest(nhisd1),drest(nhisd2),drest(nhisd3),drest(nhisd4),drest(nhisd5))
eo.yr.2 <- cbind(drest(nhis1998),drest(nhis2000),drest(nhis2002),drest(nhis2004),drest(nhis2006),drest(nhis2008))
eo.age.2 <- cbind(drest(age1),drest(age2),drest(age3),drest(age4),drest(age5))
eo.gender.2 <- cbind(drest(male),drest(female))
eo.race.2 <- cbind(drest(white),drest(black),drest(hisp),drest(other))
eo.bmi.2 <- cbind(drest(bmi1),drest(bmi2),drest(bmi3),drest(bmi4),drest(bmi5),drest(bmi6))
eo.smkstat.2 <- cbind(drest(current),drest(former))
eo.cpd.2 <- cbind(drest(cur.cpd1),drest(cur.cpd2),drest(cur.cpd3),drest(cur.cpd4),
                  drest(form.cpd1),drest(form.cpd2),drest(form.cpd3),drest(form.cpd4))
eo.comor.2 <- cbind(drest(nocomor),drest(comor1),drest(comor2),drest(comor3p))

load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6/nhis1997_09_even2_mod3.RData")
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 & 
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
master <- subset(master, analypop==1 & age>=40 & age <= 84 & !is.na(death_risk))
uspstf <- subset(master, uspstf.eligible==1)
notuspstf <- subset(master, uspstf.eligible==0)
exp_by5 <- svytotal(~death_risk,master)[1]/5
#dr5yr_cp <- c(min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=4*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=3*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=2*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=1*exp_by5)]))
#nhisd1 <- subset(master,deathrisk5yr<=dr5yr_cp[1])
#nhisd2 <- subset(master,deathrisk5yr>dr5yr_cp[1] & deathrisk5yr<=dr5yr_cp[2])
#nhisd3 <- subset(master,deathrisk5yr>dr5yr_cp[2] & deathrisk5yr<=dr5yr_cp[3])
#nhisd4 <- subset(master,deathrisk5yr>dr5yr_cp[3] & deathrisk5yr<=dr5yr_cp[4])
#nhisd5 <- subset(master,deathrisk5yr>dr5yr_cp[4])
#dr5yr_cp.3 <- dr5yr_cp
nhis1998 <- subset(master,year==1998)
nhis2000 <- subset(master,year==2000)
nhis2002 <- subset(master,year==2002)
nhis2004 <- subset(master,year==2004)
nhis2006 <- subset(master,year==2006)
nhis2008 <- subset(master,year==2008)
age1 <- subset(master,age<50)
age2 <- subset(master,age>=50 & age<60)
age3 <- subset(master,age>=60 & age<70)
age4 <- subset(master,age>=70 & age<80)
age5 <- subset(master,age>=80)
male <- subset(master,female==0)
female <- subset(master,female==1)
white <- subset(master,race==0)
black <- subset(master,race==1)
hisp <- subset(master,race==2)
other <- subset(master,race==3)
bmi1 <- subset(master,bmi<=18.5)
bmi2 <- subset(master,bmi>18.5 & bmi<=20)
bmi3 <- subset(master,bmi>20 & bmi<=25)
bmi4 <- subset(master,bmi>25 & bmi<=30)
bmi5 <- subset(master,bmi>30 & bmi<=35)
bmi6 <- subset(master,bmi>35)
current <- subset(master,current==1)
former <- subset(master,former==1)
cur.cpd1 <- subset(master,cpd<20 & current==1)
cur.cpd2 <- subset(master,cpd>=20 & cpd<30 & current==1)
cur.cpd3 <- subset(master,cpd>=30 & cpd<40 & current==1)
cur.cpd4 <- subset(master,cpd>=40 & current==1)
form.cpd1 <- subset(master,cpd<20 & former==1)
form.cpd2 <- subset(master,cpd>=20 & cpd<30 & former==1)
form.cpd3 <- subset(master,cpd>=30 & cpd<40 & former==1)
form.cpd4 <- subset(master,cpd>=40 & former==1)
nocomor <- subset(master,comorbidities==0)
comor1 <- subset(master,comorbidities==1)
comor2 <- subset(master,comorbidities==2)
comor3p <- subset(master,comorbidities>=3)
eo.3 <- cbind(drest(master),drest(uspstf),drest(notuspstf))
#eo.dr.3 <- cbind(drest(nhisd1),drest(nhisd2),drest(nhisd3),drest(nhisd4),drest(nhisd5))
eo.yr.3 <- cbind(drest(nhis1998),drest(nhis2000),drest(nhis2002),drest(nhis2004),drest(nhis2006),drest(nhis2008))
eo.age.3 <- cbind(drest(age1),drest(age2),drest(age3),drest(age4),drest(age5))
eo.gender.3 <- cbind(drest(male),drest(female))
eo.race.3 <- cbind(drest(white),drest(black),drest(hisp),drest(other))
eo.bmi.3 <- cbind(drest(bmi1),drest(bmi2),drest(bmi3),drest(bmi4),drest(bmi5),drest(bmi6))
eo.smkstat.3 <- cbind(drest(current),drest(former))
eo.cpd.3 <- cbind(drest(cur.cpd1),drest(cur.cpd2),drest(cur.cpd3),drest(cur.cpd4),
                  drest(form.cpd1),drest(form.cpd2),drest(form.cpd3),drest(form.cpd4))
eo.comor.3 <- cbind(drest(nocomor),drest(comor1),drest(comor2),drest(comor3p))

load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6/nhis1997_09_even2_mod4.RData")
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 & 
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
master <- subset(master, analypop==1 & age>=40 & age <= 84 & !is.na(death_risk))
uspstf <- subset(master, uspstf.eligible==1)
notuspstf <- subset(master, uspstf.eligible==0)
exp_by5 <- svytotal(~death_risk,master)[1]/5
#dr5yr_cp <- c(min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=4*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=3*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=2*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=1*exp_by5)]))
#nhisd1 <- subset(master,deathrisk5yr<=dr5yr_cp[1])
#nhisd2 <- subset(master,deathrisk5yr>dr5yr_cp[1] & deathrisk5yr<=dr5yr_cp[2])
#nhisd3 <- subset(master,deathrisk5yr>dr5yr_cp[2] & deathrisk5yr<=dr5yr_cp[3])
#nhisd4 <- subset(master,deathrisk5yr>dr5yr_cp[3] & deathrisk5yr<=dr5yr_cp[4])
#nhisd5 <- subset(master,deathrisk5yr>dr5yr_cp[4])
#dr5yr_cp.4 <- dr5yr_cp
nhis1998 <- subset(master,year==1998)
nhis2000 <- subset(master,year==2000)
nhis2002 <- subset(master,year==2002)
nhis2004 <- subset(master,year==2004)
nhis2006 <- subset(master,year==2006)
nhis2008 <- subset(master,year==2008)
age1 <- subset(master,age<50)
age2 <- subset(master,age>=50 & age<60)
age3 <- subset(master,age>=60 & age<70)
age4 <- subset(master,age>=70 & age<80)
age5 <- subset(master,age>=80)
male <- subset(master,female==0)
female <- subset(master,female==1)
white <- subset(master,race==0)
black <- subset(master,race==1)
hisp <- subset(master,race==2)
other <- subset(master,race==3)
bmi1 <- subset(master,bmi<=18.5)
bmi2 <- subset(master,bmi>18.5 & bmi<=20)
bmi3 <- subset(master,bmi>20 & bmi<=25)
bmi4 <- subset(master,bmi>25 & bmi<=30)
bmi5 <- subset(master,bmi>30 & bmi<=35)
bmi6 <- subset(master,bmi>35)
current <- subset(master,current==1)
former <- subset(master,former==1)
cur.cpd1 <- subset(master,cpd<20 & current==1)
cur.cpd2 <- subset(master,cpd>=20 & cpd<30 & current==1)
cur.cpd3 <- subset(master,cpd>=30 & cpd<40 & current==1)
cur.cpd4 <- subset(master,cpd>=40 & current==1)
form.cpd1 <- subset(master,cpd<20 & former==1)
form.cpd2 <- subset(master,cpd>=20 & cpd<30 & former==1)
form.cpd3 <- subset(master,cpd>=30 & cpd<40 & former==1)
form.cpd4 <- subset(master,cpd>=40 & former==1)
nocomor <- subset(master,comorbidities==0)
comor1 <- subset(master,comorbidities==1)
comor2 <- subset(master,comorbidities==2)
comor3p <- subset(master,comorbidities>=3)
eo.4 <- cbind(drest(master),drest(uspstf),drest(notuspstf))
#eo.dr.4 <- cbind(drest(nhisd1),drest(nhisd2),drest(nhisd3),drest(nhisd4),drest(nhisd5))
eo.yr.4 <- cbind(drest(nhis1998),drest(nhis2000),drest(nhis2002),drest(nhis2004),drest(nhis2006),drest(nhis2008))
eo.age.4 <- cbind(drest(age1),drest(age2),drest(age3),drest(age4),drest(age5))
eo.gender.4 <- cbind(drest(male),drest(female))
eo.race.4 <- cbind(drest(white),drest(black),drest(hisp),drest(other))
eo.bmi.4 <- cbind(drest(bmi1),drest(bmi2),drest(bmi3),drest(bmi4),drest(bmi5),drest(bmi6))
eo.smkstat.4 <- cbind(drest(current),drest(former))
eo.cpd.4 <- cbind(drest(cur.cpd1),drest(cur.cpd2),drest(cur.cpd3),drest(cur.cpd4),
                  drest(form.cpd1),drest(form.cpd2),drest(form.cpd3),drest(form.cpd4))
eo.comor.4 <- cbind(drest(nocomor),drest(comor1),drest(comor2),drest(comor3p))

load(file="~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6/nhis1997_09_even2_mod5.RData")
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 & 
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)
master  <- svydesign(id=~psu, strata=~strata, weights=~adj.wt_mort, data=nhis, nest=TRUE)
master <- subset(master, analypop==1 & age>=40 & age <= 84 & !is.na(death_risk))
uspstf <- subset(master, uspstf.eligible==1)
notuspstf <- subset(master, uspstf.eligible==0)
exp_by5 <- svytotal(~death_risk,master)[1]/5
#dr5yr_cp <- c(min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=4*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=3*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=2*exp_by5)]),
#              min(nhis$deathrisk5yr[order(nhis$deathrisk5yr,decreasing = TRUE)][which(cumsum(nhis$adj.wt_mort[order(nhis$deathrisk5yr,decreasing = TRUE)]*nhis$death_risk[order(nhis$deathrisk5yr,decreasing = TRUE)])<=1*exp_by5)]))
#nhisd1 <- subset(master,deathrisk5yr<=dr5yr_cp[1])
#nhisd2 <- subset(master,deathrisk5yr>dr5yr_cp[1] & deathrisk5yr<=dr5yr_cp[2])
#nhisd3 <- subset(master,deathrisk5yr>dr5yr_cp[2] & deathrisk5yr<=dr5yr_cp[3])
#nhisd4 <- subset(master,deathrisk5yr>dr5yr_cp[3] & deathrisk5yr<=dr5yr_cp[4])
#nhisd5 <- subset(master,deathrisk5yr>dr5yr_cp[4])
#dr5yr_cp.5 <- dr5yr_cp
nhis1998 <- subset(master,year==1998)
nhis2000 <- subset(master,year==2000)
nhis2002 <- subset(master,year==2002)
nhis2004 <- subset(master,year==2004)
nhis2006 <- subset(master,year==2006)
nhis2008 <- subset(master,year==2008)
age1 <- subset(master,age<50)
age2 <- subset(master,age>=50 & age<60)
age3 <- subset(master,age>=60 & age<70)
age4 <- subset(master,age>=70 & age<80)
age5 <- subset(master,age>=80)
male <- subset(master,female==0)
female <- subset(master,female==1)
white <- subset(master,race==0)
black <- subset(master,race==1)
hisp <- subset(master,race==2)
other <- subset(master,race==3)
bmi1 <- subset(master,bmi<=18.5)
bmi2 <- subset(master,bmi>18.5 & bmi<=20)
bmi3 <- subset(master,bmi>20 & bmi<=25)
bmi4 <- subset(master,bmi>25 & bmi<=30)
bmi5 <- subset(master,bmi>30 & bmi<=35)
bmi6 <- subset(master,bmi>35)
current <- subset(master,current==1)
former <- subset(master,former==1)
cur.cpd1 <- subset(master,cpd<20 & current==1)
cur.cpd2 <- subset(master,cpd>=20 & cpd<30 & current==1)
cur.cpd3 <- subset(master,cpd>=30 & cpd<40 & current==1)
cur.cpd4 <- subset(master,cpd>=40 & current==1)
form.cpd1 <- subset(master,cpd<20 & former==1)
form.cpd2 <- subset(master,cpd>=20 & cpd<30 & former==1)
form.cpd3 <- subset(master,cpd>=30 & cpd<40 & former==1)
form.cpd4 <- subset(master,cpd>=40 & former==1)
nocomor <- subset(master,comorbidities==0)
comor1 <- subset(master,comorbidities==1)
comor2 <- subset(master,comorbidities==2)
comor3p <- subset(master,comorbidities>=3)
eo.5 <- cbind(drest(master),drest(uspstf),drest(notuspstf))
#eo.dr.5 <- cbind(drest(nhisd1),drest(nhisd2),drest(nhisd3),drest(nhisd4),drest(nhisd5))
eo.yr.5 <- cbind(drest(nhis1998),drest(nhis2000),drest(nhis2002),drest(nhis2004),drest(nhis2006),drest(nhis2008))
eo.age.5 <- cbind(drest(age1),drest(age2),drest(age3),drest(age4),drest(age5))
eo.gender.5 <- cbind(drest(male),drest(female))
eo.race.5 <- cbind(drest(white),drest(black),drest(hisp),drest(other))
eo.bmi.5 <- cbind(drest(bmi1),drest(bmi2),drest(bmi3),drest(bmi4),drest(bmi5),drest(bmi6))
eo.smkstat.5 <- cbind(drest(current),drest(former))
eo.cpd.5 <- cbind(drest(cur.cpd1),drest(cur.cpd2),drest(cur.cpd3),drest(cur.cpd4),
                  drest(form.cpd1),drest(form.cpd2),drest(form.cpd3),drest(form.cpd4))
eo.comor.5 <- cbind(drest(nocomor),drest(comor1),drest(comor2),drest(comor3p))

inf_eo <- function(eo1,eo2,eo3,eo4,eo5){
   ave_eo <- (eo1+eo2+eo3+eo4+eo5)/5  #average E/O
   vars.bar <- (eo1[4,]+eo2[4,]+eo3[4,]+eo4[4,]+eo5[4,])/5  #within imputation variance
   v.impute <- ((eo1[3,]-ave_eo[3,])^2+(eo2[3,]-ave_eo[3,])^2+(eo3[3,]-ave_eo[3,])^2+(eo4[3,]-ave_eo[3,])^2+(eo5[3,]-ave_eo[3,])^2)/4  #between-imputation variance
   var.ave_eo <- (vars.bar+(1+1/5)*v.impute)  #variance for average E/O
   logv <- var.ave_eo/ave_eo[3,]^2  #variance of log of average E/O
   #confidence intervals for average E/O
   lower <- exp(log(ave_eo[3,])-1.96*sqrt(logv))
   upper <- exp(log(ave_eo[3,])+1.96*sqrt(logv))
   ave_eo <- rbind(ave_eo[1:3,],var.ave_eo,lower,upper)
   rownames(ave_eo) <- c("obs","exp","E/O","V(E/O)","LCL:E/O","UCL:E/O")
   return(ave_eo)
}

ave.eo <- inf_eo(eo.1,eo.2,eo.3,eo.4,eo.5)
#eo.dr <- inf_eo(eo.dr.1,eo.dr.2,eo.dr.3,eo.dr.4,eo.dr.5)
ave.yr.eo <- inf_eo(eo.yr.1,eo.yr.2,eo.yr.3,eo.yr.4,eo.yr.5)
ave.age.eo <- inf_eo(eo.age.1,eo.age.2,eo.age.3,eo.age.4,eo.age.5)
ave.gender.eo <- inf_eo(eo.gender.1,eo.gender.2,eo.gender.3,eo.gender.4,eo.gender.5)
ave.race.eo <- inf_eo(eo.race.1,eo.race.2,eo.race.3,eo.race.4,eo.race.5)
ave.bmi.eo <- inf_eo(eo.bmi.1,eo.bmi.2,eo.bmi.3,eo.bmi.4,eo.bmi.5)
ave.smkstat.eo <- inf_eo(eo.smkstat.1,eo.smkstat.2,eo.smkstat.3,eo.smkstat.4,eo.smkstat.5)
ave.cpd.eo <- inf_eo(eo.cpd.1,eo.cpd.2,eo.cpd.3,eo.cpd.4,eo.cpd.5)
ave.comor.eo <- inf_eo(eo.comor.1,eo.comor.2,eo.comor.3,eo.comor.4,eo.comor.5)

#100*(dr5yr_cp.1+dr5yr_cp.2+dr5yr_cp.3+dr5yr_cp.4+dr5yr_cp.5)/5

#eo_res <- rbind(t(ave.eo),t(eo.dr),t(ave.yr.eo),t(ave.age.eo),t(ave.gender.eo),t(ave.race.eo),t(ave.bmi.eo),t(ave.smkstat.eo),t(ave.cpd.eo),t(ave.comor.eo))
eo_res <- rbind(t(ave.eo),t(ave.yr.eo),t(ave.age.eo),t(ave.gender.eo),t(ave.race.eo),t(ave.bmi.eo),t(ave.smkstat.eo),t(ave.cpd.eo),t(ave.comor.eo))
eo_res[,1] <- eo_res[,1]/1000000  #per million
eo_res[,2] <- eo_res[,2]/1000000  #per million
rownames(eo_res) <- c("Overall","USPSTF-eligible","USPSTF-ineligible",
                      "NHIS 1998","NHIS 2000","NHIS 2002","NHIS 2004","NHIS 2006","NHIS 2008",
                      "Aged <50","Aged 50-59","Aged 60-69","Aged 70-79","Aged 80-84",
                      "Male","Female",
                      "White","Black","Hispanic","Other",
                      "BMI <=18.5","BMI 18.5-20","BMI 20<-25","BMI 25<-30","BMI 30<-35","BMI >35",
                      "Current Smoker","Former Smoker","Current <20 cpd","Current 20-<30","Current 30-<40","Current 40+",
                      "Former <20 cpd","Former 20-<30","Former 30-<40","Former 40+",
                      "No comorbidities","1 comorbidities","2 comorbidities","3+ comorbidities")

#rownames(eo_res) <- c("Overall","USPSTF-eligible","USPSTF-ineligible",
#                      "Q1","Q2","Q3","Q4","Q5",
#                      "NHIS 1998","NHIS 2000","NHIS 2002","NHIS 2004","NHIS 2006","NHIS 2008",
#                      "Aged <50","Aged 50-59","Aged 60-69","Aged 70-79","Aged 80-84",
#                      "Male","Female",
#                      "White","Black","Hispanic","Other",
#                      "BMI <=18.5","BMI 18.5-20","BMI 20<-25","BMI 25<-30","BMI 30<-35","BMI >35",
#                      "Current Smoker","Former Smoker","Current <20 cpd","Current 20-<30","Current 30-<40","Current 40+",
#                      "Former <20 cpd","Former 20-<30","Former 30-<40","Former 40+",
#                      "No comorbidities","1 comorbidities","2 comorbidities","3+ comorbidities")

write.table(eo_res,"~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/mortality_model6_validation2.csv",sep=",")