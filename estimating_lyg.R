#read in data set
#load mortality model as morat

#create data frame covar
covar <- data.frame(age=age,
                    bmi=bmi,
                    cpd=cpd,
                    emp=emp,
                    fam.lung.trend=fam.lung.trend,
                    female=female,
                    qtyears=qtyears,
                    smkyears=smkyears,
                    race=race,
                    edu6=edu6,
                    pkyr.cat=pkyr.cat,
                    prior.cancer=prior.cancer,
                    hypertension = hypertension,
                    chd = chd,
                    angina = angina,
                    heartattack = heartattack,
                    heartdisease = heartdisease,
                    stroke = stroke,
                    diab = diab,
                    bron = bron,
                    kidney = kidney,
                    liver = liver,
                    speceq = speceq,
                    year = year)

#use lcrisk model on covar to get 1-6 year lung cancer death risk - save as LCDRAT1-6

haz <- basehaz(morat)
rs <- predict(morat, covar, type = "risk",reference="sample")
z <- covar[lgindex,]
lyg <- function(x){
  print(x)
  survx <- (exp(-1*haz[which(haz$time>age[x]),1])/exp(-1*haz[which(haz$time==age[x]),1]))^rs[x]
  ages <- haz[which(haz$time>age[x]),2]
  ely <- sum(diff(c(z$age[x],ages))*survx)              
  #assume reduction to lung cancer mortality is for 6 years
  condsurvx <- (exp(-1*haz[which(haz$time>age[x]+6),1])/exp(-1*haz[which(haz$time==age[x]+6),1]))^rs[x]
  condages <- haz[which(haz$time>age[x]+6),2]           
  condely <- sum(diff(c(z$age[x]+6,condages))*condsurvx)
  lyg <- .204*z$LCDRAT1[x] + .204*z$LCDRAT2[x] + 
      .204*z$LCDRAT3[x] + .204*z$LCDRAT4[x] +
      .204*z$LCDRAT5[x] + .204*z$LCDRAT6[x]*(1+condely)
  res <- c(ely,ely+lyg,365.25*lyg)
  return(res)
} 
lyg_res <- sapply(1:nrow(z),lyg)
lyg_res <- t(lyg_res)
covar$ely[lgindex] <- lyg_res[,1]
covar$ely_ct[lgindex] <- lyg_res[,2]
covar$lg[lgindex] <- lyg_res[,3]