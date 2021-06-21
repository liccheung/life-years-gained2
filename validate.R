library(survival)
library(survey)

drest <- function(x){
  c <- survfit(morat,newdata=z[x,],start.time=z$age[x])
  yr_flwup <- 2012-z$intyear[x]  #death data until end of 2011
  if (max(c$surv[c$time>=(z$age[x]+yr_flwup)])==min(c$surv[c$time<=(z$age[x]+yr_flwup)])){
    death_risk <- 1-c$surv[c$time==(z$age[x]+yr_flwup)]
  } else {
    m <- (max(c$surv[c$time>=(z$age[x]+yr_flwup)])-min(c$surv[c$time<=(z$age[x]+yr_flwup)]))/(max(c$time[c$time>=(z$age[x]+yr_flwup)])-min(c$time[c$time<=(z$age[x]+yr_flwup)]))
    b <- max(c$surv[c$time>=(z$age[x]+yr_flwup)])-m*max(c$time[c$time>=(z$age[x]+yr_flwup)])
    death_risk <- 1-(m*(z$age[x]+yr_flwup)+b)
  }
  return(death_risk)
}

y$birthyear <- y$year-y$age
notanaly <- subset(y,analypop==0|age<40|age>80)
z <- subset(y,analypop==1&age>=40&age<=80)  #impute variables for analysis population
z$death_risk <- sapply(1:nrow(z),drest)
notanaly$death_risk <- NA
y <- rbind(z,notanaly)