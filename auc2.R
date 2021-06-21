setwd("/home/cheunglc/lyg/")

foo <- function(x){
  if (is.numeric(nhis$death_risk[[x]])==1){
    return(nhis$death_risk[[x]])
  } else {
    return(as.numeric(NA))
  }
}

popAUC <- function(data){
  
  auc <- function(case.risk, control.risk){
    
    M <- length(case.risk)
    N <- length(control.risk)
    
    CONTROL.MATRIX <- matrix(rep(control.risk, M), byrow=T, ncol=N)
    CASE.MATRIX <- matrix(rep(case.risk, each=N), byrow=T, ncol=N)
    
    sum(CASE.MATRIX>=CONTROL.MATRIX)/(M*N)
  }
  
  auc.list <- function(case, control){
    sapply(case, function(thecase){
      sapply(control, function(thecontrol) auc(thecase$death_risk, thecontrol$death_risk))
    })
  }
  
  control  <- split(data, data$died)
  case <- control[["1"]]
  control <- control[["0"]]
  
  case <- split(case, case$strata)
  control <- split(control, control$strata)
  
  # WEIGHT MATRIX
  p <- sapply(case, function(x) sum(x$adj.wt_mort))
  p <- p/sum(p)
  
  q <- sapply(control, function(x) sum(x$adj.wt_mort))
  q <- q/sum(q)
  
  aucs <- auc.list(case, control)
  
  as.numeric(q%*%aucs%*%p)
}

load(file="nhis1997_09_even_mod2.RData")

nhis$comorbidities <- nhis$emp+nhis$hypertension+nhis$chd+nhis$angina+nhis$heartattack+
  nhis$heartdisease+nhis$stroke+nhis$diab+nhis$bron+nhis$kidney+
  nhis$liver+nhis$prior.cancer+nhis$speceq
nhis$death_risk <- as.numeric(sapply(1:nrow(nhis),foo))
nhis$uspstf.eligible <- ifelse(nhis$age>=55 & nhis$age <= 80 &
                                 (nhis$current==1 | (nhis$former==1&nhis$qtyears<=15)) &
                                 nhis$pkyr.cat>=30,1,0)

nhis <- nhis[,c("uspstf.eligible","death_risk","adj.wt_mort","died","strata","age","analypop")]

smokers.4080 <- nhis[nhis$analypop==1 & nhis$age>=40 & nhis$age <= 80 & !is.na(nhis$death_risk),]
smokers4080 <- popAUC(smokers.4080)

# JACKKNIFE ESTIMATES
strata.values <- unique(smokers.4080$strata)
K <- length(strata.values)
jackknife.aucs <- sapply(strata.values, function(grp) popAUC(smokers.4080[smokers.4080$strata!=grp,]))
smokers4080.var <- sum((jackknife.aucs-smokers4080)^2)*(K-1)/K

nhis.eligible <- nhis[nhis$analypop==1 & nhis$uspstf.eligible==1 & !is.na(nhis$death_risk),]
eligible <- popAUC(nhis.eligible)

# JACKKNIFE ESTIMATES  
strata.values <- unique(nhis.eligible$strata)
K <- length(strata.values)
jackknife.aucs <- sapply(strata.values, function(grp) popAUC(nhis.eligible[nhis.eligible$strata!=grp,]))
eligible.var <- sum((jackknife.aucs-eligible)^2)*(K-1)/K


nhis.ineligible4080 <- nhis[nhis$analypop==1 & !nhis$uspstf.eligible & nhis$age>=40 & nhis$age<=80 & !is.na(nhis$death_risk),]
ineligible4080 <- popAUC(nhis.ineligible4080)

# JACKKNIFE ESTIMATES
strata.values <- unique(nhis.ineligible4080$strata)
K <- length(strata.values)
jackknife.aucs <- sapply(strata.values, function(grp) popAUC(nhis.ineligible4080[nhis.ineligible4080$strata!=grp,]))
ineligible4080.var <- sum((jackknife.aucs-ineligible4080)^2)*(K-1)/K

save(smokers4080, smokers4080.var, eligible, eligible.var, ineligible4080, ineligible4080.var, file= "auc2.RData")


