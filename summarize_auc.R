# Summary
rm(list=ls(all=TRUE))
setwd("~/Desktop/Lung cancer/lrisk/other/lifeyearsgained/v6")

inf_auc <- function(auc1,auc2,auc3,auc4,auc5){
  ave_auc <- (auc1+auc2+auc3+auc4+auc5)/5  #average auc
  vars.bar <-  ave_auc[2]  #within imputation variance
  v.impute <- ((auc1[1]-ave_auc[1])^2+(auc2[1]-ave_auc[1])^2+(auc3[1]-ave_auc[1])^2+(auc4[1]-ave_auc[1])^2+(auc5[1]-ave_auc[1])^2)/4  #between-imputation variance
  var.ave_auc <- (vars.bar+(1+1/5)*v.impute)  #variance for average auc
  #confidence intervals for average E/O
  lower <- ave_auc[1]-1.96*sqrt(var.ave_auc)
  upper <- ave_auc[1]+1.96*sqrt(var.ave_auc)
  if (ave_auc[1]/sqrt(var.ave_auc)<0) {
    ave_auc <- rbind(ave_auc[1],lower,upper,2*pnorm(ave_auc[1]/sqrt(var.ave_auc)))
  } else if (ave_auc[1]/sqrt(var.ave_auc)>0) {
    ave_auc <- rbind(ave_auc[1],lower,upper,2*(1-pnorm(ave_auc[1]/sqrt(var.ave_auc))))
  }   
  rownames(ave_auc) <- c("mean","lower","upper","2-sided pvalue")
  return(ave_auc)
}

load(file= "auc21.RData")
all1 <- c(smokers4084,smokers4084.var) 
elig1 <- c(eligible,eligible.var)
inelig1 <- c(ineligible4084,ineligible4084.var)

load(file= "auc22.RData")
all2 <- c(smokers4084,smokers4084.var) 
elig2 <- c(eligible,eligible.var)
inelig2 <- c(ineligible4084,ineligible4084.var)

load(file= "auc23.RData")
all3 <- c(smokers4084,smokers4084.var) 
elig3 <- c(eligible,eligible.var)
inelig3 <- c(ineligible4084,ineligible4084.var)

load(file= "auc24.RData")
all4 <- c(smokers4084,smokers4084.var) 
elig4 <- c(eligible,eligible.var)
inelig4 <- c(ineligible4084,ineligible4084.var)

load(file= "auc25.RData")
all5 <- c(smokers4084,smokers4084.var) 
elig5 <- c(eligible,eligible.var)
inelig5 <- c(ineligible4084,ineligible4084.var)

inf_auc(all1,all2,all3,all4,all5)
inf_auc(elig1,elig2,elig3,elig4,elig5)
inf_auc(inelig1,inelig2,inelig3,inelig4,inelig5)