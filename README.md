# life-years-gained2
Work on an updated version of the life-years-gained model that avoids model-propagating disparities

Step 1: Data processing
process NHIS data sets (sample adult, persons, and mortality for 1997-2018?) - samadult....R
impute missing variables - imputation_batch....R and nhis_imputation_function....R
create training (odd years from 1997-2014), validation (even years from 1997-2014), and projection (2015+) data sets
#Create table describing data sets

Step 2: Fit mortality model to odd years
explore mortality model (find best model by AIC) - allmodels.R
create a program to fit and save the best model
#Create table describing model

Step 3: Validate mortality model on even years
Model Calibration (expected versus observed) - validate....R
Model Discimination (AUC) - auc....R programs
#Create tables describing model validation

Step 4: Projections on 2015+ data
Calculate life-expectancy in the presence and absence of screening and years of life-gained - estimating_lyg.R

Step 5: Explore counterfactual
Examples: non-race and/or non-education models
Examples: counterfactual if the individual was a different race/ethnicity

