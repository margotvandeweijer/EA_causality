#Mendelian Randomization analyses
#last edit 09-02-22
require(fixest)

#PRS
PRS = read.table("", header=T)
PRS = subset(PRS, FID >0)
colnames(PRS)[2] = "eid"


#data without related individuals (standardized outcomes)
data = read.csv("", header=T) 

full = merge(PRS, data, by="eid")

full$Zeducation = scale(full$education)
full$Zprs = scale(full$SCORE)


vars <- list()
vars$covariates <- c() # list all covariates
covariates = paste(vars$covariates, collapse = " + ")

outcomes <- list()
outcomes$variables <- c() #outcomes
outcomes$controls <- c()  #controls

#function to perform the MR analyses

analyse_mr <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste("| Zeducation ~ Zprs")))
  
  output <- feols(f, data = full )
  assign(varname,output, envir=.GlobalEnv)
} 

main_mr <- lapply(outcomes$variables, analyse_mr)
control_mr  <- lapply(outcomes$controls, analyse_mr)

#also performed these analyses within normal ols

#we do not need the genomic PCs in this case
vars$covariates2 <- c() # list all covariates
covariates2 = paste(vars$covariates2, collapse = " + ")

#normal reg for outcomes (edu continuous)
for (i in outcomes$variables){ 
  model <- lm(paste(i[[1]], "~ Zeducation +",paste(covariates2)), data=full) 
  print(i[[1]])
  print(summary(model)) 
} 

#normal reg for control
for (i in outcomes$controls){ 
  model <- lm(paste(i[[1]], "~ Zeducation +",paste(covariates2)), data=full) 
  print(i[[1]])
  print(summary(model)) 
} 
