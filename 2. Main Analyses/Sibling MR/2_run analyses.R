#MR within sib-ships
#run analyses
library(fixest)

#read data
sib_data = read.csv("sibling_data.csv")

vars <- list()
vars$covariates <- c() # list all covariates
covariates = paste(vars$covariates, collapse = " + ")

outcomes <- list()
outcomes$variables <- c() #outcomes
outcomes$controls <- c()  #controls

#function
analyse_mrsibs <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste("| education_dev ~ prs_dev")))
  
  output <- feols(f, data = sib_data )
  assign(varname,output, envir=.GlobalEnv)
} 

main_mrsibs <- lapply(outcomes$variables, analyse_mrsibs)
control_mrsibs  <- lapply(outcomes$controls, analyse_mrsibs)
