#MR within sib-ships
#run analyses
#Margot 23-3-22
library(fixest)

#read data
sib_data = read.csv("/home/margotw/EA_WB/MR_sibships/sibling_data.csv")

vars <- list()
#note that the covariate list only includes yob here, and not age - might need to adjust later
vars$covariates <- c("sex1","batch","familysize","center_11001","center_11002","center_11003","center_11004","center_11005","center_11006",
                     "center_11007","center_11008","center_11009","center_11010","center_11011","center_11012","center_11013","center_11014",
                     "center_11016","center_11017", "center_11018","center_11020","center_11021","center_11022","center_11023",
                     "PC1" , "PC2" , "PC3" , "PC4" , "PC5" , "PC6" , "PC7" , "PC8" , "PC9" , "PC10" , "birth_order" , "summer", "autumn" , "winter","yob") # list all covariates
covariates = paste(vars$covariates, collapse = " + ")

outcomes <- list()
outcomes$variables <- c("happiness","healthhap","friendsat","finansat","famsat","worksat","meaning","Neuroticism","depression","anxiety","bipolar","cardiovascular") #outcomes
outcomes$controls <- c("Income18k","Income31k","Income52k","Income100k","height","birthweight","height10","weight10")  #controls

#function
analyse_mrsibs <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste("| education_dev ~ prs_dev")))
  
  output <- feols(f, data = sib_data )
  assign(varname,output, envir=.GlobalEnv)
} 

main_mrsibs <- lapply(outcomes$variables, analyse_mrsibs)
control_mrsibs  <- lapply(outcomes$controls, analyse_mrsibs)

#of denk ik nu te simpel