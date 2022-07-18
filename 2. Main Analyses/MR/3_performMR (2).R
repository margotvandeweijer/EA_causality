#Mendelian Randomization analyses
#last edit 09-02-22
#script based on perline's sibling_analysis script.
require(fixest)

#PRS
PRS = read.table("/home/margotw/EA_WB/MR/OUTPUT_PRS/EA_PRS.profile", header=T)
PRS = subset(PRS, FID >0)
colnames(PRS)[2] = "eid"


#data without related individuals (standardized outcomes)
data = read.csv("/home/margotw/EA_WB/data/full_data/complete.csv", header=T) 

full = merge(PRS, data, by="eid")

full$Zeducation = scale(full$education)
full$Zprs = scale(full$SCORE)


vars <- list()
vars$covariates <- c("sex1","batch","familysize","center_11001","center_11002","center_11003","center_11004","center_11005","center_11006",
                      "center_11007","center_11008","center_11009","center_11010","center_11011","center_11012","center_11013","center_11014",
                      "center_11016","center_11017", "center_11018","center_11020","center_11021","center_11022","center_11023",
                        "PC1" , "PC2" , "PC3" , "PC4" , "PC5" , "PC6" , "PC7" , "PC8" , "PC9" , "PC10" , "yob" , "summer", "autumn" , "winter","yob") # list all covariates
covariates = paste(vars$covariates, collapse = " + ")

outcomes <- list()
outcomes$variables <- c("happiness","healthhap","friendsat","finansat","famsat","worksat","meaning","Neuroticism","depression","anxiety","bipolar","cardiovascular") #outcomes
outcomes$controls <- c("Income18k","Income31k","Income52k","Income100k","height","birthweight","height10","weight10")  #controls

#function to perform the MR analyses

analyse_mr <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste("| Zeducation ~ Zprs")))
  
  output <- feols(f, data = full )
  assign(varname,output, envir=.GlobalEnv)
} 

main_mr <- lapply(outcomes$variables, analyse_mr)
control_mr  <- lapply(outcomes$controls, analyse_mr)

#also perform these analyses with normal ols

#we do not need the genomic PCs in this case
vars$covariates2 <- c("sex1", "familysize","center_11001","center_11002","center_11003","center_11004","center_11005","center_11006",
                     "center_11007","center_11008","center_11009","center_11010","center_11011","center_11012","center_11013","center_11014",
                     "center_11016","center_11017", "center_11018","center_11020","center_11021","center_11022","center_11023", 
                     "yob" , "summer", "autumn" , "winter","yob") # list all covariates
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
