#ROSLA analyses
# analyses 
rm(list=ls()) 
library(fixest)

# load dataset for ROSLA analyses
setwd("")
rosla <- read.csv("rosla.csv", header=T)

#make binary education indicator 
rosla$edu <- 0 
rosla$edu[rosla$education > 15] <- 1

# covariates to include in the analyses
#there is a standard list that will be included and a list that we need to examine (with varying age variables depending on the outcome)
vars <- list()
vars$covariates1 <- c() # list all covariates
covariates1 = paste(vars$covariates1, collapse = " + ")

# outcome variables
outcomes <- list()
outcomes$variables <- c() #outcomes
outcomes$controls <- c()  #controls

#function to perform the analyses
analyse_rosla <- function(varname){ 

f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste("| edu ~ reform")))

output <- feols(f, data = rosla )
assign(varname,output, envir=.GlobalEnv)
} 

#run without only standard covariates

main_outcomes <- lapply(outcomes$variables, analyse_rosla)
control_outcomes  <- lapply(outcomes$controls, analyse_rosla)

#add birth season
analyse_rosla2 <- function(varname){ 

f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste(" + summer + autumn + winter | edu ~ reform")))

output <- feols(f, data = rosla )
assign(varname,output, envir=.GlobalEnv)
} 

main_outcomes2 <- lapply(outcomes$variables, analyse_rosla2)
control_outcomes2  <- lapply(outcomes$controls, analyse_rosla2)

#add age
#the age variable is different for meaning, happiness, and happiness with own health. 

analyse_rosla3 <- function(varname){ 

f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste(" + summer + autumn + winter + age | edu ~ reform")))

output <- feols(f, data = rosla )
assign(varname,output, envir=.GlobalEnv)
} 

outcomes$variables1 <- c() 

main_outcomes3 <- lapply(outcomes$variables1, analyse_rosla3)
control_outcomes3  <- lapply(outcomes$controls, analyse_rosla3)

#meaning in life
meaning = feols(meaning ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_meaning | edu ~ reform, data=rosla)
 
#happiness
happiness = feols(happiness ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_happiness | edu ~ reform, data=rosla)

#happiness with own health
haphealth = feols(healthhap ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_haphealth | edu ~ reform, data=rosla)


#add year of birth (continuous)
analyse_rosla4 <- function(varname){ 

f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste(" + summer + autumn + winter + age + yob | edu ~ reform")))

output <- feols(f, data = rosla )
assign(varname,output, envir=.GlobalEnv)
} 

main_outcomes4 <- lapply(outcomes$variables1, analyse_rosla4)
control_outcomes4  <- lapply(outcomes$controls, analyse_rosla4)


#meaning in life
meaning2 = feols(meaning ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_meaning + yob | edu ~ reform, data=rosla)
 
#happiness
happiness2 = feols(happiness ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_happiness + yob | edu ~ reform, data=rosla)

#happiness with own health
haphealth2 = feols(healthhap ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_haphealth + yob| edu ~ reform, data=rosla)
 
 
#yob as dummies instead of yob continuous
analyse_rosla5 <- function(varname){ 

f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste(" + summer + autumn + winter + age + I(as.factor(yob)) | edu ~ reform")))

output <- feols(f, data = rosla )
assign(varname,output, envir=.GlobalEnv)
} 
 
main_outcomes5 <- lapply(outcomes$variables1, analyse_rosla5)
control_outcomes5  <- lapply(outcomes$controls, analyse_rosla5)
 
#meaning in life
meaning5 = feols(meaning ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_meaning + I(as.factor(yob)) | edu ~ reform, data=rosla)
 
#happiness
happiness5 = feols(happiness ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_happiness + I(as.factor(yob)) | edu ~ reform, data=rosla)

#happiness with own health
haphealth5 = feols(healthhap ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_haphealth + I(as.factor(yob))| edu ~ reform, data=rosla)


#year of birth and season, but not age (continuous)
analyse_rosla6 <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates1), paste(" + summer + autumn + winter + yob | edu ~ reform")))
  
  output <- feols(f, data = rosla )
  assign(varname,output, envir=.GlobalEnv)
} 

main_outcomes6 <- lapply(outcomes$variables1, analyse_rosla6)
control_outcomes6  <- lapply(outcomes$controls, analyse_rosla6)
  
#meaning in life
meaning6 = feols(meaning ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
                   summer + autumn + winter +yob | edu ~ reform, data=rosla)

#happiness
happiness6 = feols(happiness ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
                     summer + autumn + winter + yob | edu ~ reform, data=rosla)

#happiness with own health
haphealth6 = feols(healthhap ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
                     summer + autumn + winter + yob | edu ~ reform, data=rosla)

#normal reg incl yob and age (edu binary)
for (i in outcomes$variables1){ 
  model <- lm(paste(i[[1]], "~ edu + yob + age +",paste(covariates1)), data=rosla) 
  print(i[[1]])
  print(summary(model)) 
} 

meaning_ols = lm(meaning ~ edu + sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_meaning + yob, data=rosla)
 
happiness_ols = lm(happiness ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_happiness + yob + edu, data=rosla)


haphealth_ols = lm(healthhap ~ sex1 + familysize + center_11001 + center_11002 + center_11003 + center_11004 + center_11005 + center_11006 + center_11007 + center_11008 + center_11009 + center_11010 + center_11011 + center_11012 + center_11013 + center_11014 + center_11016 + center_11017 + center_11018 + center_11020 + center_11021 + center_11022 + center_11023 + 
 summer + autumn + winter + age_haphealth + yob + edu, data=rosla)

 
 
for (i in outcomes$controls){ 
  model <- lm(paste(i[[1]], "~ edu + yob +",paste(covariates1)), data=rosla) 
  print(i[[1]])
  print(summary(model)) 
}

#normal reg incl yob (edu continuous)
for (i in outcomes$variables){ 
  model <- lm(paste(i[[1]], "~ education + yob +",paste(covariates1)), data=rosla) 
  print(i[[1]])
  print(summary(model)) 
} 

