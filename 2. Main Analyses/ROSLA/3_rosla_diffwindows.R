#ROSLA different windows .
rm(list=ls()) 
library(fixest)

# load dataset for ROSLA analyses
complete = read.csv("", header=T)

complete$reform <- 0
complete$reform[complete$yob > 1957] <- 1
complete$reform[complete$yob == 1957 & complete$mob > 8] <- 1

complete = subset(complete, yob > 1951 & yob < 1963)
complete$delete <- 0
complete$delete[complete$yob == 1952 & complete$mob <3] <- 1
complete$delete[complete$yob == 1962 & complete$mob >2] <- 1
complete = subset(complete, delete == 0)


#10 years

rosla10 = complete
rosla10 = subset(rosla10, !is.na(education))

#make binary education indicator 
rosla10$edu <- 0 
rosla10$edu[rosla10$education > 15] <- 1

vars <- list()
vars$covariates <- c() # list all covariates
covariates = paste(vars$covariates, collapse = " + ")

# outcome variables
outcomes <- list()
outcomes$variables <- c() #outcomes
outcomes$controls <- c()  #controls


analyse_rosla10 <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste(" + summer + autumn + winter + yob | edu ~ reform")))
  
  output <- feols(f, data = rosla10 )
  assign(varname,output, envir=.GlobalEnv)
} 


main_outcomes10 <- lapply(outcomes$variables, analyse_rosla10)
control_outcomes10  <- lapply(outcomes$controls, analyse_rosla10)

### 2 year

rosla2 = subset(rosla10, yob > 1955 & yob < 1959)
rosla2$delete <- 0
rosla2$delete[rosla2$yob == 1956 & rosla2$mob < 9] <- 1
rosla2$delete[rosla2$yob == 1958 & rosla2$mob > 8] <- 1
rosla2 = subset(rosla2, delete == 0)


analyse_rosla2 <- function(varname){ 
  
  f = as.formula(paste({{varname}},paste(" ~"), paste(covariates), paste(" +  autumn + summer + winter + yob | edu ~ reform")))
  
  output <- feols(f, data = rosla2 )
  assign(varname,output, envir=.GlobalEnv)
} 


main_outcomes2 <- lapply(outcomes$variables, analyse_rosla2)
control_outcomes2  <- lapply(outcomes$controls, analyse_rosla2)


                      
