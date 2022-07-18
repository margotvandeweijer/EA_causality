# Sibling control analysis 
# Start date: 20/01/2022

# Set up #
library(tidyverse)
library(data.table)
library(nlme)
set.seed(42)

#1. Read data ##############
# Phenotype created by Margot, sent on the 12/01/2022
# Note from Margot to Perline; this file needs to be updated but I can also just run the analyses from my computer
pheno <- fread("../../../Data/full_standardized.csv")
head(pheno)

# get FID for siblings from previous project Demange et al 2022 (https://github.com/PerlineDemange/GeneticNurtureNonCog/tree/master/UKB/EA%20GWAS), based on all UKB: 
siblings <- fread("../../../Data/sibs_famid3",h=F, data.table=F, verbose=T)
colnames(siblings)[1:2]<-c("eid","FID") # 41498
head(siblings)

# get IIDS of adopted 
adoptees <- fread("../../../Data/adopted.IDs") 
head(adoptees)
colnames(adoptees)[1:2]<-c("eid","ID1")

#2. Select sibling subset ###########
#Remove siblings that are adoptees 
data_sib <- siblings[!(siblings$eid %in% adoptees$eid),] #41426

# Merge with phenotype 
data_sib <- merge(data_sib, pheno, by= "eid") #33107 
# CHECK WITH MARGOT IF SAMPLE LOSS IS EXPECTED ######

# There is NAs in the education data
summary(data_sib$education)
data_sib <- data_sib[!(is.na(data_sib$education)),] #32712

# keep only if at least 2 siblings with same FID 
number <- as.data.frame(table(data_sib$FID))
onemember <- number[which(number$Freq == 1),] #identify family with only one member in this data
data_sib <- data_sib[!(data_sib$FID %in% onemember$Var1), ] #31337
data_sib <- data_sib[order(data_sib$FID),]
head(data_sib)

#number of sibships
length(unique(data_sib$FID)) #15237

#3. Identify birth order ######
summary(data_sib$education)

data_sib$birthday <- paste0(data_sib$yob, data_sib$mob)

data_sib <- data_sib %>%
  group_by(FID) %>%
  mutate(birth_order = dense_rank(birthday))

data_sib <- as.data.frame(data_sib)
hist(data_sib$birth_order)

# Identify if they are twins 

data_sib <- data_sib %>% 
  group_by(FID) %>% 
  mutate(multiple = ifelse(duplicated(birthday)|
           duplicated(birthday, fromLast = T),
         T, F))

data_sib <- as.data.frame(data_sib)
head(data_sib)
summary(data_sib$multiple) #439 multiple

data_sib[data_sib$multiple==T, ] #check twin have same birth order 
data_sib[data_sib$FID == 1055432, ] #check twins with birth roder 2 # they do have older sib


#get number of twins to investigate potential problems
summary <- data_sib %>%
  group_by(FID) %>%
  tally(multiple ==T)
table(summary$n)
#218 twins pairs, 1 triplet

save(data_sib, file="data_sib.Rda")

#4. Get within and between family estimates for education #### 
#Between-family estimate = family average
mean_educ<- group_by(data_sib,FID) %>% summarize(m=mean(education))
colnames(mean_educ) <- c("FID", "education_mean")
data_sib<- merge(data_sib,mean_educ,by="FID")

# Within-family estimates = deviation from family average
data_sib$education_dev <- data_sib$education  - data_sib$education_mean  

#4.  ICC of education #####
# functions written by Saskia Selzam, from Selzam et al. 2019
# calculate intraclass correlations
# i.e.  The ICC is the ratio of the between-family (i.e., random intercept) variance over the total variance 
# and is an estimate of how much of the total variation in the outcome is accounted for by family
ICCest <- function(model) {
  icc <- sqrt(diag(getVarCov(model)))^2 / (sqrt(diag(getVarCov(model)))^2 + model$sigma^2 )
  as.vector(icc)
}

# intercept model
m0 <- lme(education~1, 
          random=~1|FID,
          method="ML", 
          na.action=na.omit,
          data=data_sib)
ICCest(m0) #0.40


#4. Sibling analysis ##############
# Create the sibship average education and deviation from average
# separate continuous and binary outcomes 
# linear model for continuous and LPM for binary (so lm for both)

outcomes <- list()
outcomes$variables_cont <- c("happiness","healthhap",
                        "friendsat","finansat","famsat","worksat",
                        "meaning",
                        "Neuroticism")
outcomes$variables_bin <- c("depression","anxiety","bipolar","cardiovascular")
outcomes$variables <- c(outcomes$variables_cont, outcomes$variables_bin)
outcomes$controls <- c("Income18k","Income31k","Income52k","Income100k","height","birthweight","height10","weight10")  #controls

summary(data_sib[outcomes$variables_cont])
summary(data_sib[outcomes$variables_bin])
summary(data_sib[outcomes$controls])
  
vars <- list()
vars$covariates <- c("sex1","yob","familysize", "birth_order",
                     "summer","autumn","winter",
                     "center_11001","center_11002","center_11003","center_11004", "center_11005",
                     "center_11006","center_11007","center_11008","center_11009",
                     "center_11010","center_11011","center_11012","center_11013",
                     "center_11014","center_11016","center_11017","center_11018",
                     "center_11020","center_11021","center_11022","center_11023") #leave out center10003 and spring from covariates

summary(data_sib[vars$covariates])

covariates = paste(vars$covariates, collapse = " + ")

# Analysis for the variables #run the same but replace outcomes$variables by outcomes$controls to get controls 
for (i in outcomes$controls){ 
  # Run analysis and create output variable for each outcome
  assign(paste0("model_",i[[1]]), 
         lm(paste(i[[1]], "~ education_dev + education_mean + ", 
                  paste(covariates)), data=data_sib))
} 

#Create table with all results
full_results <- NULL
for (i in outcomes$controls){ 
  results <- get(paste0("model_",i[[1]]))
  results <- as.data.frame(summary(results)$coefficients)
  results$estimates <- rownames(results)
  rownames(results) <- NULL
  results$outcome <- i[[1]]
  full_results <- rbind(full_results, results)
}
full_results <- full_results[colnames(full_results)[c(6,5,1:4)]]
#write.table(full_results, file="wellbeingEA__siblings_full_results_variables_20220311.csv", 
#           sep = ",", quote = FALSE, row.names = F)
write.table(full_results, file="wellbeingEA__siblings_full_results_controls_20220311.csv", 
           sep = ",", quote = FALSE, row.names = F)

# only education results 
educ_result <- full_results[full_results$estimates == "education_dev" |
                              full_results$estimates == "education_mean",]
educ_result
#write.table(educ_result, file="wellbeingEA__siblings_educ_results_variables_20220311.csv", 
#            sep = ",", quote = FALSE, row.names = F)
write.table(educ_results, file="wellbeingEA__siblings_educ_results_controls_20220311.csv", 
           sep = ",", quote = FALSE, row.names = F)


#Get sample size (NA are excluded)
table_sample <- NULL
for (i in outcomes$variables){
  results <- get(paste0("model_", i[[1]]))
  sample <- as.data.frame(cbind(i[[1]], results$df.residual + 34))
  table_sample <- rbind(table_sample, sample)
}
colnames(table_sample) <- c("Outcome", " Effective sample size")
table_sample
#write.table(table_sample, file="wellbeingEA__siblings_table_sample_variables_20220311.csv", 
#          sep = ",", quote = FALSE, row.names = F)
#write.table(table_sample, file="wellbeingEA__siblings_table_sample_controls_20220311.csv", 
#           sep = ",", quote = FALSE, row.names = F)

# Plot results 
educ_results <- educ_result %>% 
  rename(
    value = Estimate, 
    SE = `Std. Error`, 
    t.value = `t value`, 
    p.value = `Pr(>|t|)` 
  )

ggplot(educ_results, aes(x=outcome, y=value, fill=estimates))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))



results <- fread("wellbeingEA__siblings_full_results_variables_20220201.csv")
results <- results %>% 
  rename(
    value = Estimate, 
    SE = `Std. Error`, 
    t.value = `t value`, 
    p.value = `Pr(>|t|)` 
  )

ggplot(results[results$estimates == "sex1",], aes(x=outcome, y=value))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))+
  ggtitle("Sex")

ggplot(results[results$estimates == "yob",], aes(x=outcome, y=value))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))+
  ggtitle("Year of birth")
ggplot(results[results$estimates == "familysize",], aes(x=outcome, y=value))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))+
  ggtitle("Family size")
ggplot(results[results$estimates == "birth_order",], aes(x=outcome, y=value))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))+
  ggtitle("Birth order")

####


# Sensitivity analyses ##############
## Exclude incomplete families on the outcome before to create mean exposure ####

load("data_sib.Rda")

# Analysis for the variables #run the same but replace outcomes$variables by outcomes$controls to get controls 
for (i in outcomes$controls){ 
  # exclude missing info on outcome
  data_sib_exc <- data_sib[!is.na(data_sib[i[[1]]]),]
  # keep only if at least 2 siblings with same FID 
  number <- as.data.frame(table(data_sib_exc$FID))
  onemember <- number[which(number$Freq == 1),] #identify family with only one member in this data
  data_sib_exc <- data_sib_exc[!(data_sib_exc$FID %in% onemember$Var1), ] #31337
  data_sib_exc <- data_sib_exc[order(data_sib_exc$FID),]
  
  #create education variables
  mean_educ<- group_by(data_sib_exc,FID) %>% summarize(m=mean(education))
  colnames(mean_educ) <- c("FID", "education_mean")
  data_sib_exc<- merge(data_sib_exc,mean_educ,by="FID")

  # Within-family estimates = deviation from family average
  data_sib_exc$education_dev <- data_sib_exc$education  - data_sib_exc$education_mean

  print(nrow(data_sib_exc))
  # Run analysis and create output variable for each outcome
  assign(paste0("model_",i[[1]]),
         lm(paste(i[[1]], "~ education_dev + education_mean + ",
                  paste(covariates)), data=data_sib_exc))
} 

# sample size variable 
# 1] 12599
# [1] 12640
# [1] 8367
# [1] 8471
# [1] 8485
# [1] 4123
# [1] 4381
# [1] 21205
# [1] 31337
# [1] 31337
# [1] 31337
# [1] 31337

#sample size control 
# [1] 24361
# [1] 24361
# [1] 24361
# [1] 24361
# [1] 31226
# [1] 9606
# [1] 29718
# [1] 29643

#Create table with all results
full_results_sens <- NULL
for (i in outcomes$controls){ 
  results <- get(paste0("model_",i[[1]]))
  results <- as.data.frame(summary(results)$coefficients)
  results$estimates <- rownames(results)
  rownames(results) <- NULL
  results$outcome <- i[[1]]
  full_results_sens <- rbind(full_results_sens, results)
}
full_results_sens <- full_results_sens[colnames(full_results_sens)[c(6,5,1:4)]]
# write.table(full_results, file="wellbeingEA__siblings_full_results_variables_sensibility_20220315.csv", 
#                        sep = ",", quote = FALSE, row.names = F)
# write.table(full_results, file="wellbeingEA__siblings_full_results_controls_sensibility_20220315.csv", 
#             sep = ",", quote = FALSE, row.names = F)

educ_result <- full_results_sens[full_results_sens$estimates == "education_dev" |
                              full_results_sens$estimates == "education_mean",]
educ_result

# write.table(educ_result, file="wellbeingEA__siblings_educ_results_variables_sensibility_20220315.csv", 
#             sep = ",", quote = FALSE, row.names = F)
# write.table(educ_result, file="wellbeingEA__siblings_educ_results_controls_sensibility_20220315.csv", 
#             sep = ",", quote = FALSE, row.names = F)

# Plot results 
educ_results <- educ_result %>% 
  rename(
    value = Estimate, 
    SE = `Std. Error`, 
    t.value = `t value`, 
    p.value = `Pr(>|t|)` 
  )

ggplot(educ_results, aes(x=outcome, y=value, fill=estimates))+
  geom_bar(position = position_dodge(0.9), stat= "identity")+
  geom_errorbar(aes(ymin=value - 1.96*SE, ymax = value + 1.96*SE), 
                width=0.4, 
                position = position_dodge(0.9))
