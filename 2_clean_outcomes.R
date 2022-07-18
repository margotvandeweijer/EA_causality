#clean data for analyses

#module load 2021
#module load R/4.1.0-foss-2021a
# R

########################################################################
##load data - outcomes
########################################################################

folder = "/home/margotw/EA_WB/data/outcomes/" #path to outcome data
file_list <- list.files(path=folder, pattern="*.csv") #files

#read data 
for (i in 1:length(file_list)){
  assign(file_list[i], 
  read.csv(paste(folder, file_list[i], sep=''))
)}

########################################################################
##  Well-Being
########################################################################
## rename
#dont like these names
fam_sat = famrel.out.csv  #3 occasions
finan_sat = finsit.out.csv #3 occasions
friend_sat = friendsat.out.csv #3 occasions
health_sat = healthsat.out.csv #3 occasions
work_sat = worksat.out.csv  #3 occasions
happiness = hap.out.csv  #3 occasions
genhap = genhap.out.csv # 1 occassion
haphealth = haphealth.out.csv # 1 occassion 
meaning = meaning.out.csv # 1 occasion

############################
#select time-points
############################
#data are available for 3 timepoints for each of these variables
#we take the first one available (deviation from pre-reg)
#not the most efficient way, but it works

library(dplyr)

#function to alter dataframes with multiple time-points
addcol <- function(dataframe, varname,outputname){ 
dataframe <- dataframe %>% mutate(temp =
                     case_when(!is.na(dataframe[,2]) ~ dataframe[,2], 
                               (is.na(dataframe[,2]) & !is.na(dataframe[,3])) ~ dataframe[,3],
                               (is.na(dataframe[,2]) & is.na(dataframe[,3]) & !is.na(dataframe[,4])) ~ dataframe[,4]))

colnames(dataframe)[5] = varname
assign(outputname,dataframe, envir=.GlobalEnv)
}

# make new column with last timepoint
#check with head,tail each time if all goes well 
addcol(fam_sat,"famsat", "fam_sat")
addcol(finan_sat,"finansat", "finan_sat")
addcol(friend_sat,"friendsat", "friend_sat")
addcol(health_sat,"healthsat", "health_sat")
addcol(work_sat,"worksat", "work_sat")
addcol(happiness,"hap", "happiness")

# remove old columns
fam_sat <- fam_sat[, -c(2:4)]
finan_sat <- finan_sat[, -c(2:4)]
friend_sat <- friend_sat[, -c(2:4)]
health_sat <- health_sat[, -c(2:4)]
work_sat <- work_sat[, -c(2:4)]
happiness <- happiness[, -c(2:4)]

#dataframes with only one time-point
colnames(haphealth)[2] <- "haphealth" #2016-2017 (later than satisfaction health) - merge after recode missings
colnames(genhap)[2]  <- "genhap" #2016-2017 (later than happiness) - merge after recode missings 
colnames(meaning)[2] <- "meaning"

############################
#recode variables 
############################

#first recode NAs
#satisfaction items all coded same way 
recodemissing_sat = function(dataframe){ 
dataframe[,2][dataframe[,2] == -3 | dataframe[,2] == -1] <- NA
return(dataframe)
}

fam_sat <- recodemissing_sat(fam_sat)
finan_sat <- recodemissing_sat(finan_sat)
friend_sat <- recodemissing_sat(friend_sat)
health_sat <- recodemissing_sat(health_sat)
happiness <- recodemissing_sat(happiness)

#other codings
work_sat$worksat[work_sat$worksat == -3 | work_sat$worksat == -1 | work_sat$worksat == 7] <- NA

haphealth$haphealth[haphealth$haphealth == -818 | haphealth$haphealth == -121] <- NA
genhap$genhap[genhap$genhap == -818 | genhap$genhap == -121] <- NA
meaning$meaning[meaning$meaning == -818 | meaning$meaning == -121] <- NA

#recode files so that higher = more happiness and NA = actually NA
recodevalues = function(dataframe, newframe){
newframe = dataframe
newframe[,2][dataframe[,2] == 6] <- 1
newframe[,2][dataframe[,2] == 5] <- 2
newframe[,2][dataframe[,2] == 4] <- 3
newframe[,2][dataframe[,2] == 3] <- 4
newframe[,2][dataframe[,2] == 2] <- 5
newframe[,2][dataframe[,2] == 1] <- 6
return(newframe)
}

fam_sat <- recodevalues(fam_sat, fam_sat2)
finan_sat <- recodevalues(finan_sat, finan_sat2)
friend_sat <- recodevalues(friend_sat, friend_sat2)
health_sat <- recodevalues(health_sat, health_sat2)
happiness <- recodevalues(happiness, happiness2)
work_sat <- recodevalues(work_sat, work_sat2)
haphealth <- recodevalues(haphealth, haphealth2)
genhap <- recodevalues(genhap, genhap2)

#meaning is already coded correctly. 

############################
#combine some of the variables 
############################

#generel happiness, general happiness with own health, and meaningful life were collected in the online follow-up
#whenever we use that data we need to correct the age
#make additional variable that indicates what data was used

#happiness and general happiness are combined into one. 
#general happiness is the last time-point 

HAP = merge(happiness,genhap, by="eid")
HAP <- HAP %>% mutate(happiness =
                     case_when(!is.na(HAP[,2]) ~ HAP[,2], 
                               (is.na(HAP[,2]) & !is.na(HAP[,3])) ~ HAP[,3]))
                               
HAP <- HAP %>% mutate(timepoint_happiness =
                     case_when(!is.na(HAP[,2]) ~ "recruitment", 
                               (is.na(HAP[,2]) & !is.na(HAP[,3])) ~"followup"))                               
HAP <- HAP[, -c(2:3)] 

#satisfaction with own health and health happiness can also be combined
#happiness with own health is the last time-point

HAPhealth = merge(health_sat,haphealth, by="eid")
HAPhealth <- HAPhealth %>% mutate(healthhap =
                     case_when(!is.na(HAPhealth[,2]) ~ HAPhealth[,2], 
                               (is.na(HAPhealth[,2]) & !is.na(HAPhealth[,3])) ~ HAPhealth[,3]))
                              
                               
HAPhealth <- HAPhealth[, -c(2:3)]  

############################
#save files to temporary location
############################
                            
write.csv(fam_sat,"/home/margotw/EA_WB/data/cleaned_variables/fam_sat.csv", quote=F, row.names=F)
write.csv(finan_sat,"/home/margotw/EA_WB/data/cleaned_variables/finan_sat.csv", quote=F, row.names=F)
write.csv(friend_sat,"/home/margotw/EA_WB/data/cleaned_variables/friend_sat.csv", quote=F, row.names=F)
write.csv(HAPhealth,"/home/margotw/EA_WB/data/cleaned_variables/HAPhealth.csv", quote=F, row.names=F)
write.csv(work_sat,"/home/margotw/EA_WB/data/cleaned_variables/work_sat.csv", quote=F, row.names=F)
write.csv(HAP,"/home/margotw/EA_WB/data/cleaned_variables/HAP.csv", quote=F, row.names=F)
write.csv(meaning,"/home/margotw/EA_WB/data/cleaned_variables/meaning.csv", quote=F, row.names=F)

##################################################################
##  Mental Health
##################################################################
##Depression 
#We want to extract people that have "F32" or "F33" diagnoses in either the main or secondary file

#primary file 
ICDmain_DEP = ICD10main.out.csv %>% filter_all(any_vars(. %in% c('F320', 'F321','F322','F323','F328','F329','F330','F331','F332','F333','F334','F338','F339')))

ICDmain_DEP$DEP=1
ICDmain_DEP = data.frame(ICDmain_DEP$eid, ICDmain_DEP$DEP)
colnames(ICDmain_DEP) = c("eid","DEPmain")

#secondary file 
ICDsecond_DEP = ICD10second.out.csv %>% filter_all(any_vars(. %in% c('F320', 'F321','F322','F323','F328','F329','F330','F331','F332','F333','F334','F338','F339')))

ICDsecond_DEP$DEP=1
ICDsecond_DEP = data.frame(ICDsecond_DEP$eid, ICDsecond_DEP$DEP)
colnames(ICDsecond_DEP) = c("eid","DEPsecond")

#we can merge these files to the following depression file:
#depression = code 1286
Depression = illness.out.csv %>% filter_all(any_vars(. %in% 1286))
Depression$DEP_SR = 1
Depression = data.frame(Depression$eid, Depression$DEP_SR)
colnames(Depression) = c("eid","DEP_SR")

Depression = merge(Depression, ICDmain_DEP, by="eid", all.x=T, all.y=T)
Depression = merge(Depression, ICDsecond_DEP, by="eid", all.x=T, all.y=T)

# If any of the three depression variables = 1, we say depression =1 (and this file only contains those individuals)
Depression$depression =1
#not removing the other variables for now because we might want to do something with it for descriptives 
write.csv(Depression,"/home/margotw/EA_WB/data/cleaned_variables/Depression.csv", quote=F, row.names=F)

##Anxiety (F40 F41)
ICDmain_anx = ICD10main.out.csv %>% filter_all(any_vars(. %in% c('F400', 'F401','F402','F408','F409','F410','F411','F412','F413','F418','F419')))

ICDmain_anx$anx=1
ICDmain_anx = data.frame(ICDmain_anx$eid, ICDmain_anx$anx)
colnames(ICDmain_anx) = c("eid","ANXmain")

ICDsecond_anx = ICD10second.out.csv %>% filter_all(any_vars(. %in% c('F400', 'F401','F402','F408','F409','F410','F411','F412','F413','F418','F419')))

ICDsecond_anx$anx=1
ICDsecond_anx = data.frame(ICDsecond_anx$eid, ICDsecond_anx$anx)
colnames(ICDsecond_anx) = c("eid","ANXsecond")

#Anxiety self-report
Anxiety = illness.out.csv %>% filter_all(any_vars(. %in% 1287))
Anxiety$ANX_SR = 1
Anxiety = data.frame(Anxiety$eid, Anxiety$ANX_SR)
colnames(Anxiety) = c("eid","ANX_SR")

Anxiety = merge(Anxiety, ICDmain_anx, by="eid", all.x=T, all.y=T)
Anxiety = merge(Anxiety, ICDsecond_anx, by="eid", all.x=T, all.y=T)

Anxiety$anxiety <- 1

write.csv(Anxiety,"/home/margotw/EA_WB/data/cleaned_variables/Anxiety.csv", quote=F, row.names=F)

##Manic or Bipolar disorder (F30 F31)
ICDmain_bp = ICD10main.out.csv %>% filter_all(any_vars(. %in% c('F300','F301','F302','F302','F308','F309','F310','F311','F312','F313','F314','F315','F316','F317','F318','F319')))

ICDmain_bp$bp=1
ICDmain_bp = data.frame(ICDmain_bp$eid, ICDmain_bp$bp)
colnames(ICDmain_bp) = c("eid","BPmain")

ICDsecond_bp = ICD10second.out.csv %>% filter_all(any_vars(. %in% c('F300','F301','F302','F302','F308','F309','F310','F311','F312','F313','F314','F315','F316','F317','F318','F319')))

ICDsecond_bp$bp=1
ICDsecond_bp = data.frame(ICDsecond_bp$eid, ICDsecond_bp$bp)
colnames(ICDsecond_bp) = c("eid","BPsecond")

#self-report
Bipolar = illness.out.csv %>% filter_all(any_vars(. %in% 1291))
Bipolar$BP_SR = 1
Bipolar = data.frame(Bipolar$eid, Bipolar$BP_SR)
colnames(Bipolar) = c("eid","BP_SR")

Bipolar = merge(Bipolar, ICDmain_bp, by="eid", all.x=T, all.y=T)
Bipolar = merge(Bipolar, ICDsecond_bp, by="eid", all.x=T, all.y=T)

Bipolar$bipolar <- 1

write.csv(Bipolar,"/home/margotw/EA_WB/data/cleaned_variables/Bipolar.csv", quote=F, row.names=F)

##################################################################
## Physical health  (cardiovascular)
##################################################################
#the cardio file contains 12 instances, some of which only have 1 observation, which is very annoying
#found useful function below
cardio = cardio.out.csv %>%
  mutate(CV_problems = coalesce(X6150.0.0, X6150.0.1, X6150.0.2, X6150.0.3,X6150.1.0,X6150.1.1,X6150.1.2,X6150.1.3,X6150.2.0,X6150.2.1,X6150.2.2,X6150.2.3))  
  
#nice, works. 
cardio = data.frame(cardio$eid, cardio$CV_problems)
colnames(cardio) <- c("eid","CV_problems")

#we didnt pre-register anything ICD10 here - check with group later

#self-report
# we didnt put hypertension here but i guess that was a mistake?
Cardio_SR = illness.out.csv %>% filter_all(any_vars(. %in% c('1065','1066','1067','1068','1493','1081','1082','1083','1425')))

Cardio_SR$Cardio_SR = 1
Cardio_SR = data.frame(Cardio_SR$eid, Cardio_SR$Cardio_SR)
colnames(Cardio_SR) = c("eid","Cardio_SR")

Cardio <- merge(cardio, Cardio_SR, by="eid", all.x=T, all.y=T)

#final variable to use
Cardio$cardiovascular[Cardio$CV_problems > 0 | Cardio$Cardio_SR == 1] <- 1

write.csv(Cardio,"/home/margotw/EA_WB/data/cleaned_variables/Cardiovascular.csv", quote=F, row.names=F)
##################################################################
## Sumscores (Neuroticism)
##################################################################
 #this file is already cleaned actually
Neuroticism = Neu.out.csv
colnames(Neuroticism)[2] <- "Neuroticism"

write.csv(Neuroticism,"/home/margotw/EA_WB/data/cleaned_variables/Neuroticism.csv", quote=F, row.names=F)