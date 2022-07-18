#merge datasets. 
library(dplyr)
folder = "" 
file_list <- list.files(path=folder) #files

#read data 
for (i in 1:length(file_list)){
  assign(file_list[i], 
  read.csv(paste(folder, file_list[i], sep=''))
)}

################################################################
#Complete file with all variables
#can make subselections later if neccessary

full = Reduce(function(x,y) merge(x = x, y = y, by = "eid", all.x=T, all.y=T), list(agepcs.csv,Anxiety.csv,assess.csv,Bipolar.csv,birhtseason.csv,BW.csv,Cardiovascular.csv, country.csv, Depression.csv,Education.csv, ethnic.csv, fam_sat.csv,familysize.csv,finan_sat.csv,friend_sat.csv,HAP.csv,HAPhealth.csv,Height.csv,Height10.csv,income.csv,meaning.csv,Neuroticism.csv,sexarray.csv,Weight10.csv,work_sat.csv,yob.csv))


#We remove 1 birth season since we dont need all as dummies
full$spring <- NULL

#only include individuals born in England or Wales
full = subset(full, country == 1 | country == 2)

#exclude individuals that are not of european ancestry. 
full = subset(full, caucasian == 1)

#cardiovascular, depression, anxiety, and bipolar disorder: missing equals zero
full$depression[is.na(full$depression)] <- 0
full$bipolar[is.na(full$bipolar)] <- 0
full$anxiety[is.na(full$anxiety)] <- 0
full$cardiovascular[is.na(full$cardiovascular)] <- 0

#exclude one individual from each genetically related pair 

genrel = read.csv("/home/margotw/EA_WB/data/sample_vars/relatedness_file.txt", header=T, sep=" ")

participants = subset(full, !is.na(education))
participants = participants[,1]
library(ukbtools)

remove = ukb_gen_samples_to_remove(genrel, participants, cutoff = 0.0884) #0.0884 includes pairs with greater than 3rd-degree relatedness

temp = as.data.frame(remove)
colnames(temp) = "eid"
temp$remove_related <- 1

full2 = merge(full, temp, by="eid", all.x=T, all.y=T)

#make complete phenotype file for anyone to use

#remove some variables they will not need at this point
full2$ANX_SR <- NULL
full2$ANXmain <- NULL
full2$ANXsecond <- NULL
full2$BP_SR <- NULL
full2$BPmain <- NULL
full2$BPsecond <- NULL
full2$sex2 <- NULL
full2$DEP_SR <- NULL
full2$DEPmain <- NULL
full2$DEPsecond <- NULL
full2$CV_problems <-NULL
full2$Cardio_SR <- NULL
full2$caucasian <- NULL


#we need to make adjust the age  variable for meaning, happiness and happiness with own health are these are (partly) based on the online followup 
#add date of recruitment for the online followup
date_recruit = read.csv("")
colnames(date_recruit)[2] = "date_recruit"
full2 = merge(full2, date_recruit, by="eid")

#since we do not have the exact DOB, ill make the DOB the first day of the month 
full2$day = 01
full2$DOB <- as.Date(with(full2,paste(yob,mob,day,sep="-")),"%Y-%m-%d")
full2$date_recruit = as.Date(full2$date_recruit)

# age happiness
library(eeptools)

temp_age = subset(full2, !is.na(DOB) & !is.na(date_recruit))
temp_age = temp_age[, -c(2:69)]  
temp_age$age_followup = age_calc(temp_age$DOB, enddate = temp_age$date_recruit, units = "years", precise = TRUE)
temp_age$age_followup = floor(temp_age$age_followup)
temp_age = temp_age[, -c(2:4)]

full2 = merge(full2, temp_age, by="eid", all.x=T, all.y=T)

full2$followup_happiness[full2$timepoint_happiness == "followup"]  <- 1
full2$recruitment_happiness[full2$timepoint_happiness == "recruitment"]  <- 1

library(dplyr)
#happiness
full2$age = as.double(full2$age)

full2 <- full2 %>% mutate(age_happiness =
                     case_when(!is.na(followup_happiness) ~ full2[,73], 
                               (is.na(followup_happiness) & !is.na(recruitment_happiness)) ~ full2[,12]))     
#meanining                               
full2$age_meaning = full2$age_followup              

#happiness with health
full2$followup_haphealth[full2$time_healthhap == "followup"]  <- 1
full2$recruitment_haphealth[full2$time_healthhap == "recruitment"]  <- 1

full2 <- full2 %>% mutate(age_haphealth =
                     case_when(!is.na(followup_haphealth) ~ full2[,73], 
                               (is.na(followup_haphealth) & !is.na(recruitment_haphealth)) ~ full2[,12]))     

full2$followup_haphealth <- NULL
full2$recruitment_haphealth <- NULL
full2$followup_happiness <- NULL
full2$recruitment_happiness <- NULL
full2$day <- NULL 
full2$DOB <- NULL


write.csv(full2,"full_unstandardized.csv", quote=F, row.names=F)

#standardized continuous outcomes

full2_stand <- full2 %>% mutate_at(c("happiness","healthhap","friendsat","finansat","famsat","worksat","meaning","Neuroticism",
                                     "height","birthweight"), ~(scale(.) %>% as.vector))

write.csv(full2_stand,"full_standardized.csv", quote=F, row.names=F)

#Standardized file without genetic relatnedness
full2_stand = subset(full2_stand, is.na(remove_related))

full2_stand$remove_related <- NULL

write.csv(full2_stand,"complete.csv", quote=F, row.names=F)
