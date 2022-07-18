#script clean education
###################################################################
##load data - outcomes
###################################################################
folder = "" #path to outcome data
file_list <- list.files(path=folder, pattern="*.csv") #files

#read data 
for (i in 1:length(file_list)){
  assign(file_list[i], 
  read.csv(paste(folder, file_list[i], sep=''))
)}

library(dplyr)
###################################################################
##clean files
###################################################################
#Age at which one left education was measured on 3 occasions
#We take the last occassion as this is the most recent info on their EA
Education = edu.out.csv %>%
  mutate(education = coalesce(X845.2.0,X845.1.0,X845.0.0))

#set missings
#if individuals indicated never going to school im also setting them to missing 

Education$education[Education$education <0] <- NA

#Qualifications 

Qualifications = qualifications.out.csv 
Qualifications[,2:19][Qualifications[,2:19]<0] <- NA

#three occasions with multiple answers possible
Qualifications = Qualifications %>%
  mutate(quali2 = pmin(X6138.2.5,X6138.2.4,X6138.2.3,X6138.2.2,X6138.2.1,X6138.2.0, na.rm=T))
 
Qualifications = Qualifications %>%
  mutate(quali1 = pmin(X6138.1.5,X6138.1.4,X6138.1.3,X6138.1.2,X6138.1.1,X6138.1.0, na.rm=T))
  
Qualifications = Qualifications %>%
  mutate(quali0 = pmin(X6138.0.5,X6138.0.4,X6138.0.3,X6138.0.2,X6138.0.1,X6138.0.0, na.rm=T))

#if they indicate a lower education at a later timepoint, I remove them. 

Qualifications$delete[Qualifications$quali1 == 1 & Qualifications$quali0 < Qualifications$quali1] <- 1

Qualifications$delete[Qualifications$quali1 == 1 & Qualifications$quali0 < Qualifications$quali2] <- 1 #only 72

Qualifications = subset(Qualifications, is.na(delete))

Qualifications = Qualifications %>%
  mutate(quali = coalesce(quali0, quali1, quali2))

Qualifications = data.frame(Qualifications$eid, Qualifications$quali)

colnames(Qualifications) <- c("eid","qualifications")

Education_final <- merge(Education, Qualifications, by="eid")

Education_final$education[is.na(Education_final$education) & Education_final$qualifications == 1] <- 21

Education_final <- data.frame(Education_final$eid, Education_final$education)
colnames(Education_final) <- c("eid","education")

write.csv(Education_final,"", quote=F, row.names=F)
