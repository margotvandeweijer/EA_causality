#Script to clean covariates(and the sample variables)
###################################################################
##load data - outcomes
###################################################################
folder = "/home/margotw/EA_WB/data/covariates/" #path to outcome data
file_list <- list.files(path=folder, pattern="*.csv") #files

#read data 
for (i in 1:length(file_list)){
  assign(file_list[i], 
  read.csv(paste(folder, file_list[i], sep=''))
)}

sexarray = read.csv("/home/margotw/EA_WB/data/covariates/sex.BC.array.v2.cov", header=F, sep=" ")
agepcs = read.csv("/home/margotw/EA_WB/data/covariates/age.25PCs.v2.qcov", header=F, sep=" ")
ethnic = read.csv("/home/margotw/EA_WB/data/sample_vars/ethnic.out.csv", header=T,sep=",")
library(dplyr)

###################################################################
##clean files
###################################################################
#year of birth is fine already

colnames(yob.out.csv) <- c("eid","yob")
write.csv(yob.out.csv,"/home/margotw/EA_WB/data/cleaned_variables/yob.csv", quote=F, row.names=F)

#assessment center

assess = assess.out.csv %>%
  mutate(center = coalesce(X54.0.0, X54.1.0, X54.2.0))

assess = assess[,-c(2:4)]

library(fastDummies)
assess <- dummy_cols(assess, select_columns = 'center', remove_first_dummy=T)


write.csv(assess,"/home/margotw/EA_WB/data/cleaned_variables/assess.csv", quote=F, row.names=F)

#we dont have access to the birthorder data unfortunately.

## Family size
# biological sisters
Nsis.out.csv[ , 2:4 ][ Nsis.out.csv[ , 2:4]  == -1 | Nsis.out.csv[ , 2:4 ] == -3 ] <- NA

Nsis.out.csv = Nsis.out.csv %>%
  mutate(nsis = coalesce(X1883.2.0,X1883.1.0,X1883.0.0))
  
Nsis.out.csv = Nsis.out.csv[,-c(2:4)]

#adopted sisters
Nsis_adopt.out.csv[ , 2:4 ][ Nsis_adopt.out.csv[ , 2:4]  == -1 | Nsis_adopt.out.csv[ , 2:4 ] == -3 ] <- NA  

Nsis_adopt.out.csv = Nsis_adopt.out.csv %>%
  mutate(nsis_adopt = coalesce(X3982.2.0,X3982.1.0,X3982.0.0))
  
Nsis_adopt.out.csv = Nsis_adopt.out.csv[,-c(2:4)]

#brothers
Nbro.out.csv[ , 2:4 ][ Nbro.out.csv[ , 2:4]  == -1 | Nbro.out.csv[ , 2:4 ] == -3 ] <- NA

Nbro.out.csv = Nbro.out.csv %>%
  mutate(nbro = coalesce(X1873.2.0,X1873.1.0,X1873.0.0))
  
Nbro.out.csv = Nbro.out.csv[,-c(2:4)]

#adopted brothers
Nbro_adopt.out.csv[ , 2:4 ][ Nbro_adopt.out.csv[ , 2:4]  == -1 | Nbro_adopt.out.csv[ , 2:4 ] == -3 ] <- NA 

Nbro_adopt.out.csv = Nbro_adopt.out.csv %>%
  mutate(nbro_adopt = coalesce(X3972.2.0,X3972.1.0,X3972.0.0))
  
Nbro_adopt.out.csv = Nbro_adopt.out.csv[,-c(2:4)]

#merge files .
familysize = merge(Nsis.out.csv, Nsis_adopt.out.csv, by="eid")
familysize = merge(familysize, Nbro.out.csv, by="eid")
familysize = merge(familysize, Nbro_adopt.out.csv, by="eid")

#if not adopted, this question was not posed and so the field is NA 

familysize$familysize <- rowSums(familysize[,c(2:5)], na.rm=TRUE)
#ranges from 0 - 44 which is A BIT much - discuss cutoff here later.

familysize = familysize[,-c(2:5)]

write.csv(familysize,"/home/margotw/EA_WB/data/cleaned_variables/familysize.csv", quote=F, row.names=F)

#season of birth based on month of birth
mob.out.csv$spring <- 0
mob.out.csv$summer <- 0
mob.out.csv$autumn <- 0
mob.out.csv$winter <- 0
mob.out.csv$spring[mob.out.csv$X52.0.0 == 3 | mob.out.csv$X52.0.0 == 4 | mob.out.csv$X52.0.0 == 5] <- 1
mob.out.csv$summer[mob.out.csv$X52.0.0 == 6 | mob.out.csv$X52.0.0 == 7 | mob.out.csv$X52.0.0 == 8] <- 1
mob.out.csv$autumn[mob.out.csv$X52.0.0 == 9 | mob.out.csv$X52.0.0 == 10 | mob.out.csv$X52.0.0 == 11] <- 1
mob.out.csv$winter[mob.out.csv$X52.0.0 == 12 | mob.out.csv$X52.0.0 == 1 | mob.out.csv$X52.0.0 == 2] <- 1

#we only need three but i'll throw one out later

colnames(mob.out.csv)[2] <- "mob"

write.csv(mob.out.csv,"/home/margotw/EA_WB/data/cleaned_variables/birhtseason.csv", quote=F, row.names=F)

#im only going to give the cov and qcov file a header.
agepcs$V2 <- NULL
#we are only using 10 PCs
agepcs = agepcs[,-c(12:26)]

colnames(agepcs) <- c("eid","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","age")

write.csv(agepcs,"/home/margotw/EA_WB/data/cleaned_variables/agepcs.csv", quote=F, row.names=F)

table(sexarray$V3)  # i think this is sex
#     0      1
#247322 208621
table(sexarray$V4) #this is array
#     0      1
# 49377 406566     
table(sexarray$V5)   #but what is this - asked abdel so wait for answer (maybe it is genetic sex???)
#     0      1
#253949 201994

sexarray$V2 <- NULL
colnames(sexarray) <- c("eid","sex1","batch","sex2")

write.csv(sexarray,"/home/margotw/EA_WB/data/cleaned_variables/sexarray.csv", quote=F, row.names=F)

#sample vars
#birthcountry
country = read.csv("/home/margotw/EA_WB/data/sample_vars/country.out.csv")

country[ , 2:4 ][ country[ , 2:4]  == -1 | country[ , 2:4 ] == -3 ] <- NA

country = country %>%
  mutate(country = coalesce(X1647.2.0,X1647.1.0,X1647.0.0))
  
country = country[,-c(2:4)]

write.csv(country,"/home/margotw/EA_WB/data/cleaned_variables/country.csv", quote=F, row.names=F)

# genetic ethnic grouping. 
colnames(ethnic) = c("eid","caucasian")
write.csv(ethnic,"/home/margotw/EA_WB/data/cleaned_variables/ethnic.csv", quote=F, row.names=F)
