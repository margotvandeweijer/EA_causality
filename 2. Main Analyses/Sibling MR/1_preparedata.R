#MR within sib-ships
#prepare data
#script based on perline's sibling_analysis script.
#23-03-22 Margot
library(data.table)
library(dplyr)

# load data

pheno = read.csv("/home/margotw/EA_WB/data/full_data/full_standardized.csv", header=T)
siblings = fread("/home/margotw/EA_WB/MR_sibships/sibs_famid3", h=F, data.table=F, verbose=T)
colnames(siblings)[1:2] <- c("eid","FID") #41498
siblings$sibling = 1
adoptees = fread("/home/margotw/EA_WB/MR_sibships/adopted.IDs")
colnames(adoptees)[1:2]<-c("eid","ID1") #7437
adoptees$adopted = 1
prs = read.table("/home/margotw/EA_WB/MR/OUTPUT_PRS/EA_PRS.profile", header=T)
prs = subset(prs, FID > 0)
colnames(prs)[2] = "eid"


#merge and select sibling subset.
siblings = merge(siblings, adoptees, by="eid", all.x=T, all.y=T)
siblings = subset(siblings, is.na(adopted))
siblings$ID1 <- NULL
siblings$adopted <- NULL


pheno_prs = merge(pheno, prs, by="eid")
pheno_prs$FID <- NULL
data_sib = merge(pheno_prs, siblings, by="eid", all.x=T, all.y=T)
data_sib = subset(data_sib, sibling == 1)
#we see that the n sibs stays the same but the a subset doesnt have data -
#this is prob people that were removed because of where they were born etc. 

data_sib = subset(data_sib, !is.na(PC1))

#only people with EA data can be included.
data_sib <- data_sib[!(is.na(data_sib$education)),] #32712


# keep only if at least 2 siblings with same FID 
number <- as.data.frame(table(data_sib$FID))
onemember <- number[which(number$Freq == 1),] #identify family with only one member in this data
data_sib <- data_sib[!(data_sib$FID %in% onemember$Var1), ] #31337
data_sib <- data_sib[order(data_sib$FID),]
head(data_sib)

#number of sibships
length(unique(data_sib$FID)) #15237
data_sib$sibling <- NULL

#make birth order var
#birthday based on birth year and month
data_sib$birthday <- paste0(data_sib$yob, data_sib$mob)

data_sib <- data_sib %>%
  group_by(FID) %>%
  mutate(birth_order = dense_rank(birthday))

data_sib <- as.data.frame(data_sib)

#are there twins
data_sib <- data_sib %>% 
  group_by(FID) %>% 
  mutate(multiple = ifelse(duplicated(birthday)|
                             duplicated(birthday, fromLast = T),
                           T, F))

data_sib <- as.data.frame(data_sib)

table(data_sib$multiple) #439 multiples - should we just keep these in here?

temp = data_sib[data_sib$multiple==T, ] #check birthorder is same
temp2 = data_sib[data_sib$FID == 1055432, ] #check twins with birth roder 2 # they do have older sib


#get number of twins to investigate potential problems
summary <- data_sib %>%
  group_by(FID) %>%
  tally(multiple ==T)
table(summary$n)
#218 twins pairs, 1 triplet

write.csv(data_sib,"/home/margotw/EA_WB/MR_sibships/sibling_data.csv", quote=F, row.names=F)

#make new variables for MR

#Education
mean_educ <- group_by(data_sib,FID) %>% 
  summarize(m=mean(education))
colnames(mean_educ) <- c("FID", "education_mean")
data_sib<- merge(data_sib,mean_educ,by="FID")

data_sib$education_dev <- data_sib$education  - data_sib$education_mean  

#PRS
mean_prs <- group_by(data_sib,FID) %>% 
  summarize(m=mean(SCORE))
colnames(mean_prs) <- c("FID", "prs_mean")
data_sib<- merge(data_sib,mean_prs,by="FID")

data_sib$prs_dev <- data_sib$SCORE  - data_sib$prs_mean  

#save data
write.csv(data_sib,"/home/margotw/EA_WB/MR_sibships/sibling_data.csv", quote=F, row.names=F)