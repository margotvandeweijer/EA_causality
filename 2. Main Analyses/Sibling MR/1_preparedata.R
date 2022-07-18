#MR within sib-ships
#prepare data

library(data.table)
library(dplyr)

# load data

pheno = read.csv("", header=T)

siblings = fread("", h=F, data.table=F, verbose=T)
colnames(siblings)[1:2] <- c("eid","FID") 
siblings$sibling = 1

adoptees = fread("")
colnames(adoptees)[1:2]<-c("eid","ID1") 
adoptees$adopted = 1

prs = read.table("", header=T)
prs = subset(prs, FID > 0)
colnames(prs)[2] = "eid"

####################################################################
#merge and select sibling subset.
siblings = merge(siblings, adoptees, by="eid", all.x=T, all.y=T)
siblings = subset(siblings, is.na(adopted))
siblings$ID1 <- NULL
siblings$adopted <- NULL

pheno_prs = merge(pheno, prs, by="eid")
pheno_prs$FID <- NULL
data_sib = merge(pheno_prs, siblings, by="eid", all.x=T, all.y=T)
data_sib = subset(data_sib, sibling == 1)

data_sib = subset(data_sib, !is.na(PC1))

#only people with EA data are included.
data_sib <- data_sib[!(is.na(data_sib$education)),] 

# keep only if at least 2 siblings with same FID 
number <- as.data.frame(table(data_sib$FID))
onemember <- number[which(number$Freq == 1),] #identify family with only one member in this data
data_sib <- data_sib[!(data_sib$FID %in% onemember$Var1), ]
data_sib <- data_sib[order(data_sib$FID),]
head(data_sib)

#number of sibships
length(unique(data_sib$FID)) 
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

table(data_sib$multiple) 

temp = data_sib[data_sib$multiple==T, ] #check birthorder is same


#get number of twins to investigate potential problems
summary <- data_sib %>%
  group_by(FID) %>%
  tally(multiple ==T)
table(summary$n)
#218 twins pairs, 1 triplet

write.csv(data_sib,"sibling_data.csv", quote=F, row.names=F)

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
write.csv(data_sib,"sibling_data.csv", quote=F, row.names=F)
