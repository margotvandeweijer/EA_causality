#ROSLA analyses
# data prep
complete = read.csv("/home/margotw/EA_WB/data/full_data/complete.csv", header=T) #full dataset without related individuals

complete = subset(complete, !is.na(education))

#we first need to create a variable that indicates if someone was affected by the reform or not

#month of birth
#mob = read.csv("/home/margotw/EA_WB/data/covariates/mob.out.csv")
#colnames(mob)[2] <- "mob"

#complete = merge(complete, mob, by="eid")

#anyone born after 1 September 1957 was affected by the reform

complete$reform <- 0
complete$reform[complete$yob > 1957] <- 1
complete$reform[complete$yob == 1957 & complete$mob > 8] <- 1

#select  subjects born in a 5-year window around 1 September 1957
#so that means individuals born between  1-3-1955 and 1-3-1960

complete = subset(complete, yob > 1954 & yob < 1961)
complete$delete <- 0
complete$delete[complete$yob == 1955 & complete$mob <3] <- 1
complete$delete[complete$yob == 1960 & complete$mob >2] <- 1
complete = subset(complete, delete == 0)


write.csv(complete,"/home/margotw/EA_WB/data/full_data/rosla.csv", quote=F, row.names=F)