#script clean control variables

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
#birthweight
BW = bw.out.csv

BW$Nmeas[!is.na(BW$X20022.0.0) & !is.na(BW$X20022.1.0) & !is.na(BW$X20022.2.0)] <- 3

BW$Nmeas[is.na(BW$X20022.0.0) & !is.na(BW$X20022.1.0) & !is.na(BW$X20022.2.0)] <- 2
BW$Nmeas[!is.na(BW$X20022.0.0) & is.na(BW$X20022.1.0) & !is.na(BW$X20022.2.0)] <- 2
BW$Nmeas[!is.na(BW$X20022.0.0) & !is.na(BW$X20022.1.0) & is.na(BW$X20022.2.0)] <- 2

#IF the difference between any 2 measures > 0.5, we remove

BW$diff1 <- BW$X20022.0.0 - BW$X20022.1.0
BW$diff2 <- BW$X20022.0.0 - BW$X20022.2.0
BW$diff3 <- BW$X20022.1.0 - BW$X20022.2.0

BW$problematic[BW$diff1 > 0.5 | BW$diff1 < -0.5 | BW$diff2 > 0.5 | BW$diff2 < -0.5 | BW$diff3 > 0.5 | BW$diff3 < -0.5  ] <- 1

#additionally we also want to remove people that have BW <2.5 or >4.5
#BW$problematic2[BW$X20022.0.0 < 2.5 | BW$X20022.0.0 > 4.5 | BW$X20022#.1.0 < 2.5 | BW$X20022.1.0 > 4.5 | BW$X20022.0.0 < 2.5 | BW$X20022.0.0 > 4.5] <- 1   #42774


BW <- transform(BW, birthweight = rowMeans(BW[,2:4], na.rm = TRUE))

BW$birthweight[BW$problematic == 1] <- NA

toolow = subset(BW, birthweight < 2.50)  
toohigh  = subset(BW, birthweight > 4.50)   

BW$birthweight[BW$birthweight < 2.50 | BW$birthweight > 4.50] <- NA

BW <- BW[, -c(2:9)]

write.csv(BW,"", quote=F, row.names=F)

## Height
Height = height.out.csv

Height$Nmeas[!is.na(Height$X50.0.0) & !is.na(Height$X50.1.0) & !is.na(Height$X50.2.0)] <- 3

Height$Nmeas[is.na(Height$X50.0.0) & !is.na(Height$X50.1.0) & !is.na(Height$X50.2.0)] <- 2
Height$Nmeas[!is.na(Height$X50.0.0) & is.na(Height$X50.1.0) & !is.na(Height$X50.2.0)] <- 2
Height$Nmeas[!is.na(Height$X50.0.0) & !is.na(Height$X50.1.0) & is.na(Height$X50.2.0)] <- 2


Height$diff1 <- Height$X50.0.0 - Height$X50.1.0
Height$diff2 <- Height$X50.0.0 - Height$X50.2.0
Height$diff3 <- Height$X50.1.0 - Height$X50.2.0
# We remove cases with more than 5 cm difference

Height$problematic[Height$diff1 > 5.0 | Height$diff1 < -5.0 | Height$diff2 > 5.0 | Height$diff2 < -5.0| Height$diff3 > 5.0 | Height$diff3 < -5.0  ] <- 1  #161

#remove individuals with problematic data and take average for multiple time-points without problems

Height <- transform(Height, height = rowMeans(Height[,2:4], na.rm = TRUE))

Height = Height[,-c(2:9)]

#computing the Zscores for males and females separately requires us to merge the file with sex.
sexarray = read.csv("", header=F, sep=" ")
sexarray = sexarray[,-c(4:5)]
sexarray$V2 <- NULL
colnames(sexarray) = c("eid","sex")

Height = merge(Height, sexarray, by="eid")
men = subset(Height, sex==1)
women = subset(Height, sex==0)
men$Zheight = scale(men$height)
women$Zheight = scale(women$height)
men$height[men$Zheight > 4 | men$Zheight < -4] <- NA
women$height[women$Zheight > 4 | women$Zheight < -4] <- NA
men$Zheight <- NULL
women$Zheight <- NULL

Height = rbind(women, men)

write.csv(Height,"", quote=F, row.names=F)

##Height at age 10
# This is a categorical question
Height10 = height10.out.csv
Height10[ , 2:4 ][ Height10[ , 2:4 ] == -1 ] <- NA
Height10[ , 2:4 ][ Height10[ , 2:4 ] == -3 ] <- NA

Height10$diff1 <- Height10$X1697.0.0 - Height10$X1697.1.0
Height10$diff2 <- Height10$X1697.0.0 - Height10$X1697.2.0
Height10$diff3 <- Height10$X1697.1.0 - Height10$X1697.2.0

Height10$remove[Height10$diff1 > .01 | Height10$diff1 < -.01 | Height10$diff2 > .01 | Height10$diff2 < -.01 | 
Height10$diff3 > .01 | Height10$diff3 < -.01] <- 1   #4950

Height10 = Height10 %>%
  mutate(height10 = coalesce(X1697.0.0, X1697.1.0, X1697.2.0))

Height10$height10[Height10$remove == 1] <- NA

Height10 = Height10[,-c(2:8)]

temp = Height10
Height10$height10[temp$height10 == 2] <- 3
Height10$height10[temp$height10 == 3] <- 2

write.csv(Height10,"", quote=F, row.names=F)

#body size at age 10 
Weight10 = weight10.out.csv

Weight10[ , 2:4 ][ Weight10[ , 2:4 ] == -1 ] <- NA
Weight10[ , 2:4 ][ Weight10[ , 2:4 ] == -3 ] <- NA

Weight10$diff1 <- Weight10$X1687.0.0 - Weight10$X1687.1.0
Weight10$diff2 <- Weight10$X1687.0.0 - Weight10$X1687.2.0
Weight10$diff3 <- Weight10$X1687.1.0 - Weight10$X1687.2.0

Weight10$remove[Weight10$diff1 > .01 | Weight10$diff1 < -.01 | Weight10$diff2 > .01 | Weight10$diff2 < -.01 | 
Weight10$diff3 > .01 | Weight10$diff3 < -.01] <- 1   #6388

Weight10 = Weight10 %>%
 mutate(weight10 = coalesce(X1687.0.0, X1687.1.0, X1687.2.0))

Weight10$weight10[Weight10$remove == 1] <- NA

Weight10 = Weight10[,-c(2:8)]

temp = Weight10
Weight10$weight10[temp$weight10 == 2] <- 3
Weight10$weight10[temp$weight10 == 3] <- 2

write.csv(Weight10,"", quote=F, row.names=F)

#income
income = income.out.csv
income[ , 2:4 ][ income[ , 2:4 ] == -1 ] <- NA
income[ , 2:4 ][ income[ , 2:4 ] == -3 ] <- NA

#take most first income. 
income = income %>%
  mutate(Income = coalesce(X738.0.0, X738.1.0, X738.2.0))
  
income = income[,-c(2:4)]

income$Income18k <- 1
income$Income31k <- 1
income$Income52k <- 1
income$Income100k <- 1

income$Income18k[is.na(income$Income)] <- NA
income$Income31k[is.na(income$Income)] <- NA
income$Income52k[is.na(income$Income)] <- NA
income$Income100k[is.na(income$Income)] <- NA

income$Income18k[income$Income < 2] <- 0
income$Income31k[income$Income < 3 ] <- 0
income$Income52k[income$Income < 4 ] <- 0
income$Income100k[income$Income < 5 ] <- 0
write.csv(income,"", quote=F, row.names=F)


