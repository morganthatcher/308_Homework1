#Homework 1 IEMS 308

setwd("C:/Users/Morgon/Downloads/Senior Year/Winter/IEMS 308/Medicare_Provider_Util_Payment_PUF_CY2017")
#Med<-read.table("Medicare_Provider_Util_Payment_PUF_CY2017.txt", header=T, sep="\t", na.strings="")
#Medicare_Provider_Util_Payment_PUF_CY2017 <- read.delim("C:/Users/Morgon/Downloads/Senior Year/Winter/IEMS 308/Medicare_Provider_Util_Payment_PUF_CY2017/Medicare_Provider_Util_Payment_PUF_CY2017.txt")

#plot(Medicare_Provider_Util_Payment_PUF_CY2017$npi, Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt)
Dat1<-Medicare_Provider_Util_Payment_PUF_CY2017[2:1000001,]
Dat2<-Medicare_Provider_Util_Payment_PUF_CY2017[2000002:3000001,]
Dat3<-Medicare_Provider_Util_Payment_PUF_CY2017[3000002:4000001,]
Dat4<-Medicare_Provider_Util_Payment_PUF_CY2017[4000002:5000001,]


#Statistics to demonstrate sample representativeness of whole- for allowed amount
x<-mean(Dat1$average_Medicare_allowed_amt)
s<-sd(Dat1$average_Medicare_allowed_amt)
mu<-mean(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt, na.rm=T)
sx<-sd(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt, na.rm=T)
t<-((x-mu)/sx)
#with such a low t-value and high degrees of freedom, large p-value
#conclude that no statistically significant difference between the sample and population

t.test(Dat1$average_Medicare_allowed_amt, mu=mu)


########################################################
exDat<-Dat1[1:100,]
boxplot(exDat$average_Medicare_allowed_amt)
plot(Dat1$npi, Dat1$average_Medicare_allowed_amt)

sum(Dat1$average_Medicare_allowed_amt>1000)/sum(Dat1$average_Medicare_allowed_amt<1000)

summary(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt)
IQR(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt, na.rm=T)

################################
#Average Amount Numerical Variable Exploratory Stats

###### Boxplots and Summary Stats #############
par(mfrow=c(2,2))
#Medicare allowed amount
x<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Average Medicare Allowed Amount")
summary(x)
IQR(x, na.rm=T) #89.37

#Submitted Charge Amount
x<-Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Average Submitted Charge Amount")
summary(x)
IQR(x, na.rm=T) #241.75

#Average Medicare Standard Amount
x<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_standard_amt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Average Medicare Standard Amount")
summary(x)
IQR(x, na.rm=T) #65.09

#Average Medicare Payment Amount
x<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Average Medicare Payment Amount")
summary(x)
IQR(x, na.rm=T) #65.96


######## Take out Outliers ###########
par(mfrow=c(1,1))
#Medicare Allowed Amt
ma<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_allowed_amt
ma<-ma[which((ma<=3*IQR(ma, na.rm=T)+quantile(ma,0.75, na.rm=T))&(ma>=quantile(ma,0.25, na.rm=T)-3*IQR(ma, na.rm=T)))]
summary(ma)
IQR(ma)

#Submitted CHarge
sc<-Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt
sc<-sc[which((sc<=3*IQR(sc, na.rm=T)+quantile(sc,0.75, na.rm=T))&(sc>=quantile(sc,0.25, na.rm=T)-3*IQR(sc, na.rm=T)))]
summary(sc)
IQR(sc)

#Medicare Standard
ms<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_standard_amt
ms<-ms[which((ms<=3*IQR(ms, na.rm=T)+quantile(ms,0.75, na.rm=T))&(ms>=quantile(ms,0.25, na.rm=T)-3*IQR(ms, na.rm=T)))]
summary(ms)
IQR(ms)

#Medicare Payment
mp<-Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt
mp<-mp[which((mp<=3*IQR(mp, na.rm=T)+quantile(mp,0.75, na.rm=T))&(mp>=quantile(mp,0.25, na.rm=T)-3*IQR(mp, na.rm=T)))]
summary(mp)
IQR(mp)

########### Histograms ####################
par(mfrow=c(2,2))
hist(ma, main="Medicare Allowed")
hist(sc, main="Submitted Charge")
hist(ms, main="Medicare Standard")
hist(mp, main="Medicare Payment")


############### Density Plots ##############
plot(density(ma), main="Medicare Allowed")
plot(density(sc), main="Submitted Charge")
plot(density(ms), main="Medicare Standard")
plot(density(mp), main="Medicare Payment")

#Example
#x<-Dat1$average_Medicare_allowed_amt
#d<-density(x[which((x<=3*IQR(x)+quantile(x,0.75))&(x>=quantile(x,0.25)-3*IQR(x)))])
#plot(d, main="Density Plot: Average Medicare Allowed Amount")

######### Correlations ################
par(mfrow=c(1,1))
#plot(sc[1:9212657], ma[1:9212657])
#this section repeated mult. times for report values
cor(Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt, Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_standard_amt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")


################################################
#Service Count Statistics


###### Boxplots and Summary Stats #############
par(mfrow=c(1,3))
#Line Service Count
x<-Medicare_Provider_Util_Payment_PUF_CY2017$line_srvc_cnt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Line Service Count")
summary(x)
IQR(x, na.rm=T) 

#Unique Beneficiary
x<-Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Unique Beneficiary")
summary(x)
IQR(x, na.rm=T) 

#Beneficiary/Day
x<-Medicare_Provider_Util_Payment_PUF_CY2017$bene_day_srvc_cnt
boxplot(x[which((x<=3*IQR(x, na.rm=T)+quantile(x,0.75, na.rm=T))&(x>=quantile(x,0.25, na.rm=T)-3*IQR(x, na.rm=T)))], main="Beneficiary/Day")
summary(x)
IQR(x, na.rm=T) 



######## Take out Outliers ###########
par(mfrow=c(1,1))
#LS Count
ls<-Medicare_Provider_Util_Payment_PUF_CY2017$line_srvc_cnt
ls<-ls[which((ls<=3*IQR(ls, na.rm=T)+quantile(ls,0.75, na.rm=T))&(ls>=quantile(ls,0.25, na.rm=T)-3*IQR(ls, na.rm=T)))]
summary(ls)
IQR(ls)

#Beneficiary Unique
bu<-Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt
bu<-bu[which((bu<=3*IQR(bu, na.rm=T)+quantile(bu,0.75, na.rm=T))&(bu>=quantile(bu,0.25, na.rm=T)-3*IQR(bu, na.rm=T)))]
summary(bu)
IQR(bu)

#Beneficiary/Day
bd<-Medicare_Provider_Util_Payment_PUF_CY2017$bene_day_srvc_cnt
bd<-bd[which((bd<=3*IQR(bd, na.rm=T)+quantile(bd,0.75, na.rm=T))&(bd>=quantile(bd, 0.25, na.rm=T)-3*IQR(bd, na.rm=T)))]
summary(bd)
IQR(bd)


########### Histograms ####################
par(mfrow=c(1,3))
hist(ls, main="Line Service")
hist(bu, main="Unique Beneficiary")
hist(bd, main="Beneficiary/Day")



############### Density Plots ##############
plot(density(ls), main="Line Serivce")
plot(density(bu), main="Unique Beneficiary")
plot(density(bd), main="Beneficiary/Day")


#Example
#x<-Dat1$average_Medicare_allowed_amt
#d<-density(x[which((x<=3*IQR(x)+quantile(x,0.75))&(x>=quantile(x,0.25)-3*IQR(x)))])
#plot(d, main="Density Plot: Average Medicare Allowed Amount")

######### Correlations ################
par(mfrow=c(1,1))
#plot(sc[1:9212657], ma[1:9212657])
#this section repeated mult. times for report values
cor(Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt, Medicare_Provider_Util_Payment_PUF_CY2017$average_submitted_chrg_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")
cor(Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_standard_amt, Medicare_Provider_Util_Payment_PUF_CY2017$average_Medicare_payment_amt, use="na.or.complete")


cor(Medicare_Provider_Util_Payment_PUF_CY2017$bene_unique_cnt, Medicare_Provider_Util_Payment_PUF_CY2017$bene_day_srvc_cnt, use="na.or.complete")



#K-means clustering
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

Dat1<-na.omit(Dat1)
Dat1<-scale(Dat1)
head(Dat1)
kmeans2<-kmeans(Dat1, centers=2, nstart=25)
str(kmeans2)

Col<-College
Col<-scale(COl)
head(Col)
Col1<-Col[,2:18]
head(Col1)
COl1<-scale(Col1)
head(COl1)

mydata<-Col1
par(mfrow=c(1,1))
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)



# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

fit$centers
fit$cluster
fit$size



wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 

##########################
kmeans2<-kmeans(Col1, centers=2)
plot(Col1, col=kmeans2$cluster)

################
str(kmeans2)
fviz_cluster(kmeans2, data=Col1)
library(ggplot2)


