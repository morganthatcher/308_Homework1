library(factoextra)
library(cluster)
library(tidyverse)
df<-USArrests
df<-na.omit(df)
df<-scale(df)
head(df)
k2<-kmeans(df, centers=2, nstart=25)
fviz_cluster(k2, data=df)

#kink method for optimal number of clusters
set.seed(123)
fviz_nbclust(df, kmeans, method="wss")

#silhouette method for optimal number of clusters
fviz_nbclust(df, kmeans, method="silhouette")

avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#final kmeans clustering
set.seed(123)
final<-kmeans(df, 4, nstart=25)
fviz_cluster(final, data=df)

USArrests %>%
  mutate(Cluster=final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

Dat1<-MedData[2:101,]
One_Hot<-data.frame()



##############
One_Hot<-vector(length=nrow(Dat1))
for(i in 1:nrow(Dat1)) {
  if(Dat1[i,5] == "MD" | Dat1[i,5] == "M.D.") 
    One_Hot[i]=1
  else One_Hot[i]=0
}

x<-Dat1[1:nrow(Dat1),5]
pattern1<-"MD"
pattern<-"M.*D"
out<-vector(length=nrow(Dat1))
out<-grepl(pattern, x)


nrow(Dat1)
Dat1[,27]<-One_Hot

#cluster on average submitted charge amount
Dat<-data.frame(Dat1$average_submitted_chrg_amt, Dat1$bene_unique_cnt)
Dat<-Dat[-c(13),]
Dat<-na.omit(Dat)

Dat<-scale(Dat)


set.seed(123)
fviz_nbclust(Dat, kmeans, method="wss")

k1<-kmeans(Dat, 3, nstart=25)
fviz_cluster(k1, data=Dat)
k1$size

One_Hot<-One_Hot[-c(13)]
AS<-Dat1$average_submitted_chrg_amt[-c(13)]
BU<-Dat1$bene_unique_cnt[-c(13)]
Dat<-data.frame(AS, BU, One_Hot, k1$cluster)

#Do MDs recieve higher payout from Medicare from their submitted cost
#No


Dat %>%
  mutate(Cluster=k1$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#####################################
AllDat<-MedData
sa<-AllDat$average_submitted_chrg_amt
bu<-AllDat$bene_unique_cnt
AllDat<-AllDat[-which((sa<=132) & (sa>=0)),]
AllDat<-AllDat[-which((bu<=30) & (bu>=11)),]


#On More Data
Data<-AllDat
Data<-na.omit(Data)

#one hot MD
x<-Data[1:nrow(Data),5]
pattern<-"M.*D"
out<-vector(length=nrow(Data))
out<-grepl(pattern, x)

#one hot Place of Service
x1<-Data[1:nrow(Data),16]
pattern1<-"F"
out1<-vector(length=nrow(Data))
out1<-grepl(pattern1, x1)
mean(out1)

#one hot US
x2<-Data[1:nrow(Data),6]
pattern2<-"M"
out2<-vector(length=nrow(Data))
out2<-grepl(pattern2, x2)
mean(out2)

ClusDat<-data.frame(Data$average_submitted_chrg_amt, Data$bene_unique_cnt)
ClusDat<-na.omit(ClusDat)
ClusDat<-scale(ClusDat)
set.seed(123)
fviz_nbclust(ClusDat, kmeans, method="wss")
k<-kmeans(ClusDat, 4, nstart=25)
fviz_cluster(k, data=ClusDat)
Dat<-data.frame(ClusDat, out, k$cluster)
Dat %>%
  mutate(Cluster=k$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
k$size


##############################

One_Hot<-vector(length=nrow(Data))
for(i in 1:nrow(Data)) {
  if(Data[i,5] %like% '%MD%'| Data[i,5] %like% '%M.D.%') 
    One_Hot[i]=1
  else One_Hot[i]=0
}



df1<-USArrests
#263 Assault
for (i in 1:nrow(df1)) {
  if(df1$Assault[i]=236) {
    print "yes"
  }
}


#################################################
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()
USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")