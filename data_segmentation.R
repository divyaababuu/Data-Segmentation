seg.df<-read.csv("Data_Segmentation.csv", stringsAsFactors = TRUE)
head(seg.df,n=8)
seg.df$gender<-ifelse(seg.df$gender=="Male",0,1)
seg.df$ownHome<-ifelse(seg.df$ownHome=="ownNo",0,1)
seg.df$subscribe<-ifelse(seg.df$subscribe=="subNo",0,1)
head(seg.df,n=8)

seg.df.sc<-seg.df
seg.df.sc[,c(1,3,4)]<-scale(seg.df[,c(1,3,4)])
head(seg.df.sc,n=8)
summary(seg.df.sc, digits=2)

seg.dist<-dist(seg.df.sc)
as.matrix(seg.dist)[1:5,1:5]

seg.hc<-hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc),h=4)$lower[[1]])

seg.df[c(156,152),]
seg.df[c(156,183),]

seg.hc.segment<-cutree(seg.hc,k=4)
table(seg.hc.segment)

install.packages("cluster")
library(cluster)
clusplot(seg.df, seg.hc.segment,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points
         lines = 0, #omit distance lines between groups
         main = "Hierarchical cluster plot", # figure title
)

aggregate(seg.df, list(seg.hc.segment), mean) #mean

boxplot(seg.df$income~seg.hc.segment,ylab="Income",xlab="Cluster/Segment") #median

set.seed(96743) #inorder to run k means, you have to run seed value
seg.k <- kmeans(seg.df.sc, centers = 4)
aggregate(seg.df,list(seg.k$cluster), mean)
boxplot(seg.df$income~seg.k$cluster,ylab="Income",xlab="Cluster/Segment") #median

install.packages("mclust")
library(mclust)
seg.mc<-Mclust(seg.df.sc)
summary(seg.mc) #compare the BIC value for statistical significance - the lower the value, the better
seg.mc4<-Mclust(seg.df.sc,G=4) #increase the number of clusters to 4
summary(seg.mc4) #compare the loglikelihood value for statistical significance - the higher the value, the better
