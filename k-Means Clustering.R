# read csv file
dungree <- read.csv("E:/GitHub Projects/K-Means Clustering/dungaree.csv")
#View(dungree)

# normalize data
dungree.norm<- sapply(dungree[,2:6],scale)
#View(dungree.norm)

colnames(dungree.norm) <- c('z_fashion','z_leisure','z_stretch','z_original','z_salestot')
df <- cbind(dungree,dungree.norm)

#View(df)

head(df)

#check for missing values
sum(is.na(df$FASHION))
sum(is.na(df$STRETCH))
sum(is.na(df$LEISURE))
sum(is.na(df$ORIGINAL))
sum(is.na(df$SALESTOT))
sum(is.na(df))

#boxplot of the dungree data set
boxplot(df[,7:10], xlab = "Type of Dungaree", ylab = "z-score of the Number of jeans sold ", main = "Boxplot of Types of Dungarees")

# check for the outliers
fashion.outlier <- boxplot.stats(df$z_fashion)$out
leisure.outlier <- boxplot.stats(df$z_leisure)$out
stretch.outlier <- boxplot.stats(df$z_stretch)$out
original.outlier <- boxplot.stats(df$z_original)$out

# create unique vectors of te outlier value
fashion.outlier.un <- unique(fashion.outlier)
leisure.outlier.un<- unique(leisure.outlier)
stretch.outlier.un <- unique(stretch.outlier)
original.outlier.un <- unique(original.outlier)

# create function to remove the outliers vairable
outlier_value <- function(x, factor){
  
  v <- vector("numeric", length = 0)
  for (i in 1:length(x)){
    for (j in 1:length(factor)){
      if (x[i] == factor[j]){
        v<- c(v,i)}
      
    }
  }
  return(v)
}

# find the rows containing outlers
ve1 <- outlier_value(df$z_fashion,fashion.outlier.un)
ve2 <- outlier_value(df$z_leisure,leisure.outlier.un)
ve3 <- outlier_value(df$z_stretch,stretch.outlier.un)
ve4 <- outlier_value(df$z_original,original.outlier.un)
# remove the rows with outlier values
df <- df[-ve1,]
df <- df[-ve2,]
df <- df[-ve3,]
df <- df[-ve4,]              
boxplot(df[,7:10], xlab = "Type of Dungaree", ylab = "z-score of the Number of jeans sold ", main = "Boxplot of Types of Dungarees")


View(df)

set.seed(42)
row.names(df) <- df[,1]
View(df)

# # removing the dependent column
# df <- df[, c(-1,-6)]

# # normalize data
# dungree.norm<- sapply(dungree[,2:6],scale)
# View(dungree.norm)

# colnames(dungree.norm) <- c('z_fashion','z_leisure','z_stretch','z_original','z_salestot')
# df <- cbind(dungree,dungree.norm)
# View(df)


library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(df[,7:10], min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")



# # Perform k-means cluster analysis
# fit.km <- kmeans(df[,7:10], centers = 10, nstart=25)
# fit.km
# fit.km$cluster
# fit.km$centers
# fit.km$size

#function to calculate the withing sum of sqaures for a range of number of clusters
wssplot <- function(data, nc=10, seed=1234) {
  wss <- (nrow(df)-1)*sum(apply(df[,7:10], 2, var)) 
  for (i in 2:10) {
    set.seed(1234) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  } 
  plot(1:10, wss, type="b",main = "Optimal Number of Clusters" , xlab="Number of clusters", ylab="within groups sum of squares")
}
wssplot(df[,7:10])
abline(v=6, col="red", lty=2, lwd=3)

#k-means for 6 clusters
fit.km <- kmeans(df[,7:10], 6, nstart=25)
fit.km

#k-means for 5 clusters
fit.km <- kmeans(df[,7:10], 5, nstart=25)
fit.km

# tablularize the results of k-means clustering
table(fit.km$cluster)
# fit.km <- kmeans(df.norm, 10, nstart=25)
# fit.km
library(factoextra)
#with(df[,7:10], pairs(df[,7:10], col=c(1:3)[fit.km$cluster])) 
fviz_cluster(fit.km, df[,7:10])