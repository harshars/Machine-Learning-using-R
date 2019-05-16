################################# Hierarchical clustering ############################



iris = read.csv("D:/segmentation/hierarchical_data.csv", header = TRUE, sep = ",")

#Capping outliers

for(i in colnames(iris)) {
  iris[,i] = ifelse(iris[,i]<=quantile(iris[,i], probs = 0.01),quantile(iris[,i], probs = 0.01),
                    iris[,i])
  iris[,i] = ifelse(iris[,i]>=quantile(iris[,i], probs = 0.99),quantile(iris[,i], probs = 0.99),
                    iris[,i])
}


# Average Linkage
hc.average <- hclust(dist(scale(iris)), method="average")

png("D:/average.png",width=1600,height=800)

plot(hc.average ,main =" Average Linkage ", xlab="", sub ="", cex =.9,)

table(cutree(hc.average,h = 1.5))



# Complete Linkage
hc.complete <- hclust(dist(scale(iris)), method="complete")

png("D:/complete.png",width=1600,height=800)

plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="", cex =.9)





# Single Linkage
hc.single <- hclust(dist(scale(iris)), method="single")

png("D:/single.png",width=1600,height=800)

plot(hc.single ,main =" Single Linkage ", xlab="", sub ="", cex =.9)





# Centroid Linkage
hc.centroid <- hclust(dist(scale(iris)), method="centroid")

png("D:/centroid.png",width=1600,height=800)

plot(hc.centroid ,main =" Centroid Linkage ", xlab="", sub ="", cex =.9)




# Ward Linkage
hc.ward <- hclust(dist(scale(iris)), method="ward.D2")

png("D:/ward.png",width=1600,height=800)

plot(hc.ward ,main =" Ward Linkage ", xlab="", sub ="", cex =.9)

table(cutree(hc.ward,h=10))