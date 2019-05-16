
click_info = read.csv("D:/segmentation/kmeans_data.csv", header = TRUE, sep = ",")
rownames(click_info) = click_info$Custid

click_info = click_info[,-1]

#Capping outliers

for(i in colnames(click_info)) {
  click_info[,i] = ifelse(click_info[,i]<=quantile(click_info[,i], probs = 0.01),quantile(click_info[,i], probs = 0.01),
                          click_info[,i])
  click_info[,i] = ifelse(click_info[,i]>=quantile(click_info[,i], probs = 0.99),quantile(click_info[,i], probs = 0.99),
                          click_info[,i])
}



#kmeans
clust.data= scale(click_info)
clust.data = data.frame(clust.data)


set.seed(22)
wss <- (nrow(clust.data)-1)*sum(apply(clust.data,2,var))

for(i in 2:15)  wss[i]<- sum(fit=kmeans(clust.data,centers=i,15)$withinss)

plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with cluster sum of squares")

fit <- kmeans(clust.data,6)

fit$withinss
fit$betweenss/fit$totss

clust_level = data.frame(click_info,fit$cluster)

write.csv(clust_level,"D:/segmentation/kmeans_output1.csv")