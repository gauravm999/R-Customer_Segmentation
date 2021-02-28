#High value customers identification for an E-Commerce company
#https://lms.simplilearn.com/courses/3813/PG-DS---Data-Science-with-R/syllabus

# Find significant customers for the business who make high purchases of their favourite products. 
# The organization wants to roll out a loyalty program to the high-value customers after identification of segments. 
# Use the clustering methodology to segment customers into groups.

# library(conflicted)
library(dplyr)
library(caret)
# library(DataExplorer)

# Load Data:
setwd("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/5. Gradable_Projects")
Ecom_data<- read.csv("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/5. Gradable_Projects/Ecommerce.csv", header=TRUE)

dim(Ecom_data)
names(Ecom_data)
str(Ecom_data)
# View(Ecom_data)

summary(Ecom_data)

# Data Pre-processing and Prepration :

#Covert variables:
library(lubridate)
Ecom_data$InvoiceDate<- dmy(Ecom_data$InvoiceDate)  #factor to Date
Ecom_data$CustomerID<- as.factor(Ecom_data$CustomerID)
str(Ecom_data)
# names(Ecom_data)

#Add New variable - Sales Value:
Ecom_data<- Ecom_data %>%
  mutate(Sales_Value = Quantity * UnitPrice)

summary(Ecom_data)


# Clean the data :

#Step 1: Remove data with Negative Quantities
Ecom_data1<- Ecom_data %>%     #Quantity > 0
  filter(Quantity>0)

#Step 2: Remove data with Negative Price
Ecom_data2<- Ecom_data1 %>%     #Price > 0
  filter(UnitPrice>0)

#Step 3: Retain data for those customers who has Customer ID ie. remove Unknown customers

#Check missing values:
colSums(is.na(Ecom_data2)) # there are 132220 custoers with missing CustomerID. 

Ecom_data3<- Ecom_data2 %>%
  filter(CustomerID !="NA's") #Removed customers with missing customer IDs

# # create month, year and hour of day variables
str(Ecom_data3)
# 
# library(lubridate)
# Ecom_data3$dayOfWeek <- wday(Ecom_data3$InvoiceDate, label=TRUE)
# str(Ecom_data3)
# 
# # Separate date and time components of invoice date
Ecom_data3 <- Ecom_data3
dates<- strptime(as.Date(Ecom_data3$InvoiceDate),format="%Y-%m-%d")

Ecom_data3$Year <- as.factor(format(dates, "%Y"))
Ecom_data3$Month <- as.factor(format(dates, "%m"))
Ecom_data3$Day <- as.factor(format(dates, "%d"))
Ecom_data3$Day_w <- as.factor(format(dates, "%a"))

dim(Ecom_data3)
str(Ecom_data3)
summary(Ecom_data3)
# View(Ecom_data3)

# Aggregate Data:
Cust_data<- Ecom_data3 %>%
  select(CustomerID, Country, StockCode, InvoiceNo, Quantity, Sales_Value) %>%
  group_by(CustomerID, Country) %>%
  summarise(Units = sum(Quantity), Sales = sum(Sales_Value),
            Unique_SKUs = n_distinct(StockCode), 
            Unique_Orders = n_distinct(InvoiceNo)) %>%
  mutate(Average_Order_Value = round(Sales/Unique_Orders,2)) %>%
  arrange(CustomerID, Country)

Cust_data<- as.data.frame(Cust_data)

dim(Cust_data)
str(Cust_data)
summary(Cust_data)
# View(Cust_data)

# Add more variables - Recency and Frequency :
RFM_df<- Ecom_data3 %>% 
  group_by(CustomerID, Country) %>% 
  summarise(Recency = abs(as.numeric(as.Date("2016-11-29")-max(InvoiceDate))),
            Frequency = n_distinct(InvoiceNo), Monitary= sum(Sales_Value)/n_distinct(InvoiceNo)) %>%
  arrange(CustomerID, Country)

RFM_df<- as.data.frame(RFM_df)

str(RFM_df)
summary(RFM_df)

Cust_data2<- cbind(Cust_data, RFM_df)
head(Cust_data2)
Cust_data3<- Cust_data2[c(1:7, 10:11)] #Remove duplicate column names - CountryID, Country and Monitary (Sales)
# names(Cust_data3)
# View(Cust_data3)
str(Cust_data3)
summary(Cust_data3)

# write.csv(Cust_data3, "Customer_data_tranformed_in_R_v5.csv")

# Data Preparation completed

# EDA: Correlation Analysis


#Plots before Normalizing KPIs
par(mfrow=c(2,4))

Units_dist<- hist(Cust_data3$Units, col = 'blue', breaks=10, labels = TRUE,
                main ="Units Distrbution",
                xlab = "Units", ylab = "#Customers")

Sales_dist<- hist(Cust_data3$Sales, col = 'blue', breaks=10, labels = TRUE,
                  main ="Sales Distrbution",
                  xlab = "Sales", ylab = "#Customers")

Unique_SKUs_dist<- hist(Cust_data3$Unique_SKUs, col = 'blue', breaks=10, labels = TRUE,
                  main ="Unique_SKUs Distrbution",
                  xlab = "Unique_SKUs", ylab = "#Customers")

Unique_SKUs_dist<- hist(Cust_data3$Unique_SKUs, col = 'blue', breaks=10, labels = TRUE,
                        main ="Unique_SKUs Distrbution",
                        xlab = "Unique_SKUs", ylab = "#Customers")

Unique_Orders_dist<- hist(Cust_data3$Unique_Orders, col = 'blue', breaks=10, labels = TRUE,
                        main ="Unique_Orders Distrbution",
                        xlab = "Unique_Orderss", ylab = "#Customers")

AOV_dist<- hist(Cust_data3$Average_Order_Value, col = 'blue', breaks=10, labels = TRUE,
                          main ="Average_Order_Value Distrbution",
                          xlab = "Average_Order_Value", ylab = "#Customers")

Recency_dist<- hist(Cust_data3$Recency, col = 'blue', breaks=10, labels = TRUE,
                main ="Recency Distrbution",
                xlab = "Recency_Days", ylab = "#Customers")

Freq_dist<- hist(Cust_data3$Frequency, col = 'blue', breaks=50, labels = TRUE,
                    main ="Frequency Distrbution",
                    xlab = "Frequency", ylab = "#Customers")

dev.off()

str(Cust_data3)
Cust_data5<- Cust_data3[-c(1,2)] #with Numerical values
head(Cust_data5)
summary(Cust_data5)


library(corrplot)
Cust_cor_data<- Cust_data5 #Data with numerical variables 
str(Cust_cor_data)
cor = cor(Cust_cor_data)
corrplot(cor, method="number", type = "upper", order = "hclust", 
         tl.col = "black")

# library(PerformanceAnalytics)
# chart.Correlation(Cust_cor_data)

# Insights :
#1. Only Sales Units and Sales value is highly correlated (as expected)

# library(mclust)
# mfit<- Mclust(Cust_cor_data)
# plot(mfit)

# Idenfity Factors impacting Sales

library(randomForest)
formula<- Sales ~ Unique_SKUs + Unique_Orders + Average_Order_Value + Units + Recency + Frequency
M.rf<- randomForest(formula, data=Cust_data5, ntree=500, importance=TRUE, proximity=TRUE, mtry=3)
M.rf

## Show "importance" of variables: higher value mean more important:
round(importance(M.rf), 2)
# Significant Variables:
varImpPlot(M.rf, main = "Variable Importance")


# Build Models :
# Standarize variables

# Min-Max scaling - first create normalize function and then normalize each KPIs

# library(caret)
# preproc<- preProcess(Cust_data5, method=c("scale"))
# Cust_norm<- predict(preproc, Cust_data5)
# summary(Cust_norm)
# ?preProcess


normalize<- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Cust_norm<- Cust_data5
Cust_norm$Units<-normalize(Cust_data5$Units)
Cust_norm$Sales<-normalize(Cust_data5$Sales)
Cust_norm$Unique_SKUs<-normalize(Cust_data5$Unique_SKUs)
Cust_norm$Unique_Orders<-normalize(Cust_data5$Unique_Orders)
Cust_norm$Average_Order_Value<-normalize(Cust_data5$Average_Order_Value)
Cust_norm$Recency<-normalize(Cust_data5$Recency)
Cust_norm$Frequency<-normalize(Cust_data5$Frequency)

str(Cust_norm)

#Plots After Normalizing KPIs
par(mfrow=c(2,4))

Units_norm_dist<- hist(Cust_norm$Units, col = 'blue', breaks=10, labels = TRUE,
                       main ="Units Distrbution - Scale",
                       xlab = "Units", ylab = "#Customers")

Sales_norm_dist<- hist(Cust_norm$Sales, col = 'blue', breaks=10, labels = TRUE,
                  main ="Sales Distrbution - Normalized",
                  xlab = "Sales", ylab = "#Customers")

Unique_SKUs_norm_dist<- hist(Cust_norm$Unique_SKUs, col = 'blue', breaks=10, labels = TRUE,
                        main ="Unique_SKUs Distrbution - Normalized",
                        xlab = "Unique_SKUs", ylab = "#Customers")

Unique_SKUs_norm_dist<- hist(Cust_norm$Unique_SKUs, col = 'blue', breaks=10, labels = TRUE,
                        main ="Unique_SKUs Distrbution - Normalized",
                        xlab = "Unique_SKUs", ylab = "#Customers")

Unique_Orders_norm_dist<- hist(Cust_norm$Unique_Orders, col = 'blue', breaks=10, labels = TRUE,
                          main ="Unique_Orders Distrbution - Normalized",
                          xlab = "Unique_Orderss", ylab = "#Customers")

AOV_norm_dist<- hist(Cust_norm$Average_Order_Value, col = 'blue', breaks=10, labels = TRUE,
                main ="Average_Order_Value Distrbution - Normalized",
                xlab = "Average_Order_Value", ylab = "#Customers")

Recency_norm_dist<- hist(Cust_norm$Recency, col = 'blue', breaks=10, labels = TRUE,
                    main ="Recency Distrbution - Normalized",
                    xlab = "Recency_Days", ylab = "#Customers")

Freq_norm_dist<- hist(Cust_norm$Frequency, col = 'blue', breaks=50, labels = TRUE,
                 main ="Frequency Distrbution - Normalized",
                 xlab = "Frequency", ylab = "#Customers")

dev.off()

# Identify right no. of clusters:

# Screeplot to determine right number of clusters:

# # library(stats)
# # pc.cr <- princomp(Cust_data_scale, cor = TRUE)
# # screeplot(pc.cr, npcs = 9, type = "lines")
# 
# pc.cr2<- princomp(Cust_norm, cor = TRUE)
# screeplot(pc.cr2, npcs = 9, type = "lines")


library(factoextra)
library(NbClust)
# library(clue)
library(cluster)

# Elbow method

# fviz_nbclust(Cust_data_scale, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")


# Final plot 1
fviz_nbclust(Cust_norm, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")


# Silhouette method Plot

# Final plot 2
silhouette_score2 <- function(k){
  km <- kmeans(Cust_data5, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(Cust_norm))
  mean(ss[, 3])
}
k <- 2:20
avg_sil2<- sapply(k, silhouette_score2)
avg_sil2
plot(k, type='b', avg_sil2, xlab='Number of clusters', ylab='Average Silhouette Scores', main = "Silhoutette Score" , frame=FALSE)


# # Gap statistic
# # nboot = 50 to keep the function speedy. 
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(Cust_data_scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
#   labs(subtitle = "Gap statistic method")

# Clusterting on the data using Scale fuction
# set.seed(123)
# fit<- kmeans(Cust_data_scale, centers = 2, iter.max = 10)
# fit
# fit1<- kmeans(Cust_data_scale, centers = 3, iter.max = 10)
# fit1
# fit2<- kmeans(Cust_data_scale, centers = 4, iter.max = 10)   #Final model
# fit2
# fit3<- kmeans(Cust_data_scale, centers = 5, iter.max = 10)
# fit3

# str(Cust_norm)
# Cust_norm2<- Cust_norm[,c(1,2,4,5,7)]
# str(Cust_norm2)
  
# Clusterting on the data using Min-Max Normalization function
set.seed(123)
fit_n2<- kmeans(Cust_norm, centers = 2, iter.max = 10)
# fit_n2
fit_n3<- kmeans(Cust_norm, centers = 3, iter.max = 10)
# fit_n3
fit_n4<- kmeans(Cust_norm, centers = 4, iter.max = 10)
# fit_n4
fit_n5<- kmeans(Cust_norm, centers = 5, iter.max = 10)
fit_n5
fit_n5$centers
fit_n5$size

# # Optimal k
# kmean_withinss <- function(k) {
#   cluster <- kmeans(Cust_norm, k)
#   return (cluster$tot.withinss)
# }
# 
# # Set maximum cluster 
# max_k <-20 
# # Run algorithm over a range of k 
# wss <- sapply(2:max_k, kmean_withinss)
# 
# # Create a data frame to plot the graph
# elbow <-data.frame(2:max_k, wss)
# 
# # Plot the graph with gglop
# ggplot(elbow, aes(x = X2.max_k, y = wss)) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1, 20, by = 1))
# 
# 
# pc_cluster_2 <-kmeans(Cust_norm, 11)
# pc_cluster_2$cluster
# pc_cluster_2$centers
# pc_cluster_2$size

# plots to compare
p2 <- fviz_cluster(fit_n2, geom = "point", data = Cust_norm) + ggtitle("k = 2")
p3 <- fviz_cluster(fit_n3, geom = "point",  data = Cust_norm) + ggtitle("k = 3")
p4 <- fviz_cluster(fit_n4, geom = "point",  data = Cust_norm) + ggtitle("k = 4")
p5 <- fviz_cluster(fit_n5, geom = "point",  data = Cust_norm) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow = 2)

# Final model - fit2 with 5 clusters is selected as optimal clusters are shown through both Elbow and 
# Silhoutte score methods. The Within cluster Sum of sqrs. by cluster is 89.4%

fviz_cluster(fit_n5, data = Cust_data5,
             palette = c("blue", "green", "purple",  "red", "black"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# library(cluster)
# clusplot(Cust_data_scale, fit1$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)

# Final plot 3
library(fpc)
plotcluster(Cust_norm, fit_n5$cluster)

str(Cust_data5)
# ggplot(Cust_norm,aes(Units,Sales))+ geom_point(aes(color = as.factor(fit_n3$cluster)),size=4)
# ggplot(Cust_norm,aes(Sales, Unique_SKUs))+ geom_point(aes(color = as.factor(fit_n3$cluster)),size=4)
# ggplot(Cust_norm,aes(Sales, Average_Order_Value))+ geom_point(aes(color = as.factor(fit_n3$cluster)),size=4)
# ggplot(Cust_norm,aes(Sales, Frequency))+ geom_point(aes(color = as.factor(fit_n3$cluster)),size=4)

# get cluster means
# aggregate(Cust_data_scale,by=list(fit2$cluster),FUN=mean)

# append cluster assignment
# Cust_data_combined<- data.frame(Cust_data3, Cluster = fit2$cluster)
# View(Cust_data_combined)

Cust_data_combined2<- data.frame(Cust_data3, Kmeans_Cluster = fit_n5$cluster)
# View(Cust_data_combined2)

# library(mclust)
# mfit<- Mclust(Cust_data_combined2)
# plot(mfit)

names(Cust_data_combined2)
str(Cust_data_combined2)

# Customer Profiling :
Customer_Segment<- Cust_data_combined2 %>%
  select(CustomerID, Kmeans_Cluster, Units, Sales, Unique_SKUs, Unique_Orders, Average_Order_Value) %>%
  group_by(Kmeans_Cluster) %>%
  summarise(Unique_Customers = n_distinct(CustomerID), Avg_Units = mean(Units), Avg_Sales = mean(Sales), 
            Avg_Unique_SKUs = mean(Unique_SKUs), Avg_Unique_Orders = mean(Unique_Orders),
            AOV = mean(Average_Order_Value)) %>%
  arrange(Kmeans_Cluster)

Customer_Segment<- as.data.frame(Customer_Segment)
Customer_Segment$Kmeans_Cluster

str(Cust_data_combined2)

# Segment 2 customers - 18
KM_Customer_Segment2<- Cust_data_combined2 %>%
  select(CustomerID, Kmeans_Cluster, Country, Units, Sales, Unique_SKUs, Unique_Orders, Average_Order_Value) %>%
  group_by(CustomerID, Kmeans_Cluster) %>%
  summarise(Unique_Customers = n_distinct(CustomerID), Avg_Units = mean(Units), Avg_Sales = mean(Sales), 
            Avg_Unique_SKUs = mean(Unique_SKUs), Avg_Unique_Orders = mean(Unique_Orders),
            AOV = mean(Average_Order_Value)) %>%
  filter(Kmeans_Cluster== "2")

KM_Customer_Segment2

# --------------------------------------------------------------------------------------------------

# Hierarchical clustering

# library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# hc_n<- hcut(Cust_norm, hc_method = "euclidean")
# hc_n
# summary(hc.cut)
# # Visualize dendrogram
# fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# # Visualize cluster
# fviz_cluster(hc.cut, ellipse.type = "convex")
# 
# ?fviz_dend

dist_mat<- dist(Cust_data5, method = "euclidian")
# dist_mat
fit_h<- hclust(dist_mat, method = "ward.D")
plot(fit_h)

# Select 4 clusters
sales_clus1<- cutree(fit_h, k = 4) # with 4 clusters
table(sales_clus1)
# 4 clusters looks ideal
# split the clusters further 

sales_clus2<- cutree(fit_h, k = 5) # with 5 clusters
table(sales_clus2)
# split the 

Cust_hier_cluster<- data.frame(Cust_data_combined2, Hier_Cluster_K4 = sales_clus1, Hier_Cluster_K5 = sales_clus2)
str(Cust_hier_cluster)


# Customer Profiling - Hierarchial Clustering with 4 clusters :
Customer_Segment_hier<- Cust_hier_cluster %>%
  select(CustomerID, Hier_Cluster_K4, Units, Sales, Unique_SKUs, Unique_Orders, Average_Order_Value) %>%
  group_by(Hier_Cluster_K4) %>%
  summarise(Unique_Customers = n_distinct(CustomerID), Avg_Units = mean(Units), Avg_Sales = mean(Sales), 
            Avg_Unique_SKUs = mean(Unique_SKUs), Avg_Unique_Orders = mean(Unique_Orders),
            AOV = mean(Average_Order_Value)) %>%
  arrange(Hier_Cluster_K4)

Customer_Segment_hier<- as.data.frame(Customer_Segment_hier)
Customer_Segment_hier


# Customer Profiling - Hierarchial Clustering with 5 clusters :
Customer_Segment_hier2<- Cust_hier_cluster %>%
  select(CustomerID, Hier_Cluster_K5, Units, Sales, Unique_SKUs, Unique_Orders, Average_Order_Value) %>%
  group_by(Hier_Cluster_K5) %>%
  summarise(Unique_Customers = n_distinct(CustomerID), Avg_Units = mean(Units), Avg_Sales = mean(Sales), 
            Avg_Unique_SKUs = mean(Unique_SKUs), Avg_Unique_Orders = mean(Unique_Orders),
            AOV = mean(Average_Order_Value)) %>%
  arrange(Hier_Cluster_K5)

# Comparing Algorithms :
Customer_Segment
Customer_Segment_hier
Customer_Segment_hier2



# --------------------------------------------------------------------------------------

https://uc-r.github.io/hc_clustering

# Agglomerative Hierarchical Clustering

# Dissimilarity matrix
d<- dist(Cust_norm, method = "euclidean")
# d

# Hierarchical clustering using Complete Linkage
hc1<- hclust(d, method = "complete" )
?hclust

# Plot the obtained dendrogram
plot(hc1, cex = 0.2, hang = -1)

?hclust

# Compute with agnes - Agglomerative clustering
hc2<- agnes(Cust_norm, diss = FALSE, metric = "ward.D")
hc2
plot(hc2)
Agglomerative coefficient
hc2$ac
# [1] 0.9942324

# Evaluate Agglomerative clustering methods
# library(purrr)
# 
# m <- c( "average", "single", "complete", "ward")
# names(m) <- c( "average", "single", "complete", "ward")
# 
# #Compare Ward metod with other methods of Agglomerative clustering :
# 
# ac<- function(x) {
#   agnes(Cust_norm, method = x)$ac
# }
# 
# map_dbl(m, ac)
# # average    single  complete      ward 
# # 0.9942324 0.9908237 0.9954206 0.9995569
# 
# # Ward method gives best result
# 
# # Plot
# hc3 <- agnes(Cust_norm, method = "ward")
# pltree(hc3, cex = 0.2, hang = -1, main = "Dendrogram of agnes - Ward method")

# Insight:
# Ward method gives the maximum accuracy ~0.9955

# sub_grp<- cutree(hc2, k = 5)
# table(sub_grp)

# Divisive Hierarchical Clustering

# compute Divisive hierarchical clustering
# hc4<- diana(Cust_norm)
# 
# # Divise coefficient; amount of clustering structure found
# hc4$dc
## [1] 0.9948226

# plot dendrogram
# pltree(hc4, cex = 0.2, hang = -1, main = "Dendrogram of diana")

# Working with Dendrograms

# # Ward's method
# hc5<- hclust(Cust_norm, method = "ward.D2" )
# 
# # Cut tree into 4 groups
# sub_grp <- cutree(hc5, k = 4)
# 
# # Number of members in each cluster
# table(sub_grp)
# ## sub_grp
# ##  1  2  3  4 
# ##  7 12 19 12
# 
# USArrests %>%
#   mutate(cluster = sub_grp) %>%
#   head
# ##   Murder Assault UrbanPop Rape cluster
# ## 1   13.2     236       58 21.2       1
# ## 2   10.0     263       48 44.5       2
# ## 3    8.1     294       80 31.0       2
# ## 4    8.8     190       50 19.5       3
# ## 5    9.0     276       91 40.6       2
# ## 6    7.9     204       78 38.7       2
# 
# 
# plot(hc5, cex = 0.6)
# rect.hclust(hc5, k = 4, border = 2:5)
# 
# fviz_cluster(list(data = df, cluster = sub_grp))
# 

# Cut agnes() tree into 4 groups

# hc_a <- agnes(Cust_norm, method = "ward")
# hc_a$ac
# cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(Cust_norm)
hc_d$ac
cutree(as.hclust(hc_d), k = 4)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)


dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)


