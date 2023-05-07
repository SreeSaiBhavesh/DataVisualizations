data = read.csv("D:\\VIT\\SEM_6\\DataVisualization&Presentation\\lab\\Codes\\Market_Basket_Optimisation.csv", header=FALSE)
data2 = read.csv("/home/matlab/Mall_Customers.csv")
View(data)
dim(data)
str(data)
# per day ;et max chances of repetiotion of purchse among people be support=4 and for 7 days (4*7)/7501 ~ 0.004 support.
install.packages('arules')
library(arules) # for sparse matrix identication and performing
dataset = read.transactions(file = "D:\\VIT\\SEM_6\\DataVisualization&Presentation\\lab\\Codes\\Market_Basket_Optimisation.csv", sep = ",",rm.duplicates = T)
# Output shows only 1 item has 5 duplicated among 7501 rows of data.
# 119 unique items were present in total 7501 rows. 
summary(dataset)
# only 3.28% is data remaining we have empty data.
itemFrequencyPlot(x = dataset, topN=10) # Displays top 10 items in dataset wrt item frequnecy bought by customers
# Apriori Algorithm
rules = apriori(data = dataset,
        parameter = list(support = 0.0037, confidence = .3))
# Visualising the result
inspect(sort(rules, by='lift')[1:10]) # display the contents

#K-Means Clustering
View(data2)
summary(data2)
dataset2 = data2[,c(4,5)]
# dataset2 = data2[4:5] - another code
dataset2
library(cluster) # To form cluster
set.seed(123)
wcss = vector()
for(i in 1:10)
  wcss[i] = sum(kmeans(dataset2, i)$withinss)
plot(1:10,wcss, type='b')
# Clustering
kmeans = kmeans(x=dataset2, centers = 5)
y_kmeans = kmeans$cluster
clusplot(dataset2, y_kmeans)
