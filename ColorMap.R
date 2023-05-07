# Loading Dataset

data = read.csv("D://VIT//SEM_6//DataVisualization&Presentation//sobar-72.csv")
View(data)
sum(is.na(data))
structure(data)
# Question1 - Barplots
# i. With ggplot2

library(ggplot2)
ggplot(data, aes(x = factor(behavior_personalHygine), y = behavior_eating, fill = behavior_eating)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_classic()+ggtitle("20BCE0348 - Barplot with sequential color coding")

# ii. Coordinate Flip
library(ggplot2)

ggplot(data, aes(x = factor(behavior_personalHygine), y = behavior_eating, fill = behavior_eating)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_classic()+coord_flip()+ggtitle("20BCE0348 - Barplot with diverging color coding")

# iii. Error bars
library(ggplot2)

ggplot(data, aes(x = factor(behavior_personalHygine), y = behavior_eating, fill = behavior_eating)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = behavior_eating - mean(behavior_eating), ymax = behavior_eating + mean(behavior_eating)), position = position_dodge(width = 0.9)) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(data$behavior_eating)) +
  theme_classic()+ggtitle("20BCE0348 - Barplot with error bars with bivariate color coding")


#iv. Stacked bars

ggplot(data, aes(x=empowerment_knowledge, y=behavior_eating, fill="subgroup1"))+geom_bar(stat = "identity")+
  geom_bar(aes(x=empowerment_knowledge, y=behavior_sexualRisk, fill="subgroup2"), position = "stack", stat = "identity")+
  geom_bar(aes(x=empowerment_knowledge, y=intention_aggregation, fill="subgroup3"), position = "stack", stat = "identity")+
  scale_fill_manual(values = c("subgroup1" = "red","subgroup2"= "blue","subgroup3"= "green"))+
  ggtitle("20BCE0348 - stacked Bar plot")+
  xlab("Cervix")+
  ylab("parameters")



# Scatter Plot
# With empty shape/ blank diamond

ggplot(data, aes(factor(behavior_personalHygine), behavior_eating)) +
  geom_point(shape = 23,
             color = "black", size = 3)+
  theme_minimal()+ggtitle("20BCE0348 - Empty shape / Blank diamond")

ggplot(data, aes(behavior_personalHygine, behavior_eating)) +
  geom_point(shape = 25, fill = "blue",
             color = "black", size = 3)+
  theme_minimal()+ggtitle("20BCE0348 - 180 degree rotated traingle")



#install.packages("pheatmap")
#library("pheatmap")
#pheatmap(data[2:5,2:5],cutree_rows=4, main = "20BCE0348") 

library(reshape2)
# Divergence Heatmap
# creating correlation matrix
corr_mat <- round(cor(data[6:10]),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
# head(melted_corr_mat)

# Create the heatmap
ggplot(data = melted_corr_mat, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "cyan", mid = "white", high = "red") +
  theme_minimal()+ggtitle("Heatmap Diverging - 20BCE0348")


# Scalar Heatmap

# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(data[2:5]),2)
data[2:5]
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
# head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile()+ggtitle("Heatmap Scalar - 20BCE0348")


# Pie3D
# As my data is completely numerical, I am plotting the sample Pie3D
# install and load the required package
install.packages("plotrix")
library(plotrix)

# create a sample dataset
data <- c(25, 40, 20, 15)

# create the 3D pie chart
pie3D(data, labels = c(25, 40, 20, 15) ,explode=0.1, main="3D Pie Chart-20BCE0348")

