library(caret)
data("diamonds")
print("20BCE0348")

summary(diamonds)
head(diamonds)
sd(diamonds$price)

library(ggplot2)
# Horizontal Bar plot
barplot(diamonds$carat, diamonds$price, type='h', xlab='Carat',
        ylab = 'Price', main = 'Carat Vs Price(20BCE0348)', col = 'blue', horiz=TRUE)
# Vertical Bar plot
barplot(diamonds$carat, diamonds$price, type='h', xlab='Carat',
        ylab = 'Price', main = 'Carat Vs Price(20BCE0348)', col = 'red')
# Single box plot
boxplot(diamonds$cut)
# Multiple box plot
boxplot(diamonds[,1:4], main="20BCE0348") # all rows and first 4 cols

barplot(diamonds$color, type='h', main="20BCE0348")

barplot(diamonds$carat, diamonds$price, type='h', xlab='Carat',
        ylab = 'Wind', main = 'Ozone Vs Wind', col = 'blue', horiz=TRUE)
barplot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
        ylab = 'Wind', main = 'Ozone Vs Wind', col = 'blue')


plot(diamonds$carat, diamonds$price, main="20BCE0348")

ggplot(data=diamonds, mapping=aes(x=carat,y=price))+geom_point()+scale_x_log10()+
  scale_y_log10()+ggtitle("20BCE0348")

ggplot(data=diamonds, mapping=aes(x=carat, y=price,color=color,size=color))+
  geom_point()+scale_x_log10()+
  scale_y_log10()+ggtitle("20BCE0348")

ggplot(data=diamonds, mapping=aes(x=carat,y=price,color=color))+geom_point()+scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~color)+ggtitle("20BCE0348")

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()


my_data <- read.csv(file = "D:\\VIT\\SEM 6\\DataVisualization&Presentation\\lab\\auto-mpg.csv", stringsAsFactors = FALSE)
summary(my_data)
boxplot(my_data[,c(1,2,6,7)], col=brewer.pal(8,"Set3"), main = "20BCE0348")
colorRampPalette(brewer.pal(9,"Blues"))(100)


data("USArrests")
summary(USArrests)



library(reshape2)
cormat <- round(cor(USArrests),2)
head(cormat)
cormat_onefield <- melt(cormat)
head(cormat_onefield)
library(ggplot2)
ggplot(data = cormat_onefield, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ggtitle("Correlation (20BCE0348)")

ggplot(data = cormat_onefield, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ggtitle("20BCE0348")
