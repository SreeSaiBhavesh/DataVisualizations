data("airquality")
str(airquality)
head(airquality)
tail(airquality)
head(airquality, 8)
is.na(airquality) # shows entire data 
summary(airquality) # give statistical details and no of na values in the dataset

# plots====
plot(airquality$Ozone)
par(mfrow=c(2,2), las =0, mar=c(1,5,5,1))
plot(airquality$Ozone, airquality$Wind, type='h', main = "Histogram (20BCE0348)")
plot(airquality$Ozone, airquality$Wind, type='b', main = "Point and Line (20BCE0348)")
plot(airquality$Ozone, airquality$Wind, type='l', main = "Line (20BCE0348)")
plot(airquality$Ozone, airquality$Wind, type='p', main = "Point (20BCE0348)")
# type=h(histogram), type=b(point and line), type=l(line), type=p(point)
# Adding labels
plot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
     ylab = 'Wind', main = 'Ozone Vs Wind', col = 'blue')

par(mfrow=c(2,2), las =0, mar=c(1,5,5,1))
# Horizontal Bar plot
barplot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
     ylab = 'Wind', main = 'Ozone Vs Wind (20BCE0348)', col = 'blue', horiz=TRUE)
# Vertical Bar plot
barplot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
     ylab = 'Wind', main = 'Ozone Vs Wind (20BCE0348)', col = 'blue')
# Single box plot
boxplot(airquality$Solar.R)+ggtitle("singleBoxplot (20BCE0348)")
# Multiple box plot
boxplot(airquality[,1:4])+ggtitle("MultipleBoxplot (20BCE)348)") # all rows and first 4 cols
# Grid of charts
# mar to position the plot in its specified box.
# mfrow to have multiple plots on same window
# las = 0 -> Normal (horizontal labels on x-axis and vertical labels on y-axis)
# las = 1 -> horizontal labels on both axis
# las = 2 -> vertical labels on x-axis and horizontal labels on y-axis (opposite to normal)
# las = 3 -> Vertical labels on both axis
# las takes position in order of bottom, left, top and right
 # in mfrow first no for no of rows and second no for no of cols
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind, type='l')
barplot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
        ylab = 'Wind', main = 'Ozone Vs Wind', col = 'blue', horiz=TRUE)
barplot(airquality$Ozone, airquality$Wind, type='h', xlab='Ozone Concentration',
        ylab = 'Wind', main = 'Ozone Vs Wind', col = 'blue')

library(ggplot2)
View(mtcars)
attach(mtcars)
# if we give attach(dataset_name) no need to give dataset_name$col_name everytime when we want
# columns data, we can just directly give col_name if we executed attach
gear
detach(mtcars)

gear_fac= factor(gear,levels=c(3,4,5))
plot(gear_fac)
as.factor(cyl)

# GGPlot ====
ggplot(data=mtcars, mapping=aes(x=wt,y=mpg))+geom_line()+
  xlab ('Weight')+ylab('Milleage')+ggtitle("Visualization (20BCE0348)")
