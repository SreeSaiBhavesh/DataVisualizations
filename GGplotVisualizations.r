#Plotting and color in R
barplot(1:10,col=rainbow(7))

#colors - 1,2 and 3
x<- rnorm(30)
y<-rnorm(30)
seq(from=1,to=10,by=2)
rep(x=1:3,times=2) #rep(x=1:3,each=2)
rep(x=1:3,each=5)
plot(x,y,col=rep(x=1:3,each=10),
     pch=25)  # 25 give triangle shape ,all numbers give different shapes in graph

paste("Group",1:3)
legend("bottomright",
       legend = paste("Group",1:3),
       pch=19,
       bty='n')

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggplot2")
library(ggplot2)
display.brewer.all()
display.brewer.all(colorblindFriendly = T)

#visualization using RcolorBrewer
install.packages("gridExtra")
library(gridExtra)
mpg
View(mpg)
ggplot(mpg,aes(x=cty))+
  geom_density(aes(fill=factor(cyl)))+ggtitle("20BCE0348")

p1<-ggplot(mpg,aes(x=cty))+
  geom_density(aes(fill=factor(cyl),
                   alpha=0.8))+
  labs(title = "Dark 2",
       x="City milage",
       fill='#Cylinder')+
  scale_fill_brewer(palette = "Dark 2")+ggtitle("20BCE0348")

p2<-ggplot(mpg,aes(x=cty))+
  geom_density(aes(fill=factor(cyl),
                   alpha=0.8))+
  labs(title = "Pastel2",
       x="City milage",
       fill='#Cylinder')+
  scale_fill_brewer(palette = "Pastel2")+ggtitle("20BCE0348")

p3<-ggplot(mpg,aes(x=cty))+
  geom_density(aes(fill=factor(cyl)))+
  labs(title = "Accent",
       x="City milage",
       fill='#Cylinder')+
  scale_fill_brewer(palette = "Accent")+ggtitle("20BCE0348")

p4<-ggplot(mpg,aes(x=cty))+
  geom_density(aes(fill=factor(cyl)))+
  labs(title = "Set1",
       x="City milage",
       fill='#Cylinder')+
  scale_fill_brewer(palette = "Set1")+ggtitle("20BCE0348")

#DArk2;;;Pastel2;;;Accent;;;set1
grid.arrange(p1,p2,p3,p4,nrow=2)

#colorful map
install.packages("maps")
install.packages("viridis")
install.packages("dplyr")

library(maps)
library(viridis)
library(dplyr)
USArrests 
#if u want store in arrests object for east to access
arests <- USArrests 
View(arests)
arests$region<-tolower(rownames(USArrests))
View(arests)

#Retrive the states map
states_map <- map_data("state")
View(states_map)
#join tables arests and states
arests_map <- left_join(x=states_map,
          y=arests,
          by='region')
View(arests_map)

#Create map
ggplot(arests_map,
       aes(x=long,
           y=lat,
           group=group))+
  geom_polygon(aes(fill=Assault))+ggtitle("20BCE0348")
#for seperation
ggplot(arests_map,
       aes(x=long,
           y=lat,
           group=group))+
  geom_polygon(aes(fill=Assault),
               color='white')+
  labs("Assault arests")+ggtitle("20BCE0348")


