library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)
View(gapminder)
str(gapminder)
glimpse(gapminder) #dplyr required, in dplyr we can use pipe(%>%)
# Extract the continent asia
gapminder %>% filter(continent == 'Asia')
filter(gapminder, continent=='Asia')

# Extract year 2002 and country china
gapminder %>% filter(year==2002, country=='China')

# Extract year 1997 and display population in desc order
gapminder %>% filter(year==1997) %>% arrange(desc(pop))

# LifeExp in months
g <- gapminder %>% mutate(lifeExpMonths = lifeExp*12) # This creates a new column
g
g2 <- gapminder %>% mutate(lifeExp = lifeExp*12) # Updates existing column
g2

# Year 2007, lifeExp Months arrange in desc
g %>% filter(year==2007) %>% arrange(desc(lifeExpMonths))
gapminder %>% filter(year==2007) %>% mutate(lifeExp=lifeExp*12) %>% arrange(desc(lifeExp))

# Create an object gapminder_1952
gapminder_1952 <- gapminder %>% filter(year==1952) 
View(gapminder_1952)

# Plot x-pop and y-gdppercap - boxplot
ggplot(data=gapminder_1952, mapping=aes(x=pop,y=gdpPercap))+geom_boxplot()+ggtitle("20BCE0348")

# if we use geom_point(Scatterplot) - to find data pts of outliers
# scale_x_log10 and scale_y_log10 gives zoom in plot for easy understanding of points.
ggplot(data=gapminder_1952, mapping=aes(x=pop,y=gdpPercap))+geom_point()+scale_x_log10()+
  scale_y_log10()+ggtitle("20BCE0348")

# Scatter plot - x-pop, y-lifeExp, color-continent, size=gdppercap
ggplot(data=gapminder_1952, mapping=aes(x=pop, y=lifeExp,color=continent,size=gdpPercap))+
  geom_point()+scale_x_log10()+
  scale_y_log10()+ggtitle("20BCE0348")
# Here data overlapping takes place if we increase size of data more overlaps occurs

# subgroup continent - facet_wrap solves overlap problems to some extent with splitting graphs
ggplot(data=gapminder, mapping=aes(x=pop,y=lifeExp,color=continent))+geom_point()+scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~continent)+ggtitle("20BCE0348")

# sub-graph based on year
ggplot(data=gapminder, mapping=aes(x=pop, y=lifeExp, color=continent))+geom_point()+
  scale_x_log10()+scale_y_log10()+facet_wrap(~year)+ggtitle("20BCE0348")

# Summarize median lifeExp for india
gapminder %>% filter(country=='India') %>% summarize(MedianLifeExp_India = median(lifeExp))

# For all continents -> median lifeExp
gapminder %>% group_by(continent) %>% summarize((MedianLifeExp_continents = median(lifeExp)))

# For all continents and each year -> median lifeExp
gapminder %>% group_by(continent, year) %>% summarize((MedianLifeExp_continents = median(lifeExp)))

# medianlifeExp yearwise assign to by_year object
by_year <- gapminder %>% group_by(year) %>% summarize(MedianLifeExp_years = median(lifeExp))
by_year

# medianlifeExp of continents yearwise assign to by_year2 object
by_year2 = gapminder %>% group_by(continent, year) %>% summarize((MedianLifeExp_continents = median(lifeExp)))
by_year2                                                

# Line/Scatter plot for year and MedianLifeExp
ggplot(data=by_year, mapping = aes(x=year, y=MedianLifeExp_years))+geom_point()+ggtitle("20BCE0348")
ggplot(data=by_year, mapping = aes(x=year, y=MedianLifeExp_years))+geom_line()+
  expand_limits(y=0)+ggtitle("20BCE0348") # by expand_limits we can start x or y values from our given numbers.
