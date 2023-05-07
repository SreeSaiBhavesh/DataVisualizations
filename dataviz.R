
# --------------- Text Analysis -------------------

install.packages("textplot")
library(textplot)
install.packages("textdata")
library(textdata)
install.packages('corpus')
library(corpus)
install.packages("cran")
library(cran)
install.packages("tidytext")
library(tidytext)
install.packages("wordcloud")
library(wordcloud)
library(tidyverse)
library(tibble)
install.packages("readr")
library(readr)


#1. load shakespeare.rda into r environment

load("D:\\VIT\\SEM_6\\DataVisualization&Presentation\\lab\\Codes\\shakespeare.rda")
View(shakespeare)

#2. Pipe the shakespeare data frame to the next line
# Use count to find out how many titles/types there are
shakespeare %>% count(title)
print("20BCE0348")

#3. Load tidytext/ tidyverse
library(tidyr)
library(ggplot2)
library(readr)
library(textdata)
library(tibble)
library(dplyr)

linenumbers <- c(1:141067)
tidy_shakespeare<-cbind(tidy_shakespeare,linenumbers)
colnames(tidy_shakespeare)[3] <- "linenumber"

tidy_shakespeare2<-cbind(tidy_shakespeare2,linenumbers)
colnames(tidy_shakespeare2)[4] <- "linenumber"
#4. create and object tidy_shakespeare
# Group by the titles of the plays
# Define a new column linenumber
# Transform the non-tidy text data to tidy text data
library(dplyr)
library(tidyr)

# Group by title and define a new column line number
tidy_shakespeare <- shakespeare %>%
  group_by(title) %>% mutate(linenumber = row_number())

# Transform non-tidy text data to tidy text data
tidy_shakespeare <- tidy_shakespeare %>%
  unnest_tokens(word, text)
tidy_shakespeare

#5. Pipe the tidy Shakespeare data frame to the next line
# Use count to find out how many times each word is used
library(stringr)
text <- shakespeare["text"]
text <- text %>% mutate(words = str_split(text, "\\s+"))
text <- text %>% unnest(words)
word_counts <- text %>% count(words)
word_counts

library(dplyr)
tidy_shakespeare %>%
  count(word, sort = TRUE) %>% group_by(type) %>% ungroup()
print("20BCE0348")
tidy_shakespeare

#6. Sentiment analysis of tidy_shakespeare assin to object shakespeare_sentiment
# Implement sentiment analysis with the "bing" lexicon
?get_sentiments
library(tidytext)

# Get the bing lexicon
lexicon <- get_sentiments("bing")

# Perform sentiment analysis
shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, linenumber) %>%
  ungroup()
shakespeare_sentiment
print("20BCE0348")

#7. shakespeare_sentiment
# Find how many positive/negative words each play has
library(dplyr)

shakespeare_sentiment %>%
  group_by(title) %>%
  summarize(num_positive = sum(sentiment > 0),
            num_negative = sum(sentiment < 0))


#8. Tragedy or comedy from tidy_shakespeare  assign to sentiment_counts
# Implement sentiment analysis using the "bing" lexicon
# Count the number of words by title, type, and sentiment
library(dplyr)

# Load the bing lexicon
lexicon <- get_sentiments("bing")

tidy_shakespeare2 <- shakespeare %>%
  group_by(title, type) %>% mutate(linenumber = row_number())

tidy_shakespeare2 <- tidy_shakespeare2 %>%
  unnest_tokens(word, text)

tidy_shakespeare2

shakespeare_sentiment2 <- tidy_shakespeare2 %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, linenumber) %>%
  ungroup()
shakespeare_sentiment2


# Join with the lexicon and group by title, type, and sentiment
sentiment_counts <- tidy_shakespeare2 %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, type, sentiment) %>%
  summarize(count = n()) %>%
  ungroup()

sentiment_counts
#9. from sentiment_counts
# Group by the titles of the plays
# Find the total number of words in each play
# Calculate the number of words divided by the total
# Filter the results for only negative sentiment then arrange percentage in asc order
library(dplyr)

# Group by title and calculate the total number of words in each play
total_words <- sentiment_counts %>%
  group_by(title) %>%
  summarize(total = sum(count))

# Calculate the percentage of negative words for each play
negative_words <- sentiment_counts %>%
  group_by(title) %>%
  filter(sentiment == "negative") %>%
  summarize(negative = sum(count))

percentages <- total_words %>%
  inner_join(negative_words, by = "title") %>%
  mutate(percentage = negative / total * 100) %>%
  filter(!is.na(percentage)) %>%
  arrange(percentage)

total_words
negative_words
percentages
#10 Most common positive and negative words and assign to word_could
# Implement sentiment analysis using the "bing" lexicon
# Count by word and sentiment
library(dplyr)

# Load the bing lexicon
lexicon <- get_sentiments("bing")

# Join with the lexicon
shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(lexicon, by = "word")

# Count by word and sentiment
word_count <- shakespeare_sentiment %>%
  count(word, sentiment, sort = TRUE)

# Filter for positive and negative words
word_cloud <- word_count %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  slice_max(n = 20, order_by = n)

wordcloud(words = word_cloud$word, freq = word_cloud$n, min.freq = 1,
          max.words=120, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#11. extract top 10 words from word_counts and assing to top_words
# Group by sentiment
# Take the top 10 for each sentiment and ungroup it
# Make word a factor in order of n
library(dplyr)

top_words <- word_count %>%
  group_by(sentiment) %>%
  slice_max(n = 10, order_by = n) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = unique(word[order(n, decreasing = TRUE)])))

top_words
#12 Use aes() to put words on the x-axis and n on the y-axis
# Make a bar chart with geom_col()
# facet_wrap for sentiments and apply scales  as free
#Move x to y and y to x
library(ggplot2)

ggplot(top_words, aes(x = n, y = word, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free") +
  scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "red")) +
  labs(title = "Top 10 Positive and Negative Words in Shakespeare's Plays",
       x = "Frequency", y = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 10, margin = margin(r = 5)),
        axis.text.x = element_text(size = 10, margin = margin(t = 5)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)))



#13 from tidy_shakespeare Calculating a contribution score
# Count by title and word
# Implement sentiment analysis using the "afinn" lexicon
# Group by title
# Calculate a contribution for each word in each title
library(dplyr)

# Load the AFINN lexicon
lexicon <- get_sentiments("afinn")

# Count by title and word
word_counts <- tidy_shakespeare %>% unnest_tokens(word, text) %>% inner_join(lexicon, by = "word") %>% 
  group_by(title, word) %>% summarise(n = n(), score = sum(value)) %>% ungroup()

# Group by title
title_counts <- word_counts %>%
  group_by(title) %>%
  summarise(total_words = sum(n), total_score = sum(score))

# Calculate a contribution for each word in each title
contribution_scores <- word_counts %>%
  left_join(title_counts, by = "title") %>%
  mutate(contribution = score / total_score * 100)
contribution_scores



# --------------- USARRESTS ------------------

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






# --------------- Color Map ----------------------

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



# --------------- GapMinder ----------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)
View(gapminder)
str(gapminder)
glimpse(gapminder) #dplyr required, in dplyr we can use pipe(%>%)
# Extract the continent Asia
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




# --------------- GGPLOT ---------------

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

