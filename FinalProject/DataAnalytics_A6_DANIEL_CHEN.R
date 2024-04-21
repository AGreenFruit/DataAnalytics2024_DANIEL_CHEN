setwd('C:/Users/danie/Documents/DataAnalytics2024_DANIEL_CHEN/FinalProject')
data <- read.csv('Real_Estate_Sales_2001-2021_GL_20240326.csv')
View(data)

library(ggplot2)
library(ggpubr)
library(dplyr)
library(rpart)
library(rpart.plot)

# Drop columns that aren't necessary
df <- data.frame(data$List.Year,data$Town,data$Assessed.Value,data$Sale.Amount,data$Property.Type,data$Residential.Type)
colnames(df) <- c("Year","Town","Assessed.Value","Sale.Amount","Property.Type","Residential.Type")
View(df)

######################
##### Simple EDA #####
######################

# Year distribution
ggplot(data=df,aes(x=Year))+geom_bar()

# Top 5 property types by count
df %>% filter(Property.Type!="") %>% 
  count(Property.Type) %>% arrange(desc(n)) %>%
  slice(1:5) %>% ggplot(aes(Property.Type,n))+geom_col()

# Bar chart of property type
ggplot(data=df,aes(x=Property.Type))+geom_bar()

######################
###### Analysis ######
######################

# Data filtering - Keep certain property types
df <- df %>% filter(Property.Type=="Single Family"|
              Property.Type=="Residential"|
              Property.Type=="Condo"|
              Property.Type=="Two Family"|
              Property.Type=="Three Family") %>%
  as.data.frame()

df$Property.Type <- as.factor(df$Property.Type)

# Remove outliers (Houses w/ Assessed value over 100 mil or < 100)
df <- df[df$Assessed.Value < 100000000,]
df <- df[df$Assessed.Value > 100,]

# Houses w/ Sale amount over 100 mil or < 100
df <- df[df$Sale.Amount < 100000000,]
df <- df[df$Sale.Amount > 100,]
View(df)


# Year distribution after filtering
ggplot(data=df,aes(x=Year))+geom_bar()

# Assessed Value distribution
sa_plot <- ggplot(data=df,aes(x=Sale.Amount))+geom_histogram(binwidth=100000)
av_plot <- ggplot(data=df,aes(x=Assessed.Value))+geom_histogram(binwidth=100000)
ggarrange(sa_plot,av_plot)

# Assessed Value vs Sale Amount
ggplot(data=df,aes(x=Assessed.Value,y=Sale.Amount))+geom_point()
cor(df$Assessed.Value,df$Sale.Amount)

######################
####### Models #######
######################

#### Multiple Linear Regression ####
# Separate training and test sets by year
train <- df[df$Year<2019,]
test <- df[df$Year>2018,]

model1 <- lm(Sale.Amount~Assessed.Value+Year,data=train)
summary(model1)

model2 <- lm(Sale.Amount~Assessed.Value+Year,data=test)
summary(model2)

predictions <- predict(model1,newdata=test)
mean((predictions-test$Sale.Amount)**2)

model3 <- lm(Assessed.Value~Sale.Amount+Year,data=train)
summary(model3)

predictions <- predict(model3,newdata=test)
mean((predictions-test$Assessed.Value)**2)

model4 <- lm(Sale.Amount~Year,data=train)
summary(model4)

model5 <- lm(Sale.Amount~Year,data=test)
summary(model5)

model6 <- lm(Sale.Amount~Year,data=df)
summary(model6)

plot(df$Year,df$Sale.Amount, main= "Year vs Sale Amount",ylab="Sale Amount",xlab="Year")
abline(model4,col='blue')
abline(model5,col='red')
abline(model6,col='green')


#### Decision Tree ####
set.seed(111)
train_ind <- sample(seq_len(nrow(df)), size = floor(0.80 * nrow(df)))
train <- df[train_ind,]
test <- df[-train_ind,]

tree1 <- rpart(Sale.Amount~Year,data=train,control=rpart.control(minbucket=6, cp=0.002))
rpart.plot(tree1)

tree2 <- rpart(Assessed.Value~Year,data=train,control=rpart.control(minbucket=6, cp=0.00026))
rpart.plot(tree2)

tree3 <- rpart(Sale.Amount~Year+Property.Type,data=train,control=rpart.control(minbucket=6, cp=0.0001))
rpart.plot(tree3)

predictions <- predict(tree3,newdata=test)
mean((predictions-test$Sale.Amount)**2)