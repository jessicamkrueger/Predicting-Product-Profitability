library(caret)
library(dplyr)
library(gridExtra)
library(rpart.plot)

setwd("~/Ubiqum/Ubiqum Work/Project 2/Task 3 - Predicting Sales Volume/Original Data Sets")
existingG_A_I <- read.csv2("existing_Gender_Age_Instore.csv")
existingRP_EI_D <- read.csv2("existing_RelativePice_environmentalImpact_Durability.csv")

#are column names the same for each data set?
colnames(existingG_A_I) == colnames(existingRP_EI_D)

#join tables based on all matching columns of both data sets 
xNames <- colnames(existingG_A_I) #create vector of column names
xNames <- xNames[1:19] #select first 19 column names bc they match to other data set
existingFull <- full_join(existingG_A_I, existingRP_EI_D, by = c(xNames)) #join data
#on all matching column names

str(existingFull) #understand structure of data set
summary(existingFull) #look at summary of data set

#filtering to just 4 specific product types
Products <- c("PC", "Netbook", "Smartphone", "Laptop")
ProductsOnly <- filter(existingFull, Product_type %in% Products)

#adjust variable types to be factors or ordered factors
existingFull$Product_ID <- as.factor(existingFull$Product_ID)
existingFull$X <- as.factor(existingFull$X)
existingFull$Depth <- as.numeric(existingFull$Depth)
existingFull$Gender <- as.factor(existingFull$Gender)
existingFull$Age <- as.ordered(existingFull$Age)
existingFull$In_store <- as.factor(existingFull$In_store)
existingFull$Environment_Impact <- as.factor(existingFull$Environment_Impact)
existingFull$Would_consumer_recomend__product <- as.ordered(existingFull$Would_consumer_recomend__product)

#initial exploration of variables - histograms and barcharts
ggplot(existingFull, aes(Product_type)) +
  geom_bar()
ggplot(existingFull, aes(Age)) +
  geom_bar()
ggplot(existingFull, aes(Gender)) +
  geom_bar()
ggplot(existingFull, aes(In_store)) +
  geom_bar()
ggplot(existingFull, aes(Environment_Impact)) +
  geom_bar()
ggplot(existingFull, aes(Relative_Price)) +
  geom_histogram(color = "black", binwidth = .25) +
  scale_x_continuous(breaks = seq(-3, 3.5, .25))
ggplot(existingFull, aes(Durability_standard)) +
  geom_histogram(color = "black", binwidth = .05) +
  scale_x_continuous(breaks = seq(0, 1, .1))
ggplot(existingFull, aes(Volume)) +
  geom_histogram(color = "black", binwidth = 100) 
ggplot(existingFull, aes(X5Stars)) +
  geom_histogram(color = "black", binwidth = 50) 
ggplot(existingFull, aes(X4Stars)) +
  geom_histogram(color = "black", binwidth = 50) 
ggplot(existingFull, aes(X3Stars)) +
  geom_histogram(color = "black", binwidth = 50)
ggplot(existingFull, aes(X2Stars)) +
  geom_histogram(color = "black", binwidth = 50) 
ggplot(existingFull, aes(X1Stars)) +
  geom_histogram(color = "black", binwidth = 50) 
ggplot(existingFull, aes(Positive_service_review)) +
  geom_histogram(color = "black", binwidth = 50) 
ggplot(existingFull, aes(Negative_service_review)) +
  geom_histogram(color = "black", binwidth = 50) 

#boxplots of each variable to locate potential outliers
boxplot(ProductsOnly$Volume, horizontal = TRUE) 
#boxplot of the sales volume for each of the 4 products we want to predict
ggplot(ProductsOnly, aes(x= Product_type, y = Volume)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1500, 100))
ggplot(ProductsOnly, aes(x= Product_type, y = Positive_service_review)) +
  geom_boxplot()

#decision tree on new data set
dTree <- rpart(Volume ~ ., data = readyData3, cp = .02)
rpart.plot(dTree)

#further data exploration...all variables have long tails
ggplot(readyData6, aes(X5Stars)) +
  geom_histogram(color = "black", binwidth = 10) 
ggplot(readyData6, aes(X4Stars)) +
  geom_histogram(color = "black", binwidth = 10) 
ggplot(readyData6, aes(Positive_service_review)) +
  geom_histogram(color = "black", binwidth = 5) 
ggplot(readyData6, aes(Negative_service_review)) +
  geom_histogram(color = "black", binwidth = 2) 
ggplot(readyData6, aes(Volume)) +
  geom_histogram(color = "black", binwidth = 50) 
