
setwd("~/Ubiqum/Project 2/Task 3 - Predicting Sales Volume/R Files/RDS Files")
load("Teamdata.RDS")

library(caret)
library(dplyr)
library(gridExtra)
library(rpart.plot)
library(bnstruct)
library(VIM)
library(tidyr)

#Original team data set
Team_Data <- data
Team_Data$Product_type <- as.factor(Team_Data$Product_type)
Team_Data$Product_ID <- as.factor(Team_Data$Product_ID)
Team_Data$Depth <- as.numeric(Team_Data$Depth)
Team_Data$Gender <- as.factor(Team_Data$Gender)
Team_Data$Age <- as.ordered(Team_Data$Age)
Team_Data$In_store <- as.factor(Team_Data$In_store)
Team_Data$Environment_Impact <- as.factor(Team_Data$Environment_Impact)

#Removing  Printer (#166), Printer Supplies (#129), Game Console (#363)
Team_Data2 <- filter(Team_Data, Product_ID != 166, 
                     Product_ID != 129, Product_ID != 363)

#find zeros in dataset
zeros <- filter(Team_Data2, Volume == 0 |Weigth == 0 | Depth == 0 | Width == 0 | Heigth == 0)

#plot distribution of zeros
ggplot(zeros, aes(Product_type)) +
  geom_bar()
#mostly printer supplies and a few game consoles, smartphones, software

#Creating new data set that removes all zeros for weight & dimension
DimZeros <- filter(Team_Data2, Weigth == 0 | Depth == 0 | Width == 0 | Heigth == 0)
ZeroIDs <- DimZeros$Product_ID
noZeroData <- filter(Team_Data2, !(Product_ID %in% ZeroIDs))

#changing Depth to numeric
Team_Data$Depth <- as.numeric(Team_Data$Depth)
Team_Data2$Depth <- as.numeric(Team_Data2$Depth)
noZeroData$Depth <- as.numeric(noZeroData$Depth)

#plot dimension and weight variables against volume to understand relationship
p1 <- ggplot(noZeroData, aes(x=Weigth, y=Volume)) +
  geom_point(alpha = 1/5)
p2 <- ggplot(noZeroData, aes(x=Depth, y=Volume)) +
  geom_point(alpha = 1/5) +
  coord_cartesian(xlim = c(0, 35))
p3 <- ggplot(noZeroData, aes(x=Width, y=Volume)) +
  geom_point(alpha = 1/5) +
  coord_cartesian(xlim = c(0, 35))
p4 <- ggplot(noZeroData, aes(x=Heigth, y=Volume)) +
  geom_point(alpha = 1/5)
grid.arrange(p1, p2, p3, p4, ncol = 2)

#correlation matrix
noZeroData2 <- select(noZeroData, Weigth:Heigth, Volume)
cor(noZeroData2)
#output
#Weigth       Depth       Width      Heigth     Volume
#Weigth  1.0000000  0.15056468  0.46259099  0.58718539 -0.1754556
#Depth   0.1505647  1.00000000 -0.05767379 -0.04294773  0.3232194
#Width   0.4625910 -0.05767379  1.00000000  0.48557009 -0.1938642
#Heigth  0.5871854 -0.04294773  0.48557009  1.00000000 -0.1212161
#Volume -0.1754556  0.32321937 -0.19386416 -0.12121614  1.0000000


#decision tree
dTree <- rpart(Volume ~ ., data = noZeroData, cp = .02)
rpart.plot(dTree)

#removing printer supplies that have zero dimensions
PSupplies <- filter(DimZeros, Product_type == "Printer Supplies")
PsupID <- PSupplies$Product_ID
#product IDs 282-304

#remove printer supplies with missing dimensions from data set
Team_Data3 <- filter(Team_Data2, !(Product_ID %in% PsupID))

#look at values for Smartphone depths
Phones <- filter(Team_Data3, Product_type == "Smartphone", Weight = !=0)
summary(Phones)
WeightMeanPh <- mean(Phones$Weigth)
DepthMeanPh <- mean(Phones$Depth)
#impute mean for zero values on phones
Team_Data3$Weigth[(Team_Data3$Weigth == 0) & (Team_Data3$Product_type == "Smartphone")] <- WeightMeanPh
Team_Data3$Weigth[(Team_Data3$Weigth == WeightMeanPh) & (Team_Data3$Product_type == "Smartphone")]

Team_Data3$Depth[(Team_Data3$Depth == 0) & (Team_Data3$Product_type == "Smartphone")] <- DepthMeanPh
Team_Data3$Depth[(Team_Data3$Depth == DepthMeanPh) & (Team_Data3$Product_type == "Smartphone")]


#look at values for Game Consoles
GameC <- filter(Team_Data3, Product_type == "Game Console", Width != 0)
summary(GameC)
boxplot(GameC$Weigth) #no outliers, use mean (4.607)
boxplot(GameC$Depth) #no outliers, use mean (4.957)
boxplot(GameC$Width) #no outliers, use mean (9.833)
boxplot(GameC$Heigth) #no outliers, use mean (7.897)

#create mean values for Game Console 
WeightMean <- mean(GameC$Weigth)
DepthMean <- mean(GameC$Depth)
HeigthMean <- mean(GameC$Heigth)
WidthMean <- mean(GameC$Width)

#insert mean values for 0 values in Game Console product type
Team_Data3$Weigth[(Team_Data3$Weigth == 0) & (Team_Data3$Product_type == "Game Console")] <- WeightMean
Team_Data3$Weigth[(Team_Data3$Weigth == WeightMean) & (Team_Data3$Product_type == "Game Console")]

Team_Data3$Depth[(Team_Data3$Depth == 0) & (Team_Data3$Product_type == "Game Console")] <- DepthMean
Team_Data3$Depth[(Team_Data3$Depth == DepthMean) & (Team_Data3$Product_type == "Game Console")]

Team_Data3$Heigth[(Team_Data3$Heigth == 0) & (Team_Data3$Product_type == "Game Console")] <- HeigthMean
Team_Data3$Heigth[(Team_Data3$Heigth == HeigthMean) & (Team_Data3$Product_type == "Game Console")]

Team_Data3$Width[(Team_Data3$Width == 0) & (Team_Data3$Product_type == "Game Console")] <- WidthMean
Team_Data3$Width[(Team_Data3$Width == WidthMean) & (Team_Data3$Product_type == "Game Console")]

#check Game Console to be sure means have been imputed, no more 0 values
GameC2 <- filter(Team_Data3, Product_type == "Game Console")
summary(GameC2)

#dummify the data
Team_Data3$Product_ID <- as.integer(Team_Data3$Product_ID)
Team_Data3$Gender <- as.integer(Team_Data3$Gender)
Team_Data3$In_store <- as.integer(Team_Data3$In_store)
Team_Data3$Environment_Impact <- as.integer(Team_Data3$Environment_Impact)
Team_Data3$Age <- as.factor(Team_Data3$Age)
Team_DataDummy <- dummyVars(" ~ .", data = Team_Data3)
readyData <- data.frame(predict(Team_DataDummy,  newdata = Team_Data3))

#removing rows with all zeros for reviews
allzeros <- filter(readyData, X1Stars == 0, X2Stars == 0, 
                        X3Stars == 0, X4Stars == 0, X5Stars == 0, 
                        Positive_service_review == 0, 
                        Negative_service_review == 0, Volume > 0)

#removing rows with all zeros for reviews and volume > 0
readyData3 <- filter(readyData, !(X1Stars == 0 & X2Stars == 0 & 
                        X3Stars == 0 & X4Stars == 0 & X5Stars == 0 & 
                        Positive_service_review == 0 &
                        Negative_service_review == 0 & Volume > 0))

#imputing with kNN for all rows with zeros for reviews and volume > 0
#changing all zeros in reviews to NA's
readyData2 <- readyData
readyData2$X5Stars[(readyData2$X5Stars == 0)] <- NA
readyData2$X4Stars[(readyData2$X4Stars == 0)] <- NA
readyData2$X3Stars[(readyData2$X3Stars == 0)] <- NA
readyData2$X2Stars[(readyData2$X2Stars == 0)] <- NA
readyData2$X1Stars[(readyData2$X1Stars == 0)] <- NA
readyData2$Positive_service_review[(readyData2$Positive_service_review == 0)] <- NA
readyData2$Negative_service_review[(readyData2$Negative_service_review == 0)] <- NA
readyData2kNN <- kNN(readyData2, variable = c("X5Stars", "X4Stars", "X3Stars", 
                        "X2Stars", "X1Stars", "Positive_service_review",
                        "Negative_service_review"))
summary(readyData2kNN)
readyData2kNN <- select(readyData2kNN, "Product_type.Accessories":"Durability_standard")
summary(readyData2kNN)

#only change to NA the 24 rows with all zeros -- NOT WORKING!
readyData4 <- readyData
readyData4$X5Stars[(readyData4$X5Stars == 0 & 
                      readyData4$X4Stars == 0 &
                      readyData4$X3Stars == 0 &
                      readyData4$X2Stars == 0 &
                      readyData4$X1Stars == 0 &
                      readyData4$Positive_service_review == 0 &
                      readyData4$Negative_service_review == 0 &
                      readyData4$Volume < 0)] 
summary(readyData4)

#finding mean of volume by product type
summaryD <- Data2kNN %>%
  group_by(Product_type) %>%
  summarize(mean_vol = mean(Volume)) %>%
  arrange(mean_vol)
