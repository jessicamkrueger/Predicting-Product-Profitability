#feature selection
#readyDataFS <- select(readyData, Product_type.Accessories:Product_type.Tablet, X4Stars, X2Stars, 
#                    Positive_service_review, Gender, Age.1:Age.4, In_store, Volume)

#select features for model
modelDataNOFS <- select(modelDataNO, -"In_store", -"Age.1", -"Age.2", 
                -"Age.3", -"Age.4", -"Gender", -"Profit_margin",
                -"Durability_standard", -"Environment_Impact", 
                -"Height", -"Width", -"Weight", -"Depth", -"Relative_Price",
                -"Would_consumer_recomend__product")

#readyData6 <- select(readyData5, -"In_store", -"Age.1", -"Age.2", 
#                     -"Age.3", -"Age.4", -"Gender", -"Profit_margin")   

#LowVolModFS <- select(LowVolMod, -"Product_type.Game.Console", -"Product_type.Software")
#, 
#-"Durability_standard", -"Prices", -"Profit_margin"
#, 

set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(modelDataNOFS$Volume, p = .75, list = FALSE)
training <- modelDataNOFS[inTraining,]
testing <- modelDataNOFS[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#train regression model
set.seed(234)
tuneGrid1 <- expand.grid(mtry = c(17, 23, 27, 33))
Modelrf <- train(Volume~., data = training, method = "rf", 
                 trControl=fitControl, tuneGrid = tuneGrid1)

#predictor variables
predictors(Modelrf)

#make predictions
testPredrf <- predict(Modelrf, testing)

#performace measurment
postResample(testPredrf, testing$Volume)

#plot errors
plot(testPredrf,testing$Volume)
abline(1,1)
summary(testPredrf)
summary(testing$Volume)

#plot the model
#plot(Model)
Modelrf

#review training and testing data sets for differences
boxplot(training)
boxplot(testing)

boxplot(training$Prices, testing$Prices)
boxplot(training$X5Stars, testing$X5Stars)
boxplot(training$X4Stars, testing$X4Stars)
boxplot(training$X2Stars, testing$X2Stars)
boxplot(training$Positive_service_review, testing$Positive_service_review)
boxplot(training$Negative_service_review, testing$Negative_service_review)
boxplot(training$Volume, testing$Volume)

hist(training$Positive_service_review)
hist(testing$Positive_service_review)

hist(training$Prices)
hist(testing$Prices)
boxplot(readyData3FS$Prices)
boxplot(readyData3FS$Negative_service_review)
boxplot(readyData3FS$Positive_service_review)
boxplot(readyData3FS$X2Stars)
boxplot(readyData3FS$X4Stars)
boxplot(readyData3FS$X5Stars)
boxplot(readyData3FS$Volume)

#removing more outliers from data discovered by looking at differences
#between training and testing sets
readyData5 <- filter(readyData3FS, Positive_service_review < 300)
readyData5 <- filter(readyData5, Prices < 2000)
readyData5 <- filter(readyData5, Negative_service_review < 100)
readyData5 <- filter(readyData5, X2Stars < 370)

#what is the median vol. per product type?

#remove game consoles and software from data set (median)
LowVolMod <- filter(modelData, Product_type.Game.Console != 1, Product_type.Software != 1)
#model metrics did not improve

#remove game consoles and software from data set (mean)
LowVolModMean <- filter(modelData, Product_type.Game.Console != 1, 
                    Product_type.Software != 1,
                    Product_type.Accessories != 1)

#remove all products types except the four we want
FourProducts <- filter(modelData, Product_type.Game.Console != 1, 
                       Product_type.Software != 1, 
                       Product_type.Accessories != 1,
                       Product_type.Display != 1,
                       Product_type.Printer != 1, 
                       Product_type.Printer.Supplies != 1,
                       Product_type.Tablet != 1)
FourProducts <- select(FourProducts, -"Product_type.Game.Console", 
                       -"Product_type.Software", 
                       -"Product_type.Accessories",
                       -"Product_type.Display",
                       -"Product_type.Printer", 
                       -"Product_type.Printer.Supplies",
                       -"Product_type.Tablet")
#Model metrics did not improve

#remove outliers from modelData
modelDataNO <- filter(modelData, Positive_service_review < 300)
modelDataNO <- filter(modelDataNO, Prices < 2000)
modelDataNO <- filter(modelDataNO, Negative_service_review < 100)
modelDataNO <- filter(modelDataNO, X2Stars < 370)

