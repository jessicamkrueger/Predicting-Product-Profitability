#feature selection
#readyDataFS <- select(readyData, Product_type.Accessories:Product_type.Tablet, X4Stars, X2Stars, 
#                    Positive_service_review, Gender, Age.1:Age.4, In_store, Volume)

#select features for model
readyData3FS <- select(modelData,
                       -"In_store", -"Age.1", -"Age.2", 
                       -"Age.3", -"Age.4", -"Gender", -"Profit_margin"
                       -"Relative_price", -"Environment_impact", -"Durability_standard")


, 
-"Durability_standard", -"Prices", -"Profit_margin"
, 

set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(modelDataNOFS$Volume, p = .75, list = FALSE)
training <- modelDataNOFS[inTraining,]
testing <- modelDataNOFS[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#train regression model
Modelsvm <- train(Volume~., data = training, method = "svmLinear", trControl=fitControl)

#predictor variables
predictors(Modelsvm)

#make predictions
testPredSVM <- predict(Modelsvm, testing)

#performace measurment
postResample(testPredSVM, testing$Volume)

#plot errors
plot(testPredSVM,testing$Volume)
abline(1,1)
summary(testPredSVM)
summary(testing$Volume)

#plot the model
plot(Model)
Modelsvm
