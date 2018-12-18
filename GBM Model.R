readyData3FS <- select(modelData,
                       -"In_store", -"Age.1", -"Age.2", 
                       -"Age.3", -"Age.4", -"Gender", -"Profit_margin")


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
ModelGBM <- train(Volume~., data = training, method = "gbm", trControl=fitControl)

#predictor variables
predictors(ModelGBM)

#make predictions
testPredGBM <- predict(ModelGBM, testing)

#performace measurment
postResample(testPredGBM, testing$Volume)

#plot errors
plot(testPredGBM,testing$Volume)
abline(1,1)
summary(testPredGBM)
summary(testing$Volume)

#plot the model
#plot(Model)
ModelGBM

#Output
#postResample(testPredLM1, testing$Volume)
#RMSE    Rsquared         MAE 
#225.1108758   0.7147158 159.1332449 

#summary(testPredLM1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-122.81   16.42  156.70  235.06  382.01 1285.91 
#> summary(testing$Volume)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0    16.0    58.0   218.9   226.0  2052.0 