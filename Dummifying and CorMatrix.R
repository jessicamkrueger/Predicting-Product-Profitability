install.packages("corrplot")
library(corrplot)

load("~/Ubiqum/Project 2/Task 3/R Files/cleanData.RDS")
Team_Data3$Product_type <- as.factor(Team_Data3$Product_type)
Team_Data3$Depth <- as.numeric(Team_Data3$Depth)
Team_DataDummy <- dummyVars(" ~ .", data = Team_Data3)
readyData <- data.frame(predict(Team_DataDummy,  newdata = Team_Data3))

#Correlation matrix on dummified data
6Matdf <- as.data.frame(corMat)
corMatdfV <- corMatdf %>% select(Volume, everything())

#correlation matrix on data with removed rows
corMat2 <- cor(modelDataNO)
corrplot(corMat2, type = "upper", tl.cex = 0.5, method = "number", number.cex = 0.5, tl.srt = 45, title = "Correlation Matrix with no Zero Data")
#find correlations over .85
findCorrelation(corMat2, cutoff = .85)
readyDataCOR <- readyData3[c(14, 15, 17)]

#dummify the new product list
FinalFourDummy <- dummyVars(" ~ .", data = FinalFour)
FinalFourReady <- data.frame(predict(FinalFourDummy,  newdata = FinalFour))
