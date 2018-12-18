setwd("~/Ubiqum/Project 2/Task 3/Original Data Sets")
newG_A_I <- read.csv2("newproduct_Gender_Age_Instore.csv")
newRP_EI_D <- read.csv2("newproduct_RelativePice_environmentalImpact_Durability.csv")

#are column names the same for each data set?
colnames(newG_A_I) == colnames(newRP_EI_D)

#join tables based on all matching columns of both data sets 
xNames <- colnames(newG_A_I) #create vector of column names
xNames <- xNames[1:19] #select first 19 column names bc they match to other data set
newFull <- full_join(newG_A_I, newRP_EI_D, by = c(xNames)) #join data
#on all matching column names. now we have 25 variables

#filter just for four product types (smartphone, PC, laptop, netbook)
FinalFour <- filter(newFull, Product_type == "Smartphone" | 
                    Product_type == "Laptop" |
                    Product_type == "Netbook" |
                    Product_type == "PC")

#make predictions on new data
#rename columns
FinalFourReady <- rename(FinalFourReady, Product_type.Game.Console = 
                           Product_type.GameConsole)
FinalFourReady <- rename(FinalFourReady, Product_type.Printer.Supplies = 
                           Product_type.PrinterSupplies)
finalPredictions <- predict(Modelrf, FinalFourReady)
#add column to new data
FinalFour$Volume_Pred <- finalPredictions
save(FinalFourReady, file = "FinalFour.RDS")
write.csv(FinalFour, file="C2.T3output.csv", row.names = TRUE)
