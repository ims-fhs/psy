library(randomForest)

set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, importance=TRUE, proximity=TRUE, na.action=na.omit)
print(ozone.rf)
round(importance(ozone.rf), 2)
