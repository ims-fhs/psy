imsbasics::clc()
library(tree)

source("r/00-psy-import.r")

library(dplyr)
df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)
male <- as.numeric(df$gender) - 1
iss <- cbind(iss, male)

iss <- as.data.frame(sapply(iss, as.logical))

library(randomForest)

set.seed(131)

psy.rf <- randomForest(as.factor(impact_on_work) ~ ., data=iss[1:10000,], importance = TRUE)
print(psy.rf)
round(importance(psy.rf), 2)


