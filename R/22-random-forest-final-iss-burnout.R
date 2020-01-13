imsbasics::clc()
library(tree)
library(boot)
library(dplyr)

source("r/00-psy-import.r")



df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_) %>% select(-iss_burnout)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)
male <- as.numeric(df$gender) - 1
iss <- cbind(iss, male)

iss <- as.data.frame(sapply(iss, as.logical))
iss_sample <- iss
iss_sample_true <- iss_sample[iss_sample$impact_on_work, ]
iss_sample_false <- iss_sample[!iss_sample$impact_on_work, ]
set.seed(1)
iss_sample_false <- iss_sample_false[sample(1:nrow(iss_sample_false), nrow(iss_sample_true)), ]
iss_sample <- rbind(iss_sample_true, iss_sample_false)

library(randomForest)

set.seed(1)


rf <- randomForest(as.factor(impact_on_work) ~ ., data=iss_sample)

iss_sample$impact_on_work
prediction <- predict(rf, newdata=iss_sample)
prediction <- as.logical(prediction)
prediction

sum(iss_sample$impact_on_work == prediction)/nrow(iss_sample)
sum(prediction > iss_sample$impact_on_work)/nrow(iss_sample)
sum(prediction < iss_sample$impact_on_work)/nrow(iss_sample)

rf$confusion
importance <- as.data.frame(rf$importance, stringsAsFactors = FALSE)
importance <- cbind(row.names(importance), importance)
importance <- importance[order(rf$importance, decreasing = TRUE), ]
View(importance)

sum(iss_sample$impact_on_work[rf$votes[, 2] >= 1])/sum(rf$votes[, 2] >= 1)

iss$impact_on_work
prediction <- predict(rf, newdata=iss)
prediction <- as.logical(prediction)
prediction

sum(iss$impact_on_work == prediction)/nrow(iss)
sum(prediction > iss$impact_on_work)/nrow(iss)
sum(prediction < iss$impact_on_work)/nrow(iss)
