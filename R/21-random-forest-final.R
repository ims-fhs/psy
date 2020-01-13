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

nrow <- 2000
iss <- as.data.frame(sapply(iss, as.logical))
iss_sample <- iss
iss_sample_true <- iss_sample[iss_sample$impact_on_work, ]
iss_sample_false <- iss_sample[!iss_sample$impact_on_work, ]
set.seed(1)
iss_sample_false <- iss_sample_false[sample(1:nrow(iss_sample_false), nrow(iss_sample_true)), ]
iss_sample <- rbind(iss_sample_true, iss_sample_false)

library(randomForest)

set.seed(1)

assertthat::assert_that(assertthat::are_equal(nrow(iss_sample), nrow))
assertthat::assert_that(assertthat::are_equal(ncol(iss_sample), 94))

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

save(rf, file = "data/rf.Rdata")
save(prediction, file = "data/prediction.Rdata")
save(iss_sample, file = "data/iss_sample.Rdata")


load("data/rf.Rdata")
load("data/prediction.Rdata")
load("data/iss_sample.Rdata")


iss.tree <- tree(as.factor(impact_on_work)~., data = iss_sample)
summary(iss.tree)
plot(iss.tree)
text(iss.tree, pretty = 0)

prediction_tree <- predict(iss.tree, iss_sample, type = "class")
table(iss_sample$impact_on_work, prediction_tree)

?tree
iss.cv <- cv.tree(iss.tree,,prune.tree)
plot(iss.cv)


simple.tree <- tree(as.factor(impact_on_work) ~ iss_depression + iss_stress, data = iss_sample)
summary(simple.tree)
plot(simple.tree)
text(simple.tree, pretty = 0)

prediction_simple_tree <- predict(simple.tree, iss_sample, type = "class")
table(iss_sample$impact_on_work, prediction_simple_tree)

# Full predictions
full_pred_rf <- predict(rf, iss, type = "class")
full_pred_st <- predict(simple.tree, iss, type = "class")

table(iss$impact_on_work, full_pred_rf)
table(iss$impact_on_work, full_pred_st)

table(iss$impact_on_work, iss$iss_depression)
table(iss$impact_on_work, iss$iss_stress)


library(ggplot2)

ggplot(iss, aes(impact_on_work)) + 
  geom_bar(aes(fill = impact_on_work), show.legend = FALSE) + 
  labs(x = "", y = "Anzahl Fälle", title = "Beeinträchtigungen der Arbeitsfähigkeit") +
  scale_fill_manual(values = c("darkgreen", "red")) + theme_grey(base_size = 22)

conf_rf <- as.data.frame(table(iss$impact_on_work, full_pred_rf))
names(conf_rf) <- c("observed", "predicted", "freq")

ggplot(conf_rf) + 
  geom_col(aes(x=observed,y=freq,fill=predicted)) +
  labs(x = "", y = "Anzahl Fälle", title = "Beeinträchtigungen der Arbeitsfähigkeit") +
  scale_fill_manual(values = c("darkgreen", "red")) + theme_grey(base_size = 22)

conf_st <- as.data.frame(table(iss$impact_on_work, full_pred_st))
names(conf_st) <- c("observed", "predicted", "freq")

ggplot(conf_st) + 
  geom_col(aes(x=observed,y=freq,fill=predicted)) +
  labs(x = "", y = "Anzahl Fälle", title = "Beeinträchtigungen der Arbeitsfähigkeit") +
  scale_fill_manual(values = c("darkgreen", "red")) + theme_grey(base_size = 22)




conf <- as.data.frame(rf$confusion)
conf$actual <- row.names(conf)
conf <- tidyr::pivot_longer(conf, c(`FALSE`, `TRUE`))
conf$prediction <- conf$name

ggplot(conf, aes(x = actual, fill = prediction)) + geom_bar(position = "fill")
