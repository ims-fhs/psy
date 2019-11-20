imsbasics::clc()
library(tree)
library(boot)
library(dplyr)

source("r/00-psy-import.r")



df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)
male <- as.numeric(df$gender) - 1
iss <- cbind(iss, male)

nrow <- 2000
iss <- as.data.frame(sapply(iss, as.logical))
iss_sample <- iss
iss_sample_true <- iss_sample[iss_sample$impact_on_work, ][1:(nrow/2), ]
iss_sample_false <- iss_sample[!iss_sample$impact_on_work, ][1:(nrow/2), ]

iss_sample <- rbind(iss_sample_true, iss_sample_false)

library(randomForest)

set.seed(131)

assertthat::assert_that(assertthat::are_equal(nrow(iss_sample), nrow))
assertthat::assert_that(assertthat::are_equal(ncol(iss_sample), 94))

entry_time <- Sys.time()
prediction <- NULL
for (i in 1:nrow(iss_sample)) {
  print(i)
  rf <- randomForest(as.factor(impact_on_work) ~ ., data=iss_sample[-i,])
  prediction[[i]] <- predict(rf, newdata=iss_sample[i,])
  time <- Sys.time()
  duration_left <- (time-entry_time)/i*((nrow(iss_sample)-i))
  print(paste0("Voraussichtliches Ende: ", time + duration_left))
}

iss_sample$impact_on_work
prediction <- as.logical(prediction-1)
prediction

sum(iss_sample$impact_on_work == prediction)/nrow
sum(prediction > iss_sample$impact_on_work)/nrow
sum(prediction < iss_sample$impact_on_work)/nrow
