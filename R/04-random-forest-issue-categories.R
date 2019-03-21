imsbasics::clc()
df <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
df <- df[,!sapply(df,function(x) sum(is.na(x)) > .8*nrow(df))]
df <- df[complete.cases(df), ]


df$issues_services <- as.character(sapply(df$issues_services, function(x) strsplit(x, " ")[[1]][1]))
df <- df %>% mutate_if(is.character, as.factor)
df <- df[, !as.logical(sapply(df, function(x) length(levels(x))) > 53)]

library(randomForest)

set.seed(131)
train=sample(1:nrow(df),1000)
set.seed(131)
psy.rf <- randomForest(impact_on_work ~ ., data = df, subset = train, importance=TRUE, proximity=TRUE, na.action=na.omit)
psy.rf
plot(psy.rf)
