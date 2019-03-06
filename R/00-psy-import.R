imsbasics::clc()
library(tidyverse)

df <- xlsx::read.xlsx(file = "data/All Cases 2018.xlsx", sheetName = "ICAS210219", stringsAsFactors = FALSE)[1:10,]
df1 <- df
df1$issues_services <- gsub("\\s+|:|/", "", df1$issues_services)
df1 <- df1 %>% 
  mutate(issues_services = map(issues_services, ~strsplit(., ',')[[1]] %>% sort),
         I = row_number()) %>% 
  unnest(issues_services) %>%
  spread(issues_services, issues_services) %>% 
  mutate_at(-(1:17), ~!is.na(.)) %>% 
  select(-I)

df1$impact_on_work <- as.factor(df1$impact_on_work)
df1 <- df1[,!sapply(df1,function(x) any(is.na(x)))]
df1 <- df1 %>% mutate_if(is.character, as.factor)

library(randomForest)

set.seed(131)
psy.rf <- randomForest(impact_on_work ~ ., data = df1, importance=TRUE, proximity=TRUE, na.action=na.omit)

print(psy.rf)
View(round(importance(psy.rf), 2))
