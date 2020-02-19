imsbasics::clc()

library(tidyverse)
library(ggplot2)

source("r/00-psy-import.r")
df$impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
sum(df$impact_on_work)
sum(df$gender == "M" & df$impact_on_work)
sum(df$gender == "F" & df$impact_on_work)
sum(df$gender == "M" & df$impact_on_work & df$iss_violence)
sum(df$gender == "F" & df$impact_on_work & df$iss_violence)
sum(df$gender == "M" & df$impact_on_work & df$iss_alcohol_use_misuse_abuse)
sum(df$gender == "F" & df$impact_on_work & df$iss_alcohol_use_misuse_abuse)
{
  iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
  iss$gender <- df$gender
  iss$impact_on_work <- df$impact_on_work
  iss %<>% filter(impact_on_work == TRUE) %>% select(-gender) %>% select(-impact_on_work)
  iss <- colSums(iss)
  iss <- as.data.frame(sort(iss, decreasing = TRUE))
  iss$issue <- row.names(iss)
  iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
  iss$percentage <- iss$percentage/sum(iss$percentage)*100
  iss <- iss[1:20, -1]
  
  ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
  iss$gender <- df$gender
  iss$impact_on_work <- df$impact_on_work
  iss %<>% filter(impact_on_work == TRUE, gender == "M") %>% select(-gender) %>% select(-impact_on_work)
  iss <- colSums(iss)
  iss <- as.data.frame(sort(iss, decreasing = TRUE))
  iss$issue <- row.names(iss)
  iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
  iss$percentage <- iss$percentage/sum(iss$percentage)*100
  iss <- iss[1:20, -1]
  
  ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
  iss$gender <- df$gender
  iss$impact_on_work <- df$impact_on_work
  iss %<>% filter(impact_on_work == TRUE, gender == "F") %>% select(-gender) %>% select(-impact_on_work)
  iss <- colSums(iss)
  iss <- as.data.frame(sort(iss, decreasing = TRUE))
  iss$issue <- row.names(iss)
  iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
  iss$percentage <- iss$percentage/sum(iss$percentage)*100
  iss <- iss[1:20, -1]
  
  ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
