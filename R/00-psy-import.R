imsbasics::clc()
library(tidyverse)

df <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
df1 <- df
# df1$issues_services <- gsub("\\s+|:|/", "", df1$issues_services)

df1 <- df1 %>% 
  mutate(issues_services = map(issues_services, ~strsplit(., ',')[[1]] %>% sort),
         I = row_number()) %>% 
  unnest(issues_services) %>%
  spread(issues_services, issues_services) %>% 
  mutate_at(-(1:17), ~!is.na(.)) %>% 
  select(-I)
