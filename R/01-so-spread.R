df <- data.frame(A = NA, B = NA, C = c("a,b,c", "c,b", "d,a"), stringsAsFactors = FALSE)
df

df_goal <- data.frame(A = NA, B = NA, a = c(TRUE, FALSE, TRUE), b = c(TRUE, TRUE, FALSE), c = c(TRUE, TRUE, FALSE), d = c(FALSE, FALSE, TRUE))
df_goal

df <- cbind(df[, 1:2], as.data.frame(t(apply(read.table(text = df$C, sep = ",", as.is = TRUE, fill = TRUE, na.strings = "")
                                             , 1, 
                                             FUN = function(x) sort(x, decreasing= FALSE, na.last = TRUE))), stringsAsFactors = FALSE))
df <- cbind(df[, 1:2], as.data.frame(sapply(c("a", "b", "c", "d"), function(y) {sapply(1:nrow(df), function(x) {ifelse(y %in% df[x, ], TRUE, FALSE)})})))
df
identical(df, df_goal)

#
library(tidyr)

df <- data.frame(A = NA, B = NA, C = c("a,b,c", "c,b", "d,a"), stringsAsFactors = FALSE)
df

df_goal <- data.frame(A = NA, B = NA, a = c(TRUE, FALSE, TRUE), b = c(TRUE, TRUE, FALSE), c = c(TRUE, TRUE, FALSE), d = c(FALSE, FALSE, TRUE))
df_goal

df %>% separate(C, c("a", "b", "c", "d")) %>% spread(a, b)
df %>% separate(C, c("a", "b", "c", "d")) 

?separate
?spread

df %>% separate(C, c("a", "b", "c", "d"))


# from SO
library(tidyverse)
library(data.table)
df <- data.frame(A = NA, B = NA, C = c("a,b,c", "c,b", "d,a"), stringsAsFactors = FALSE)
df %>% 
  mutate(C = map(C, ~strsplit(., ',')[[1]] %>% sort),
         I = row_number()) %>% 
  unnest(C) %>%
  spread(C, C) %>% 
  mutate_at(-(1:3), ~!is.na(.)) %>% 
  select(-I)
