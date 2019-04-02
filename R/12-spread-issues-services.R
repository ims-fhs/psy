imsbasics::clc()
df <- data.frame(feature_service = c("a:A", "a:A, b:A", "a:A, a:B", "a:B, b:B, c:B", "d:A, d:B"), stringsAsFactors = FALSE)
df

df_goal <- data.frame(feature_a = c(TRUE, TRUE, TRUE, TRUE, FALSE), feature_b = c(FALSE, TRUE, FALSE, TRUE, FALSE), feature_c = c(FALSE, FALSE, FALSE, TRUE, FALSE)
                 , feature_d = c(FALSE, FALSE, FALSE, FALSE, TRUE), service_A = c(TRUE, TRUE, TRUE, FALSE, TRUE), service_B = c(FALSE, FALSE, TRUE, TRUE, TRUE))
df_goal

foo <- strsplit(df$feature_service, ",")
# Get all possible features
feature <- unique(unlist(lapply(foo, function(x) trimws(sub(":.*", ":", x)))))
# Get all possible services
service <- unique(unlist(lapply(foo, function(x) trimws(sub(".*:", ":", x)))))

# Generate occurrence table
result <- sapply(c(feature, service), grepl, df$feature_service)
# Name final result
colnames(result) <- c(paste0("feature_", sub(":", "", feature)),
                      paste0("service_", sub(":", "", service)))

df_goal == as.data.frame(result)


# install.packages("qdapTools")
out <- qdapTools::mtabulate(strsplit(df$feature_service, "[:, ]"))[-1] > 0
out

library(tidyverse)
df %>% 
  rownames_to_column('rn') %>% 
  separate_rows(feature_service) %>%
  distinct(rn, feature_service) %>%
  mutate(n = TRUE) %>%
  spread(feature_service, n, fill = FALSE)
