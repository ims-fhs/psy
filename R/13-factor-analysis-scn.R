# https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
imsbasics::clc()

library("cluster")
library("factoextra")
library("magrittr")

source("r/00-psy-import.r")

library(dplyr)
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
iss <- iss[1:100, ]
res.dist <- get_dist(iss, stand = TRUE, method = "euclidian")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(iss, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(iss, 5, nstart = 25)
# Visualize
fviz_cluster(km.res, data = iss,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

res.hc <- iss %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

install.packages("mvtnorm")
