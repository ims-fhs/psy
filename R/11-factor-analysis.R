# Dissimilarity for mixed data types with R-function “cluster::daisy” calculating Gower’s dissimilarity
library(cluster)
library(car)
rm(list=ls())
source("r/00-psy-import.r")

## A function to use identify to select points, and overplot the
## points with another symbol as they are selected
# identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
# {
#     xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
#     sel <- rep(FALSE, length(x))
#     while(sum(sel) < n) {
#         ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
#         if(!length(ans)) break
#         ans <- which(!sel)[ans]
#         points(x[ans], y[ans], pch = pch)
#         sel[ans] <- TRUE
#     }
#     ## return indices of selected points
#     which(sel)
# }

# subset due to memory shortage
sample_size <- 4000
sdf <- df[1:sample_size, ]
# sdf <- df[sample(1:nrow(df), sample_size, replace = F), ]
# make factor variables out of booleans
col_ids <- grep("iss_", colnames(sdf))
sdf <- sdf%>%mutate_each(list(factor), col_ids)
sdf$iss_ <- NULL
sdf$iss_na <- NULL

# ind_vars <- c("gender", "language_code", grep("iss_", colnames(sdf), value = TRUE))
ind_vars <- grep("iss_", colnames(sdf), value = TRUE)

# a) dissimilarity matrix by daisy, metric multidimensional scaling
# plot by dissimilarity matrix

diss_matrix <- daisy(sdf[, ind_vars])
coords <- cmdscale(diss_matrix)
plot(coords)


# overall labelling
# showLabels(coords[,1], coords[,2], labels=sdf$language_code, method="identify",
#    cex=1, location=c("lr", "ab", "avoid"), )

# interactive labelling
# identify(coords[,1], coords[,2], labels = sdf$id, pos = FALSE,
#                  n = length(coords[,1]), plot = TRUE, offset = 0.5, cex = 0.7, col="red")

# Group 1: male, english speaking, employee,
# issues:  legal, tax, international relocation, housing tenancies,
g1 <- which(coords[,2] < 0.0 & coords[,2] > -0.01 & coords[,1] > -0.022 & coords[,1] < -0.019)
# View(sdf[g1,])

# Group 2: male, de speaking, employee,
# issues:  housing tenancies, consumer_goods_services, health, insurance, tax
g2 <- which(coords[,2] < 0.013 & coords[,2] > 0.008 & coords[,1] > -0.020 & coords[,1] < -0.017)
# View(sdf[g2,])

# Group 3: male, french speaking, employee,
# issues: iss_housing_tenancies, iss_tax, iss_benefits, iss_insurance
g3 <- which(coords[,2] < -0.015 & coords[,2] > -0.19 & coords[,1] > -0.017 & coords[,1] < -0.014)
# View(sdf[g3,])

# Group 4: 
# issues: 
g4 <- which(coords[,2] < 0.013 & coords[,2] > 0.01 & coords[,1] > 0.007 & coords[,1] < 0.01)
# View(sdf[g3,])



sort(colSums(subset(df, id %in% sdf[g3, "id"])[,grep("iss_", colnames(sdf), value = TRUE)]))

View(subset(df, id %in% sdf[g2, "id"]))
