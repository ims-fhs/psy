# devtools::install_github("ims-fhs/cld", ref = "dev")
library(cld)
library(magrittr)

plots <- list()

cld <- import("R/clds/200508_HoMeMiCa_vs_BeBeBe_v3.mdl")
cld$polarity <- ""
cld %>% plot

