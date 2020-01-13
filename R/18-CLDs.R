# devtools::install_github("ims-fhs/cld", ref = "dev")
library(cld)
library(magrittr)

plots <- list()

cld <- import("R/clds/200113-referenzgruppe0.mdl")

cld <- import("R/clds/200113-referenzgruppe1.mdl")
cld$polarity = ""
plots$frame1 <- cld %>% 
  link(`zufriedenheit mit der arbeitsproduktivität`) %>% 
  link(`zufriedenheit mit der arbeitsproduktivität`) %>% 
  plot

plots$frame2 <- cld %>% 
  link(`zufriedenheit mit der arbeitsproduktivität` %->% arbeitszeit) %>% 
  link(arbeitszeit) %>% 
  plot

plots$frame3 <- cld %>% 
  link(`zufriedenheit mit der arbeitsproduktivität` %->% arbeitszeit %->% `erledigte aufgaben`) %>% 
  link(`erledigte aufgaben`) %>% 
  plot

plots$frame4 <- cld %>% 
  link(`zufriedenheit mit der arbeitsproduktivität` %->% arbeitszeit %->% `erledigte aufgaben` %->% `zufriedenheit mit der arbeitsproduktivität`) %>% 
  link(`zufriedenheit mit der arbeitsproduktivität`) %>% 
  plot

plots$frame5 <- cld %>% 
  link(`zufriedenheit mit der arbeitsproduktivität` %->% energielevel,  arbeitszeit %->% energielevel) %>% 
  link(`energielevel`) %>% 
  plot



cld <- import("R/clds/200113-referenzgruppe2.mdl")
cld$polarity = ""

plots$frame6 <- cld %>% 
  link(`zufriedenheit mit der care` %->% `zeit für care` %->% `erbrachte care` %->% `zufriedenheit mit der care`) %>%
  link(`zufriedenheit mit der care`) %>%
  plot

plots$frame7 <- cld %>% 
  link(`zeit für care` %->% energielevel, arbeitszeit %->% energielevel, `zufriedenheit mit der care` %->% energielevel, `zufriedenheit mit der arbeitsproduktivität` %->% energielevel) %>%
  link(energielevel) %>%
  plot




cld <- import("R/clds/200113-referenzgruppe3.mdl")
cld$polarity = ""

plots$frame8 <- cld %>% 
  link(`erbrachte care` %->% `zufriedenheit mit der care` %->% `verantwortungsbereich in der care sphäre`,
       `erledigte aufgaben` %->% `zufriedenheit mit der arbeitsproduktivität` %->% `ressourcen in der erwerbssphäre`) %>%
  link(`verantwortungsbereich in der care sphäre`, `ressourcen in der erwerbssphäre`) %>%
  plot

plots$frame9 <- cld %>% 
  link(`verantwortungsbereich in der care sphäre` %->% `eigene ziele care` %->% `zufriedenheit mit der care`,
       `ressourcen in der erwerbssphäre` %->% `eigene ziele arbeit` %->% `zufriedenheit mit der arbeitsproduktivität`) %>%
  link(`zufriedenheit mit der care`, `zufriedenheit mit der arbeitsproduktivität`) %>%
  plot

plots$frame10 <- cld %>% 
  link(`schocks care sphäre` %->% `verantwortungsbereich in der care sphäre`,
       `schocks erwerbssphäre` %->% `ressourcen in der erwerbssphäre`) %>%
  link(`schocks care sphäre`, `schocks erwerbssphäre`) %>%
  plot


cld <- import("R/clds/200113-referenzgruppe4.mdl")
cld$polarity = ""

plots$frame11 <- cld %>% 
  link(`aneignung der rolle` %->% `entlastung in der care`,
       `ressourcen in der erwerbssphäre` %->% `selbstmanagementfähigkeit` %->% `entlastung in der erwerbssphäre`,
       `wirtschaftliche, soziale und psychische bewältigungsressourcen` %->% `selbstmanagementfähigkeit` %->% `entlastung in der care`) %>%
  link(`entlastung in der care`, `entlastung in der erwerbssphäre`) %>%
  plot


library(officer)
library(export)
graph2ppt(plots$frame1, file = "rplot", aspectr = 1.5, scaling = 120)
graph2ppt(plots$frame2, file = "rplot", aspectr = 1.5, scaling = 120, append = TRUE)
graph2ppt(plots$frame3, file = "rplot", aspectr = 1.5, scaling = 120, append = TRUE)
graph2ppt(plots$frame4, file = "rplot", aspectr = 1.5, scaling = 120, append = TRUE)
graph2ppt(plots$frame5, file = "rplot", aspectr = 1.5, scaling = 120, append = TRUE)
graph2ppt(plots$frame6, file = "rplot", aspectr = 1.5, scaling = 120, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame7, file = "rplot", aspectr = 1.5, scaling = 120, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame8, file = "rplot1", aspectr = 1.5, scaling = 180, upscale = TRUE)
graph2ppt(plots$frame9, file = "rplot1", aspectr = 1.5, scaling = 180, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame10, file = "rplot1", aspectr = 1.5, scaling = 180, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame11, file = "rplot1", aspectr = 1.5, scaling = 180, upscale = TRUE, append = TRUE)
