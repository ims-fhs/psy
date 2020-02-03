# devtools::install_github("ims-fhs/cld", ref = "dev")
library(cld)
library(magrittr)

plots <- list()

cld <- import("R/clds/200113-referenzgruppe5.mdl")
cld$polarity <- ""

cld %>% plot

plots$frame1 <- cld %>%
  link(`zufriedenheit mit der arbeit`, `zufriedenheit mit der care`) %>%
  plot

plots$frame2 <- cld %>%
  link(`zufriedenheit mit der arbeit` %->% arbeitseinsatz, `zufriedenheit mit der care` %->% careeinsatz) %>%
  link(arbeitseinsatz, careeinsatz) %>%
  plot

plots$frame3 <- cld %>%
  link(`zufriedenheit mit der arbeit` %->% arbeitseinsatz %->% `zufriedenheit mit der arbeit`, `zufriedenheit mit der care` %->% careeinsatz %->% `zufriedenheit mit der care`) %>%
  link(`zufriedenheit mit der arbeit`, `zufriedenheit mit der care`) %>%
  plot

plots$frame4 <- cld %>%
  link(`zufriedenheit mit der arbeit` %->% arbeitseinsatz %->% energielevel %->% `zufriedenheit mit der arbeit`, `zufriedenheit mit der care` %->% careeinsatz %->% energielevel %->% `zufriedenheit mit der care`) %>%
  link(`zufriedenheit mit der arbeit`, `zufriedenheit mit der care`) %>%
  plot

plots$frame5 <- cld %>%
  link(`zufriedenheit mit der arbeit` %->% `Ressourcen und Ziele in der Erwerbssphäre`, `zufriedenheit mit der care` %->% `Verantwortungsbereich und Ziele in der Care sphäre`) %>%
  link(`Ressourcen und Ziele in der Erwerbssphäre`, `Verantwortungsbereich und Ziele in der Care sphäre`) %>%
  plot

plots$frame6 <- cld %>%
  link(`schocks care`) %>%
  plot

plots$frame7 <- cld %>%
  link(`schocks care` %->% `Verantwortungsbereich und Ziele in der Care sphäre`) %>%
  link(`Verantwortungsbereich und Ziele in der Care sphäre`) %>%
  plot

plots$frame8 <- cld %>%
  link(`schocks care` %->% `Verantwortungsbereich und Ziele in der Care sphäre` %->% `Zufriedenheit mit der Care`) %>%
  link(`Zufriedenheit mit der Care`) %>%
  plot

plots$frame9 <- cld %>%
  link(`Verantwortungsbereich und Ziele in der Care sphäre` %->% `Entlastung in der Care`,
       bewältigungsressourcen %->% selbstmanagementfähigkeit %->% `entlastung in der care`,
       bewältigungsressourcen %->% selbstmanagementfähigkeit %->% `entlastung bei der arbeit`,
       `ressourcen und ziele in der erwerbssphäre` %->% selbstmanagementfähigkeit %->% `entlastung bei der arbeit`
  ) %>%
  link(`Entlastung in der care`, `entlastung bei der arbeit`) %>%
  plot


library(officer)
library(export)
graph2ppt(plots$frame1, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE)
graph2ppt(plots$frame2, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame3, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame4, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame5, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame6, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame7, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame8, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
graph2ppt(plots$frame9, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
