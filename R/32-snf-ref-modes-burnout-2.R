imsbasics::clc()
# devtools::install_github("ims-fhs/cld", ref = "snf")
library(cld)
library(magrittr)

plots <- list()

cld <- import("R/clds/210409_EURO21.mdl")
cld$polarity <- ""
cld %>% plot

# type 1: worker with high ethos
plots$frame1 <- cld %>%
  link(`self realization goals` %->% `expectations`) %>% 
  link(expectations) %>%
  describe(type = "text", "the 'high ethos' worker starts with high goals and high expectations on his own work.") %>%
  plot

plots$frame2 <- cld %>%
  link(`conditions`) %>%
  link(conditions) %>%
  describe(type = "text", "now let's imagine he's confronted with declining amount of time for a unit of work due to changing organisational conditions.") %>%
  plot


plots$frame3 <- cld %>%
  link(conditions %->% long) %>%
  link(long) %>%
  describe(type = "text", "all other things unchanged, this would lower his 'long term work capacity' (ltwc) over time..") %>%
  describe(type = "ref_mode", 0/.8 %-% .5/.7) %>%
  plot

plots$frame4 <- cld %>%
  link(conditions %->% long) %>%
  link(long) %>%
  describe(type = "text", "..but maybe the guy can cope those external changes? and keep his level of 'long term work capacity'?..") %>%
  describe(type = "ref_mode", 0/.8 %-% .5/.8) %>%
  plot

plots$frame5 <- cld %>%
  link(conditions %->% long) %>%
  link(long) %>%
  describe(type = "text", "..or does he even thrive under such conditions? Let's have a look.") %>%
  describe(type = "ref_mode", 0/.8 %-% .5/.9) %>%
  plot

plots$frame6 <- cld %>%
  link(conditions %->% long %->% goals) %>%
  link(goals) %>%
  describe(type = "text", "The goals (say expected quality from a worker) are sinking..") %>%
  plot

plots$frame7 <- cld %>%
  link(conditions %->% long %->% goals %->% expectations) %>%
  link(expectations) %>%
  describe(type = "text", "..but not the man's expectations on the results of his own activity. (he's not the guy that is sloppy..)") %>%
  plot

plots$frame8 <- cld %>%
  link(conditions %->% long %->% goals %->% expectations %->% efficacy, `results of own activity` %->% efficacy) %>%
  link(efficacy) %>%
  describe(type = "text", "as his results cannot meet his expectations anymore, his perceived self-efficacy sinks as well.") %>%
  plot

plots$frame9 <- cld %>%
  link(conditions %->% long %->% goals %->% expectations %->% efficacy, `results of own activity` %->% efficacy %->% long) %>%
  link(long) %>%
  describe(type = "text", "and here we go: this lowers his long term capacity even more.") %>%
  describe(type = "ref_mode", 0/.8 %(% 0.5/.5) %>%
  plot

plots$frame10 <- cld %>%
  link(efficacy %->% need %->% results) %>%
  link(results) %>%
  describe(type = "text", "But stop. The guy has other options. As soon as his 'perceived self efficacy' starts sinking, the guy reacts, works more (longer more intense) and gets better results.") %>%
  plot

plots$frame11 <- cld %>%
  link(efficacy %->% need %->% results %->% efficacy) %>%
  link(efficacy) %>%
  describe(type = "text", "His perceived self efficacy gets back on track..") %>%
  plot

plots$frame12 <- cld %>%
  link(efficacy %->% need %->% results %->% efficacy %->% long) %>%
  link(long) %>%
  describe(type = "text", "..he can recreate part of his 'long term work capacity' through pushing harder.") %>%
  describe(type = "ref_mode", 0/.8 %-% 0.5/.7) %>%
  plot

plots$frame13 <- cld %>%
  link(need %->% short) %>%
  link(short) %>%
  describe(type = "text", "But there's something else. As he pushes and pushes, his energy level starts to sink as well.") %>%
  plot

plots$frame14 <- cld %>%
  link(need %->% short %->% results) %>%
  link(results) %>%
  describe(type = "text", "Therefore, he is not as effective as he used to be. His results sink (compared to the amount of time and energy he invests).") %>%
  plot

plots$frame15 <- cld %>%
  link(need %->% short %->% results %->% need) %>%
  link(need) %>%
  describe(type = "text", "This, again, lowers his 'perceived self efficacy'.") %>%
  plot

plots$frame16 <- cld %>%
  link(need %->% short %->% results %->% efficacy %->% long) %>%
  link(long) %>%
  describe(type = "text", "The decline of his 'long term work capacity' is accentuated once again.") %>%
  describe(type = "ref_mode", 0/.8 %(% 0.5/.5) %>%
  plot

plots$frame17 <- cld %>%
  link(long) %>%
  link(long) %>%
  describe(type = "text", "Over time his 'long term work capacity' shrinks more and more, which eventually leads to burnout.") %>%
  describe(type = "ref_mode", 0/.8 %(% 0.7/.0) %>%
  plot

# ggplot2::ggsave(
#   filename = "plots.png", 
#   plot = gridExtra::marrangeGrob(plots, nrow=1, ncol=1), 
#   width = 15, height = 9
# )

ggpubr::ggexport(plotlist = plots, res = 180, width = 1500, height = 1500, filename = "test.png")

# library(officer)
# # library(export)
# graph2doc(plots$frame1, file = "rplot1")
# graph2doc(plots$frame2, file = "rplot1", append = TRUE)
# graph2doc(plots$frame3, file = "rplot1", append = TRUE)
# graph2doc(plots$frame4, file = "rplot1", append = TRUE)
# graph2doc(plots$frame5, file = "rplot1", append = TRUE)
# graph2doc(plots$frame6, file = "rplot1", append = TRUE)
# graph2doc(plots$frame7, file = "rplot1", append = TRUE)
# graph2doc(plots$frame8, file = "rplot1", append = TRUE)
# graph2doc(plots$frame9, file = "rplot1", append = TRUE)
# graph2doc(plots$frame10, file = "rplot1", append = TRUE)
# graph2doc(plots$frame11, file = "rplot1", append = TRUE)
# graph2doc(plots$frame12, file = "rplot1", append = TRUE)
# graph2doc(plots$frame13, file = "rplot1", append = TRUE)
# graph2doc(plots$frame14, file = "rplot1", append = TRUE)
# graph2doc(plots$frame15, file = "rplot1", append = TRUE)
# graph2doc(plots$frame16, file = "rplot1", append = TRUE)
# graph2doc(plots$frame17, file = "rplot1", append = TRUE)
# # # graph2ppt(plots$frame5, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# # # graph2ppt(plots$frame6, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# # # graph2ppt(plots$frame7, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# # # graph2ppt(plots$frame8, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# # # graph2ppt(plots$frame9, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
