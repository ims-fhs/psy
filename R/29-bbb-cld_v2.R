imsbasics::clc()
# devtools::install_github("ims-fhs/cld", ref = "snf")
library(cld)
library(magrittr)

plots <- list()

cld <- import("R/clds/200609_BBB_R.mdl")
cld$polarity <- ""
cld %>% plot

cld %>%
  link(`arbeitszufriedenheit`) %>%
  link(`arbeitszufriedenheit`) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Die Variable `Arbeitszufriedenheit und Selbstwirksamkeit` beschreibt auf der Bedeutungsebene, wie ein Individuum seine eigene Tätigkeit bedeutet.") %>%
  plot

cld %>%
  link(`arbeitszufriedenheit`) %>%
  link(`arbeitszufriedenheit`) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Eine positive Bedeutung führt zu einem hohen Wert an `Arbeitszufriedenheit und Selbstwirksamkeit`, eine negative Bedeutung zu einem tiefen Wert.") %>%
  plot

cld %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Bedingt ist die `Arbeitszufriedenheit und Selbstwirksamkeit` dabei durch die eigene Tätigkeit und die Erwartungen an die eigene Tätigkeit.") %>%
  plot

plots$frame4 <- cld %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Hohe `Erwartungen an die eigene Tätigeit` senken die `Arbeitszufriedenheit und Selbstwirksamkeit`; Tiefe Erwartungen erhöhen die `Arbeitszufriedenheit und Selbstwirksamkeit`.") %>%
  plot

plots$frame5 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Hohe `Ergebnisse eigener Tätigeit` steigern die `Arbeitszufriedenheit und Selbstwirksamkeit`; Tiefe Ergebnisse senken die `Arbeitszufriedenheit und Selbstwirksamkeit`.") %>%
  plot

plots$frame6 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Entscheidend ist, in welchem Ergebnisse die Ergebnisse mit den Erwartungen stehen.") %>%
  plot

plots$frame7 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(`arbeitszufriedenheit`) %>%
  describe(type = "text", "Eine Veränderung der `Arbeitszufriedenheit und Selbstwirksamkeit` wird nun auf zwei Weisen begründet.") %>%
  plot

plots$frame8 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv, erwartungen %->% arbeitszufriedenheit) %>%
  link(arbeitszufriedenheit) %>%
  link(subjektiv) %>%
  describe(type = "text", "Kurzfristig führt die Begründung einer hohen `Arbeitszufriedenheit und Selbstwirksamkeit` zu einer Senkung der `subjektiven Beanspruchung`.") %>%
  plot

plots$frame9 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv, erwartungen %->% arbeitszufriedenheit) %>%
  link(arbeitszufriedenheit) %>%
  link(subjektiv) %>%
  describe(type = "text", "Damit wird der Effekt beschrieben, dass man die `Subjektive Beanspruchung` über Handlungsregulation so steuert, dass man bei sinkender (oder tiefer) `Arbeitszufriedenheit und Selbstwirksamkeit` den `Druck` etwas erhöht, bei steigender (oder hoher) `Arbeitszufriedenheit und Selbstwirksamkeit` den `Druck` etwas senkt.") %>%
  plot

plots$frame10 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv, erwartungen %->% arbeitszufriedenheit) %>%
  link(arbeitszufriedenheit) %>%
  link(subjektiv) %>%
  describe(type = "text", "Diese erste, kurzfristige Möglichkeit der Handlungsregulation kennen alle Individuen. Sie ist jedoch unterschiedlich stark ausgeprägt bezüglich der Geschwindigkeit und Stärke der Anpassung der `Subjektiven Beanspruchung` an eine Veränderung der `Arbeitszufriedenheit und Selbstwirksamkeit`.") %>%
  plot

plots$frame11 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv %->% ergebnisse, erwartungen %->% arbeitszufriedenheit) %>%
  link(subjektiv) %>%
  link(ergebnisse) %>%
  describe(type = "text", "Die Veränderung der `Subjektiven Beanspruchung` über die Handlungsbegründung aufgrund der Bedeutung einer veränderten `Arbeitszufriedenheit und Selbstwirksamkeit` ermöglicht in der Folge eine Veränderung der Bedingungen: Bei einer Erhöhung der `Subjektiven Beanspruchung` steigen in der Folge die `Ergebnisse eigener Tätigkeit`, bei einer Senkung der `Subjektiven Beanspruchung` sinken in der Folge auch die `Ergebnisse eigener Tätigkeit`.") %>%
  plot

plots$frame12 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv %->% ergebnisse, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse %->% arbeitszufriedenheit %->% subjektiv %->% ergebnisse) %>%
  link(arbeitszufriedenheit, subjektiv) %>%
  describe(type = "text", "Dieser erste kurzfristig (innert Tagen oder Wochen) wirkende Feedbackprozess erlaubt es, die `Subjektive Beanspruchung` und die `Arbeitszufriedenheit und Selbstwirksamkeit` in Balance zu halten.") %>%
  plot

plots$frame13 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(arbeitszufriedenheit) %>%
  describe(type = "text", "Wie und wohin kann sich nun die `Arbeitszufriedenheit und Selbstwirksamkeit` entwickeln?") %>%
  plot

plots$frame14 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(arbeitszufriedenheit) %>%
  describe(type = "text", "Sind die Erwartungen konstant höher als die Ergebnisse, erodiert die Arbeitszufriedenheit und strebt gegen 0") %>%
  describe(type = "ref_mode", 0/0.5 %)% 10/0.2) %>%
  plot

plots$frame15 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(arbeitszufriedenheit) %>%
  describe(type = "text", "Sind die Erwartungen konstant tiefer als die Ergebnisse, steigt die Arbeitszufriedenheit und strebt gegen 1.") %>%
  describe(type = "ref_mode", 0/0.5 %(% 10/0.8) %>%
  plot

plots$frame16 <- cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(arbeitszufriedenheit) %>%
  describe(type = "text", "Sind die Erwartungen und die Ergebnisse immer gleich gross, bleibt die Arbeitszufriedenheit auf dem Ausgangswert.") %>%
  describe(type = "ref_mode", 0/0.5 %-% 10/0.5) %>%
  plot

cld %>%
  link(ergebnisse %->% arbeitszufriedenheit, erwartungen %->% arbeitszufriedenheit) %>%
  link(ergebnisse, arbeitszufriedenheit, erwartungen) %>%
  link(arbeitszufriedenheit) %>%
  describe(type = "text", "Sind die Erwartungen und die Ergebnisse immer gleich gross, bleibt die Arbeitszufriedenheit auf dem Ausgangswert.") %>%
  describe(type = "ref_mode", 0/0.5 %-% 10/0.5) %>%
  plot


library(officer)
library(export)
graph2doc(plots$frame1, file = "rplot1")
graph2doc(plots$frame2, file = "rplot1", append = TRUE)
graph2doc(plots$frame3, file = "rplot1", append = TRUE)
graph2doc(plots$frame4, file = "rplot1", append = TRUE)
graph2doc(plots$frame5, file = "rplot1", append = TRUE)
graph2doc(plots$frame6, file = "rplot1", append = TRUE)
graph2doc(plots$frame7, file = "rplot1", append = TRUE)
graph2doc(plots$frame8, file = "rplot1", append = TRUE)
graph2doc(plots$frame9, file = "rplot1", append = TRUE)
graph2doc(plots$frame10, file = "rplot1", append = TRUE)
graph2doc(plots$frame11, file = "rplot1", append = TRUE)
graph2doc(plots$frame12, file = "rplot1", append = TRUE)
graph2doc(plots$frame13, file = "rplot1", append = TRUE)
graph2doc(plots$frame14, file = "rplot1", append = TRUE)
graph2doc(plots$frame15, file = "rplot1", append = TRUE)
graph2doc(plots$frame16, file = "rplot1", append = TRUE)
# graph2ppt(plots$frame5, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# graph2ppt(plots$frame6, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# graph2ppt(plots$frame7, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# graph2ppt(plots$frame8, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
# graph2ppt(plots$frame9, file = "rplot1", aspectr = 1.7, scaling = 130, upscale = TRUE, append = TRUE)
