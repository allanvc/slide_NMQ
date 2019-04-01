# grafico orçamento FNDE

FNDE <- c(61.3, 59.98, 62.84, 58.38, 54.48)

MJ <- c(12.11, 12.18, 13.70, 15.29, 17.13)

MCTIC <- c(18.98, 13.5, 15.25, 12.87, 15.36) 

MAPA <- c(21.2, 21.63, 22.5, 22.03, 22.63)

ano <- 2015:2019

library(tidyr)

df_orc <- data.frame(ano, FNDE, MJ, MCTIC, MAPA)


df_long <- gather(df_orc, orgao, orcamento, 2:5, factor_key=TRUE)

str(df_long)

library(reshape2)

df_long <- melt(df_orc, measure.vars = 2:4)

help("melt")

str(df_orc)

library(plotly)

p <- plot_ly(df_orc, x = ~ano, y = ~MJ, type = 'bar', marker=list(opacity=0.6), name = 'Justiça') %>%
  add_trace(y = ~MCTIC, marker=list(opacity=0.6), name = 'MCTIC') %>%
  add_trace(y = ~MAPA, marker=list(opacity=0.6), name = 'MAPA') %>%
  add_trace(y = ~FNDE, marker=list(opacity=0.6), name = 'FNDE', visible="legendonly") %>%
  layout(yaxis = list(title = "$ (em bilhões)"))

library(htmlwidgets)
library(plotly)
saveWidget(as_widget(p), file = "/home/allan/Documents/r-projects/apres_NMQ1/assets/widgets/plotly_orcamento.html")
