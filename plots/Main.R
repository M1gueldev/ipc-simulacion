require(tidyverse)
require(ggplot2)
library(ggplot2)
library(tidyverse)
require("devtools")
library("devtools")
install_github("exoplanetX/greyforecasting")
library(greyforecasting)


arr <- c(101.17, 101.49, 101.36, 101.22, 101.34, 101.45, 101.5, 101.67, 101.58, 101.76, 102.03, 102.38, 102.61, 102.46, 102.44, 102.58, 103.05, 103.21, 103.44, 103.96, 103.88, 104.35, 105.51, 103.88, 103.85, 103.79, 103.91, 104.34, 104.32, 104.69, 104.84, 105.41, 104.35, 104.63, 104.35, 104.58, 105.07, 105.24, 105.12, 105.05, 104.88, 104.89, 105.05, 105.59, 105.39, 105.15, 105.35, 105.52, 105.85, 105.98, 105.93, 105.96, 106.36, 106.77, 107.19, 107.23)
d <- data_frame(
  mes = seq.Date(from = as.Date('2018-01-01'), to = as.Date('2022-08-01'), by = 'month'),
  ipc = arr
)

pl <- ggplot(d, mapping = aes(x=mes, y= ipc)) + labs(title = '', x='Mes', y='IPC (%)')
pl + geom_point() + geom_line(colour='blue')

gmo <- gm(d$ipc, term = 3)
verl <- verhulst(d$ipc, term = 3)
lin <- lm(data = d, formula = 'ipc ~ mes')

models <- data.frame(
  mes = d$mes,
  gm = gmo$fitted,
  ver = verl$fitted,
  lin = lin$fitted.values
)

plot_gm <-  pl + geom_point() + geom_line(mapping = aes(colour='ipc')) + 
  geom_line(data = models, mapping = aes(mes, gm, colour = 'GM(1,1)') ) + 
  geom_point(data = models, mapping = aes(mes, gm, colour = 'GM(1,1)'), shape = 15) +
  labs(colour = 'Modelo', title = 'Modelo GM(1,1)')

plot_ver <- pl + geom_point() + geom_line(mapping = aes(colour='ipc')) + 
  geom_line(data = models, mapping = aes(mes, ver, colour = 'Modelo de Verhulst')) + 
  geom_point(data = models, mapping = aes(mes, ver, colour = 'Modelo de Verhulst'),  shape = 17) +
  labs(colour = 'Modelo', title = 'Modelo de Verhulst')

plot_lin <- pl + geom_point() + geom_line(mapping = aes(colour='ipc')) + 
  geom_line(data = models, mapping = aes(mes, lin, colour = 'Modelo Lineal'), show.legend = TRUE) + 
  geom_point(data = models, mapping = aes(mes, lin, colour = 'Modelo Lineal'), shape = 18) +
  labs(colour = 'Modelo', title = 'Modelo Lineal')

plot_main <- pl + geom_point() + geom_line(mapping = aes(colour='ipc')) + 
  geom_line(data = models, mapping = aes(mes, gm, colour = 'GM(1,1)') ) + 
  geom_point(data = models, mapping = aes(mes, gm, colour = 'GM(1,1)'), shape = 15) +
  geom_line(data = models, mapping = aes(mes, ver, colour = 'Modelo de Verhulst')) + 
  geom_point(data = models, mapping = aes(mes, ver, colour = 'Modelo de Verhulst'),  shape = 17) +
  geom_line(data = models, mapping = aes(mes, lin, colour = 'Modelo Lineal'), show.legend = TRUE) + 
  geom_point(data = models, mapping = aes(mes, lin, colour = 'Modelo Lineal'), shape = 18) +
  labs(colour = 'Modelo', title = 'Comparativa de Modelos')

plot_gm
plot_ver
plot_lin
plot_main

new_month <- seq.Date(from = as.Date('2022-09-01'), to = as.Date('2022-11-01'), by = 'month')

prediction <- data.frame(
  mes = new_month,
  gm = round(gmo$forecasts,2),
  ver = round(verl$forecasts,2),
  lin = round(predict(lin, newdata = data.frame(mes = new_month)),2)
)

ggplot(prediction, mapping = aes(x = mes)) + 
  geom_point(aes(y=gm, colour = 'GM(1,1)', label=gm)) + 
  geom_line(aes(y=gm, colour = 'GM(1,1)')) +
  geom_point(aes(y=ver, colour = 'Modelo de Verhulst')) + 
  geom_line(aes(y=ver, colour = 'Modelo de Verhulst')) +
  geom_point(aes(y=lin, colour = 'Modelo Lineal')) + 
  geom_line(aes(y=lin, colour = 'Modelo Lineal')) +
  geom_text(aes(y=gm, label= gm),check_overlap = TRUE) +
  geom_text(aes(y=ver, label= ver), check_overlap = TRUE) +
  geom_text(aes(y=lin, label= lin), check_overlap = TRUE) +
  labs(colour = 'Modelo', title = 'Comparativa de Predicciones', x='Mes', y='IPC (%)')


