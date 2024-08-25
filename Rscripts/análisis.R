library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

Button<- read_excel("Data.xlsx")
Button <- Button %>% setNames(c("num", "tiempo", "temp" ))
max <- Button$temp[which.max(Button$temp)]
min <- Button$temp[which.min(Button$temp)]
promedio <- mean(Button$temp)
quantile(Button$temp, c(seq(0, 1, 0.05)))
max
min
promedio
menores <- Button[which(Button$temp < quantile(Button$temp, 0.05)), ]
mayores <- Button[which(Button$temp > quantile(Button$temp, 0.95)), ]
Button %>% ggplot(aes(tiempo, temp)) + 
  geom_point() + 
  geom_hline(yintercept = 36) + 
  geom_hline(yintercept = 34) + 
  xlab("Tiempo") + 
  ylab("Temperatura (°C)")

p <- Button %>% filter(temp < 34.0)
b <- Button %>% filter(temp > 36)
percent_menor <- nrow(p) * 100 / nrow(Button)
percent_mayor <- nrow(b) * 100 / nrow(Button)
percent_mayor
percent_menor









