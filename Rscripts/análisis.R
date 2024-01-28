library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

Button<- read_excel("Data/Button 862(E 410) 2024-01-04 16_36_19 Argentina Standard Time (Data Argentina Standard Time).xlsx")
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
Button %>% ggplot(aes(tiempo, temp)) + geom_point()
Button %>% filter(temp < 34.0)








