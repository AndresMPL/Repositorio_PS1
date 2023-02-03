# PROBLEM SET 1

##### **Diccionario de datos:** <https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html>

##### **Descripción DANE:**<chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://ignaciomsarmiento.github.io/GEIH2018_sample/ddi-documentation-spanish-608.pdf>

##### **Etiquetas y niveles:** <https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html>

rm(list=ls())
cat("\014")
set.seed(100101)

library(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest)

## Loading

#data1  https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html
#data2  https://ignaciomsarmiento.github.io/GEIH2018_sample/page2.html
#data3  https://ignaciomsarmiento.github.io/GEIH2018_sample/page3.html
#data4  https://ignaciomsarmiento.github.io/GEIH2018_sample/page4.html
#data5  https://ignaciomsarmiento.github.io/GEIH2018_sample/page5.html
#data6  https://ignaciomsarmiento.github.io/GEIH2018_sample/page6.html
#data7  https://ignaciomsarmiento.github.io/GEIH2018_sample/page7.html
#data8  https://ignaciomsarmiento.github.io/GEIH2018_sample/page8.html
#data9  https://ignaciomsarmiento.github.io/GEIH2018_sample/page9.html
#data10 https://ignaciomsarmiento.github.io/GEIH2018_sample/page10.html



tabla0 <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

tabla0 <- as_tibble(tabla0)

str(tabla0)







## Data description




## Age-wage profile



## The gender earnings GAP



## Predicting earnings


