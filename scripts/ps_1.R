# PROBLEM SET 1

##### **Diccionario de datos:** <https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html>

##### **Descripción DANE:**<chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://ignaciomsarmiento.github.io/GEIH2018_sample/ddi-documentation-spanish-608.pdf>

##### **Etiquetas y niveles:** <https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html>

rm(list=ls())
cat("\014")
set.seed(100101)

require(pacman)
library(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest)

## Loading

tabla0 <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
tabla0_tb <- as_tibble(tabla0)

str(tabla0)
str(tabla0_tb)     #ensayos




## Data description




## Age-wage profile



## The gender earnings GAP



## Predicting earnings


