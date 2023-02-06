# PROBLEM SET 1

rm(list=ls())
cat("\014")
set.seed(100101)

library(pacman)
library(dplyr)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest)

## Loading


#cargando las páginas
url1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
url2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
url3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")
url4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")
url5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")
url6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")
url7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")
url8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")
url9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")
url10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")



#Para revisar las variables seleccionadas
tablaps1 <- tabla0 %>% select(age,
                            y_salary_m_hu, #salary - real hourly (usual) - principal occ. (includes tips and commissions)
                            y_total_m_ha,  #income salaried + independents total - nominal hourly
                            y_ingLab_m_ha, #labor income salaried - nomial hourly - all occ. (includes tips and commissions
                            p6500, #Antes de descuentos ¿cuánto ganó el mes pasado en este empleo? - Revisar!
                            ocu, #	=1 occupied; =0 otherwise
                            oficio, #occupation
                            sex, #=1 male, =0 female
                            ocu,	#=1 occupied; =0 otherwise
                            pea, #=1 if ocu or dsi; =0 otherwise
                            dsi, #=1 if unemployed; =0 otherwise
                            pet, #Población en edad de trabajar 1:sí 0: no // ocupados + desocupados + inactivos
                            relab,
                            ina, #Inactivo 1: sí
                            inac, #=1 if inactive; =0 otherwise
                              ) %>%
                    mutate(age2 = age^2,
                           ln_y_salary = log(y_salary_m_hu),
                           ln_y_total = log(y_total_m_ha),
                           ln_y_ingLab = log(y_ingLab_m_ha),
                           ln_p65002 = log(p6500),
                           #valores NA
                           #oficio
                            ) %>%
                      filter(age >= 18) %>%
                      filter(inac == 0)

        tabla1$female <- ifelse(tabla1$sex == 0, 1,0)


#verifico el filtro
summary(tabla1$age)

grupos_edad <- tabla1 %>% 
                      group_by(age) %>% 
                      tally() %>% 
                      arrange(age) %>% 
                      rename("total_edad"=n, "edad"=age)
                      View(grupos_edad)

#verifico el genero
count(tabla1, sex)
count(tabla1, female)




## Data description



## Age-wage profile



## The gender earnings GAP



## Predicting earnings


