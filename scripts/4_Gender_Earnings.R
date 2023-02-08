
#-----------------------------------------------------------------------
#                          (3) Brecha salarial por género 
#-----------------------------------------------------------------------
  
  setwd("C:/Users/User/OneDrive - Universidad de los Andes/06_Big Data/Taller 1")
  tps1_female <- read.csv("tps1.csv")
  
  library(dplyr)
  library(pacman)
  
  p_load(rio,
         tidyverse,
         skimr,
         caret,
         readxl,
         rvest,
         stargazer)

# Variables que trabajaremos

  y1 <- as.numeric(tps1_female$ln_y_salary)
  x1 <- as.numeric(tps1_female$female)
  x2 <- as.numeric(tps1_female$maxEducLevel)

# Limpieza de datos

  #Aseguramos que las variables sean numéricas -Revisar-

  tps1_female <- tps1_female %>% mutate(y1=as.numeric(y1), x1=as.numeric(x1), x2=as.numeric(x2))

  #Incrementamos la variable control para hacer mas clara la diferencia
  
  tps1_female <- tps1_female %>% mutate(y1=ifelse(x2==1,y1+8,y1))

# Regresiones

  reg1 <- lm(y1 ~ x2 + x1, data = tps1_female)
  stargazer(reg1, type= "text", digits=7)

# (1) Regresión de la variable x1 en x2 y guardamos los residuos
  tps1_female <- tps1_female %>% mutate(x1ResidF = lm(x1~x2, tps1_female)$residuals)

# (2) Regresión de y en x2 y guardamos los residuos
  tps1_female <- tps1_female %>% mutate(y1ResidF = lm(y1~x2, tps1_female)$residuals)

# (3) Regresión de los residuos de (2) sobre los residuos de (1)
  reg2 <- lm(y1ResidF~x1ResidF, tps1_female)
  stargazer(reg1, reg2, type = "text", digits = 7)

#Verificamos que los coeficientes de reg1 y reg 2 sean iguales
  sum(resid(reg1)^2)
  sum(resid(reg2)^2)

#Verificamos que el error estandar de reg1 y reg 2 sean parecidos
  sqrt(diag(vcov(reg2))*(72/71))[2]
  sqrt(diag(vcov(reg1)))[3]

# Obervar - Omitir un regresor cambia los coeficientes de otros regresores

  r1<-lm(y1 ~ x2 + x1,tps1_female)
  r2<-lm(y1~x1, tps1_female)
  stargazer(r1,r2, type = "text", digits = 7)

#Analizar Correlación
  with(tps1_female,cor(x2,x1))

# Gráfico

  ggplot(tps1_female,aes(y=y1,x=x1,group=x2,col=factor(x2))) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    geom_abline(slope=r2$coefficients[2],	#pendiente
                intercept=r2$coefficients[1], 	#intercepto
                color="orange", size=1) +
    theme_bw()

# Regresión 3 - Esperamos X (~x2) cambie el coeficiente, pero no los otros coeficientes.

  tps1_female <- tps1_female %>% mutate(new_x1 = x1 + 1000*x2) #k =1000
  r3 <- lm(y1~new_x1+x2, tps1_female)
  stargazer(r1, r3, type = "text", digits = 7)

# Regresión 4

  r4 <- lm(y1~x2+x1ResidF, tps1_female)
  stargazer(r4, type = "text", digits = 7)
  with(tps1_female, cor(x1ResidF, x2))

# Regresión 5

  r5<-lm(y1~x1ResidF, tps1_female)
  stargazer(r4, r5, type = "text", digits = 7)

# Gráfico

  ggplot(tps1_female,aes(y=y1,x=x1ResidF,group=x2,col=factor(x2))) +
    geom_point() +
    geom_abline(slope=r4$coefficients[3],
                intercept=r4$coefficients[1],
                color="red", size=1) +
    geom_abline(slope=r4$coefficients[3],
                intercept=r4$coefficients[1]+r4$coefficients[2],
                color="blue", size=1) +
    geom_abline(slope=r5$coefficients[2],
                intercept=r5$coefficients[1],
                color="darkgreen", size=1) +
    theme_bw()

# Regresión 6

  r6<-lm(y1ResidF~x1ResidF+x2,tps1_female)
  stargazer(r4,r5,r6,type="text",digits=7)

# Regresión 7

  r7<-lm(y1ResidF~x1ResidF,tps1_female)
  stargazer(r6,r7,type="text",digits=7)
  
  ggplot(tps1_female,aes(y=y1ResidF,x=x1ResidF,group=x2,col=factor(x2))) +
    geom_point() +
    geom_abline(slope=r6$coefficients[2],
                intercept=r6$coefficients[1],
                color="red", size=1) +
    geom_abline(slope=r7$coefficients[2],
                intercept=r7$coefficients[1],
                color="darkgreen", size=1) +
    theme_bw()
