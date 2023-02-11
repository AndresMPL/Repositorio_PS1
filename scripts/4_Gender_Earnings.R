
#------------------------------------------------------------------------------#
#
#                       4 - Brecha salarial por género 
#
#    Salario = Bo + B1*Edad + B2*Mujer + B3*EducNivel + B4*PuestoTrabajo
#
#------------------------------------------------------------------------------#

rm(list=ls())  

library(pacman)
  
  p_load(rio,
         tidyverse,
         skimr,
         caret,
         readxl,
         rvest,
         stargazer,
         knitr)
  
  library(dplyr)
  


#Cargamos la base de datos que vamos a usar

 
  tps1_female <- dt_final

#Seleccionamos y validamos las variables que necesitamos



#Especificamos variables que usaremos y sus interacciones

    y1 <- tps1_female$salario
    x1 <- tps1_female$age
    x2 <- tps1_female$female
    x3 <- tps1_female$maxEducLevel
    x4 <- tps1_female$age^2
    x5 <- tps1_female$age * tps1_female$maxEducLevel
    x6 <- tps1_female$age * tps1_female$female
    x7 <- tps1_female$female * tps1_female$maxEducLevel
    x8 <- tps1_female$college


# Perfil Edad-Salario--------------------------------------------------------
  
  grafico1 <- ggplot(data=tps1_female,
              mapping = aes(x=age , y = salario, group=as.factor(sex) , color=as.factor(sex))) +
              geom_point() + 
              stat_smooth(method = lm, se = FALSE, level=0.95) + 
              theme_bw()
  
  grafico1
  
  reg_line <- lm(salario~age,   data = tps1_female)
  stargazer(reg_line, type = "text", digits=7, 
                      title="Perfil Salario ~ Edad", 
                      column.labels= "Modelo 1", 
                      covariate.labels = "Edad",
                      dep.var.labels = "Ln(Salario)")
  
  tps1_female$yhat_line= predict(reg_line)
  
  reg_sq <- lm(salario~age + I(age^2),   data = tps1_female)
  stargazer(reg_sq, type= "text", digits=7, title="Perfil Salario ~ Edad + Edad^2")
  tps1_female$yhat_sq= predict(reg_sq)
  
  perfil = tps1_female %>%
                        group_by(age) %>%
                        summarize(mean_y = mean(salario),
                                  yhat_line = mean(yhat_line),
                                  yhat_sq=mean(yhat_sq),
                                  .groups="drop")
  
  grafico2 <- ggplot(perfil) + 
              geom_point(aes(x = age, y = mean_y), color = "#FF4500", size = 2) + 
              geom_line(aes(x = age, y = yhat_line), color = "#0000EE", size = 1) +
              geom_line(aes(x = age, y = yhat_sq), color = "#00CD00", size = 1) +
              labs(title = "Perfil Edad-Salario", x = "Edad", y = "Salario") +
              theme_bw()

grafico2 #revisar atípicos


#Edades pico con intervalos de confianza----------------------------



# FWL --------------------------------------------------------------------------

#Incrementamos la variable control para hacer mas clara la diferencia

  #summary(tps1_female$salario) #voy a incrementar female=1 
  #tps1_female <- tps1_female %>% mutate(salario=ifelse(female==1,salario+1000,salario))

# unconditional wage gap 

reg_unconditional <- lm(Ingresos_laborales ~ female , data = tps1_female)
stargazer(reg_unconditional, type= "text", digits=7, title="unconditional wage gap")

#Modelo Original

  reg1 <- lm(y1 ~ x1 + x2, data = tps1_female)
  stargazer(reg1, type= "text", digits=7, title="Modelo Original")

#(1) Regresión de la variable x1 en x2 y guardamos los residuos

  tps1_female <- tps1_female %>% mutate(x1Resid = lm(x1~x2, tps1_female)$residuals)

#(2) Regresión de y en x2 y guardamos los residuos

tps1_female <- tps1_female %>% mutate(y1Resid = lm(y1~x2, tps1_female)$residuals)

#(3) Regresión de los residuos de (2) sobre los residuos de (1)

  reg2 <- lm(y1Resid~x1Resid, tps1_female)
  stargazer(reg1, reg2, type = "text", digits = 7, title="Comparación de Modelos")


#Verificamos que los coeficientes de reg1 y reg2 sean iguales

  sum(resid(reg1)^2)
  sum(resid(reg2)^2)


#Ajustamos los grados de libertad y verificamos que el error estandar de reg1 y reg 2 sea igual, 

  sqrt(diag(vcov(reg2))*(reg2$df.residual/reg1$df.residual))[2]
  sqrt(diag(vcov(reg1)))[2]


#Estimamos si existe Correlación entre x1 y x2

  reg3 <- lm(y1 ~ x1, tps1_female)   #Si omitimos un regresor se cambia los coeficientes de otros regresores
  
  stargazer(reg1, reg3, type = "text", digits = 7, tittle = "Comparación de modelos omitiendo una variable") #comparar el coeficiente de x1
  
  with(tps1_female,cor(x2,x1)) #Esto sucede porque existe correlación entre x1 y x2


#Gráfico

  grafico3 <- ggplot(tps1_female,aes(y=y1,x=x1,group=x2,col=factor(x2))) +
              geom_point() +
              geom_smooth(method = lm, se = FALSE) +
              geom_abline(slope=reg3$coefficients[2],	#pendiente - aquí vemos la regresión solo de y1~x1
                          intercept=reg3$coefficients[1], #intercepto
                          color="blue", size=1) +
              labs(title = "Perfil Edad-Salario-Género", x = "Edad", y = "Salario") +
              theme_bw()
  
  grafico3

  #La línea azul muestra la regresión del salario solo con la edad
  #Las otras dos líneas muestran el salario con la edad, separando el género.
  #La línea rosada - hombres
  #La línea celeste - mujeres
  #El salario si está influido por el género, cuando es hombre aumenta más que cuando son mujeres
  #Por eso cuando incluimos la línea de las mujeres la regresión azul cae


#FWL con bootstrap--------------------------------------------------------------





