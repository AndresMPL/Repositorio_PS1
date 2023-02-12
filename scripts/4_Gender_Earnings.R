
#------------------------------------------------------------------------------#
#
#                       4 - Brecha salarial por género 
#
#            Ingresos_laborales = Bo + B1*Edad + B2*Mujer + *Controles*
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
         knitr,
         boot)
  
  library(dplyr)
  

#Cargamos la base de datos que vamos a usar

  tps1_female <- dt_final


#Especificamos variables que usaremos y sus interacciones

    y1 <- tps1_female$Ingresos_laborales
    x1 <- tps1_female$age
    x2 <- tps1_female$female
    x3 <- tps1_female$maxEducLevel
    x4 <- tps1_female$age^2
    x5 <- tps1_female$age * tps1_female$maxEducLevel
    x6 <- tps1_female$age * tps1_female$female
    x7 <- tps1_female$female * tps1_female$maxEducLevel
    x8 <- tps1_female$college


# Perfil Edad-Ingresos_laborales--------------------------------------------------------
  
    
  #perfil edad-salario-género con observaciones

  perfil_3a <- ggplot(data=dt_final, mapping = aes(x=age , y = Ingresos_laborales, group=as.factor(sex) , color=as.factor(sex))) +
              geom_point() + 
              stat_smooth(method = lm,se = TRUE, level=0.95) + 
              labs(title = 'Perfil Edad vs. Salario por Género', x = 'Edad', y = 'Salarios') + 
              theme_bw()
    
     
  perfil_3b <- ggplot(data=dt_final, mapping = aes(x=age , y = Ingresos_laborales, group=as.factor(sex) , color=as.factor(sex))) +
              geom_point() + 
              stat_smooth(method = lm,formula= y ~ poly(x, 2), se = TRUE, level=0.95) + 
              labs(title = 'Perfil Edad vs. Salario por Género', x = 'Edad', y = 'Salarios') + 
              theme_bw()

    
  #perfil edad-salario-género sin observaciones
    
  perfil_4 <- ggplot(data=dt_final, mapping = aes(x=age , y = Ingresos_laborales, group=as.factor(sex) , color=as.factor(sex))) +
              stat_smooth(method = lm,formula= y ~ poly(x, 2), se = TRUE, level=0.95) + 
              labs(title = 'Perfil Edad vs. Salario por Género', x = 'Edad', y = 'Salarios') + 
              theme_bw()
    

  reg_line <- lm(Ingresos_laborales~age,   data = tps1_female)
  stargazer(reg_line, type = "text", digits=7, 
                      title="Perfil Ingresos laborales ~ Edad y Género", 
                      column.labels= "Modelo Perfil", 
                      covariate.labels = "Edad",
                      dep.var.labels = "Ln(Ingresos Lab.)")
  
  tps1_female$yhat_line= predict(reg_line)
  
  reg_sq <- lm(Ingresos_laborales~age + I(age^2),   data = tps1_female)
  stargazer(reg_sq, type= "text", digits=7, 
            title="Perfil Ingresos_laborales ~ Edad + Edad^2",
            column.labels = "Modelo Perfil",
            covariate.labels = "Edad",
            dep.var.labels = "Ln(Ingresos Lab.)")
  
  tps1_female$yhat_sq= predict(reg_sq)
  
  perfil_promedio = tps1_female %>%
                    group_by(age, sex) %>%
                    summarize(mean_y = mean(Ingresos_laborales),
                    yhat_line = mean(yhat_line),
                    yhat_sq=mean(yhat_sq),
                    .groups="drop")
          
  perfil_5 <- ggplot(perfil_promedio) + 
              geom_point(aes(x = age, y = mean_y, group=as.factor(sex)), color = "#FF4500", size = 2) + 
              geom_line(aes(x = age, y = yhat_line), color = "#0000EE", size = 1) +
              geom_line(aes(x = age, y = yhat_sq), color = "#00CD00", size = 1) +
              labs(title = "Perfil Ingresos laborales ~ Edad y Género", x = "Edad", y = "Salario") +
              theme_bw()
  
  perfil_5 #revisar atípicos


#Edades pico con intervalos de confianza----------------------------

    reg_intervalo_conf <- function(x, y) {
    n <- length(y) #tamaño de la muestra
    lm_model <- lm(y ~ x + I(x^2)) #modelo que evaluamos
    
    #Guardamos los coeficientes
    b0 <- lm_model$coefficients[1]
    b1 <- lm_model$coefficients[2]
    b2 <- lm_model$coefficients[3]
    
    #Calculamos SSE y MSE
    sse <- sum((y - lm_model$fitted.values)^2)
    mse <- sse / (n - 2)
    
    t_val <- qt(0.975, n - 2) #Calculamos el valor crítico de t-value
    
    #Evaluamos el modelo con los valores
    x_new <- 1:max(x)
    y_fit <- b0 + b1*x_new + b2*x_new^2
    
    #Calculamos el error estandar de la regresión
    se <- sqrt(sum((y - y_fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    
    # Fit a new linear model that extends past the given data points (for plotting)
    x_new2 <- 1:max(x + 1000)
    y_fit2 <- b0 + b1*x_new2 + b2*x_new2^2
    
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y_fit2 + t_val * se)
    slope.lower <- suppressWarnings(y_fit2 - t_val * se)
    
    # Collect the computed confidence bands into a data.frame and name the colums
    bands <- data.frame(cbind(slope.lower , slope.upper))
    colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
    
    # Plot the fitted linear regression line and the computed confidence bands
    plot(x, y, cex = 1, pch = 21, bg = '#E8E8E8', col="#757575")
    lines(y_fit2, col = 'black', lwd = 2)
    lines(bands[1], col = 'blue', lty = 2, lwd = 2)
    lines(bands[2], col = 'blue', lty = 2, lwd = 2)
    
    return(bands)
    
  }
  
  intervalo_conf <- reg_intervalo_conf(tps1_female$age, tps1_female$Ingresos_laborales)

# FWL --------------------------------------------------------------------------

#Incrementamos la variable control para hacer mas clara la diferencia

  #summary(tps1_female$Ingresos_laborales) #voy a incrementar female=1 
  #tps1_female <- tps1_female %>% mutate(Ingresos_laborales=ifelse(female==1,Ingresos_laborales+1000,Ingresos_laborales))

#Unconditional wage gap 

  reg_unconditional <- lm(Ingresos_laborales ~ female , data = tps1_female)
  stargazer(reg_unconditional, type= "text", digits=7, title="unconditional wage gap")

#Conditional Model

  tps1_fwl <- tps1_female %>%
            select(age, cuentaPropia, informal, maxEducLevel3, maxEducLevel4, maxEducLevel5, maxEducLevel6, maxEducLevel7, 
            microEmpresa, experiencia, Ingresos_laborales, female, age_squred)

  reg1 <- lm(Ingresos_laborales ~ female + age + cuentaPropia + informal + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
                microEmpresa + experiencia + Ingresos_laborales + female + age_squred, data = tps1_fwl)
  
  stargazer(reg1, type= "text", digits=7, title="Modelo Original")

#(1) Regresión de la variable x1 en x2 y guardamos los residuos

  tps1_fwl <- tps1_fwl %>% mutate(female_Resid = lm(female ~ age + cuentaPropia + informal + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
                                                       microEmpresa + experiencia + Ingresos_laborales + female + age_squred, tps1_fwl)$residuals)

#(2) Regresión de y en x2 y guardamos los residuos

  tps1_fwl <- tps1_fwl %>% mutate(ingresos_Resid = lm(Ingresos_laborales ~ age + cuentaPropia + informal + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
                                                     microEmpresa + experiencia + Ingresos_laborales + female + age_squred, tps1_fwl)$residuals)

#(3) Regresión de los residuos de (2) sobre los residuos de (1)

  reg2 <- lm(ingresos_Resid~female_Resid, tps1_fwl)
  
  stargazer(reg_unconditional, reg1, reg2, type = "text", digits = 7, title="Comparación de Modelos")


#Verificamos que los coeficientes de reg1 y reg2 sean iguales

  sum(resid(reg1)^2)
  sum(resid(reg2)^2)


#Ajustamos los grados de libertad y verificamos que el error estandar de reg1 y reg 2 sea igual, 

  sqrt(diag(vcov(reg2))*(reg2$df.residual/reg1$df.residual))[2]
  sqrt(diag(vcov(reg1)))[2]


#Gráfico

  reg3 <- lm(Ingresos_laborales~age, tps1_fwl)
  
  grafico3 <- ggplot(tps1_fwl,aes(y=Ingresos_laborales,x=age,group=female,col=factor(female))) +
              geom_point() +
              geom_smooth(method = lm, se = FALSE) +
              geom_abline(slope=reg3$coefficients[2],	#pendiente - aquí vemos la regresión solo de y1~x1
                          intercept=reg3$coefficients[1], #intercepto
                          color="blue", size=1) +
              labs(title = "Perfil Ingresos laborales ~ Edad y Género", x = "Edad", y = "Salario") +
              theme_bw()
                   
  grafico3

  #La línea azul muestra la regresión del Ingresos_laborales solo con la edad
  #Las otras dos líneas muestran el Ingresos_laborales con la edad, separando el género.
  #La línea rosada - hombres
  #La línea celeste - mujeres
  #El Ingresos_laborales si está influido por el género, cuando es hombre aumenta más que cuando son mujeres
  #Por eso cuando incluimos la línea de las mujeres la regresión azul cae


#FWL con bootstrap--------------------------------------------------------------

  bt_red2 <- function(data,index){
    
    #take the residuals
    
    tps1_female <- tps1_female %>% mutate(female_Resid = lm(female ~ age + cuentaPropia + informal + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
                                                              microEmpresa + experiencia + Ingresos_laborales + female + age_squred, tps1_female, subset = index)$residuals)
    
    tps1_female <- tps1_female %>% mutate(ingresos_Resid = lm(Ingresos_laborales ~ age + cuentaPropia + informal + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
                                                                microEmpresa + experiencia + Ingresos_laborales + female + age_squred, tps1_female, subset = index)$residuals, subset = index)
    
    
    #  Regress the residuals, get the coefficients
    coefs <- lm(ingresos_Resid~female_Resid, tps1_female, subset = index)$coefficients
    
    b1 <- coefs[2]
    
    #return coefficients
    
    return(b1)
  }
  
  #test example  

  bt_red2(tps1_female, 1:nrow(tps1_female))
  
  
  
  #bootsatrap results
  
  results <- boot(tps1_female, bt_red2, R=1000)
  results
  
  stargazer(results, type= "text", digits = 7, title="bootsatrap results" )


