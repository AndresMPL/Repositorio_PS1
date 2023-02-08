
#-----------------------------------------------------------------------#
#                       4 - Brecha salarial por género 
#
#                Salario = Bo + B1*Edad + B2*Mujer + B3EducNivel
#
#-----------------------------------------------------------------------#

library(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer)

library(dplyr)


#Base de datos que vamos a usar

rm(tps1_female)
setwd("C:/Users/User/OneDrive - Universidad de los Andes/06_Big Data/Taller 1")
tps1_female <- read.csv("base_fin.csv")


#Definimos las variables que trabajaremos

#   y1 <- Salario
#   x1 <- Edad
#   x2 <- Mujer
#   x3 <- Nivel_Educación


#Limpiamos los datos de NA

filas_total <- nrow(tps1_female) #contamos las filas

na_total <- sum(is.na(tps1_female$y_salary_m_hu)) #guardamos las filas NA en salario

tps1_female <- tps1_female %>% filter(y_salary_m_hu != "NA") #eliminamos las filas NA en salario

na_total <- na_total + sum(is.na(tps1_female$maxEducLevel)) #agregamos el número filas que queden con NA en Educ Level

tps1_female <- tps1_female %>% filter(maxEducLevel != "NA") #eliminamos las filas con NA en Educ Level

filas_final <- nrow(tps1_female) #contamos las filas que quedaron

filas_total - na_total - filas_final  #debe dar 0 cuando se cumpla la diferencia después de las eliminaciones


#Seleccionamos las variables que necesitamos, geramos nuevas variables y aseguramos las que necesitamos numéricas

tps1_female <-  tps1_female %>% select(directorio,y_salary_m_hu, age, sex, clase,college,cotPension,cuentaPropia,dsi,estrato1,fex_c,formal,totalHoursWorked,ingtotob, ingtotes, ingtot,iof1es, iof2es, iof6es, maxEducLevel, oficio, p550, p6090, p6580s1, p6920, p7500s1a1, p7500s2a1, p7510s5a1) %>%
  mutate(salario = log(y_salary_m_hu)) %>%
  mutate(age2 = age^2)

tps1_female$female <- ifelse(tps1_female$sex == 0, 1,0) %>% as.numeric()

tps1_female <- tps1_female %>% mutate(salario=as.numeric(salario)) %>% 
  mutate(maxEducLevel=as.numeric(maxEducLevel)) %>%
  mutate(age2=as.numeric(age2))


count(tps1_female, female, sex) #verificamos la creación del género correctamente


#verificamos no haya ningun NA

conteo_na <- sum(is.na(tps1_female$salario)) + 
  sum(is.na(tps1_female$y_salary_m_hu)) + 
  sum(is.na(tps1_female$female)) + 
  sum(is.na(tps1_female$maxEducLevel))

conteo_na #cero es correcto


# Perfil de edad-salario---------------------------------------------

grafico1 <- ggplot(data=tps1_female,
                   mapping = aes(x=age , y = salario, group=as.factor(sex) , color=as.factor(sex))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

reg_line <- lm(salario~age,   data = tps1_female)
summary(reg_line)
tps1_female$yhat_line= predict(reg_line)


reg_sq <- lm(salario~age + age2,   data = tps1_female)
summary(reg_sq)
tps1_female$yhat_sq= predict(reg_sq)

perfil = tps1_female %>%  
  group_by(age) %>%
  summarize(mean_y = mean(salario),
            yhat_line = mean(yhat_line),
            yhat_sq=mean(yhat_sq),
            .groups="drop")

grafico2 <- ggplot(perfil) + 
  geom_point(aes(x = age, y = mean_y), color = "#FF4500", size = 2) + 
  geom_line(aes(x = age, y = yhat_line), color = "#0000EE", size = 1.5) +
  geom_line(aes(x = age, y = yhat_sq), color = "#00CD00", size = 1.5) +
  labs(title = "Perfil Edad-Salario", x = "Edad", y = "Salario") +
  theme_bw()


# FWL ---------------------------

#Incrementamos la variable control para hacer mas clara la diferencia

#summary(tps1_female$salario) #voy a incrementar female=1 
#tps1_female <- tps1_female %>% mutate(salario=ifelse(female==1,salario+1000,salario))


#Regresiones

y1 <- tps1_female$salario
x1 <- tps1_female$age
x2 <- tps1_female$female
x3 <- tps1_female$maxEducLevel

reg1 <- lm(salario ~ age + female, data = tps1_female)
stargazer(reg1, type= "text", digits=7)

#(1) Regresión de la variable x1 en x2 y guardamos los residuos
tps1_female <- tps1_female %>% mutate(x1Resid = lm(age~female, tps1_female)$residuals)

#(2) Regresión de y en x2 y guardamos los residuos
tps1_female <- tps1_female %>% mutate(y1Resid = lm(salario~female, tps1_female)$residuals)

#(3) Regresión de los residuos de (2) sobre los residuos de (1)
reg2 <- lm(y1Resid~x1Resid, tps1_female)
stargazer(reg1, reg2, type = "text", digits = 4)


#Verificamos que los coeficientes de reg1 y reg2 sean iguales

sum(resid(reg1)^2)
sum(resid(reg2)^2)


#Ajustamos los grados de libertad y verificamos que el error estandar de reg1 y reg 2 sea igual, 

sqrt(diag(vcov(reg2))*(reg2$df.residual/reg1$df.residual))[2]
sqrt(diag(vcov(reg1)))[2]




#Estimamos que si existe Correlación entre x1 y x2

with(tps1_female,cor(x2,x1))
reg3 <- lm(y1 ~ x1, tps1_female)  


#Gráfico

grafico3 <- ggplot(tps1_female,aes(y=y1,x=x1,group=x2,col=factor(x2))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(slope=reg3$coefficients[2],	#pendiente - aquí vemos la regresión solo de y1~x1
              intercept=reg3$coefficients[1], #intercepto
              color="blue", size=1) +
  theme_bw()

## Comprobaciones---------------------------------------------

#Obervar - Omitir un regresor cambia los coeficientes de otros regresores

r1<-lm(y1 ~ x2 + x1,tps1_female)

stargazer(r1,reg3, type = "text", digits = 4)


#Regresión 3 - Esperamos X (~x2) cambie el coeficiente, pero no los otros coeficientes.

tps1_female <- tps1_female %>% mutate(new_x1 = x1 + 1000*x2) #k =1000
r3 <- lm(y1~new_x1+x2, tps1_female)
stargazer(r1, r3, type = "text", digits = 4)

#Regresión 4

r4 <- lm(y1~x2+x1Resid, tps1_female)
stargazer(r4, type = "text", digits = 4)
with(tps1_female, cor(x1Resid, x2))

#Regresión 5

r5<-lm(y1~x1Resid, tps1_female)
stargazer(r4, r5, type = "text", digits = 4)

#Gráfico

grafico4 <- ggplot(tps1_female,aes(y=y1,x=x1Resid,group=x2,col=factor(x2))) +
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

#Regresión 6

r6<-lm(y1Resid~x1Resid+x2,tps1_female)
stargazer(r4,r5,r6,type="text",digits=4)

#Regresión 7

r7<-lm(y1Resid~x1Resid,tps1_female)
stargazer(r6,r7,type="text",digits=4)

grafico5 <- ggplot(tps1_female,aes(y=y1Resid,x=x1Resid,group=x2,col=factor(x2))) +
  geom_point() +
  geom_abline(slope=r6$coefficients[2],
              intercept=r6$coefficients[1],
              color="red", size=1) +
  geom_abline(slope=r7$coefficients[2],
              intercept=r7$coefficients[1],
              color="darkgreen", size=1) +
  theme_bw()


#FWL con bootstrap ---------------------------






# Comparaciones -------------------------------

