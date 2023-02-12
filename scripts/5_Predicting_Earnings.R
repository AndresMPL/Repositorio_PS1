#---------------------------------------------------------------------------
#                           
#                               Taller 1
#
#   Grupo 5:  Isabella Mendez Pedraza.
#             Manuela Ojeda Ojeda.
#             Juan Sebastian Tellez Melo.
#             Andres Mauricio Palacio Lugo.
#
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#                      5. Predicting earnings
#---------------------------------------------------------------------------

#seleccionar las variables que se usaran 

dt_final_P5 <- dt_final %>%
  select(age, cuentaPropia, informal, maxEducLevel3, maxEducLevel4, maxEducLevel5, maxEducLevel6, maxEducLevel7, 
         microEmpresa, experiencia, Ingresos_laborales, female, age_squred)



set.seed(10101) 

#dividir la base 70% - 30%

dt_final_P5$id <- 1:nrow(dt_final_P5)


train  <- dt_final_P5 %>% dplyr::sample_frac(0.70) ## train sample
test   <- dplyr::anti_join(dt_final_P5, train, by = 'id') ## test sample

dim(train)

#calculamos los modelos y los MSE

model1<-lm(Ingresos_laborales ~ age + age_squred ,data=train)
test$model1<-predict(model1,newdata = test)
with(test,mean((Ingresos_laborales-model1)^2))

model2<-lm(Ingresos_laborales ~ female ,data=train)
test$model2<-predict(model2,newdata = test)
with(test,mean((Ingresos_laborales-model2)^2))

model3<-lm(Ingresos_laborales ~ female + age + age_squred ,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((Ingresos_laborales-model3)^2))

model4<-lm(Ingresos_laborales ~ female + age + cuentaPropia + informal + 
             maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
             microEmpresa + experiencia + Ingresos_laborales + female + age_squred ,data=train)
test$model4<-predict(model4,newdata = test)
with(test,mean((Ingresos_laborales-model4)^2))


