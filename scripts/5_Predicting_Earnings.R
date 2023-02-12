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
         microEmpresa, experiencia, Ingresos_laborales, female, age_squred, oficio2:oficio99)

set.seed(10101) 

#dividir la base 70% - 30%

dt_final_P5$id <- 1:nrow(dt_final_P5)


train  <- dt_final_P5 %>% dplyr::sample_frac(0.70) ## train sample
test   <- dplyr::anti_join(dt_final_P5, train, by = 'id') ## test sample

train <- select(train, -id)
test <- select(test, -id)

#calculamos los modelos y los MSE

## modelos ya usados  

model1<-lm(Ingresos_laborales ~ female ,data=train)
test$model1<-predict(model1,newdata = test)
with(test,mean((Ingresos_laborales-model1)^2))

model2<-lm(Ingresos_laborales ~ age + age_squred ,data=train)
test$model2<-predict(model2,newdata = test)
with(test,mean((Ingresos_laborales-model2)^2))

model3<-lm(Ingresos_laborales ~ female + age + age_squred ,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((Ingresos_laborales-model3)^2))

model4<-lm(Ingresos_laborales ~ female + age + cuentaPropia + informal + 
             maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
             microEmpresa + experiencia +  age_squred ,data=train)
test$model4<-predict(model4,newdata = test)
with(test,mean((Ingresos_laborales-model4)^2))

##nuevos modelos 

model5<-lm(Ingresos_laborales ~ . ,data=train)
test$model5<-predict(model5,newdata = test)
with(test,mean((Ingresos_laborales-model5)^2))

model6<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=train)
test$model6<-predict(model6,newdata = test)
with(test,mean((Ingresos_laborales-model6)^2))

model7<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal + poly(experiencia,4,raw=TRUE),data=train)
test$model7<-predict(model7,newdata = test)
with(test,mean((Ingresos_laborales-model7)^2))

model8<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE),data=train)
test$model8<-predict(model8,newdata = test)
with(test,mean((Ingresos_laborales-model8)^2))

model9<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE)*microEmpresa ,data=train)
test$model9<-predict(model9,newdata = test)
with(test,mean((Ingresos_laborales-model9)^2))

model10<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE)*microEmpresa*age ,data=train)
test$model10<-predict(model10,newdata = test)
with(test,mean((Ingresos_laborales-model10)^2))

# LOOCV: Los modelos con menor error de predicción son los modelos 5 y 6 

##Errores de predicción

test <- test %>%
  mutate(pre_errors_model5 = Ingresos_laborales-model5) 

test <- test %>%
  mutate(pre_errors_model6 = Ingresos_laborales-model6) 
