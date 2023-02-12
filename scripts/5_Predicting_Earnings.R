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

set.seed(10101) 

#seleccionar las variables que se usaran 

dt_final_P5 <- dt_final %>%
                select(age, cuentaPropia, informal, maxEducLevel3, maxEducLevel4, maxEducLevel5, maxEducLevel6, maxEducLevel7, 
                microEmpresa, experiencia, Ingresos_laborales, female, age_squred, oficio2:oficio99)

#Dividir la base 70% - 30%

dt_final_P5$id <- 1:nrow(dt_final_P5)

train  <- dt_final_P5 %>% dplyr::sample_frac(0.70) ## train sample
test   <- dplyr::anti_join(dt_final_P5, train, by = 'id') ## test sample

train <- select(train, -id)
test <- select(test, -id)

#calculamos los modelos y los MSE

##modelos ya usados  

model1<-lm(Ingresos_laborales ~ female ,data=train)
        test$model1<-predict(model1,newdata = test)
        MSE_model1 <- with(test,mean((Ingresos_laborales-model1)^2))#Calculating the MSE
        MSE_model1

model2<-lm(Ingresos_laborales ~ age + age_squred ,data=train)
        test$model2<-predict(model2,newdata = test)
        MSE_model2 <-with(test,mean((Ingresos_laborales-model2)^2))#Calculating the MSE

model3<-lm(Ingresos_laborales ~ female + age + age_squred ,data=train)
        test$model3<-predict(model3,newdata = test)
        MSE_model3 <-with(test,mean((Ingresos_laborales-model3)^2))#Calculating the MSE

model4<-lm(Ingresos_laborales ~ female + age + cuentaPropia + informal + 
        maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
        microEmpresa + experiencia +  age_squred ,data=train)
        test$model4<-predict(model4,newdata = test)
        MSE_model4 <-with(test,mean((Ingresos_laborales-model4)^2))#Calculating the MSE

##nuevos modelos 

model5<-lm(Ingresos_laborales ~ . ,data=train)
        test$model5<-predict(model5,newdata = test)
        MSE_model5 <-with(test,mean((Ingresos_laborales-model5)^2)) #Calculating the MSE

model6<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=train)
        test$model6<-predict(model6,newdata = test)
        MSE_model6 <-with(test,mean((Ingresos_laborales-model6)^2))#Calculating the MSE

model7<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal + poly(experiencia,4,raw=TRUE),data=train)
        test$model7<-predict(model7,newdata = test)
        MSE_model7 <-with(test,mean((Ingresos_laborales-model7)^2))#Calculating the MSE

model8<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE),data=train)
        test$model8<-predict(model8,newdata = test)
        MSE_model8 <-with(test,mean((Ingresos_laborales-model8)^2))#Calculating the MSE

model9<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE)*microEmpresa ,data=train)
        test$model9<-predict(model9,newdata = test)
        MSE_model9 <-with(test,mean((Ingresos_laborales-model9)^2))#Calculating the MSE

test_MSE <- cbind(MSE_model1, MSE_model2, MSE_model3, MSE_model4, MSE_model5, MSE_model6, MSE_model7, MSE_model8, MSE_model9)
colnames(test_MSE) <- c("model 1", "model 2", "model 3", "model 4", "model 5", "model 6", "model 7", "model 8", "model 9" )
test_MSE


##Errores de predicción

test <- test %>%
  mutate(pre_errors_model5 = Ingresos_laborales-model5) 

test <- test %>%
  mutate(pre_errors_model6 = Ingresos_laborales-model6) 



# LOOCV: Los modelos con menor error de predicción son los modelos 5 y 6 
  
  #Leave-One-Out Cross-Validation (LOOCV)-----------------------------------------
  
  #Modelo 5
  
  model5_L<-lm(Ingresos_laborales ~ . ,data=dt_final_P5) ###calculamos el modelo con todas las observaciones 
  
  dt_final_P5$hats_P_M5 <- hatvalues(model5_L)## encontramos los "High Leverage Points""
  summary(dt_final_P5$hats_P_M5) ### encontramos una observacion que genera h = 1, entonces la eliminamos 
  
  dt_final_P5_M5 <- dt_final_P5 %>%
  filter(hats_P_M5 != 1)
  
  
  
  
  dt_final_P5$model5_L<-predict(model5_L, data=dt_final_P5)
  
  dt_final_P5 <- dt_final_P5 %>%
    mutate(least_squares_M5 = (Ingresos_laborales - model5_L)^2)
  
  dt_final_P5$hats_P_M5 <- hatvalues(model5_L)
  summary(dt_final_P5$hats_P_M5) ### encontramos una observacion que genera h = 1, entonces la eliminamos 
  
  dt_final_P5 <- dt_final_P5 %>%
    filter(hats_P_M5 != 1)
  
  
  dt_final_P5$hats_M5 <- (1- dt_final_P5$hats_P_M5)^2
  
  dt_final_P5$lq_hat_M5 <- (dt_final_P5$least_squares_M5/dt_final_P5$hats_M5)
  
  CV_M5 <- (1/nrow(dt_final_P5) * colSums(as.matrix(dt_final_P5$lq_hat_M5), dims = 1))
  
  
  #Modelo 6
  
  model6_L<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=dt_final_P5)
  
  dt_final_P5$model6_L<-predict(model6_L, data=dt_final_P5)
  
  dt_final_P5 <- dt_final_P5 %>%
    mutate(least_squares_M6 = (Ingresos_laborales - model6_L)^2)
  
  dt_final_P5$hats_P_M6 <- hatvalues(model6_L)
  
  TW2 <- dt_final_P5 %>% subset(dt_final_P5$hats_P_M6==1)

  dt_final_P5$hats_M6 <- (1- dt_final_P5$hats_P_M6)^2
  
  dt_final_P5 <- dt_final_P5 %>%
    filter(hats_P_M6 != 1)
  
  dt_final_P5$lq_hat_M6 <- (dt_final_P5$least_squares_M6/dt_final_P5$hats_M6)
  
  CV_M6 <- (1/nrow(dt_final_P5) * colSums(as.matrix(dt_final_P5$lq_hat_M6), dims = 1))
  
  CV_M6
  
  
#Tabla de comparación
  
  test_compar <- cbind(MSE = c(MSE_model1, MSE_model2, MSE_model3, MSE_model4, MSE_model5, MSE_model6, MSE_model7, MSE_model8, MSE_model9, MSE_model10, mse2_model5, mse2_model6)) %>%
    cbind(Modelo = c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M05-L","M06-L"))
  
  test_compar <- test_compar %>% as.data.frame() %>%arrange(Modelo)
  View(test_compar)
  
  test_compar <- test_compar %>% mutate(MSE = as.numeric(MSE))
  test_compar <- test_compar %>% mutate(MSE = round(test_compar$MSE,4))
  
    modelos <-  ggplot(data=test_compar, mapping = aes(x=Modelo , y = MSE)) +
    geom_point() + 
    stat_smooth(method = lm,se = TRUE, level=0.95) + 
    labs(title = 'Modelos', x = 'Modelo', y = 'MSE') + 
    theme_bw()
  
  modelos
  
  
  
  #Leave-One-Out Cross-Validation (LOOCV)-----------------------------------------
  #Validación cruzada de K-Fold
  
  #Generamos las partes para evaluar los modelos
  
  #Datos para hacer el modelo reproducible
  set.seed(01010)
  
  # Evaluaremos en K partes
  K <- nrow(dt_final_P5)
  
  #Dividimos el conjunto de datos en K partes
  index <- split(1:nrow(dt_final), 1: K)
  
  head(index[[1]])
  lapply(index,length)
  
  #Aplicamos la lista de partes al conjunto de datos
  splt <- lapply(1:K, function(ind) dt_final[index[[ind]], ])
  head(splt[[1]])
  
  #Evaluamos los modelos dejando fuera una de las partes
  
  #Modelo 5 - model5
  
  m1 <- lapply(1:K, function(ii) lm(Ingresos_laborales ~ . , data = rbindlist(splt[-ii]))) 
  p1 <- lapply(1:K, function(ii) data.frame(predict(m1[[ii]], newdata = rbindlist(splt[ii]))))
  p1[1] #Comprobamos el vector creado con predicciones
  
  for (i in 1:K) {
    colnames(p1[[i]])<-"yhat_1" #agregamos la predicción
    splt[[i]] <- cbind(splt[[i]], p1[[i]])
  }
  
  head(splt[[1]])
  
  #Calculamos el MSE
  
  mse2_k5 <- lapply(1:K, function(ii) mean((splt[[ii]]$Ingresos_laborales - splt[[ii]]$yhat)^2))
  mse2_k5 #MSE en cada parte evaluada
  mse2_model5 <- mean(unlist(mse2_k5))
  
  #Modelo 6 - model6
  
  m2 <- lapply(1:K, function(ii) lm(Ingresos_laborales ~ . + female*cuentaPropia*informal, data = rbindlist(splt[-ii]))) 
  p2 <- lapply(1:K, function(ii) data.frame(predict(m2[[ii]], newdata = rbindlist(splt[ii]))))
  p2[1] #Comprobamos el vector creado con predicciones
  
  for (i in 1:K) {
    colnames(p2[[i]])<-"yhat_2" #agregamos la predicción
    splt[[i]] <- cbind(splt[[i]], p2[[i]])
  }
  
  head(splt[[1]])
  
  #Calculamos el MSE
  
  mse2_k6 <- lapply(1:K, function(ii) mean((splt[[ii]]$Ingresos_laborales - splt[[ii]]$yhat_2)^2))
  mse2_k6 #MSE en cada parte evaluada
  mse2_model6 <- mean(unlist(mse2_k6))
 