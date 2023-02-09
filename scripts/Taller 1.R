##########################################
#              Taller 1
##########################################




##########################################
#              0. Pasos previos
##########################################


# Limpieza de area de trabajo --------------------------------------------

rm(list = ls())

# Seleccionar directorio---------------------------------------------------

setwd("/Users/manuelaojeda/Desktop/Universidad /MAESTRIA")

# Cargar paquetes---------------------------------------------------

require(pacman)
p_load(tidyverse, rvest)

##########################################
#       1.1 Scrapping (obtener datos)
##########################################

#Creamos un vector con los diferentes links ---------------------------------
link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

#Se crea un loop para agrupar las diferentes tablas --------------------------

base <-data.frame()
for(i in link){
  print(i)
  
  GEIH<- read_html(i) %>% 
    html_table() %>%
    as.data.frame()
  base <- rbind(base, GEIH)
}

#Visualizar base -------------------------
View(base)

#Exportar base ---------------------------
write.csv(base, "GEIH.csv")

##########################################
#       1.2 Limpieza de datos
##########################################


#Selección de Variables de interés ----------------------------
dt_total <- base %>% 
  select(directorio,age,clase,college,cotPension,cuentaPropia,dsi,estrato1,fex_c,formal,totalHoursWorked,ingtotob, ingtotes, ingtot,iof1es, iof2es, iof6es, maxEducLevel, oficio, p550, p6090, p6580s1, p6920, p7500s1a1, p7500s2a1, p7510s5a1, sex)
view(dt_total)

#Filtrar los individuos empleados y mayores de edad -------------------

base_fin <- dt_total %>% 
  subset(age>=18) %>% 
  subset(dsi==0)

#Sacamos el porcentaje de missing values por variable ---------------------------
missing_percentage <-sapply(base_fin, function(y) sum(length(which(is.na(y))))/length(base_fin$directorio))
#creo una función para saber cuantos NAs hay por columna ------------------------
data_x <- as.data.frame(missing_percentage)
View(missing_percentage)

#Se eliminan las variables con un alto porcentaje de missing value (>50%)----------------------
var <- cbind(Var_name = rownames(data_x), data_x)
rownames(var) <- 1:nrow(var)
var_delete <- var[var$missing_percentage>=0.5,]
var_keep <- var[var$missing_percentage<0.5,]
count(var) # Contamos cuantas variables tenemos en total ----------------------
count(var_keep) # Contamos cuantas variables tienen % missing menor o igual a 50% -----------------
count(var_delete) # Contamos cuantas variables tienen % missing mayor a 50% ---------------------

##########################################
#       1.3 Estadisticas descriptivas
##########################################

#Seleccionamos las variables que cumplen con el requisito y sacamos estadísticas descriptivas
dt_final <- dt_interes %>% select(age, college, cotPension, cuentaPropia, directorio, dsi, estrato1, fex_c, hoursWorkUsual, ingtot, ingtotob, maxEducLevel, oficio, p6426, p7500s1a1, p7500s2a1, p7510s5a1, sex, totalHoursWorked)
View(dt_final)
stargazer(dt_final, type='latex')

#Imputamos Missing Values y comparamos estadísticas descriptivas
dt_imputado <-  kNN(dt_final, variable = c("cotPension", "hoursWorkUsual", "p6426", "totalHoursWorked"), k = 6)
summary(dt_imputado)
stargazer(dt_imputado, type='latex')

##########################################
#              1.4 Gráficas
##########################################

hist(x=dt_imputado$age, weights=dt_imputado$fex_c, main='', 
     xlab='Edad', ylab='Frecuencia', fill='dodgerblue1')
sum(dt_imputado$fex_c)
hist(x=dt_imputado$college)
hist(x=dt_imputado$cotPension)
PieChart(estrato1, hole=0, values="%", data=dt_earnings, fill=1:6, weights=dt_imputado$fex_c, radius=1, main="")
PieChart(sex, hole=0, values="%", data=dt_earnings, fill=1:6, weights=dt_imputado$fex_c, radius=1, main="")


