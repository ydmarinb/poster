#---------------------------leer base-----------------------------
base <- read.delim('Base poster.txt')

#----------------------------Cargar paquetes----------------------------------
require(caret)
library(mlbench)
#---------------------Adecuacion base de datos------------------------
set.seed(107)#Semilla
#Se parte la base de datos en dos sub-base entrenamiento y prueba
particion <- createDataPartition(
  y = base$CEC,
  p = .75,
  list = FALSE
)


entrenamiento <- base[ particion,]
prueba  <- base[-particion,]



#--------------------------------Se crea el modelo------------------------------

modelo <- train(
  CEC ~ .,
  data = entrenamiento,
  method ='brnn',
  preProc = c("center", "scale")
)

#----------------------------Se predice con el modelo---------------------------------

p <- predict(modelo,prueba)

#-----------------------Se calcula el MSE------------------------
mse <- round(mean((y - p)^2), digits=2)
mse
#-----------------------------------------------------------------------------






  

#--------------------------------Se crea el modelo------------------------------

modelo1 <- train(
  CEC ~ .,
  data = entrenamiento,
  method ='treebag',
  preProc = c("center", "scale")
)

#----------------------------Se predice con el modelo---------------------------------

p1 <- predict(modelo1,prueba)

#-----------------------Se calcula el MSE------------------------
mse1 <- round(mean((y - p1)^2), digits=2)
mse1


data.frame(y,p1)





