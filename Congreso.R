base <- read.delim("https://raw.githubusercontent.com/ydmarinb/Poster/master/Base.txt")
attach(base)
base$carga <- as.factor(base$carga)
#---------------------------------------------
#Partición de conjunto de datos
#---------------------------------------------
set.seed(2018)
particion <- createDataPartition(base$CEC, p=0.8, list = F)
entrenamiento <- base[particion,]
prueba <- base[-particion,]
y <- prueba[,1]
#----------------------------------------------------------
#Maquinas de vectores de soporte
#----------------------------------------------------------
library(caret)
library(e1071)

mod1 <- svm(CEC~., data = entrenamiento)
pre1 <- predict(mod1, prueba)

mse1 <- mean((y-pre1)^2)
mse1

cor1 <- cor(y,pre1)
cor1

#------------------------------------------
#Arbol de regresión
#------------------------------------
library(rpart)
library(rpart.plot)

mod2 <- rpart(CEC ~., data = entrenamiento, method = "anova")
pre2 <- predict(mod2, prueba)

mse2 <- mean((y-pre2)^2)
mse2

cor2 <- cor(y,pre2)
cor2

mod2$cptable
rpart.plot(mod2)
plotcp(mod2)
#-----------------------------------------
#Bosque aleatorio
#-----------------------------------------
library(randomForest)

mod3 <- randomForest(x = entrenamiento[,2:5], y = entrenamiento[,1], 
                     ntree = 1000, xtest = prueba[,2:5], ytest = prueba[,1],
                     importance = T, keep.forest = T)

pre3 <- predict(mod3, prueba)

mse3 <- mean((y-pre3)^2)
mse3

cor3 <- cor(y,pre3)
cor3

mod3$importance




