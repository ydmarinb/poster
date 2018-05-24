#--------------------- Se Carga la base de datos-------------------------------- 
base <- read.delim('Base.txt',header = T)
base$carga <- as.factor(base$carga)
#------------------------------ Carga de paquetes----------------------
require(rpart)
require(rpart.plot)

#------------------Ajuste de la base de datos---------------------------------
n <- nrow(base)  # Numero de observaciobes
k <- ceiling(0.70 * n)# numero de observaciones 70%
set.seed(12345)   # To fix a seed
index <- sample(x=1:n, size=k)#generar numeros aleatorios
index
train <- base[ index, ]# Train dataset
test  <- base[-index, ]# Test dataset

#-------------------------------Contruccion del modelo--------------------------
modelo <- rpart(CEC~tiempo+
                  velocidad+Mezclas+carga,method = "anova",data = train)
rpart.plot(modelo)
plotcp(modelo)


#------------------Predicciones------------------------------------------------
yp <- predict(modelo,test)


y.true <- test[, 10]
mse1 <- round(mean((y.true - yp)^2), digits=2)
mse1
