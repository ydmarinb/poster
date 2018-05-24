#------------POSTER CON BASE FINAL con carga (factor)!!!!! ------------------------------
#---------Leyendo la base de datos-------------------
library(readxl)
setwd("E:/")
cargafactor <- read_excel("DATOS FINAL.xlsx")
View(cargafactor)
str(cargafactor)
## convirtiendo la variable mezcla en factor
cargafactor$Mezclas <- factor(cargafactor$Mezclas, levels = c("Diesel","Biodiesel", "Unarias", "Binarias", "Ternarias"))
cargafactor$carga <- factor(cargafactor$carga, levels = c("0", "1", "2", "3", "4"))
str(cargafactor)

#----------------------------Analisis descriptivo-----------------------------------
#algunos datos importantes
min(cargafactor$CEC)
max(cargafactor$CEC)
#martiz de correlaciones
cor(cargafactor[,-c(4,5)])

## grafico 3d para CEC vs velocidad + tiempo
require(scatterplot3d)
scatterplot3d(x = cargafactor$velocidad, y = cargafactor$tiempo, z = cargafactor$CEC, pch = 16, cex.lab = 1.5,
              highlight.3d = TRUE, type = "h", angle = 30)

#dispersion de CEC vs tiempo notamos que se puede utilizar modelos polinomiales
plot(cargafactor$CEC~cargafactor$tiempo)
plot(cargafactor$CEC~cargafactor$velocidad)

#dispersion CEC vs velocidad discriminado por nivel de carga
#vemos que hay un leve relacion con la carga
plot(cargafactor$CEC~cargafactor$velocidad, col = as.numeric(cargafactor$carga), pch = 19, cex = 1, las = T)
legend("bottomleft", legend = c("0", "1", "2", "3", "4"), pch = 19, col=c(1:5), cex=0.7, inset = 0.05)

# Dispercion CEC vs velocidad discriminado por nivel de mezclas
plot(cargafactor$CEC~cargafactor$velocidad, col = as.numeric(cargafactor$Mezclas), pch = 19, cex = 1, las = T)
legend("bottomleft", legend = c("Diesel", "Biodiesel", "unarias", "binarias", "ternarias"), pch = 19, col = c(1:5), cex = 0.7, inset = 0.05)

# plot
plot(cargafactor$CEC~cargafactor$carga)
# Boxplot de CEC vs Mezclas y CEC vs carga
boxplot(cargafactor$CEC~cargafactor$Mezclas, ylim = c(96, 196))
boxplot(cargafactor$CEC~cargafactor$carga, col = "lightskyblue", xlab = "Carga", ylab = "CEC",
        main = "Consumo especifico de combustible por tipo de carga", ylim = c(96,200))

# Densidad de la variable respuesta
plot(density(cargafactor$CEC))
rug(cargafactor$CEC)


#-----------------------Escogiendo Distribucion para la variable respuesta-----------------
require(gamlss)
fit <- fitDist(cargafactor$CEC, type = "realplus")
fit$fits
fit

#histograma y mejores ajustes con BCPEo, GA, IG, LOGNO
par(mfrow = c(2,2))
fitBCPEo <- histDist(y = cargafactor$CEC, family = BCPEo, main = "BCPEo")
fitGA <- histDist(y = cargafactor$CEC, family = GA, main = "GA")
fitIG <- histDist(y = cargafactor$CEC, family = IG, main = "IG")
fitLOGNO <- histDist(y = cargafactor$CEC, family = LOGNO2, main = "LOGNO")

#mejorando los histogramas
par(mfrow = c(2,2))
hist(cargafactor$CEC,
     main = expression(paste("BCPEo  (", mu, " = 134.073,  ", sigma, " = 0.1708751,  ", nu, " = 0.005245,  ", tau, " = 4.877866)")),
     ylab = "Densidad", freq = F, breaks = 10, xlab = "Consumo especifico de Combustible [g/kWh]")
curve(dBCPEo(x, mu = exp(fitBCPEo$mu.coef),
             sigma = exp(fitBCPEo$sigma.coef),
             nu = fitBCPEo$nu.coef,
             tau = exp(fitBCPEo$tau.coef)), add = T, lwd = 3)

hist(cargafactor$CEC,
     main=expression(paste("Gamma  (", mu, " = 137.4907,  ", sigma," = 0.1680716)")),
     ylab='Densidad', freq=F, breaks=10, xlim = c(80,200), ylim = c(0,0.020),
     xlab='Consumo especifico de combustible [g/kWh]')
curve(dGA(x, mu=exp(fitGA$mu.coef), 
          sigma=exp(fitGA$sigma.coef)), add=T, lwd=3)

hist(cargafactor$CEC,
     main=expression(paste("IG  (", mu, " = 137.4907, ", sigma," = 0.0145)")),
     ylab='Densidad', freq=F, breaks=10, xlim = c(80,200), ylim = c(0, 0.020),
     xlab='Consumo especifico de combustible [g/kWh]')
curve(dIG(x, mu=exp(fitIG$mu.coefficients), 
          sigma=exp(fitIG$sigma.coefficients)), add=T, lwd=3)

hist(cargafactor$CEC,
     main=expression(paste("LOGNO2  (", mu, " = 4.909, ", sigma," = 0.1696)")),
     ylab='Densidad', freq=F, breaks=10, xlim = c(80,200), ylim = c(0, 0.020),
     xlab='Consumo especifico de combustible [g/kWh]')
curve(dLOGNO(x, mu=fitLOGNO$mu.coefficients, 
             sigma=exp(fitLOGNO$sigma.coefficients)), add=T, lwd=3)


#-------------------------Aplicacion Gamlss----------------------------#
Horizonte <- formula(~ velocidad + tiempo + I(tiempo^2) + carga + Mezclas)
                       #velocidad * tiempo + velocidad * carga +
                       #velocidad * Mezclas +
                       #I(velocidad^2) +
                       #I(tiempo^2))

# Seleccionando variables con stepGAICALL.A de gamlss 
# para BCPEo
bcp0 <- gamlss(CEC ~ 1, data = cargafactor, family = BCPEo())
bcp1 <- stepGAICAll.A(bcp0, trace = FALSE,
                      scope = list(lower= ~ 1, upper=Horizonte),
                      sigma.scope = list(lower=~1, upper=Horizonte),
                      nu.scope = list(lower=~1, upper=Horizonte),
                      tau.scope = list(lower=~1, upper=Horizonte))
bcp1
#para GAMMA
ga0 <- gamlss(CEC ~ 1, data = cargafactor, family = GA)
ga1 <- stepGAICAll.A(ga0, trace = FALSE,
                     scope = list(lower=~1, upper=Horizonte),
                     sigma.scope = list(lower=~1, upper=Horizonte))
ga1
# para IG
ig0 <- gamlss(CEC ~1, data = cargafactor, family = IG)
ig1 <- stepGAICAll.A(ig0, trace = FALSE,
                     scope = list(lower = ~1, upper = Horizonte),
                     sigma.scope = list(lower = ~1, upper=Horizonte))
ig1
# para LOGNO
logno0 <- gamlss(CEC ~1, data = cargafactor, family = LOGNO(mu.link = "log"))
logno1 <- stepGAICAll.A(logno0, trace = FALSE,
                        scope = list(lower=~1, upper=Horizonte),
                        sigma.scope = list(lower=~1, upper=Horizonte))
logno1
# para NO
no0 <- gamlss(CEC~1, data = cargafactor, family = NO)
no1 <- stepGAICAll.A(no0, trace = FALSE,
                     scope = list(lower=~1, upper = Horizonte),
                     sigma.scope = list(lower=~1, upper = Horizonte))
no1
# para IGAMMA
igamma0 <- gamlss(CEC~1, data = cargafactor, family = IGAMMA)
igamma1 <- stepGAICAll.A(igamma0, trace = FALSE,
                         scope = list(lower=~1, upper = Horizonte),
                         sigma.scope = list(lower=~1, upper = Horizonte))
# para GG
gg0 <- gamlss(CEC~1, data = cargafactor, family = GG)
gg1 <- stepGAICAll.A(gg0, trace = FALSE,
                     scope = list(lower=~1, upper = Horizonte),
                     sigma.scope = list(lower=~1, upper = Horizonte),
                     nu.scope = list(lower=~1, upper = Horizonte))
gg1


#comparando todos
require(MASS)
AIC(bcp1,
    ga1, ig1, logno1,
    no1, igamma1, gg1, k=log(nrow(cargafactor)))
wp(bcp1) ### descartado 
plot(ga1)
wp(ga1)
plot(ig1)
wp(ig1)
plot(logno1)
wp(logno1)
plot(no1) #descartado
wp(no1) ## descartado
plot(igamma1)
wp(igamma1)
plot(gg1)
wp(gg1)


# solo modelos buenos
AIC(ga1, ig1, logno1, igamma1, gg1, k=log(nrow(cargafactor)))

# los tres mejores modelos logno1, ga1, igamma1
logno2 <- gamlss(CEC ~ tiempo + I(tiempo^2) + Mezclas + velocidad,
                 sigma.formula = ~ velocidad + Mezclas + carga,
                 data = cargafactor, family = LOGNO(mu.link = "log"))

ga2 <- gamlss(CEC ~ tiempo + I(tiempo^2) + velocidad + Mezclas,
              sigma.formula = ~ velocidad + Mezclas + carga,
              family = GA, data = cargafactor)

igamma2 <- gamlss(CEC ~ tiempo + I(tiempo^2)+ velocidad + Mezclas,
                  sigma.formula = ~ velocidad + Mezclas + carga,
                  family = IGAMMA, data = cargafactor)

#-----------------Escogiendo el mejor model entre tres --------------#
#solo los modelos actualizados
AIC(logno2, ga2, igamma2, k = log(nrow(cargafactor)))
#comparando con R2
Rsq(logno2)
Rsq(ga2)
Rsq(igamma2)
#Comparando con MSE
newdata <- cargafactor[,-1]
cecreal <- cargafactor[,1]
#para logno2
logajus <- predict(logno2, newdata, what = "mu", type = "response")
logajus <- exp(logajus)
MSElogno <- round(mean((cecreal - logajus)^2), digits = 2)
#para ga2
gaajus <- predict(ga2, newdata, what = "mu", type = "response")
MSEga <- round(mean((cecreal - gaajus)^2), digits = 2)
#para igamma
igammaajus <- predict(igamma2, newdata, what = "mu", type = "response")
MSEigamma <- round(mean((cecreal - igammaajus)^2), digits = 2)

data.frame(MSElogno, MSEga, MSEigamma)

#sigma estimado
sumodlogno2 <- summary(logno2)

#residuales
par(mfrow=c(1, 3), bg='white')
wp(logno2)
title("Lognormal (LOGNO)")
wp(ga2)
title("Gamma (GA)")
wp(igamma2)
title("Inversa gamma (IGAMMA)")

#correlacion
corlogno2 <- cor(logajus, cecreal)
corga <- cor(gaajus, cecreal)
corigamma <- cor(igammaajus, cecreal)
data.frame(corlogno2, corga, corigamma)

#mejor modelo
wp(logno2)
plot(logno2)
summary(logno2)


#----------------------------modelos no parametricos--------------------
modloess <- loess(CEC ~ velocidad + tiempo,
                  data = cargafactor, span = 0.75, degree = 2)
loessajus <- predict(modloess, newdata)
MSEloess <- round(mean((cecreal - loessajus)^2), digits = 2)
MSEloess
corloess <- cor(cecreal, loessajus)
corloess


### MODELO ELEGIDO ####
modelofinal <- gamlss(CEC ~ tiempo + I(tiempo^2) + Mezclas + velocidad,
                 sigma.formula = ~ velocidad + Mezclas + carga,
                 data = cargafactor, family = LOGNO)
modelofinal


