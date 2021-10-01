#MODELOS

#CASTELLÓN
library(datasets)
str(tablacastellon)
datosc <-tablacastellon[,c('num_casos','X0.viajes','más.de.2.viajes','temperatura','humedad','so2')]
attach(datosc)
str(datosc)

#todos los datos
cor(datosc)
pairs(datosc)

modelo1<-lm(num_casos~ .,data=datosc)
summary(modelo1)

L1<-step(modelo1,direction='both')
summary(L1)

#residuos
residuosL1<-L1$residuals
ajustadosL1<-L1$fitted.values
plot(X0.viajes,residuosL1);abline(h=0)
plot(más.de.2.viajes,residuosL1);abline(h=0)
plot(temperatura,residuosL1);abline(h=0)
plot(so2,residuosL1);abline(h=0)
plot(ajustadosL1,residuosL1);abline(h=0)

#normalidad
hist(residuosL1)
qqnorm(residuosL1);qqline(residuosL1)
ks.test(residuosL1,pnorm)
shapiro.test(residuosL1)

#homocedasticidad
library(lmtest)
bptest(L1)
library(car)
ncvTest(L1)

#análisis de influencia
cooks.distance(L1)
dfbetas(L1)
dffits(L1)
hatvalues(L1)
require(car)
influencePlot(L1)
influenceIndexPlot(L1)

#modelo cuadrático

Lc<-lm(num_casos~X0.viajes+más.de.2.viajes+temperatura+so2+I(temperatura^2))
summary(Lc)
Lcs<-step(Lc,direction='both')
summary(Lcs)

#residuos
residuosLcs<-Lcs$residuals
ajustadosLcs<-Lcs$fitted.values
plot(X0.viajes,residuosLcs);abline(h=0)
plot(temperatura,residuosLcs);abline(h=0)
plot(ajustadosLcs,residuosLcs);abline(h=0)

#normalidad
hist(residuosLcs)
qqnorm(residuosLcs);qqline(residuosLcs)
ks.test(residuosLcs,pnorm)
shapiro.test(residuosLcs)

#homocedasticidad
library(lmtest)
bptest(Lcs)
library(car)
ncvTest(Lcs)

#análisis de influencia
cooks.distance(Lcs)
dfbetas(Lcs)
dffits(Lcs)
hatvalues(Lcs)
require(car)
influencePlot(Lcs)
influenceIndexPlot(Lcs)

#sin 106
datos.new <- subset.data.frame(datosc,dimnames(datosc)[[1]]!= "106")
str(datos.new)
rectac <- lm(num_casos~X0.viajes+temperatura+I(temperatura^2),data=datos.new)
Lc106 <- step(rectac,direction="both")
summary(Lc106)

#residuos
residuosLc106<-Lc106$residuals
ajustadosLc106<-Lc106$fitted.values
plot(datos.new$X0.viajes,residuosLc106);abline(h=0)
plot(datos.new$temperatura,residuosLc106);abline(h=0)
plot(ajustadosLc106,residuosLc106);abline(h=0)

#normalidad
hist(residuosLc106)
qqnorm(residuosLc106);qqline(residuosLc106)
ks.test(residuosLc106,pnorm)
shapiro.test(residuosLc106)

#homocedasticidad
library(lmtest)
bptest(Lc106)
library(car)
ncvTest(Lc106)

#análisis de influencia
cooks.distance(Lc106)
dfbetas(Lc106)
dffits(Lc106)
hatvalues(Lc106)
require(car)
influencePlot(Lc106)
influenceIndexPlot(Lc106)

#sin 3
datos.new3 <- subset.data.frame(datosc,dimnames(datosc)[[1]]!= "3")
str(datos.new3)
rectac3 <- lm(num_casos~X0.viajes+temperatura+I(temperatura^2),data=datos.new3)
Lc3 <- step(rectac3,direction="both")
summary(Lc3)

#residuos
residuosLc3<-Lc3$residuals
ajustadosLc3<-Lc3$fitted.values
plot(datos.new3$X0.viajes,residuosLc3);abline(h=0)
plot(datos.new3$temperatura,residuosLc3);abline(h=0)
plot(ajustadosLc3,residuosLc3);abline(h=0)

#normalidad
hist(residuosLc3)
qqnorm(residuosLc3);qqline(residuosLc3)
ks.test(residuosLc3,pnorm)
shapiro.test(residuosLc3)

#homocedasticidad
library(lmtest)
bptest(Lc3)
library(car)
ncvTest(Lc3)

#análisis de influencia
cooks.distance(Lc3)
dfbetas(Lc3)
dffits(Lc3)
hatvalues(Lc3)
require(car)
influencePlot(Lc3)
influenceIndexPlot(Lc3)

#prueba con loglineal

modelolog = glm (num_casos~ .,family=poisson(link=log) , data=datosc)
summary(modelolog)
Llog<-step(modelolog)
summary(Llog)

#residuos
residuosLlog<-Llog$residuals
ajustadosLlog<-Llog$fitted.values
plot(X0.viajes,residuosLlog);abline(h=0)
plot(más.de.2.viajes,residuosLlog);abline(h=0)
plot(temperatura,residuosLlog);abline(h=0)
plot(ajustadosLlog,residuosLlog);abline(h=0)

#normalidad
hist(residuosLlog)
qqnorm(residuosLlog);qqline(residuosLlog)
ks.test(residuosLlog,pnorm)
shapiro.test(residuosLlog)

#homocedasticidad
library(lmtest)
bptest(Llog)
library(car)
ncvTest(Llog)

#VALENCIA

library(datasets)
str(tablavalencia)
datosv <-tablavalencia[,c('num_casos','X0.viajes','más.de.2.viajes','temperatura','humedad','so2')]
attach(datosv)
str(datosv)

#modelo marzo

datosvm<-data.frame(num_casos[15:45],X0.viajes[1:31],más.de.2.viajes[1:31],temperatura[1:31],humedad[1:31],so2[1:31])
attach(datosvm)
str(datosvm)
cor(datosvm)
paiprs(datosvm)

modelo<-lm(num_casos.15.45.~ .,data=datosvm)
summary(modelo)

L<-step(modelo,direction='both')
summary(L)

#residuos
residuosL<-L$residuals
ajustadosL<-L$fitted.values
plot(más.de.2.viajes.1.31.,residuosL);abline(h=0)
plot(temperatura.1.31.,residuosL);abline(h=0)
plot(so2.1.31.,residuosL);abline(h=0)
plot(ajustadosL,residuosL);abline(h=0)

#normalidad
hist(residuosL)
qqnorm(residuosL);qqline(residuosL)
ks.test(residuosL,pnorm)
shapiro.test(residuosL)

#homocedasticidad
library(lmtest)
bptest(L)
library(car)
ncvTest(L)

#análisis de influencia
cooks.distance(L)
dfbetas(L)
dffits(L)
hatvalues(L)
require(car)
influencePlot(L)
influenceIndexPlot(L)

#sin 9
datos.newv <- subset.data.frame(datosvm,dimnames(datosvm)[[1]]!= "9")
str(datos.newv)
rectav <- lm(num_casos.15.45.~más.de.2.viajes.1.31.+temperatura.1.31.+so2.1.31.,data=datos.newv)
L9 <- step(rectav,direction="both")
summary(L9)

#residuos
residuosL9<-L9$residuals
ajustadosL9<-L9$fitted.values
plot(datos.newv$X0.viajes,residuosL9);abline(h=0)
plot(datos.newv$temperatura,residuosL9);abline(h=0)
plot(ajustadosL9,residuosL9);abline(h=0)

#normalidad
hist(residuosL9)
qqnorm(residuosL9);qqline(residuosL9)
ks.test(residuosL9,pnorm)
shapiro.test(residuosL9)

#homocedasticidad
library(lmtest)
bptest(L9)
library(car)
ncvTest(L9)

#análisis de influencia
cooks.distance(L9)
dfbetas(L9)
dffits(L9)
hatvalues(L9)
require(car)
influencePlot(L9)
influenceIndexPlot(L9)

#ALICANTE

library(datasets)
str(tablaalicante)
datosa <-tablaalicante[,c('num_casos','X0.viajes','más.de.2.viajes','temperatura','humedad','so2')]
attach(datosa)
str(datosa)

#modelo fase 0

datosa0<-data.frame(num_casos[65:78],X0.viajes[50:63],más.de.2.viajes[50:63],temperatura[50:63],humedad[50:63],so2[50:63])
attach(datosa0)
str(datosa0)
cor(datosa0)
paiprs(datosa0)

modelof0<-lm(num_casos.65.78.~ .,data=datosa0)
summary(modelof0)

Lf0<-step(modelof0,direction='both')
summary(Lf0)

#residuos
residuosLf0<-Lf0$residuals
ajustadosLf0<-Lf0$fitted.values
plot(so2.50.63.,residuosLf0);abline(h=0)
plot(ajustadosLf0,residuosLf0);abline(h=0)

#normalidad
hist(residuosLf0)
qqnorm(residuosLf0);qqline(residuosLf0)
ks.test(residuosLf0,pnorm)
shapiro.test(residuosLf0)

#homocedasticidad
library(lmtest)
bptest(Lf0)
library(car)
ncvTest(Lf0)

#análisis de influencia
cooks.distance(Lf0)
dfbetas(Lf0)
dffits(Lf0)
hatvalues(Lf0)
require(car)
influencePlot(Lf0)
influenceIndexPlot(Lf0)

#sin 2, 4, 10
datos.new0 <- subset.data.frame(datosa0,dimnames(datosa0)[[1]]!= "2")
datos.new01 <- subset.data.frame(datos.new0,dimnames(datos.new0)[[1]]!= "4")
datos.new02 <- subset.data.frame(datos.new01,dimnames(datos.new01)[[1]]!= "10")
str(datos.new02)
L2 <- lm(num_casos.65.78.~so2.50.63.,data=datos.new02)
summary(L2)

#residuos
residuosL2<-L2$residuals
ajustadosL2<-L2$fitted.values
plot(datos.new0$so2,residuosL9);abline(h=0)
plot(ajustadosL2,residuosL2);abline(h=0)

#normalidad
hist(residuosL2)
qqnorm(residuosL2);qqline(residuosL2)
ks.test(residuosL2,pnorm)
shapiro.test(residuosL2)

#homocedasticidad
library(lmtest)
bptest(L2)
library(car)
ncvTest(L2)

#análisis de influencia
cooks.distance(L2)
dfbetas(L2)
dffits(L2)
hatvalues(L2)
require(car)
influencePlot(L2)
influenceIndexPlot(L2)

#modelo fase 1

datosa1<-data.frame(num_casos[79:92],X0.viajes[64:77],más.de.2.viajes[64:77],temperatura[64:77],humedad[64:77],so2[64:77])
attach(datosa1)
str(datosa1)
cor(datosa1)
paiprs(datosa1)

modelof1<-lm(num_casos.79.92.~ .,data=datosa1)
summary(modelof1)

Lf1<-step(modelof1,direction='both')
summary(Lf1)

#residuos
residuosLf1<-Lf1$residuals
ajustadosLf1<-Lf1$fitted.values
plot(temperatura.64.77.,residuosLf1);abline(h=0)
plot(humedad.64.77.,residuosLf1);abline(h=0)
plot(ajustadosLf1,residuosLf1);abline(h=0)

#normalidad
hist(residuosLf1)
qqnorm(residuosLf1);qqline(residuosLf1)
ks.test(residuosLf1,pnorm)
shapiro.test(residuosLf1)

#homocedasticidad
library(lmtest)
bptest(Lf1)
library(car)
ncvTest(Lf1)

#análisis de influencia
cooks.distance(Lf1)
dfbetas(Lf1)
dffits(Lf1)
hatvalues(Lf1)
require(car)
influencePlot(Lf1)
influenceIndexPlot(Lf1)

#sin 9
datos.new1 <- subset.data.frame(datosa1,dimnames(datosa1)[[1]]!= "9")
str(datos.new1)
L9 <- lm(num_casos.79.92.~temperatura.64.77.+humedad.64.77.,data=datos.new1)
summary(L9)

#residuos
residuosL9<-L9$residuals
ajustadosL9<-L9$fitted.values
plot(datos.new1$temperatura,residuosL9);abline(h=0)
plot(datos.new1$humedad,residuosL9);abline(h=0)
plot(ajustadosL9,residuosL9);abline(h=0)

#normalidad
hist(residuosL9)
qqnorm(residuosL9);qqline(residuosL9)
ks.test(residuosL9,pnorm)
shapiro.test(residuosL9)

#homocedasticidad
library(lmtest)
bptest(L9)
library(car)
ncvTest(L9)

#análisis de influencia
cooks.distance(L9)
dfbetas(L9)
dffits(L9)
hatvalues(L9)
require(car)
influencePlot(L9)
influenceIndexPlot(L9)

#modelo fase 2

datosa2<-data.frame(num_casos[93:106],X0.viajes[78:91],más.de.2.viajes[78:91],temperatura[78:91],humedad[78:91],so2[78:91])
attach(datosa2)
str(datosa2)
cor(datosa2)
paiprs(datosa2)

modelof2<-lm(num_casos.93.106.~ .,data=datosa2)
summary(modelof2)

Lf2<-step(modelof2,direction='both')
summary(Lf2)

#residuos
residuosLf2<-Lf2$residuals
ajustadosLf2<-Lf2$fitted.values
plot(más.de.2.viajes.78.91.,residuosLf2);abline(h=0)
plot(temperatura.78.91.,residuosLf2);abline(h=0)
plot(so2.78.91.,residuosLf2);abline(h=0)
plot(ajustadosLf2,residuosLf2);abline(h=0)

#normalidad
hist(residuosLf2)
qqnorm(residuosLf2);qqline(residuosLf2)
ks.test(residuosLf2,pnorm)
shapiro.test(residuosLf2)

#homocedasticidad
library(lmtest)
bptest(Lf2)
library(car)
ncvTest(Lf2)

#análisis de influencia
cooks.distance(Lf2)
dfbetas(Lf2)
dffits(Lf2)
hatvalues(Lf2)
require(car)
influencePlot(Lf2)
influenceIndexPlot(Lf2)
