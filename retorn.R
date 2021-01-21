library("Hmisc")
library(tseries)
library(fBasics)
library(fRegression)
library(lmtest)
library(sandwich)
library(dplyr)
library(car) 
library(lattice)

### Analisando os pressupostos ###
 
adf.test(ret) #estacionáridade
adf.test(ibov)
kpss.test(ret) #Esse teste é mais "forte"
kpss.test(ibov)
adf.test(sent)
kpss.test(sent)

acf(ibov) #autocorrelação
acf(ret) #autocorrelação Parcial
pacf(ibov)
pacf(ret)
acf(sent)
pacf(sent)

modelo<-cbind(ret,ibov, sent)#Correlação das variáveis 
modelo<-as.table(modelo)
rcorr(as.matrix(modelo))

#gráficos
png(filename="figura1.png", height=20, width=20, unit="cm", res=300)
par(mfrow=c(5,1), mar=c(3,3,2,2), oma=c(3,3,2,2))
plot(ret, main = 'Ret')
plot(ibov, main = 'Ibov')
plot(sent1, main = 'Sent')
mtext(side=1, text="Tempo (Dias)", outer=T)
dev.off()

### primeira regressão ###
reg<-lm(ret~ibov + sent)
summary(reg)
jarqueberaTest(reg$residuals) #normalidade
lmTest(reg,'dw')#autocorrelação dos resíduos
vif(reg) #multicolinearidade
lmTest(reg,"bp") #heterocedasticidade
coeftest(reg, vcovHC(reg)) #ajustar os resíduos que estavam com heterocedasticidade 

### segunda regressão ###
sentmais<-ifelse(sent>0,sent,0)
sentmenos<-ifelse(sent<0,sent,0)
ibovmais<-ifelse(ibov>0,ibov,0)
ibovmenos<-ifelse(ibov<0,ibov,0)

reg<-lm(ret~ibov+sentmais+sentmenos)
summary(reg)
jarqueberaTest(reg$residuals) #normalidade
lmTest(reg,'dw')#autocorrelação dos resíduos
vif(reg) #multicolinearidade
lmTest(reg,"bp") #heterocedasticidade
coeftest(reg, vcovHC(reg)) #ajustar os resíduos que estavam com heterocedasticidade 

### Terceira regressão ###
reg<-lm(ret~ibov+sent*ibovmais+sent*ibovmenos-sent-ibovmais-ibovmenos)
summary(reg)
jarqueberaTest(reg$residuals) #normalidade
lmTest(reg,'dw')#autocorrelação dos resíduos
vif(reg) #multicolinearidade
lmTest(reg,"bp") #heterocedasticidade
coeftest(reg, vcovHC(reg)) #ajustar os resíduos que estavam com heterocedasticidade 
