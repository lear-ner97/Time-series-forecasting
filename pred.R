
library("forecast")


train <- window(quantity,start=2004,end=c(2012,12))
test <- window(quantity,start=2013) 



fitLES <- ets(train,model="ANN")

summary(fitLES)

predLES <- forecast(fitLES,h=36) 
plot(predLES) 
points(test,type='l',col='red',lwd=2) 
legend('top',c("Valeurs observées","Prédictions"),col=c("red","blue"),lty=rep(1,2),lwd = rep(2,2))



fitLED <- ets(train,model="AAN") 
summary(fitLED)




predLED <- forecast(fitLED,h=36)
plot(predLED)
points(test,type='l',col='red',lwd=2)
legend('top',c("Valeurs observées","Prédictions"),col=c("red","blue"),lty=rep(1,2),lwd = rep(2,2))



fitHW <- ets(quantity,model="AAA") 
summary(fitHW)



predHW <- forecast(fitHW,h=24) 
plot(predHW)
points(test,type='l',col='red',lwd=2) 
legend('top',c("Valeurs observées","Prédictions"),col=c("red","blue"),lty=rep(1,2),lwd = rep(2,2))





























