
library(readr)
quantity <- as.data.frame(read_csv("dataexamen.csv"))
View(quantity)


#2
summary(quantity)
str(quantity)

hist(quantity)

plot.ts(quantity)



quantity <- ts(quantity,start=2004,frequency=12)

#chronogramme
plot(quantity,xlab='Temps',ylab="Evolution de la quantité")

#plot de la série par mois
monthplot(quantity)

#auto-corrélations
lag.plot(quantity,lags=12,layout=c(3,4),do.lines=TRUE)

result_acf=acf(quantity)
print(data.frame(result_acf$lag,result_acf$acf)[1:10,])



plot  ( 1:length(quantity),   quantity,type="l")
points((1:length(quantity))-1,quantity,type="l",col="red")


result_pacf=pacf(quantity)
print(data.frame(result_pacf$lag,result_pacf$acf)[1:10,])



fit1 <- decompose(quantity) 
plot(fit1)


plot(quantity,xlab='Temps',ylab="Evolution de la quantité",main='decompose( ) avec modèle additif')
points(fit1$trend,type='l',col=2) 
points(fit1$trend+fit1$seasonal,type='l',col='purple') 
legend('topleft',c(expression(X[t]),expression(m[t]),expression(m[t]+s[t])),col=c (1,2,'purple'),lty=1)




fit2 <- decompose(quantity,type='multiplicative') 
plot(fit2)




plot(quantity,xlab='Temps',ylab="Evolution de la quantité",main='decompose( ) avec modèle multiplicatif')
points(fit2$trend,type='l',col=2)
points(fit2$trend*fit2$seasonal,type='l',col='purple') 
legend('topright',c(expression(X[t]),expression(m[t]),expression(m[t]*s[t])),col=c (1,2,'purple'),lty=1)





plot(fit2$figure,type='l',xlab='mois',ylab='motif périodique')






quantity <- ts(quantity,start=2004,frequency=12,end=2014)
fit3 <- stl(quantity,s.window=12)
plot(fit3)




plot(quantity,xlab='Temps',ylab="Evolution de la quantité",main='stl') 
points(fit3$time.series[,2],type='l',col=2) 
points(fit3$time.series[,2]+fit3$time.series[,1],type='l',col='purple') 
legend('topleft',c(expression(X[t]),expression(m[t]),expression(m[t]+s[t])),col=c (1,2,'purple'),lty=1)






