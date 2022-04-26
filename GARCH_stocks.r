data <-read.csv("G:/Rstudio/TS2020/Stocks/Russtockdaily.txt", sep='\t', dec=',')
install.packages("forecast")
library("forecast")
install.packages("lmtest")
library("lmtest")
install.packages("tseries")
library("tseries")
install.packages("urca")
library("urca")
install.packages("TSA")
library("TSA")


#ARMA ìîäåëü ïî öåíàì àêöèé
P<-data$SBER[101:4441]
plot(P)
logP<-log(P)
plot(logP)
d1SBER<-diff(logP, differences=1)
plot(d1SBER)

#ñëó÷àéíîå áëóæäàíèå èëè íåò?
Acf(d1SBER)
Box.test(d1SBER, lag = 8, type = c("Ljung-Box"), fitdf = 1)

#ARMA model
eacf(d1SBER)
ARMAmodel <- Arima(d1SBER, c(1,0,4), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(ARMAmodel)
summary(ARMAmodel)
Box.test(residuals(ARMAmodel), lag = 8, type = c("Ljung-Box"), fitdf = 5)


#íàëè÷èå ARCH-ıôôåêòà?
Acf(residuals(ARMAmodel)^2)
Box.test(residuals(ARMAmodel)^2, lag = 8, type = c("Ljung-Box"), fitdf = 1)


#ARCH-GARCH model
install.packages("rugarch")
library("rugarch")

Pacf(d1SBER^2, 100)
spec = ugarchspec(variance.model = list(model = 'gjrGARCH',garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 4), include.mean = TRUE, archm = TRUE), distribution.model = "std")
garch.fit = ugarchfit(spec, d1SBER)
garch.fit 
coef(garch.fit)

?ugarchspec

Acf(residuals(garch.fit))
Acf(residuals(garch.fit)^2)
Acf(residuals(garch.fit, standardize="TRUE"))
Acf(residuals(garch.fit, standardize="TRUE")^2)

write.csv(residuals(garch.fit, standardize="TRUE"), "G:/Rstudio/test_i.txt")
stres <-read.csv("G:/Rstudio/test_i.txt", sep=",", dec='.')
plot(stres)
shapiro.test(stres$V1)

Box.test(stres$V1, lag = 8, type = c("Ljung-Box"), fitdf = 1)
Box.test(stres$V1^2, lag = 8, type = c("Ljung-Box"), fitdf = 1)



#ïğîãíîç äîõîäíîñòè
prognoz <- ugarchforecast(garch.fit, n.ahead = 1000)
plot(prognoz@forecast$seriesFor)
Yforecast<-c(d1SBER, prognoz@forecast$seriesFor)
plot(Yforecast)



#ïğîãíîç ñèãìû
plot(sigma(garch.fit))
prognoz <- ugarchforecast(garch.fit, n.ahead = 1000)
plot(sigma(prognoz))

#ïğîãíîç ÑÊÎ
sigmaforecast<-c(garch.fit@fit$sigma, sigma(prognoz))
plot(sigmaforecast)


