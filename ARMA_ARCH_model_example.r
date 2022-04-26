data <-read.csv("G:/Rstudio/DJIA_1896/DJA.txt", sep='\t', dec=',')
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


#ARMA ìîäåëü ïî èíôëÿöèè

#ğàñ÷åò  ğàçíîñòåé
d1CPI<-diff(data$CPI, differences=1)
d2CPI<-diff(data$CPI, differences=2)
d3CPI<-diff(data$CPI, differences=3)


#ëîãàğèôì óğîâíÿ öåí
plot(data$CPI)
#óğîâåíü öåí
plot(exp(data$CPI))
#ãîäîâàÿ èíôëÿöèÿ (íåïğåğûâíî íà÷. äîõîäíîñòü èëè ëîãäîõîäíîñòü)
plot(d1CPI)

#Ïğîâåğêà íà ñòàöèîíàğíîñòü Óğîâíÿ öåí
Pacf(data$CPI)
ur.df(data$CPI, type="drift", lags = 1, 
      selectlags = "Fixed")

#Ïğîâåğêà íà ñòàöèîíàğíîñòü ãîäîâîé èíôëÿöèè
Pacf(d1CPI)
ur.df(d1CPI, type="drift", lags = 4, 
      selectlags = "Fixed")


#ñòğîèì ìîäåëü ARMA
eacf(d1CPI)
m_infl_v2_ARMA11 <- Arima(data$CPI, c(1,1,1), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(m_infl_v2_ARMA11)
summary(m_infl_v2_ARMA11)
Acf(residuals(m_infl_v2_ARMA11))
Box.test(residuals(m_infl_v2_ARMA11), lag = 7, type = c("Ljung-Box"), fitdf = 2)
shapiro.test(residuals(m_infl_v2_ARMA11))

#ARCH ıôôåêò
Acf(residuals(m_infl_v2_ARMA11)^2)
Box.test(residuals(m_infl_v2_ARMA11)^2, lag = 7, type = c("Ljung-Box"), fitdf = 2)
Pacf(residuals(m_infl_v2_ARMA11)^2)


#ARCH norm
install.packages("rugarch")
library("rugarch")

spec = ugarchspec(variance.model = list(model = 'sGARCH',garchOrder = c(15, 0)), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), distribution.model = "norm")
garch.fit = ugarchfit(spec, d1CPI)
garch.fit 
coef(garch.fit)


Acf(residuals(garch.fit))
Acf(residuals(garch.fit)^2)
Acf(residuals(garch.fit, standardize="TRUE"))
Acf(residuals(garch.fit, standardize="TRUE")^2)


write.csv(residuals(garch.fit, standardize="TRUE"), "G:/Rstudio/test_i.txt")
stres <-read.csv("G:/Rstudio/test_i.txt", sep=',', dec='.')
shapiro.test(stres$V1)

Box.test(stres$V1, lag = 7, type = c("Ljung-Box"), fitdf = 2)
Box.test(stres$V1^2, lag = 7, type = c("Ljung-Box"), fitdf = 2)


#ARCH std
spec = ugarchspec(variance.model = list(model = 'sGARCH',garchOrder = c(15, 0)), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), distribution.model = "std")
garch.fit = ugarchfit(spec, d1CPI)
garch.fit 
coef(garch.fit)


Acf(residuals(garch.fit))
Acf(residuals(garch.fit)^2)
Acf(residuals(garch.fit, standardize="TRUE"))
Acf(residuals(garch.fit, standardize="TRUE")^2)


write.csv(residuals(garch.fit, standardize="TRUE"), "G:/Rstudio/test_i.txt")
stres <-read.csv("G:/Rstudio/test_i.txt", sep=',', dec='.')

Box.test(stres$V1, lag = 7, type = c("Ljung-Box"), fitdf = 2)
Box.test(stres$V1^2, lag = 7, type = c("Ljung-Box"), fitdf = 2)


#GARCH
spec <- ugarchspec(variance.model = list(model = 'sGARCH',garchOrder = c(1, 2)), mean.model = list(armaOrder = c(2, 1), include.mean = TRUE), distribution.model = "norm")
garch.fit <- ugarchfit(spec, d1CPI)
garch.fit 
coef(garch.fit)

Acf(residuals(garch.fit))
Acf(residuals(garch.fit)^2)
Acf(residuals(garch.fit, standardize="TRUE"))
Acf(residuals(garch.fit, standardize="TRUE")^2)

write.csv(residuals(garch.fit, standardize="TRUE"), "G:/Rstudio/test_i.txt")
stres <-read.csv("G:/Rstudio/test_i.txt", sep=',', dec='.')

Box.test(stres$V1, lag = 7, type = c("Ljung-Box"), fitdf = 3)
Box.test(stres$V1^2, lag = 7, type = c("Ljung-Box"), fitdf = 3)

#ïğîãíîç
prognoz <- ugarchforecast(garch.fit, n.ahead = 299)
sigma(prognoz)
prognoz@forecast$sigmaFor
prognoz@forecast$seriesFor

#ïğîãíîç d1CPI
CPIforecast<-c(d1CPI, prognoz@forecast$seriesFor)
plot(CPIforecast)

#ïğîãíîç ÑÊÎ
sigmaforecast<-c(garch.fit@fit$sigma, sigma(prognoz))
plot(sigmaforecast)

