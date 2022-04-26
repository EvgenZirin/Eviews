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


'ARMA ìîäåëü ïî èíôëÿöèè'


'ğàñ÷åò  ğàçíîñòåé'
d1CPI<-diff(data$CPI, differences=1)
d2CPI<-diff(data$CPI, differences=2)
d3CPI<-diff(data$CPI, differences=3)


'ëîãàğèôì óğîâíÿ öåí'
plot(data$CPI)
'óğîâåíü öåí'
plot(exp(data$CPI))
'ãîäîâàÿ èíôëÿöèÿ (íåïğåğûâíî íà÷. äîõîäíîñòü èëè ëîãäîõîäíîñòü)'
plot(d1CPI)

'Ïğîâåğêà íà ñòàöèîíàğíîñòü Óğîâíÿ öåí'
Pacf(data$CPI)
ur.df(data$CPI, type="drift", lags = 1, 
      selectlags = "Fixed")
'âûâîä - ãèïîòåçà î íåñòàöèîíàğíîñòè óğîâåíÿ öåí íå îòêëîíÿåòñÿ'

'Ïğîâåğêà íà ñòàöèîíàğíîñòü ãîäîâîé èíôëÿöèè'
Pacf(d1CPI)
ur.df(d1CPI, type="drift", lags = 4, 
      selectlags = "Fixed")
'âûâîä - ãèïîòåçà î íåñòàöèîíàğíîñòè ãîäîâîé èíôëÿöèè îòêëîíÿåòñÿ, ïî ãîäîâîé èíôëÿöèè  ìîæíî ñòğîèòü ARMA ìîäåëü'

'ñòğîèì ìîäåëü AR ïî ãîäîâîé èíôëÿöèè'
'âàğèàíò 1. ÷åğåç ôóíêöèş ëèíåéíîé ğåãğåññèè (OLS)'
d1CPI_l1 <- c(0,d1CPI[1:length(d1CPI)-1])
d1CPI_l2 <- c(0,0,d1CPI[2:length(d1CPI)-2])
d1CPI_l3 <- c(0,0,0,d1CPI[3:length(d1CPI)-3])
d1CPI_l4 <- c(0,0,0,0,d1CPI[4:length(d1CPI)-4])
d1CPI_l5 <- c(0,0,0,0,0,d1CPI[5:length(d1CPI)-5])
d1CPI_l6 <- c(0,0,0,0,0,0,d1CPI[6:length(d1CPI)-6])
d1CPI_l7 <- c(0,0,0,0,0,0,0,d1CPI[7:length(d1CPI)-7])
d1CPI_l8 <- c(0,0,0,0,0,0,0,0,d1CPI[8:length(d1CPI)-8])
d1CPI_l9 <- c(0,0,0,0,0,0,0,0,0,d1CPI[9:length(d1CPI)-9])
m_infl_v1_AR4=lm(d1CPI~d1CPI_l1+d1CPI_l2+d1CPI_l3+d1CPI_l4)
summary(m_infl_v1_AR4)
sd(residuals(m_infl_v1_AR4))
Acf(residuals(m_infl_v1_AR4))
Box.test(residuals(m_infl_v1_AR4), lag = 7, type = c("Ljung-Box"), fitdf = 4)
m_infl_v1_AR6=lm(d1CPI~d1CPI_l1+d1CPI_l2+d1CPI_l3+d1CPI_l4+d1CPI_l5+d1CPI_l6)
sd(residuals(m_infl_v1_AR6))
summary(m_infl_v1_AR6)
Acf(residuals(m_infl_v1_AR6))
Box.test(residuals(m_infl_v1_AR6), lag = 7, type = c("Ljung-Box"), fitdf = 6)


'âàğèàíò 2. ÷åğåç ôóíêöèş Arima ïàêåòà Forecast'
'AR4'
m_infl_v2_AR4 <- Arima(data$CPI, c(4,1,0), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(m_infl_v2_AR4)
summary(m_infl_v2_AR4)
'NB:  mean èëè intercept â ìîäåëè âàğèàíòà 2 íå ğàâåí intercept â ìîäåëè âàğèàíòà 1'
'ıòî ïîòîìó, ÷òî â ôóíêöèè Arima intercept åñòü mean(yt), ñîîòâåòñòâåííî,'
'a0 åñòü mean(yt)*(1-ñóìì(ak))'
Acf(residuals(m_infl_v2_AR4))
Box.test(residuals(m_infl_v2_AR4), lag = 7, type = c("Ljung-Box"), fitdf = 4)

'AR6'
m_infl_v2_AR6 <- Arima(data$CPI, c(6,1,0), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(m_infl_v2_AR6)
summary(m_infl_v2_AR6)
'NB:  mean èëè intercept â ìîäåëè âàğèàíòà 2 íå ğàâåí intercept â ìîäåëè âàğèàíòà 1'
'ıòî ïîòîìó, ÷òî â ôóíêöèè Arima intercept åñòü mean(yt), ñîîòâåòñòâåííî,'
'a0 åñòü mean(yt)*(1-ñóìì(ak))'
Acf(residuals(m_infl_v2_AR6))
Box.test(residuals(m_infl_v2_AR6), lag = 7, type = c("Ljung-Box"), fitdf = 6)

'ñòğîèì ìîäåëü MA'
Acf(d1CPI)
m_infl_v2_MA14 <- Arima(data$CPI, c(0,1,14), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(m_infl_v2_MA14)
summary(m_infl_v2_MA14)
Acf(residuals(m_infl_v2_MA14))
Box.test(residuals(m_infl_v2_MA14), lag = 17, type = c("Ljung-Box"), fitdf = 14)

'ñòğîèì ìîäåëü ARMA'
eacf(d1CPI)
m_infl_v2_ARMA11 <- Arima(data$CPI, c(1,1,1), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(m_infl_v2_ARMA11)
summary(m_infl_v2_ARMA11)
Acf(residuals(m_infl_v2_ARMA11))
Box.test(residuals(m_infl_v2_ARMA11), lag = 7, type = c("Ljung-Box"), fitdf = 2)


'ñğàâíåíèå ìîäåëåé ïî AIC'
m_infl_v2_AR4$aic
m_infl_v2_AR6$aic
m_infl_v2_MA14$aic
m_infl_v2_ARMA11$aic

'ñğàâíåíèå ìîäåëåé ïî BIC'
m_infl_v2_AR4$bic
m_infl_v2_AR6$bic
m_infl_v2_MA14$bic
m_infl_v2_ARMA11$bic
m_infl_v2_ARMA55$bic

'ïğîãíîçèğîâàíèå'
forecast_AR4<-forecast(m_infl_v2_AR4, h=500)
plot(forecast_AR4)
forecast_AR4
write.csv(forecast_AR4, "G:/Rstudio/test_i.txt")
?Arima
?forecast

forecast_AR6<-forecast(m_infl_v2_AR6, h=500)
plot(forecast_AR6) 

forecast_MA14<-forecast(m_infl_v2_MA14, h=500)
plot(forecast_MA14)

forecast_ARMA11<-forecast(m_infl_v2_ARMA11, h=500)
plot(forecast_ARMA11)

auto.arima(data$CPI)
?AIC
AIC(m_infl_v2_ARMA11)

'ôóíêöèÿ auto.arima'
auto.arima(data$CPI, ic = "aic")
auto<-auto.arima(data$CPI, ic = "aicc")
forecast_auto<-forecast(auto, h=500)
plot(forecast_auto)






'èìèòàöèÿ äàííûõ ïî ARIMA'
'ARMA(1,1)'
a1<-0.1
b1<-0.3
a0<-0
ts.sim<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                  sd = 0.2)+a0
plot (ts.sim)


'ARMA(0,0)'
mean<-0.5
sd<-0.5
ts.sim1 <- arima.sim(n = 100, list())
ts.sim1<-mean+ts.sim1*sd
plot (ts.sim1)

'ñêëåèòü äâå èìèòàöèè'
ts.sim_both<-c(ts.sim, ts.sim1)
plot (ts.sim_both, type="l")


'ARIMA(1,1,1)'
a1<-0.1
b1<-0.3
a0<-0
P0<-100
ts.sim2 <- arima.sim(n = 100, list(order = c(1,1,1), ar = c(a1), ma=c(b1)), sd = 0.2)+a0
ts.sim2<-c(P0,P0+ts.sim2)
plot (ts.sim2, type="l")






'ñïğàâî÷íî'
'àëãîğèòì ïåğåáîğà ìîäåëåé ïî AIC'
'ğàáîòàåò 10-15 ìèíóò'
aic_min<-0
p<-0
while (p<13){
q<-0
    while (q<13){
    a<-Arima(data$CPI, c(p,1,q), include.constant =TRUE, method = c("CSS-ML"))  
    'print(a$aic)'
    if (a$aic<aic_min)
    {
      aic_min<-a$aic
      p_opt<-p
      q_opt<-q
    }
    q<-q+1
    }
  p<-p+1
}
p_opt
q_opt
aic_min 


'Arch-Garch'
datae2 <-read.csv("F:/Rstudio/TS2020/e2.txt", sep='\t', dec=',')

e2<-datae2$e2[seq(1200,1426)]
e2<-datae2$e2[seq(1200,1426)]
plot(e2)
Pacf(e2)
eacf(e2)

m_e2<-Arima(e2, c(1,0,1), include.constant =TRUE, method = c("CSS-ML"))  
forecast_m_e2<-forecast(m_e2, h=300)
summary(m_e2)
plot(forecast_m_e2) 
mn<-c(e2^0.5, forecast_m_e2$mean[seq(1,300)]^0.5)
up<-c(e2^0.5, forecast_m_e2$upper[seq(1,300)]^0.5)
dn<-c(e2^0.5, forecast_m_e2$lower[seq(1,300)]^0.5)
dn
plot(dn)
lines(mn)
lines(up)


forecast_m_e2$lower
forecast_m_e2$lower^0.5


'ìîäåëü íà ëîãàğèôìå'
e2<-log(datae2$e2)
plot(e2[seq(1200,1426)])
Pacf(e2[seq(1200,1426)])
eacf(e2[seq(1200,1426)])

m_e2<-Arima(e2, c(1,0,1), include.constant =TRUE, method = c("CSS-ML"))  
forecast_m_e2<-forecast(m_e2, h=300)
plot(forecast_m_e2) 
mn<-c(exp(e2[seq(1200,1426)])^0.5, exp(forecast_m_e2$mean[seq(1,300)])^0.5)
up<-c(exp(e2[seq(1200,1426)])^0.5,exp(forecast_m_e2$upper[seq(1,300)])^0.5)
dn<-c(exp(e2[seq(1200,1426)])^0.5,exp(forecast_m_e2$lower[seq(1,300)])^0.5)
plot(mn)
lines(dn)
lines(up)



'EGARCH'

install.packages("rugarch")
plot(d1CPI)
library("rugarch")

install.packages("fGarch")
library("fGarch")

spec = ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1, 1)), mean.model = list(armaOrder = c(4, 0), include.mean = TRUE))
egarchsnp.fit = ugarchfit(spec, d1CPI)
egarchsnp.fit 
coef(egarchsnp.fit)

?ugarchspec


?garchFit
model = garchFit(~arma(4,0)+garch(1,1), data = d1CPI, trace = TRUE)
summary(model)
plot(e2)

?ugarchspec


spec
spec <- ugarchspec()
show(spec)

