data <-read.csv("G:/Rstudio/TS2020/IP_SemiQ.txt", sep='\t', dec=',')

install.packages("forecast")
library("forecast")
install.packages("lmtest")
library("lmtest")
install.packages("tseries")
library("tseries")
install.packages("vars")
library("vars")
install.packages("urca")
library("urca")
install.packages("TSA")
library("TSA")
install.packages("strucchange")
library("strucchange")





#пример US IP Semiconductors

d0<-data$IP_SEMI[seq(1,216)]
plot(d0)
d0<-log(d0)
plot(d0)

Pacf(d0)
ur.df(d0, type="drift", lags = 1, 
      selectlags = "Fixed")

d1<-diff(d0, differences=1)
plot(d1)
Pacf(d1)
ur.df(d1, type="drift", lags = 1, 
      selectlags = "Fixed")

eacf(d1)

#нулевая гипотеза: нет SB
modelARMA<-Arima(d0, c(1,1,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 6, type = c("Ljung-Box"), fitdf = 1)
forecast_ARMA<-forecast(modelARMA, h=60)
plot(forecast_ARMA)
forecast_ARMA
write.csv(forecast_ARMA, "G:/Rstudio/forecast.txt")


'структурный разрыв SUP-F'
b<-1
m<-30
e<-210

stats<-rep(NA, e)
while(m<e-30)
{
   'короткая модель'
   m_short <- Arima(d1[seq(b,e)], c(1,0,0), include.constant=TRUE, method = c("CSS-ML"))
   ess_sh <- sum(residuals(m_short)^2)
   q<-length(coef(m_short))
   'длинная модель'
   fit1 <- Arima(d1[seq(b,m)], order = c(1,0,0), include.mean = TRUE, method="CSS-ML")
   fit2 <- Arima(d1[seq(m+1,e)], order = c(1,0,0), include.mean = TRUE, method="CSS-ML")
   ess_lo <- sum(c(residuals(fit1), residuals(fit2))^2)
   'F стат'
   stats[m] <- (ess_sh - ess_lo)/(ess_lo/(e-b+1-2*q))
   m<-m+1
}
plot(stats, type = "l")
abline(h = qchisq(0.01, df = q, lower.tail = FALSE), lty = 2, col = "red")
which.max(stats)
1-pchisq(stats[which.max(stats)], df = q)
stats[which.max(stats)]




'прогноз на основе модели после'
b1<-which.max(stats)+1

d0<-data$IP_SEMI[seq(b1,e+1)]
d0<-log(d0)
plot(d0)

Pacf(d0)
ur.df(d0, type="drift", lags = 1, 
      selectlags = "Fixed")

d1<-diff(d0, differences=1)
Pacf(d1)
ur.df(d1, type="drift", lags = 1, 
      selectlags = "Fixed")

modelARMA_SB<-Arima(d0, c(1,1,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA_SB)
summary(modelARMA_SB)
Acf(residuals(modelARMA_SB))
Box.test(residuals(modelARMA_SB), lag = 6, type = c("Ljung-Box"), fitdf = 1)
forecast_ARMA_SB<-forecast(modelARMA_SB, h=60)
plot(forecast_ARMA_SB)
forecast_ARMA_SB
write.csv(forecast_ARMA_SB, "G:/Rstudio/forecast.txt")



#пакет strucchange
#данные
d0<-data$IP_SEMI[seq(1,211)]
d0<-log(d0)
d1<-diff(d0, differences=1)
d1<-d1[1:210]
d1_l1 <- c(0,d1[1:length(d1)-1])
d1_l2 <- c(0,0,d1[2:length(d1)-2])

#Sup-F test (supWald)
stat <- Fstats(d1 ~ d1_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")

#Ave-F test (supWald)
stat <- Fstats(d1 ~ d1_l1, from = 0.2, to = NULL)
plot(stat, alpha = 0.01, aveF=TRUE)
sctest(stat, type = "aveF")

#Exp-F test (supWald)
sctest(stat, type = "expF")


#empirical fluctuation process - estimates-based processes

#OLS-CUSUM вручную
fit <- Arima(d1, c(1,0,0), include.constant=TRUE, method = c("CSS"))
e <- residuals(fit)
sigma <- sqrt(fit$sigma2)
n <- length(d1)
cs <- cumsum(e)/(sigma*n^0.5)

require(strucchange)
retval <- list()
retval$coefficients <- coef(fit)
retval$sigma <- sigma
retval$process <- cs
retval$type.name <- "OLS-based CUSUM test"
retval$lim.process <- "Brownian bridge"
'retval$datatsp <- tsp(x)'
class(retval) <- c("efp")
plot(retval, alpha = 0.1)
sctest(retval)


# OLS-CUSUM   strucchange
datay <- data.frame(d1, d1_l1, d1_l2)
colnames(datay) <- c("y", "ylag1", "ylag2")
stat <- efp(y ~ ylag1,  type = "OLS-CUSUM", data = datay)
plot(stat, alpha = 0.1, functional = NULL)
sctest(stat)

#Recursive-CUSUM   strucchange
stat <- efp(y ~ ylag1,  type = "Rec-CUSUM", data = datay)

#OLS/Rec-MOSUM   strucchange
stat <- efp(y ~ ylag1,  type = "OLS-MOSUM", h=0.5, data = datay)
stat <- efp(y ~ ylag1,  type = "Rec-MOSUM", h=0.5, data = datay)

#empirical fluctuation process - estimates-based processes
stat <- efp(y ~ ylag1,  type = "RE", data = datay)
stat <- efp(y ~ ylag1,  type = "ME",h=0.5, data = datay)

#дополнительно
#примерно тот же функционал, что RE, ME
stat <- efp(y ~ ylag1,  type = "Score-CUSUM", data = datay)
stat <- efp(y ~ ylag1,  type = "Score-MOSUM", h=0.5, data = datay)

#empirical M-fluctuation process
datay <- data.frame(d1, d1_l1)
colnames(datay) <- c("y", "ylag1")
stat <- gefp(y ~ ylag1, data = datay)
plot(stat, alpha = 0.01)
sctest(stat)


# RE
datay <- data.frame(d1, d1_l1, d1_l2)
colnames(datay) <- c("y", "ylag1", "ylag2")
stat <- efp(y ~ ylag1,  type = "RE", data = datay)
plot(stat, alpha = 0.1, functional = NULL)
sctest(stat)

# ME
datay <- data.frame(d1, d1_l1, d1_l2)
colnames(datay) <- c("y", "ylag1", "ylag2")
stat <- efp(y ~ ylag1,  type = "ME",h=0.5, data = datay)
plot(stat, alpha = 0.1, functional = NULL)
sctest(stat)


#частичные структурные разрывы
#partial SB xlag1
datay <- data.frame(d1, d1_l1)
colnames(datay) <- c("y", "ylag1")
stat <- gefp(y ~ ylag1, data = datay, parm = "ylag1")
plot(stat, alpha = 0.1)
sctest(stat)


#partial SB интерсепт
stat <- gefp(y ~ ylag1, data = datay, parm = 1)
plot(stat, alpha = 0.1)
sctest(stat)



'Bai Perron'
d1<-ts(d1, start=1)
d1_l1 <- c(0,d1[1:length(d1)-1])
stat <- breakpoints(d1 ~ d1_l1)
summary(stat)
plot(stat)
## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(stat)
## confidence intervals
ci <- confint(stat)
breakdates(ci)
ci
plot(d1)
lines(ci)









#эксперимент с CUSUM на основе
x1 <- arima.sim(model = list(order = c(1,0,0), ar = 0.5), n = 100)*0.02+0.03
x2 <- arima.sim(model = list(order = c(1,0,0), ar = 0.5), n = 100)*0.02-0.03
x <- ts(c(x1, x2))
plot(x)
x_l1 <- c(0,x[1:length(x)-1])
x_l2 <- c(0,0,x[2:length(x)-2])


#Sup-F test (supWald)
stat <- Fstats(x ~ x_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")




#OLS-CUSUM strucchange
datax <- data.frame(x, x_l1, x_l2)
colnames(datax) <- c("x", "xlag1", "xlag2")
stat <- efp(x ~ xlag1, type = "OLS-CUSUM", data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#Rec-CUSUM strucchange
stat <- efp(x ~ xlag1, type = "Rec-CUSUM", data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#RE strucchange
stat <- efp(x ~ xlag1, type = "RE", data = datax)
plot(stat, alpha = 0.1, functional=NULL)
sctest(stat)






#OLS-MOSUM strucchange
stat <- efp(x ~ xlag1, type = "OLS-MOSUM", h=0.5, data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#Rec-MOSUM strucchange
stat <- efp(x ~ xlag1, type = "Rec-MOSUM", h=0.5, data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#ME strucchange
stat <- efp(x ~ xlag1, type = "ME", h=0.5, data = datax)
plot(stat, alpha = 0.1, functional=NULL)
sctest(stat)




#дополнительно
#примерно тот же функционал, что RE, ME

#Score-CUSUM strucchange
stat <- efp(x ~ xlag1, type = "Score-CUSUM", data = datax)
plot(stat, alpha = 0.1, functional=NULL)
sctest(stat)

#Score-MOSUM strucchange
stat <- efp(x ~ xlag1, type = "Score-MOSUM", h=0.5, data = datax)
plot(stat, alpha = 0.1, functional=NULL)
sctest(stat)

#empirical M-fluctuation process - strucchange
stat <- gefp(x ~ xlag1, data = datax)
plot(stat, alpha = 0.1, aggregate = FALSE)
plot(stat, alpha = 0.1)
sctest(stat)






#пример full versus partial SB

x1 <- arima.sim(model = list(order = c(2,0,0), ar = c(0.5, 0.3)), n = 100)*0.02+0.07
x2 <- arima.sim(model = list(order = c(2,0,0), ar = c(0.5, 0.3)), n = 100)*0.02+0.01
x <- ts(c(x1, x2))
plot(x)
x_l1 <- c(0,x[1:length(x)-1])
x_l2 <- c(0,0,x[2:length(x)-2])
datax <- data.frame(x, x_l1, x_l2)
colnames(datax) <- c("x", "xlag1", "xlag2")



#full SB xlag1
stat <- gefp(x ~ xlag1+xlag2, data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#partial SB xlag1
stat <- gefp(x ~ xlag1+xlag2, data = datax, parm = "xlag1")
plot(stat, alpha = 0.1)
sctest(stat)

#partial SB xlag2
stat <- gefp(x ~ xlag1+xlag2, data = datax, parm = "xlag2")
plot(stat, alpha = 0.1)
sctest(stat)

#partial SB интерсепт
stat <- gefp(x ~ xlag1+xlag2, data = datax, parm = 1)
plot(stat, alpha = 0.1)
sctest(stat)




















'имитация данных по ARIMA'
'ARMA(1,1)'
a1<-0.1
b1<-0
a0<-0
ts.sim<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                  sd = 0.2)+a0
plot (ts.sim)


'имитация данных по ARIMA'
'ARMA(1,1)'
a1<-0.5
b1<-0
a0<-0
ts.sim1<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                   sd = 0.2)+a0
plot (ts.sim1)


'имитация данных по ARIMA'
'ARMA(1,1)'
a1<-0.5
b1<-0
a0<--0.05
ts.sim2<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                   sd = 0.2)+a0
plot (ts.sim2)

'склеить три имитации'
ts.sim_all<-c(ts.sim, ts.sim1, ts.sim2)
plot (ts.sim_all, type="l")














'множество SB'
'имитация данных по ARIMA'
'ARMA(1,0)'
a1<-0.1
b1<-0
a0<-0
ts.sim<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                  sd = 0.2)+a0
plot (ts.sim)


'имитация данных по ARIMA'
'ARMA(1,1)'
a1<-0.3
b1<-0
a0<-0.1
ts.sim1<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                  sd = 0.2)+a0
plot (ts.sim1)


'имитация данных по ARIMA'
'ARMA(1,0)'
a1<--0.5
b1<-0
a0<-0
ts.sim2<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                   sd = 0.2)+a0
plot (ts.sim2)


'ARMA(1,0)'
a1<--0.2
b1<-0
a0<--0.05
ts.sim3<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                  sd = 0.2)+a0
plot (ts.sim3)


'ARMA(1,0)'
a1<-0.5
b1<-0
a0<-0.03
ts.sim4<-arima.sim(n = 100, list(ar = c(a1), ma=c(b1)),
                   sd = 0.2)+a0
plot (ts.sim4)


'склеить пять имитации'
ts.sim_all<-c(ts.sim, ts.sim1, ts.sim2, ts.sim3, ts.sim4)
plot (ts.sim_all, type="l")


'линейная модель'
Pacf(ts.sim_all)
eacf(ts.sim_all)

#нулевая гипотеза: нет SB
modelARMA<-Arima(ts.sim_all, c(2,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 6, type = c("Ljung-Box"), fitdf = 1)


#нулевая гипотеза: нет SB. Самая простая альтернатива
modelARMA<-Arima(ts.sim_all, c(1,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 6, type = c("Ljung-Box"), fitdf = 1)

x<-ts.sim_all   
plot(x, type="l")
x_l1 <- c(0,x[1:length(x)-1])
x_l2 <- c(0,0,x[2:length(x)-2])

#Sup-F test (supWald)
stat <- Fstats(x ~ x_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")

#Sup-F test (supWald) тест 'до'
x<-ts.sim_all 
x<-x[291:403]  
plot(x, type="l")
x_l1 <- c(0,x[1:length(x)-1])
x_l2 <- c(0,0,x[2:length(x)-2])
stat <- Fstats(x ~ x_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")


#Sup-F test (supWald) тест 'после'
x<-ts.sim_all 
x<-x[404:500]  
plot(x, type="l")
x_l1 <- c(0,x[1:length(x)-1])
x_l2 <- c(0,0,x[2:length(x)-2])
stat <- Fstats(x ~ x_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")


#оценка параметров 'после'
x<-ts.sim_all 
x<-x[404:500] 
modelARMA<-Arima(x, c(1,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 4, type = c("Ljung-Box"), fitdf = 1)






'Bai Perron'
x<-ts.sim_all 
d1<-ts(x, start=1)
d1_l1 <- c(0,d1[1:length(d1)-1])
stat <- breakpoints(d1 ~ d1_l1)
summary(stat)
plot(stat)
## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(stat)
## confidence intervals
ci <- confint(stat)
breakdates(ci)
ci
plot(d1)
lines(ci)














#OLS-CUSUM strucchange
datax <- data.frame(x, x_l1, x_l2)
colnames(datax) <- c("x", "xlag1", "xlag2")
stat <- efp(x ~ xlag1, type = "OLS-CUSUM", data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#Rec-CUSUM strucchange
stat <- efp(x ~ xlag1, type = "Rec-CUSUM", data = datax)
plot(stat, alpha = 0.1)
sctest(stat)

#RE strucchange
stat <- efp(x ~ xlag1, type = "RE", data = datax)
plot(stat, alpha = 0.1, functional=NULL)
sctest(stat)











