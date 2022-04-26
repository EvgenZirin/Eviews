data <-read.csv("G:/Rstudio/DJIA_1919/DJIA_1919.txt", sep='\t', dec=',')
data <-read.csv("G:/Rstudio/DJIA_1896/_new/RDJA_SB_IPI.txt", sep='\t', dec=',')
data <-read.csv("G:/Rstudio/DJIA_1896/DJA.txt", sep='\t', dec=',')
data <-read.csv("G:/Rstudio/TS2020/Stocks/Russtockdaily.txt", sep='\t', dec=',')

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



d0<-log(data$SBER[101:4441])

#ARMA
plot(d0)
d1<-diff(d0, differences=1)
eacf(d1)

modelARMA<-Arima(d0, c(0,1,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 6, type = c("Ljung-Box"), fitdf = 0)
forecast_ARMA<-forecast(modelARMA, h=600)
plot(forecast_ARMA)



#GARCH
install.packages("rugarch")
library("rugarch")


Pacf(d1^2, 100)
spec = ugarchspec(variance.model = list(model = 'sGARCH',garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE), distribution.model = "norm")
garch.fit = ugarchfit(spec, d1)
garch.fit 
coef(garch.fit)

Acf(residuals(garch.fit))
Acf(residuals(garch.fit)^2)
Acf(residuals(garch.fit, standardize="TRUE"))
Acf(residuals(garch.fit, standardize="TRUE")^2)




#d1 ñêîğğåêòèğîâàííàÿ íà èçìåí÷èâóş âîëàòèëüíîñòü (íà ARCH-ıôôåêò)
d1adj<-d1/garch.fit@fit$sigma*sd(residuals(garch.fit))
plot(d1adj)


#Sup-F òåñò äëÿ d1
d1_l1 <- c(0,d1[1:length(d1)-1])
d1_l2 <- c(0,0,d1[2:length(d1)-2])
stat <- Fstats(d1 ~ 1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")

#Sup-F òåñò äëÿ d1adj
d1adj<-d1adj[1:length(d1adj)]
d1adj_l1 <- c(0,d1adj[1:length(d1adj)-1])
d1adj_l2 <- c(0,0,d1adj[2:length(d1adj)-2])
stat <- Fstats(d1adj ~ 1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")
data$date[2330]


#Arma íà ïîäïåğèîäå
d1<-diff(d0, differences=1)
d1<-d1[2027:length(d1)]
eacf(d1)

modelARMA<-Arima(d1, c(0,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(modelARMA)
summary(modelARMA)
Acf(residuals(modelARMA))
Box.test(residuals(modelARMA), lag = 8, type = c("Ljung-Box"), fitdf = 0)
forecast_ARMA<-forecast(modelARMA, h=600)
plot(forecast_ARMA)


#Bai Perron äëÿ d1
d1<-diff(d0, differences=1)
d1<-ts(d1, start=1)
d1_l1 <- c(0,d1[1:length(d1)-1])
stat <- breakpoints(d1 ~ 1)
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


#Bai Perron äëÿ d1adj
d1<-ts(d1adj, start=1)
d1_l1 <- c(0,d1[1:length(d1)-1])
stat <- breakpoints(d1 ~ 1)
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