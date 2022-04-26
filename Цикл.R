install.packages("forecast", dependencies = TRUE)
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
install.packages("Matrix")
library("Matrix")
install.packages("matlib")
library("matlib")
install.packages("C:/Users/AdMin/Desktop/учеба/Временные ряды/ЛР№4/portes", repos = NULL, type="source")
library("portes")
library('readr')
library('readxl')
library('dplyr')
install.packages("rmgarch")
install.packages("marima")
library('marima')
library('rmgarch')
data <- read_excel("C:/Users/AdMin/Desktop/учеба/ВКР/Data_LN/Banks_LN.xlsx")

'bigData - исходные показатели за весь период 2010-2021'
'В bigData - исходные показатели'
'5Y spread - зависимая'

'data - исходные показатели за период 2010-2019'

data_D1 = data[2:nrow(data),]
for(i in 1:ncol(data_D1)){
  data_D1[,i] = diff(cbind(data[,i])[,1])
}
tr1011 = c(1:519)
tr1213 = c(520:1041)
tr1415 = c(1042:1563)
tr1617 = c(1564:2084)
tr1819 = c(2085:2606)
tr1019 = c(1:2606)

train_per = as.data.frame(cbind(tr1011,tr1213,tr1415,tr1617,tr1819))
train_per[520:522,1] = rep(NA, 3)
train_per[522,4] = NA

te10 = c(1:259)
te11 = c(260:519)
te12 = c(520:780)
te13 = c(781:1041)
te14 = c(1042:1302)
te15 = c(1303:1563)
te16 = c(1564:1824)
te17 = c(1825:2084)
te18 = c(2085:2345)
te19 = c(2346:2606)
teCovid = c(2607:2867)

test_per = as.data.frame(cbind(te10, te11,te12, te13,te14, te15, te16, te17, te18, te19, teCovid))
test_per[260:261,1] = rep(NA, 2)
test_per[261,2] = NA
test_per[261,8] = NA

t=2
data_d1 = data_D1[as.numeric(train_per[,t] %>% na.omit()),]
pivot_table = data.frame(Maturity = 1:30, OOF_Error = 1:30, 'te10' = 1:30, 'te11' = 1:30 , 'te12' = 1:30, 'te13' = 1:30,
                         'te14' = 1:30, 'te15' = 1:30, 'te16' = 1:30, 'te17' = 1:30, 'te18' = 1:30, 'te19' = 1:30, 'teCovid' = 1:30)
pivot_table[1:3,1] = rep('X6M', 3)
pivot_table[4:6,1] = rep('X1Y', 3)
pivot_table[7:9,1] = rep('X2Y', 3)
pivot_table[10:12,1] = rep('X3Y', 3)
pivot_table[13:15,1] = rep('X4Y', 3)
pivot_table[16:18,1] = rep('X5Y', 3)
pivot_table[19:21,1] = rep('X7Y', 3)
pivot_table[22:24,1] = rep('X10Y', 3)
pivot_table[25:27,1] = rep('X20Y', 3)
pivot_table[28:30,1] = rep('X30Y', 3)

pivot_table[1:30,2] = rep(c('RMSE', 'MAE', 'MAPE'), 10)


for (w in c(7,2:6,8:11)){
  data_d1[,7] = data_d1[,w]
  data[,7] = data[,w]
  data_D1[,7] = data_D1[,w]
  
  VarDataInd = c(7,12,13,15,19)
  df = data.frame(data_d1[,VarDataInd])
  VARsel = VARselect(df, lag.max = 15, type="const")
  varsel = as.data.frame(VARsel$selection)[,1]
  p = ifelse((varsel[1]-varsel[2] == 0) &
               (varsel[3]-varsel[4] == 0), max(varsel[1],varsel[3]),
             ifelse((varsel[1]-varsel[3] == 0) &
                      (varsel[2]-varsel[4] == 0), max(varsel[1],varsel[2]),
                    ifelse((varsel[1]-varsel[4] == 0) &
                             (varsel[2]-varsel[3] == 0), max(varsel[1],varsel[2]),
                           ifelse((varsel[1]-varsel[2] == 0)||
                                    (varsel[1]-varsel[3] == 0)||
                                    (varsel[1]-varsel[4] == 0), varsel[1],
                                  ifelse((varsel[2]-varsel[3] == 0)||
                                           (varsel[2]-varsel[4] == 0), varsel[2],
                                         ifelse((varsel[3]-varsel[4] == 0),varsel[3], max(varsel)))))))
  
  var<-VAR(df,
           p = p,
           type = "const")
  
  Hosk = Hosking(var, lags=seq(var$p+0.5, 2*var$p, 0.5))
  LMcL = LiMcLeod(var, lags=seq(var$p+0.5, 2*var$p, 0.5))
  
  thresh = ifelse(var$p*ncol(df)>=4, 0.5, 0)
  resvar1<-restrict(var, method = c("ser"), thresh = thresh)
  
  for (t1 in 1:11){
    data_test = data.frame(data_D1[as.numeric(test_per[,t1] %>% na.omit()),VarDataInd], data_D1[as.numeric(test_per[,t1] %>% na.omit()),VarDataInd[1]])
    names(data_test) = c(names(resvar1$varresult), "ForecastCDS")
    CDS_forecast = as.numeric(c(data[as.numeric(test_per[,t1] %>% na.omit()),][resvar1$p+1,w], rep(0, nrow(data_test)-(resvar1$p+1)+1)))
    CDS_hist = data$X5Y[test_per[,t1] %>% na.omit()+1][(resvar1$p):(length(test_per[,t1] %>% na.omit()))]
    'ct - table of coefficients'
    ct = data.frame(as.data.frame(resvar1$varresult$X5Y$coefficients),
                    var=row.names(data.frame(as.data.frame(resvar1$varresult$X5Y$coefficients))))
    names(ct) = c('coef', 'var')
    for (q in (resvar1$p+1):nrow(data_test)) {
      forecast_q = 0
      m=1
      if(names(resvar1$varresult$X5Y$coefficients[length(resvar1$varresult$X5Y$coefficients)]) == 'const'){
        while(m <= length(resvar1$varresult$X5Y$coefficients)-1){
          forecast_m = ct$coef[m]*data_test[q-as.numeric(substr(ct$var[m], nchar(ct$var[m]), nchar(ct$var[m]))),
                                            which(colnames(data_test) == stringr::str_extract(ct$var[m], pattern = names(resvar1$varresult)) %>% na.omit())]
          forecast_q = forecast_q+forecast_m
          m=m+1
        }
        
        forecast_q = forecast_q+ct$coef[length(resvar1$varresult$X5Y$coefficients)]
      }else{
        while(m <= length(resvar1$varresult$X5Y$coefficients)){
          forecast_m = ct$coef[m]*data_test[q-as.numeric(substr(ct$var[m], nchar(ct$var[m]), nchar(ct$var[m]))),
                                            which(colnames(data_test) == stringr::str_extract(ct$var[m], pattern = names((resvar1$varresult))) %>% na.omit())]
          forecast_q = forecast_q+forecast_m
          m=m+1
        }
      }
      data_test[q,ncol(data_test)] = forecast_q
      CDS_forecast[q-resvar1$p+1] = CDS_hist[q-resvar1$p]+forecast_q
      
    }
    dataErr = data.frame(y = exp(CDS_hist[(15-resvar1$p+1):length(CDS_hist)]), y_f = exp(CDS_forecast[(15-resvar1$p+1):length(CDS_forecast)]))
    Resid = dataErr$y-dataErr$y_f
    RMSE = sqrt(mean((dataErr$y-dataErr$y_f)^2))
    MAE = mean(abs(dataErr$y-dataErr$y_f))
    MAPE = mean(abs(dataErr$y-dataErr$y_f)/dataErr$y)
    pivot_table[which(pivot_table$Maturity == colnames(data_D1)[w]),which(colnames(pivot_table) == colnames(test_per)[t1])] = c(RMSE,MAE,MAPE)
  }
  
}
write.csv(pivot_table, "C:/Users/AdMin/Desktop/учеба/ВКР/test_forecast/Banks/Banksnew_1011.csv")