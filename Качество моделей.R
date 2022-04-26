library(dplyr)
library(ggplot2)

data <- read.csv("C:/COVID2.csv",header = TRUE, sep =";")
ds1 <- data$Date
ds<-strptime(ds1, format = "%d.%m.%Y")
y <- data$RUSSIA
data2<- data.frame(y,ds)


ggplot(data2, aes(x = ds, y = y)) + geom_point() +
  stat_smooth(method = "lm") + 
  xlab("Дата") + 
  ylab("Количество заразившихся")


ggplot(data2, aes(x = ds, y = y)) + geom_point() +
  stat_smooth(method = "auto") + 
  xlab("Дата") + 
  ylab("Количество заразившихся")

y <- data2$y
x <- data2$ds

#  Строим модель с одним предиктором:
n <- dim(as.matrix(x))[1]
m <- dim(as.matrix(x))[2] 
M_reg <- lm(y ~ x)
pred <- predict(M_reg)
RSS <- sum((y - pred) * (y - pred))
RMSE <- sqrt(RSS/n)
RSE <- sqrt(RSS/(n - m - 1)) 
c(RSS, RMSE, RSE)

Rsquared <- 1 - RSS/sum((mean(y) - y)^2)
F <- (sum((mean(y) - pred)^2)/m)/(RSS/(n - m - 1))
p <- pf(q = F, df1 = m, df2 = (n - m - 1), lower.tail = FALSE)
c(Rsquared, F, p)

summary(M_reg)
anova(M_reg)


#ЗАДАНИЕ
#НЕ линейная модель - полином второй степени

ggplot(data2, aes(x = ds, y = y)) + geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,2)) + 
  xlab("Дата") + 
  ylab("Количество заразившихся")

M_reg2 <- lm(y ~ poly(x,2))
pred2 <- predict(M_reg2)
RSS2 <- sum((y - pred2) * (y - pred2))
RMSE2 <- sqrt(RSS2/n)
RSE2 <- sqrt(RSS2/(n - m - 1)) 
c(RSS2, RMSE2, RSE2)

Rsquared2 <- 1 - RSS2/sum((mean(y) - y)^2)
F2 <- (sum((mean(y) - pred)^2)/m)/(RSS2/(n - m - 1))
p2 <- pf(q = F2, df1 = m, df2 = (n - m - 1), lower.tail = FALSE)
c(Rsquared2, F2, p2)

summary(M_reg2)
anova(M_reg2)




