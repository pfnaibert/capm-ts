library(ggplot2)
library(ggthemes)
library(scales)
library(reshape2)

library(gridExtra) # para vaios graficos juntos
library(lmtest) # testes: breusch-godfrey, breuch-pagan
library(tseries) # jarque-bera test
source("./funs.R")

# LOAD DATA
mydata <- readRDS("./data/mydata.rds"); mydata$date <- rownames(mydata); head(mydata)
myrets <- readRDS("./data/myrets.rds"); myrets$date <- rownames(myrets); head(myrets)
myretx <- cbind("date"=myrets[,5], myrets[,c(1,2,3)] - myrets[,4]); head(myretx)

# gráficos de nível
plot.level(mydata, "BBAS3", "BBAS3", "Monthly Price")
plot.level(mydata, "BVSP","BVSP", "Monthly Level")
plot.level(mydata, "USD", "BRL/USD", "Monthly Price")

# gráficos de retorno
plot.ret(myrets, "BBAS3", "BBAS3", "Monthly Return")
plot.ret(myrets, "BVSP","BVSP", "Monthly Return")
plot.ret(myrets, "USD", "BRL/USD", "Monthly Return")
plot.ret(myrets, "SELIC", "SELIC", "Monthly Rate")

# gráficos de retorno em excesso
plot.ret(myretx, "BBAS3", "BBAS3", "Monthly Excess Return")
plot.ret(myretx, "BVSP","BVSP", "Monthly Excess Return")
plot.ret(myretx, "USD", "BRL/USD", "Monthly Excess Return")

# CAPM
library(stargazer)
mod.1 <- lm(BBAS3 ~ BVSP, data=myrets)
summary(mod.1)
stargazer(mod.1, type="text", out="myCAPM.txt")

plot(myrets$BVSP, myrets$BBAS3)
abline(a=mod.2$coefficients[1], b=mod.2$coefficients[2])

# CAPM
mod.2 <- lm(BBAS3 ~ BVSP, data=myretx)
summary(mod.2)
stargazer(mod.2, type="text", out="myCAPM.txt")

plot(myretx$BVSP, myretx$BBAS3)
abline(a=mod.2$coefficients[1], b=mod.2$coefficients[2])

# Y = a +bX
BBSA3.pred = SELIC.pred + mod.2$coefficients[1] + mod.2$coefficients[2]*(BVSP.pred - SELIC.pred)
