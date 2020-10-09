# carregando pacotes
library(quantmod)
library(Quandl)

# Download data
getSymbols(c("BBAS3.SA","^BVSP"),
           periodicity='monthly',
           from='2010-12-01',
           to='2020-09-30')
USD   <- Quandl('BCB/3695', order='asc', type='xts', start_date='2010-12-01')
selic <- Quandl("BCB/4390",type = 'xts', start_date='2011-01-01', end_date="2020-10-01")

# Verificar periodicidades
periodicity(BBAS3.SA)
periodicity(BVSP)
periodicity(USD)
periodicity(selic)

# Bindar em nível
mydata <- data.frame(cbind(BBAS3.SA[-1,6], BVSP[-1,6], USD[-1,]))
colnames(mydata) <- c("BBAS3","BVSP", "USD")

# visualize
head(mydata)
tail(mydata)

# Salvar Level
write.csv(mydata, file = "./data/mydata.csv", row.names = F, quote = F)
saveRDS(mydata, "./data/mydata.rds")

################################################
# log returns
ret.bbas3 <- monthlyReturn(BBAS3.SA[,6], type="log")[-1,]
ret.bvsp  <- monthlyReturn(BVSP[,6], type="log")[-1,]
ret.usd   <- monthlyReturn(USD, type="log")[-1,]

# Bindar rets
myrets <- data.frame(cbind(ret.bbas3, ret.bvsp, ret.usd, selic/100))
colnames(myrets) <- c("BBAS3","BVSP", "USD", "SELIC")

# visualize
head(myrets)
tail(myrets)


# Salvar Rets
write.csv(myrets, file = "./data/myrets.csv", row.names = F, quote = F)
saveRDS(myrets, "./data/myrets.rds")




