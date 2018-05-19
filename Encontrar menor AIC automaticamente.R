################ for para encontrar melhor valor AIC #######################
    # Valor do AIC deve ser o menor possível (mesmo que negativo) #

www <- "http://www.massey.ac.nz/~pscowper/ts/cbe.dat"
CBE <- read.table(www, header = TRUE)
Elec.ts <- ts(CBE[,3], start = 1958, freq = 12)
Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts)~Time + I(Time^2) + factor(Imth))
acf(resid(Elec.lm))

data.lm <- Elec.lm
data.ts <- Elec.ts
Imth    <- cycle(data.ts)

best.order <- c(0,0,0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(data), order = best.order))
  best.aic <- fit.aic
}


# Previsão a partir do melhor modelo estimado
new.time <- seq(length(data.ts), length = 36)
new.data <- data.frame(Time = new.time, Imth = rep(1:12,3))
predict.lm <- predict(data.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 36)
data.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991, freq = 12)
ts.plot(cbind(data.ts, data.pred), lty = 1:2)


