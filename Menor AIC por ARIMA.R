library("car")
library("moments")
library("nortest")
library("e1071")


# Leitura da base de dados
x.ts <- log(AirPassengers)
x.ts <- AirPassengers

plot(x.ts)
points(x.ts, pch=19, cex=0.5)

# Transforma��o da base de dados em S�ries Temporais

# Explora��o dos dados (ACF, PACF, BOXPLOT, ETC...)
acf(x.ts) # Com tend�ncia e sazonalidade
acf(diff(x.ts)) # Aparentemente correlacionados
pacf(x.ts)

decomp = stl(x.ts, s.window = "periodic")
plot(decomp)

trend = decompose(x.ts, type="additive")
plot(x.ts)
points(x.ts, pch=19, cex=0.5)
lines(trend$trend)
decomp2 = stl(x.ts, s.window = "periodic", t.window=10)
lines(decomp2$time.series[,2], col="blue")

Box.test(x.ts, lag=7, type="Ljung-Box", fitdf=0) #abaixo de 0,05 a correla��o � significativa

plot(x.ts-trend$trend)


# F U N � � O   D E   D E T E C � � O 
# Resultado obtido aqui deve ser mais ou menos o imaginado na explora��o dos dados
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  
  for (p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
    for (P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order=c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2*fit$loglik + (log(n)+1)*length(fit$coef)
      
      if(fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, 
       best.fit, 
       best.model
       )
}


# A N � L I S E   F I N A L 
best.arima.select <- get.best.arima(x.ts, maxord = c(1,1,1,1,1,1))
best.arima.select <- get.best.arima(x.ts, maxord = c(2,2,2,2,2,2))
best.arima.select <- get.best.arima(x.ts, maxord = c(3,3,3,3,3,3))

# Estima��o dada pelo ajuste do modelo
best.arima.fit <- best.arima.select[[2]]

# Mostra se todos os termos s�o significativos. Caso n�o seja, haver� um *
summary.ARIMA = function(adj){
  coefs = adj$coef
  desvio = sqrt(diag(adj$var.coef))
  tval = coefs/desvio
  rdf = length(adj$residuals)-length(coefs)
  results=cbind(coefs, desvio, tval, 2*pt(abs(tval), rdf, lower.tail = FALSE))
  colnames(results) = c("estimates", "std.err.", "t value", "Pr(>|t|)")
  results
}

summary.ARIMA(best.arima.fit)




# T E S T E   D E   N O R M A L I D A D E   D O S   R E S � D U O S 
# Teste se o res�duo � ou n�o autocorrelacionado
# Res�duo dever� ser parecido com ru�do branco, sem correla��o
acf(resid(best.arima.fit))
x11()
tsdiag(best.arima.fit)
shapiro.test(best.arima.fit$residuals) 
# Abaixo de 0,05 podemos aceitar que os res�duos seguem distribui��o aprox. normal
# OK para AirPassengers, res�duos s�o n�o correlcionados

skewness(resid(best.arima.fit)) # Valor deve ser pr�ximo a 0,3

qqnorm(resid(best.arima.fit))
qqline(resid(best.arima.fit))

probplot(resid(best.arima.fit), qdist=qnorm) # M�dia 0. Aprox. Normal


# A P � S   D E C I D I R   S O B R E   O   M E L H O R   M O D E L O 
# Valores de ARIMA(p,d,q)(P,D,Q)
p <- best.arima.select[[3]][1]
d <- best.arima.select[[3]][2]
q <- best.arima.select[[3]][3]
P <- best.arima.select[[3]][4]
D <- best.arima.select[[3]][5]
Q <- best.arima.select[[3]][6]

# E S T I M A � � O 

# Estima��o modelo ARIMA(p,d,q)(P,D,Q)
x.ajuste <- arima(x.ts, order = c(p,d,q),
                 seas = list(order=c(P,D,Q),
                 frequency(x.ts)), method = "CSS")
# Estima��o modelo HoltWinters
x.ajuste <- HoltWinters(x.ts, beta=0.9, gamma=FALSE) #sem sazonallidade gamma = FALSE
x.ajuste <- HoltWinters(x.ts) #com tend�ncia e sazonalidade


# P R E D I � � O
x.predict.ajuste <- predict(x.ajuste, n.ahead = 12)$pred


# G R � F I C O   F I N A L 
ts.plot(cbind( x.ts,
              predict(x.ajuste, n.ahead=24, prediction.interval = TRUE,
                      level = 0.95)$pred), lty = 1:2)


# Armazena predi��o
p <- predict(x.ajuste, n.ahead=24, prediction.interval = TRUE,
        level = 0.95)$pred

# Un��o dos dados antigos com predi��o
base = ts.union(x.ts,p)

# Final Table
final.table = cat(toJSON(base, pretty = FALSE))
