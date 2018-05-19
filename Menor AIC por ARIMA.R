library("car")
library("moments")
library("nortest")
library("e1071")


# Leitura da base de dados
x.ts <- log(AirPassengers)
x.ts <- AirPassengers

plot(x.ts)
points(x.ts, pch=19, cex=0.5)

# Transformação da base de dados em Séries Temporais

# Exploração dos dados (ACF, PACF, BOXPLOT, ETC...)
acf(x.ts) # Com tendência e sazonalidade
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

Box.test(x.ts, lag=7, type="Ljung-Box", fitdf=0) #abaixo de 0,05 a correlação é significativa

plot(x.ts-trend$trend)


# F U N Ç Ã O   D E   D E T E C Ç Ã O 
# Resultado obtido aqui deve ser mais ou menos o imaginado na exploração dos dados
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


# A N Á L I S E   F I N A L 
best.arima.select <- get.best.arima(x.ts, maxord = c(1,1,1,1,1,1))
best.arima.select <- get.best.arima(x.ts, maxord = c(2,2,2,2,2,2))
best.arima.select <- get.best.arima(x.ts, maxord = c(3,3,3,3,3,3))

# Estimação dada pelo ajuste do modelo
best.arima.fit <- best.arima.select[[2]]

# Mostra se todos os termos são significativos. Caso não seja, haverá um *
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




# T E S T E   D E   N O R M A L I D A D E   D O S   R E S Í D U O S 
# Teste se o resíduo é ou não autocorrelacionado
# Resíduo deverá ser parecido com ruído branco, sem correlação
acf(resid(best.arima.fit))
x11()
tsdiag(best.arima.fit)
shapiro.test(best.arima.fit$residuals) 
# Abaixo de 0,05 podemos aceitar que os resíduos seguem distribuição aprox. normal
# OK para AirPassengers, resíduos são não correlcionados

skewness(resid(best.arima.fit)) # Valor deve ser próximo a 0,3

qqnorm(resid(best.arima.fit))
qqline(resid(best.arima.fit))

probplot(resid(best.arima.fit), qdist=qnorm) # Média 0. Aprox. Normal


# A P Ó S   D E C I D I R   S O B R E   O   M E L H O R   M O D E L O 
# Valores de ARIMA(p,d,q)(P,D,Q)
p <- best.arima.select[[3]][1]
d <- best.arima.select[[3]][2]
q <- best.arima.select[[3]][3]
P <- best.arima.select[[3]][4]
D <- best.arima.select[[3]][5]
Q <- best.arima.select[[3]][6]

# E S T I M A Ç Ã O 

# Estimação modelo ARIMA(p,d,q)(P,D,Q)
x.ajuste <- arima(x.ts, order = c(p,d,q),
                 seas = list(order=c(P,D,Q),
                 frequency(x.ts)), method = "CSS")
# Estimação modelo HoltWinters
x.ajuste <- HoltWinters(x.ts, beta=0.9, gamma=FALSE) #sem sazonallidade gamma = FALSE
x.ajuste <- HoltWinters(x.ts) #com tendência e sazonalidade


# P R E D I Ç Ã O
x.predict.ajuste <- predict(x.ajuste, n.ahead = 12)$pred


# G R Á F I C O   F I N A L 
ts.plot(cbind( x.ts,
              predict(x.ajuste, n.ahead=24, prediction.interval = TRUE,
                      level = 0.95)$pred), lty = 1:2)


# Armazena predição
p <- predict(x.ajuste, n.ahead=24, prediction.interval = TRUE,
        level = 0.95)$pred

# Unção dos dados antigos com predição
base = ts.union(x.ts,p)

# Final Table
final.table = cat(toJSON(base, pretty = FALSE))
