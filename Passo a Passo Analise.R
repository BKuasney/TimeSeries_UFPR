# Fazemos a analise descritiva dos dados ################################################################

data(AirPassengers)
AP = AirPassengers
plot(AP)

points(AP, pch=19, cex=0.5)
# Caso a base de dados seja externa (nesse exemplo com mais de uma variável)
  # Temperatura = ts(dados$temperatura, start=1997, deltat=1/365)
  # Umidade = ts(dados$umidade, start=1997, deltat=1/365)
# Caso a base tenha mais de uma variável temporal: ts.plot(Temperatura, Umidade)

# Para analisar apenas um período:
  # A.1956 = window(mKWH, start=c(1956,1), end=c(1956,12),freq=12)

acf(AirPassengers) # Com tendência e sazonalidade
acf(diff(AP)) # Aparentemente correlacionados


# Descobrimos se tem tendência e sazonalidade ##########################################################
decomp = stl(AP, s.window = "periodic")
plot(decomp)
Res = decomp$time.series[,3] # Armazenamos o "remainder"



# Médias Móveis para retirada de tendência e sazonalidade ###############################################
  # Retiramos a tendência e sazonalidade, utilizamos o gráfico de "random"
AP.decom = decompose(AP, "multiplicative") # tipo da componente sazonal
plot(AP.decom)
acf(AP.decom$random[7:138])



# Testamos se possui alguma autocorrelação significativa ################################################
Box.test(AirPassengers, lag=7, type="Ljung-Box", fitdf=0) #Significativos apenas acima de 7
Box.test(ts(Res), lag=7, type="Ljung-Box", fitdf=2)
Box.test(ts(AP.decom$random[7:138]), lag=7, type="Ljung-Box", fitdf=2)
# Hipótese nula é rejeitada para ambos
  # signficia que algumas correlações são significativamente diferentes de zero
  # Isso é apresentado no gráfico abaixo


par(mfrow=c(1,2),cex.main=0.9,cex.lab=0.7,cex.axis=0.7,mar=c(5,4,1,1),pch=16,font.main=3)
# Autocorrelação Dados Originais
plot(acf(ts(AirPassengers), plot=F), xlab = 'Defasagem', main = "(a) Dados Originais", ylab='Autocorrelação')
# Autocorrelação do Resto e não do resíduo - Residuos
plot(acf(ts(Res), plot=F), xlab='Defasagem', main="(b) Série do Res", ylab='Autocorrelação')
# Autocorrelação Dados sem tendência e sem sazonalidade
plot(acf(ts(AP.decom$random[7:138]), plot=F), xlab='Defasagem', main="Outro gráfico", ylab='Autocorrelação')
# No primeiro correlograma percebemos a influência da tendência e da sazonalidade.
# No segundo correlograma percebemos autocorrelações significativas para os resíduos
# No terceiro correlograma percebemos novamente autocorrelações significativas apenas com os dados aleatórios

# Isso confirma que as correlações são significativamente difernetes de zero
  # Ou seja, que realmente possuímos relações entre os dados

# Observação ###
# Caso os restos não possuam correlações significativas, isso indica apenas que não rpecisamos analisar esses dados
# A correlação significativa é mais importante nos dados originais






