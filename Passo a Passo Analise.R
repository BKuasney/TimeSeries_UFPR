# Fazemos a analise descritiva dos dados ################################################################

data(AirPassengers)
AP = AirPassengers
plot(AP)

points(AP, pch=19, cex=0.5)
# Caso a base de dados seja externa (nesse exemplo com mais de uma vari�vel)
  # Temperatura = ts(dados$temperatura, start=1997, deltat=1/365)
  # Umidade = ts(dados$umidade, start=1997, deltat=1/365)
# Caso a base tenha mais de uma vari�vel temporal: ts.plot(Temperatura, Umidade)

# Para analisar apenas um per�odo:
  # A.1956 = window(mKWH, start=c(1956,1), end=c(1956,12),freq=12)

acf(AirPassengers) # Com tend�ncia e sazonalidade
acf(diff(AP)) # Aparentemente correlacionados


# Descobrimos se tem tend�ncia e sazonalidade ##########################################################
decomp = stl(AP, s.window = "periodic")
plot(decomp)
Res = decomp$time.series[,3] # Armazenamos o "remainder"



# M�dias M�veis para retirada de tend�ncia e sazonalidade ###############################################
  # Retiramos a tend�ncia e sazonalidade, utilizamos o gr�fico de "random"
AP.decom = decompose(AP, "multiplicative") # tipo da componente sazonal
plot(AP.decom)
acf(AP.decom$random[7:138])



# Testamos se possui alguma autocorrela��o significativa ################################################
Box.test(AirPassengers, lag=7, type="Ljung-Box", fitdf=0) #Significativos apenas acima de 7
Box.test(ts(Res), lag=7, type="Ljung-Box", fitdf=2)
Box.test(ts(AP.decom$random[7:138]), lag=7, type="Ljung-Box", fitdf=2)
# Hip�tese nula � rejeitada para ambos
  # signficia que algumas correla��es s�o significativamente diferentes de zero
  # Isso � apresentado no gr�fico abaixo


par(mfrow=c(1,2),cex.main=0.9,cex.lab=0.7,cex.axis=0.7,mar=c(5,4,1,1),pch=16,font.main=3)
# Autocorrela��o Dados Originais
plot(acf(ts(AirPassengers), plot=F), xlab = 'Defasagem', main = "(a) Dados Originais", ylab='Autocorrela��o')
# Autocorrela��o do Resto e n�o do res�duo - Residuos
plot(acf(ts(Res), plot=F), xlab='Defasagem', main="(b) S�rie do Res", ylab='Autocorrela��o')
# Autocorrela��o Dados sem tend�ncia e sem sazonalidade
plot(acf(ts(AP.decom$random[7:138]), plot=F), xlab='Defasagem', main="Outro gr�fico", ylab='Autocorrela��o')
# No primeiro correlograma percebemos a influ�ncia da tend�ncia e da sazonalidade.
# No segundo correlograma percebemos autocorrela��es significativas para os res�duos
# No terceiro correlograma percebemos novamente autocorrela��es significativas apenas com os dados aleat�rios

# Isso confirma que as correla��es s�o significativamente difernetes de zero
  # Ou seja, que realmente possu�mos rela��es entre os dados

# Observa��o ###
# Caso os restos n�o possuam correla��es significativas, isso indica apenas que n�o rpecisamos analisar esses dados
# A correla��o significativa � mais importante nos dados originais






