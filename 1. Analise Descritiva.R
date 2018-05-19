######################################
# A P O S T I L A   L U C Â M B I O ##
######################################


# Número mensal de nascidos vivos nos Estados Unidos entre janeiro de 1948 e dezmebro de 1979
# Dados em milhares
install.packages("astsa")
require("astsa")

plot(birth, xlab='Ano', ylab='No. de nascidos (milhares)',
      main='Número mensal de nascidos vivos nos \nEstados Unidos entre 1948 e 1979')
points(birth, pch=19, cex=0.5)
box(which="outer", lty="solid", lwd=2)

# Visualizando a informação:
  # A oscilação dos dados indica presença de sazonalidade
  # O crescimento, descrescimento e novamente crescimento indica tendência


###############################################################
###############################################################
###############################################################


# Caso a base de dados seja externa, é necessário transformar para uma série temportal ts()
# Ex:
dados = read.csv('http://people.ufpr.br/~lucambio/CE017/2S2015/atmosfera.csv', h=T,sep=";")
Temperatura = ts(dados$temperatura, start=1997, deltat=1/365)
Umidade = ts(dados$umidade, start=1997, deltat=1/365)
ts.plot(Temperatura, Umidade)

# Visualizando separadamente para melhor entender:
# Temperatura
plot(Temperatura)
# Percebemos que a temperatura no começo do ano é alta
# diminuindo até os meses no meio do ano, correspondentes ao inversno
# e depois aumenta novamente.
# Isso significa claramente um comportamento sazonal, segundos as estações do ano

# Umidade
plot(Umidade)
# Sobre a umidade, podemos apenas afirmar que existe alta variabilidade


################################################################
################################################################
################################################################


Prod = read.table("http://people.ufpr.br/~lucambio/CE017/2S2015/elec.txt", h=T)
names(Prod)
mKWH = ts(Prod$mKWH, start=c(1956,1), frequency=12)
plot(mKWH, xlab='Anos', main='Produção mensal de eletricidade na Austrália\n
     Janeiro 1956 até agosto de 1995', ylab='Milhões de KWH', type='l')
points(mKWH, pch=19, cex=0.5)
box(which="outer", lty="solid", lwd=2)
# Visualizando os dados:
  # Percebemos um suave, mas constante crescimento das observações
  # Isso caracteriza a presença de tendência
  # Podemos afirmar ainda, que entre os anos 1956 e 1990
  # houve crescimento quase exponencial.
  # Estabilizando um pouco nos anos 90

  # Os dados são mensais e correspondentes à produção de eletricidade
  # a qual supomos que seja sazonal, isto é,
  # em determinada época do ano se produz mais para atender a demanda
  # e noutras menos.

# Para descobrirmos devemos observar o comportamento em alguns anos

A.1956 = window(mKWH, start=c(1956,1), end=c(1956,12),freq=12)
A.1957 = window(mKWH, start=c(1957,1), end=c(1957,12),freq=12)
A.1958 = window(mKWH, start=c(1958,1), end=c(1958,12),freq=12)
A.1959 = window(mKWH, start=c(1959,1), end=c(1959,12),freq=12)

par(mfrow=c(2,2), cex.main=0.9, cex.lab=0.7, cex.axis=0.7, mar=c(5,4,4,2),pch=16, font.main=3)

plot(A.1956, xlab='1956', main='Produção mensal de eletricidade na Austrália',
     ylab='Milhões de KWH', type='l')
points(A.1956, pch=19,cex=0.5)

plot(A.1957, xlab='1957', main='Produção mensal de eletricidade na Austrália',
     ylab='Milhões de KWH', type='l')
points(A.1956, pch=19,cex=0.5)

plot(A.1958, xlab='1958', main='Produção mensal de eletricidade na Austrália',
     ylab='Milhões de KWH', type='l')
points(A.1956, pch=19,cex=0.5)

plot(A.1959, xlab='1959', main='Produção mensal de eletricidade na Austrália',
     ylab='Milhões de KWH', type='l')
points(A.1956, pch=19,cex=0.5)

# Observamos portanto que sempre ocorre um pico de produção nos meses de inverno




# VOltando ao primeiro exemplo temos:

install.packages("astsa")
require("astsa")

plot(birth, xlab='Ano', ylab='No. de nascidos (milhares)',
     main='Número mensal de nascidos vivos nos \nEstados Unidos entre 1948 e 1979')
points(birth, pch=19, cex=0.5)
box(which="outer", lty="solid", lwd=2)

# Aplicando Médias Móveis: ##########################################################
  # Obervamos que os dados correspondem a informações mensais
  # portantpo p=12, o que significa que definiremos o valor de 13 (p+1)
  # utilizando o R podemos calcular a curva de médias móveis
    # tanto para o modelo aditivo quanto multiplicativo
decomp = decompose(birth, type="additive")
par(mfrow=c(1,1), mar=c(5,4,4,2))
plot(birth, xlab='Ano', ylab='No. de nascimentos (milahres)',
     main='Número mensal de nascidos vivos nos \nEstados Unidos entre 1948 e 1979')
lines(decomp$tren)

# Devemos esperar que a diferença Yt - Tt represente
# uma série temporal sem tendência. Abaixo:
par(mfrow=c(1,1), mar=c(5,4,4,2))
plot(birth-decomp$trend, xlab='Ano', ylab='No. de nascidos (milhares) - tendência estimada',
     main='Número mensal de nascidos vivos nos n Estados Unidos entre 1948 e 1979')
# Temos agora uma série temporal sem tendência


################################
# Aplicando a Regressão Local:
data(unemp)
plot(unemp)
# Comando stl nos dá a componente sazonal, a tendência e o componente irregular
decomp1 = stl(unemp, s.window = "periodic", t.window=1) # quanto maior o valor de t.window mais suavizado
decomp2 = stl(unemp, s.window = "periodic", t.window=5) # se t.window não setado, curva será suavizada
plot(unemp, xlab='Ano', ylab="no. de desempregados (milhares)",
     main='Número mensal de desempregados nos \nEstados Unidos entre 1948 e 1978')
# Utilizaremos a segunda coluna que é referente à sua tendência
lines(decomp1$time.series[,2], col="red")
lines(decomp2$time.series[,2], col="blue")


decomp = stl(unemp, s.window = "periodic")
plot(decomp, main='Número mensal de desempregados nos \nEstados Unidos entre 1948 e 1978')


# Observamos a variação da tendência que vai de 200 a 800
  # Possui tendência
# Observamos também a variação da sazonalidade, que vai de -40 a 40
  # Possui sazonalidade
  # Podemos afirmar ainda que sua sazonalidade é aditiva
    # Pois possui o mesmo padrão sempre, independente do tempo



################################
# Aplicando a diferenciação:

# primeira diferença diff(birth,1)
plot(diff(birth,1), xlab='Ano', ylab='Primeira diferenciação',
      main='Número mensal de nascidos vivos nos \nEstados Unidos entre 1948 e 1979')
# Resultado obtido é bem parecido com o sistema de médias móveis

# Caso ainda seja necesário, obtemos a segunda diferença diff(birth,2)
plot(diff(birth,2), xlab='Ano', ylab='Primeira diferenciação',
     main='Número mensal de nascidos vivos nos \nEstados Unidos entre 1948 e 1979')
# A qual é mais parecida ainda com a estimação por médias móveis


################################
# S A Z O N A L I D A D E

# para verificar se há sazonalidade e identificando o tipo de sazonalidade:
decomp = stl(unemp, s.window = "periodic")
plot(decomp, main='Número mensal de desempregados nos \nEstados Unidos entre 1948 e 1978')
  # Observamos que há sazonlidade, devido a variação da componente de sazonalidade
  # variando de -40 a 40



