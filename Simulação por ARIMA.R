par(mfrow=c(2,1), mar=c(5,4,3,2))
plot(arima.sim(list(order=c(1,0,0), ar=0.9), n=100), ylab='Y',
     main = expression(AR(1)~~~alpha==+0.9))
plot(arima.sim(list(order=c(1,0,0), ar=-0.9), n=100), ylab='Y',
     main = expression(AR(1)~~~alpha==-0.9))
