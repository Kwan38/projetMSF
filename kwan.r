plot (RentH$Dates, RentH$Rt, type ='l', col='red')


.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)
acf(RentH$Rt)
pacf(RentH$Rt)
acf(RentH$Rt,lag.max = 100)
pacf(RentH$Rt,lag.max = 100)

m1 = meanSet(Const=0, Ar=c(2,1), Ma=c(.3))
m1

v1 = varSet(Garch=list(ConstVar=.1, Arch=c(.1),Garch=c(.8)))
v1

r1 = residualsSet('NORMAL')
r1

mod1 = modelSet(m1,v1,r1)
mod1

