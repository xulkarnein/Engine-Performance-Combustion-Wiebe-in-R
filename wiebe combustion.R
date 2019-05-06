rm(list = ls())
if(!is.null(dev.list())) dev.off()
library(R.matlab)
library(zoom)
#veriler/datas####
c<-readMat("ornek_olcum.mat", maxLength=NULL, fixNames=TRUE, drop=c("singletonLists"),
           sparseMatrixClass=c("Matrix", "SparseM", "matrix"), verbose=FALSE)
alp<-array(as.numeric(unlist(c["ca"])))-360
wiebea<-6.908
md<-.27
mp<-1.5
Qp<-590
Qd<-521
tetd<-150
tetp<-24
adv<--5.5
Hu<-44.5
k<-1.34
P<-2.2
vc<-0.0265
B<-0.0825
S<-.082
R<-S/2
Rgas<-8.3145
lambda<-R/.132
Tinit<-293
Tcool<-50
Temp<-0
Temp[1]<-Tinit*(1+1/.7*((P[1]+1)^((k-1)/k)-1))-Tcool
Mfuel<-(Qp+Qd)/Hu
molmasair<-28.97
Nair<-P*(B^2/4*S*3.14)/R/Temp[1]*1000000
Mair<-Nair*molmasair
Lsto<-14.5
afr<-Mair/Mfuel/Lsto

#iþlemler/calculations####
a<-seq(-360,360, length.out = 1441)
Sx<-R*((1-cos(a*pi/180))+lambda/4*(1-cos(2*a*pi/180)))
v<-Sx*pi*B^2/4*1000+vc
shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}
vv<-shifter(v)
dv<-vv-v
steps<- c(1:length(a))
dp<-0
dqp<-integer(length(a))
dqd<-integer(length(a))
dfp<-integer(length(a))
dfd<-integer(length(a))
comp<-seq(.5,tetp,length.out = tetp*2)
comd<-seq(.5,tetd,length.out = tetd*2)
seqp<-seq(which(a==adv,arr.ind=TRUE),which(a==adv,arr.ind=TRUE)+tetp*2,length.out = tetp*2)
seqd<-seq(which(a==adv,arr.ind=TRUE),which(a==adv,arr.ind=TRUE)+tetd*2,length.out = tetd*2)
for (j in seq(1,tetp*2)) {
  dqp[seqp[j]]<-Qp*wiebea*(mp+1)*(comp[j]/tetp)^mp*exp(-wiebea*(comp[j]/tetp)^(mp+1))/tetp}
for (j in seq(1,tetd*2)) {
  dqd[seqd[j]]<-Qd*wiebea*(md+1)*(comd[j]/tetd)^md*exp(-wiebea*(comd[j]/tetd)^(md+1))/tetd}
for (j in seq(1,tetp*2)) {
  dfp[seqp[j]]<-(1-exp(-wiebea*(comp[j]/tetp)^(mp+1)))*(Qp/(Qp+Qd))}
for (j in seq(1,tetd*2)) {
  dfd[seqd[j]]<-(1-exp(-wiebea*(comd[j]/tetd)^(md+1)))*(Qd/(Qp+Qd))}
dfp[(720+tetp*2+adv*2):length(a)]<-Qp/(Qp+Qd)
dfd[(720+tetd*2+adv*2):length(a)]<-Qd/(Qp+Qd)
dq<-dqp+dqd
df<-dfd+dfp
Pm<-P[1]
dpm<-0
for (j in head(steps,-1)){
  if (a[j] < -180 | a[j] > 180  ){dp[j]<-0
  dpm[j]<-0
  P[j+1]<- P[j]+dp[j]
  Pm[j+1]<- Pm[j]+dpm[j]
  }
  else{
  dp[j]<-(k-1)*dq[j]/100/v[j]-k*P[j]*dv[j]/v[j]
  P[j+1]<- P[j]+dp[j]
  dpm[j]<--k*Pm[j]*dv[j]/v[j]
  Pm[j+1]<- Pm[j]+dpm[j]}
}
Temp<-P*v/R/Nair*1000
#çizimler/plots####
par(mar = c(5, 5, 3, 12))
plot(a,P,type="l",ylab = "Pressure[bar]",xlab = expression(alpha^o*KMA),lty=1,col="red")
lines(a,Pm)
lines(alp,c[["P.experimental"]],type = "l", lty =4,ylab="",xlab="")
axis(side=2)
par(new=T)
plot(a, dq,xaxt = "n",type="l", yaxt = "n",ylab="",xlab="",ylim = c(0,max(dq)*2),col="blue")
mtext(expression(paste(frac(dQ,d*alpha),"[",J^o*KMA^-1,"]")), side = 4, line = 3)
axis(side = 4)
par(new=T)
plot(a,df,type = "l",lty=2,col="black", axes=F, ylim=c(0,2),ylab="",xlab="")
mtext("Fuel Fraction", side = 4, line = -1)
axis(side = 4, pos=320)
grid()
legend("topleft", c("Pressure",expression(dQ/d*alpha,"Motoring Pressure","Fuel Fraction","P.exp")),
       col = c("red","blue","black","black","black"), lty = c(1,1,1,2,4))
