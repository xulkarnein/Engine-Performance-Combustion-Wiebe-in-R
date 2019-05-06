#yakýt ekonomisi dersi ödevi
#####
#hafýza ve çizim sýfýrlama
rm(list = ls())
if(!is.null(dev.list())) dev.off()
#####
#burada gerekli olan kütüphane tanýmlarý eklenir
install_prompt<-readline(prompt = "Gerekli kütüphaneler(xlsx,deldir,plot3D,MASS) yüklensin mi 1-Evet/2-Zaten yüklü: ")
if (install_prompt==1) {
install.packages("xlsx")
install.packages("deldir")
install.packages("plot3D")
install.packages("MASS")
}
library(xlsx)
library(deldir)
library(plot3D)
library(MASS)
#####
#çalýþma klasörü ve okunacak dosya ayarlanýr
#bazý seçenekler
#input_file<-readline(prompt = "Çalýþma dosya adýný giriniz (*.xlsx): ")
#setwd("C:/Users/eozcan/Documents")
#setwd(workdir)
#workdir<-readline(prompt = "Çalýþma klasörünü giriniz(C:/x/y/z): ")
setwd("C:/Users/eozcan/Desktop/masaalti/DOKTORA/MAK 5117 yakýt eko/algoritma projesi/algoritma")
input_file<-"Fuel_economy_eo_standart.xlsx"
input_track<-readline(prompt = "koþulacak yolu seçiniz (1-test, 2-tam gaz, 3-azami hýz, 4-wedel, 5-pivot, 6-munson test area):" )
input_track_string<-c("Test","Tam gaz","Azami hýz","Wedel","Pivot","Munson test area")
attach(read.xlsx(input_file,sheetIndex = 1,endRow = 2))
attach(read.xlsx(input_file,sheetIndex = as.integer(input_track)+3))
attach(read.xlsx(input_file,sheetIndex = 3))
attach(read.xlsx(input_file,sheetIndex = 2,endRow = 2))
#####
#hesaplama baþlangýcý
road<-readline(prompt = "yuvarlanma direnci için yol tipini seçiniz (1-offroad(sadece pivot)/2-concrete):" )
road_string<-c("Offroad","Concrete")
v<-v/3.6
if (road == 1) {mw0<-mw0hor;Rv<-Rvhor;fr<-fRhor} else {mw0=mw0con;Rv=Rvcon;fr=fRcon}
FL<-rhoL*AL*Cd*v^2/2
FSB<-mfz*(g*sin(atan(p))+b*1.05)
Fga<-mfz*g/2+mfz*v^2*hs/abs(r)/SW
Fgi<-mfz*g/2-mfz*v^2*hs/abs(r)/SW
iLr<-2*abs(r)/SW/Rv
mwr<-(mw0*(1-iLr/iLK)/(1+iLr)^n)
mwr<- mwr*(sign(mwr)+(sign(mwr)^2-sign(mwr))/2)
Ka<-Fga*(fr*sign(1+1/iLr)+mwr*Lk/2/SW)+(FL+FSB)/2
Ki<-Fgi*(fr*sign(1-1/iLr)-mwr*Lk/2/SW)+(FL+FSB)/2
Ta<-Ka*Dk/2
Ti<-Ki*Dk/2
va<-v*(1+1/iLr)
vi<-v*(1-1/iLr)
na<-va*60/Dk/pi
ni<-vi*60/Dk/pi
nz<-(na+ni)/2*iE*i0s
nn<-(na-ni)/2*(1-i0)*iE*iS
Tz<-(Ta+Ti)/iE/i0s
Tn<-(Ta-Ti)/(1-i0)/iE/iS
Pa<-Ka*va
Pi<-Ki*vi
Pz<-Tz*nz/9550
Pn<-Tn*nn/9550
Pde<-Pz/n0/nN/ndt
Pse<-Pn/nhtot
Pe<-Pde+Pse
i<-c(i1,i2,i3,i4)
up_shift<-c(1:length(i))
down_shift<-c(length(i):1)
N=0
I=0
Torq=0
for (j in t) {
  for (jj in up_shift) {
    N[j]<-v[j]*i[jj]*iE*i0s*60/pi/Dk 
    I[j]<-jj
    if (N[j]<1000) {N[j]<-1000;I[j]<-1 }
    if (Pse[j]>250) {N[j]<-2600;I[j]<-1}
    Torq[j]<-Pe[j]*9550/N[j]
    if (N[j]<=2700) {break}
  }
}
del <- deldir(x, y, z = z)
ttt <- triang.list(del)
tttt <- tile.list(del)
bsfc_last<-0
fc_cum<-c(0,0)
x_cum<-0
fc<-0
for (j in t) {
  tille<-which.tile(N[j],Torq[j],tttt)
  k<-ttt[tille]
  asdb<-k[[1]]
  bsfc_last[j]<-mean(asdb$z)
  if (Pe[j]<0) {Pe[j]<-0}
  fc[j]<-Pe[j]*bsfc_last[j]/1000/3600
  fc_cum[j+1]<-fc_cum[j]+fc[j]
  x_cum[j+1]<-x_cum[j]+v[j]
}
t_step<-length(t)
total_consumption<-sum(Pe*bsfc_last/1000/3600)
kg_per_km<-total_consumption/(mean(v)*max(t)/1000)
#hesaplama sonu
#####
#çizim ve sonuç yazdýrma
detach("package:xlsx",TRUE)
library(openxlsx)
wb <- createWorkbook()
big_df<-data.frame("time_s"=t,"distance_m"=x_cum[2:(t_step+1)],
                  "Velocity_km_h"=v*3.6,"Inclanation"=p,"acceleration_m_s2"=b,
                  "rev_radius"=1/r,"Engine_Torque_Nm"=Torq,"Engine_Power_kW"=Pe,
                  "Engine_Speed_rpm"=N,"Gear"=I,"BSFC_g_kWh"=bsfc_last,
                  "Cum_Cons_kg"=fc_cum[2:(t_step+1)],"F_Cons_kg_h"=bsfc_last*Pe/1000,
                  "Driving_Power_kW"=Pde,"Steering_Power_kW"=Pse,"Centralshaft_rpm"=nz,"zeroshaft_rpm"=nn,
                  "Centralshaf_torque_Nm"=Tz,"zeroshaft_torque_Nm"=Tn)

addWorksheet(wb, "Summary", gridLines = FALSE)
df1<-data.frame("time_s"=t,"______Eng_Torque_Nm______"=Torq,"Eng_Power_kW"=Pe,"Eng_Speed_rpm"=N,
                "Gear"=I)
plot(df1,pch=20, cex=.5,cex.main =.8, cex.axis =.5, main = "Performance Summary")
insertPlot(wb, 1, width = 8, height = 5, fileType = "png", units = "in")
addWorksheet(wb, "Plots", gridLines = FALSE)
df2<-data.frame("time_s"=t,"distance_m"=x_cum[2:(t_step+1)],"_____Velocity_km_h_____"=v*3.6,
                "Inclanation"=p,"acceleration_m_s2"=b,"rev_radius"=1/r)
plot(df2,pch=20, cex=.5,cex.main =.8,cex.axis =.5,main = "Road Properties")
insertPlot(wb, 2, xy = c("J", 1), width = 16, height = 10,  fileType = "png", 
           units = "cm")
df3<-data.frame("time_s"=t,"Engine_Power_kW"=Pe,"BSFC_g_kWh"=bsfc_last,
                "____Cum_Cons_kg____"=fc_cum[2:(t_step+1)],"F_Cons_kg_h"=bsfc_last*Pe/1000)
plot(df3,pch=20, ps=.1,cex=.5,cex.main =.8,cex.axis =.5,main = "Fuel Economy")
insertPlot(wb, 2, xy = c("A", 21), width = 16, height = 10, fileType = "png", 
           units = "cm")
df4<-data.frame("time_s"=t,"Driving_Power_kW"=Pde,"Steering_Power_kW"=Pse,
                "____Centralshaft_rpm____"=nz,"Zeroshaft_rpm"=nn)
plot(df4,pch=20, cex=.5,cex.axis =.8,cex.main =.5,main = "Steering&Power Distribution")
insertPlot(wb, 2, xy = c("J", 21), width = 16, height = 10,  fileType = "png", 
           units = "cm")
df5<-data.frame("time_s"=t,"Engine_Torque_Nm"=Torq,"____Centralshaf_torque_Nm____"=Tz,
                "Zeroshaft_torque_Nm"=Tn)
plot(df5,pch=20, cex=.5,cex.axis =.8,cex.main =.5,main = "Torque Distribution")
insertPlot(wb, 2, xy = c("A", 40), width = 16, height = 10,  
           fileType = "png", units = "cm")
par(mfrow=c(2,2))
par(cex=0.5)
hist(Torq)
hist(N)
hist(Pe)
hist(I)
insertPlot(wb, 2, xy = c("J", 40), width = 16, height = 10,  fileType = "png", units = "cm")
par(mfrow=c(1,1))
scatter2D(x,y, colvar = z,xlab="speed[rpm]", ylab="torque[Nm]",
          main=paste(input_track_string[as.integer(input_track)],
          "on",road_string[as.integer(road)],"Route"), 
          col = rainbow(100), pch=19, cex=1.5,cex.main=.8)
grid(10,10)
lines(N,Torq, col = "black",lwd = 2)
lines(x[1:22],y[1:22],col = "blue",lwd = 2)
lines(x[243:264],y[243:264],col = "blue",lwd = 2)
insertPlot(wb, 2, width = 16, height = 10,  fileType = "png", units = "cm")
addWorksheet(wb, "Datas", gridLines = FALSE)
writeDataTable(wb, sheet = 3,big_df)
output_filename<-paste(input_track_string[as.integer(input_track)],"on",
                       road_string[as.integer(road)],"results.xlsx")
saveWorkbook(wb, output_filename, overwrite = TRUE)
