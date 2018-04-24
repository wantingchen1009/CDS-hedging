#library(xlsx)
#library(readxl)
library(Kendall)
library(openxlsx)

###
UKdata<-read.xlsx("cds1.xlsx",sheet="UK",detectDates = TRUE)
ukdata<-UKdata[2:2597,2:5]
uk<-matrix(as.numeric(as.matrix(ukdata)),nrow=2596,ncol=4)
defaultprob<-uk/0.6
defaultprob<-defaultprob/10000
survivalprob<-1-defaultprob

Kendall(survivalprob[1:2596,1],survivalprob[1:2596,2])
#12 0.739
Kendall(survivalprob[1:2596,1],survivalprob[1:2596,3])
#13 0.761
Kendall(survivalprob[1:2596,1],survivalprob[1:2596,4])
#14 0.79
Kendall(survivalprob[1:2596,2],survivalprob[1:2596,3])
#23 0.62
Kendall(survivalprob[1:2596,2],survivalprob[1:2596,4])
#24 0.878
Kendall(survivalprob[1:2596,3],survivalprob[1:2596,4])
#34 0.668

###
Francedata<-read.xlsx("cds1.xlsx",sheet="France",detectDates = TRUE)
Frdata<-Francedata[2:2597,2:4]
Fr<-matrix(as.numeric(as.matrix(Frdata)),nrow=2596,ncol=3)
Frdefaultprob<-Fr/0.6
Frdefaultprob<-Frdefaultprob/10000
Frsurvivalprob<-1-Frdefaultprob

Kendall(Frsurvivalprob[1:2596,1],Frsurvivalprob[1:2596,2])
#12 0.807
Kendall(Frsurvivalprob[1:2596,1],Frsurvivalprob[1:2596,3])
#13 0.855
Kendall(Frsurvivalprob[1:2596,2],Frsurvivalprob[1:2596,3])
#23 0.79

###
Germanydata<-read.xlsx("cds1.xlsx",sheet="Germany",detectDates = TRUE)
Gerdata<-Germanydata[2:2597,2:6]
Ger<-matrix(as.numeric(as.matrix(Gerdata)),nrow=2596,ncol=5)
Gerdefaultprob<-Ger/0.6
Gerdefaultprob<-Gerdefaultprob/10000
Gersurvivalprob<-1-Gerdefaultprob

Kendall(Gersurvivalprob[1:2596,1],Gersurvivalprob[1:2596,2])
#12 1
Kendall(Gersurvivalprob[1:2596,1],Gersurvivalprob[1:2596,3])
#13 1
Kendall(Gersurvivalprob[1:2596,1],Gersurvivalprob[1:2596,4])
#14 1
Kendall(Gersurvivalprob[1:2596,1],Gersurvivalprob[1:2596,5])
#15 0.617
Kendall(Gersurvivalprob[1:2596,2],Gersurvivalprob[1:2596,3])
#23 1
Kendall(Gersurvivalprob[1:2596,2],Gersurvivalprob[1:2596,4])
#24 1
Kendall(Gersurvivalprob[1:2596,2],Gersurvivalprob[1:2596,5])
#25 0.617
Kendall(Gersurvivalprob[1:2596,3],Gersurvivalprob[1:2596,4])
#34 1
Kendall(Gersurvivalprob[1:2596,3],Gersurvivalprob[1:2596,5])
#35 0.617
Kendall(Gersurvivalprob[1:2596,4],Gersurvivalprob[1:2596,5])
#45 0.617

###
Spaindata<-read.xlsx("cds1.xlsx",sheet="Spain",detectDates = TRUE)
Spdata<-Spaindata[2:2590,2:5]
Sp<-matrix(as.numeric(as.matrix(Spdata)),nrow=2589,ncol=4)
Spdefaultprob<-Sp/0.6
Spdefaultprob<-Spdefaultprob/10000
Spsurvivalprob<-1-Spdefaultprob

Kendall(Spsurvivalprob[1:2589,1],Spsurvivalprob[1:2589,2])
#12 0.775
Kendall(Spsurvivalprob[1:2589,1],Spsurvivalprob[1:2589,3])
#13 0.764
Kendall(Spsurvivalprob[1:2589,1],Spsurvivalprob[1:2589,4])
#14 0.764
Kendall(Spsurvivalprob[1:2589,2],Spsurvivalprob[1:2589,3])
#23 0.944
Kendall(Spsurvivalprob[1:2589,2],Spsurvivalprob[1:2589,4])
#24 0.944
Kendall(Spsurvivalprob[1:2589,3],Spsurvivalprob[1:2589,4])
#34 1

###
Italydata<-read.xlsx("cds1.xlsx",sheet="Italy",detectDates = TRUE)
italydata<-Italydata[2:2515,2:5]
italy<-matrix(as.numeric(as.matrix(italydata)),nrow=2514,ncol=4)
Itdefaultprob<-italy/0.6
Itdefaultprob<-Itdefaultprob/10000
Itsurvivalprob<-1-Itdefaultprob

Kendall(Itsurvivalprob[1:2514,1],Itsurvivalprob[1:2514,2])
#12 0.906
Kendall(Itsurvivalprob[1:2514,1],Itsurvivalprob[1:2514,3])
#13 0.672
Kendall(Itsurvivalprob[1:2514,1],Itsurvivalprob[1:2514,4])
#14 0.672
Kendall(Itsurvivalprob[1:2514,2],Itsurvivalprob[1:2514,3])
#23 0.648
Kendall(Itsurvivalprob[1:2514,2],Itsurvivalprob[1:2514,4])
#24 0.648
Kendall(Itsurvivalprob[1:2514,3],Itsurvivalprob[1:2514,4])
#34 1


###########################calculate systemic shock###########################################################

alphatheta<-read.xlsx("sa.xlsx",1,colNames = FALSE)

###
alphahat1<-4/(1/alphatheta[1,1]+1/alphatheta[1,2]+1/alphatheta[1,3]+1/alphatheta[1,4])  
alphapower1<-(alphahat1)^(1/alphatheta[1,5])
#lambda0 de 1/theta, mu0?, arrival time of systemic shock
shock1<-((defaultprob[,1]+defaultprob[,2]+defaultprob[,3]+defaultprob[,4])/4)^(1/alphatheta[1,5])*alphapower1
#tau of arrival time of systemic shock and marginal default time
uktau<-c(0,0,0,0)
uktau[1]<-0.8505023#Kendall(shock1,defaultprob[,1])
uktau[2]<-0.8736753#Kendall(shock1,defaultprob[,2])
uktau[3]<-0.7268717#Kendall(shock1,defaultprob[,3])
uktau[4]<-0.9146276#Kendall(shock1,defaultprob[,4])
ukalpha<-alphatheta[1,1:4]
plot(as.numeric(ukalpha),uktau,xlim=c(0,1),ylim=c(0,1),pch=21,col="red",xlab="alpha",ylab="kendall tau",main='UK')
abline((alphatheta[1,5]-1)/alphatheta[1,5],1/alphatheta[1,5])

###
alphahat2<-3/(1/alphatheta[2,1]+1/alphatheta[2,2]+1/alphatheta[2,3])
alphapower2<-(alphahat2)^(1/alphatheta[2,4])
#lambda0 de 1/theta, mu0?, arrival time of systemic shock
shock2<-((Frdefaultprob[,1]+Frdefaultprob[,2]+Frdefaultprob[,3])/3)^(1/alphatheta[2,4])*alphapower2
#tau of arrival time of systemic shock and marginal default time
frtau<-c(0,0,0)
frtau[1]<-0.915#Kendall(shock2,Frdefaultprob[,1])
frtau[2]<-0.865#Kendall(shock2,Frdefaultprob[,2])
frtau[3]<-0.902#Kendall(shock2,Frdefaultprob[,3])
fralpha<-alphatheta[2,1:3]
plot(as.numeric(fralpha),frtau,xlim=c(0,1),ylim=c(0,1),pch=21,col="red",xlab="alpha",ylab="kendall tau",main='France')
abline((alphatheta[2,4]-1)/alphatheta[2,4],1/alphatheta[2,4])

###
alphahat3<-5/(1/alphatheta[3,1]+1/alphatheta[3,2]+1/alphatheta[3,3]+1/alphatheta[3,4]+1/alphatheta[3,5])
alphapower3<-(alphahat3)^(1/alphatheta[3,6])
#lambda0 de 1/theta, mu0?, arrival time of systemic shock
shock3<-((Gerdefaultprob[,1]+Gerdefaultprob[,2]+Gerdefaultprob[,3]+Gerdefaultprob[,4]+Gerdefaultprob[,5])/5)^(1/alphatheta[3,6])*alphapower3
#tau of arrival time of systemic shock and marginal default time
gertau<-c(0,0,0,0,0)
gertau[1]<-0.912#Kendall(shock3,Gerdefaultprob[,1])
gertau[2]<-0.912#Kendall(shock3,Gerdefaultprob[,2])
gertau[3]<-0.912#Kendall(shock3,Gerdefaultprob[,3])
gertau[4]<-0.912#Kendall(shock3,Gerdefaultprob[,4])
gertau[5]<-0.705#Kendall(shock3,Gerdefaultprob[,5])
geralpha<-alphatheta[3,1:5]
plot(as.numeric(geralpha),gertau,xlim=c(0,1),ylim=c(0,1),pch=21,col="red",xlab="alpha",ylab="kendall tau",main='Germany')
abline((alphatheta[3,6]-1)/alphatheta[3,6],1/alphatheta[3,6])

###
alphahat4<-4/(1/alphatheta[4,1]+1/alphatheta[4,2]+1/alphatheta[4,3]+1/alphatheta[4,4])
alphapower4<-(alphahat4)^(1/alphatheta[4,5])
#lambda0 de 1/theta, mu0?, arrival time of systemic shock
shock4<-((Spdefaultprob[,1]+Spdefaultprob[,2]+Spdefaultprob[,3]+Spdefaultprob[,4])/4)^(1/alphatheta[4,5])*alphapower4
#tau of arrival time of systemic shock and marginal default time
sptau<-c(0,0,0,0)
sptau[1]<-0.832#Kendall(shock4,Spdefaultprob[,1])
sptau[2]<-0.935#Kendall(shock4,Spdefaultprob[,2])
sptau[3]<-0.93#Kendall(shock4,Spdefaultprob[,3])
sptau[4]<-0.93#Kendall(shock4,Spdefaultprob[,4])
spalpha<-alphatheta[4,1:4]
plot(as.numeric(spalpha),sptau,xlim=c(0,1),ylim=c(0,1),pch=21,col="red",xlab="alpha",ylab="kendall tau",main='Spain')
abline((alphatheta[4,5]-1)/alphatheta[4,5],1/alphatheta[4,5])

###
alphahat5<-4/(1/alphatheta[5,1]+1/alphatheta[5,2]+1/alphatheta[5,3]+1/alphatheta[5,4])
alphapower5<-(alphahat5)^(1/alphatheta[5,5])
#lambda0 de 1/theta, mu0?, arrival time of systemic shock
shock5<-((Itdefaultprob[,1]+Itdefaultprob[,2]+Itdefaultprob[,3]+Itdefaultprob[,4])/4)^(1/alphatheta[5,5])*alphapower5
#tau of arrival time of systemic shock and marginal default time
ittau<-c(0,0,0,0)
ittau[1]<-0.848 #Kendall(shock5,Itdefaultprob[,1])
ittau[2]<-0.872 #Kendall(shock5,Itdefaultprob[,2])
ittau[3]<-0.729 #Kendall(shock5,Itdefaultprob[,3])
ittau[4]<-0.918 #Kendall(shock5,Itdefaultprob[,4])
italpha<-alphatheta[5,1:4]
plot(as.numeric(italpha),ittau,xlim=c(0,1),ylim=c(0,1),pch=21,col="red",xlab="alpha",ylab="kendall tau",main='Italy')
abline((alphatheta[5,5]-1)/alphatheta[5,5],1/alphatheta[5,5])


##############################################hedge##############################################################

#############################UK###################################
### MtM Hedge
ukdate<-as.Date(UKdata[2:2597,1])
ukhedge1<-0.6*shock1

### Static Hedge
ukindex=0.6*((defaultprob[,1]+defaultprob[,2]+defaultprob[,3]+defaultprob[,4])/4)
ukhedge2<-alphahat1*ukindex*10

### EWMA Hedge
ukE1=shock1*ukindex
ukE2=ukindex^2
for(i in 2:2596){
  ukE1[i]=0.94*ukE1[i-1]+0.06*ukE1[i]
  ukE2[i]=0.94*ukE2[i-1]+0.06*ukE2[i]
}
ukhedge3=(ukE1/ukE2)/100

#plot
plot(ukdate,ukhedge1,col="red",type="l",xlab="date",ylab="UK hedge",ylim=c(0, 0.5))
lines(ukdate,ukhedge2,col="blue",type="l")
lines(ukdate,ukhedge3,col="green",type="l")
legend("topright",cex=0.6,c("MTM hedge","Static hedge","EWMA hedge"),
       lty=c(1,1,1),col=c("red", "blue","green"))

###########################France#####################################
### MtM Hedge
frdate<-as.Date(Francedata[2:2597,1])
frhedge1<-0.6*shock2

### Static Hedge
frindex=0.6*((Frdefaultprob[,1]+Frdefaultprob[,2]+Frdefaultprob[,3])/3)
frhedge2<-alphahat2*frindex*10

### EWMA Hedge
frE1=shock2*frindex
frE2=frindex^2
for(i in 2:2596){
  frE1[i]=0.94*frE1[i-1]+0.06*frE1[i]
  frE2[i]=0.94*frE2[i-1]+0.06*frE2[i]
}
frhedge3=(frE1/frE2)/100

#plot
plot(frdate,frhedge1,col="red",type="l",xlab="date",ylab="Francec hedge",ylim=c(0, 0.5))
lines(frdate,frhedge2,col="blue",type="l")
lines(frdate,frhedge3,col="green",type="l")
legend("topright",cex=0.6,c("MTM hedge","Static hedge","EWMA hedge"),
       lty=c(1,1,1),col=c("red", "blue","green"))

###########################Germany####################################
### MtM Hedge
gerdate<-as.Date(Germanydata[2:2597,1])
gerhedge1<-0.6*shock3

### Static Hedge
gerindex=0.6*((Gerdefaultprob[,1]+Gerdefaultprob[,2]+Gerdefaultprob[,3]+Gerdefaultprob[,4]+Gerdefaultprob[,5])/5)
gerhedge2<-alphahat3*gerindex*10

### EWMA Hedge
gerE1=shock3*gerindex
gerE2=gerindex^2
for(i in 2:2596){
  gerE1[i]=0.94*gerE1[i-1]+0.06*gerE1[i]
  gerE2[i]=0.94*gerE2[i-1]+0.06*gerE2[i]
}
gerhedge3=(gerE1/gerE2)/100

#plot
plot(gerdate,gerhedge1,col="red",type="l",xlab="date",ylab="Germany hedge",ylim=c(0, 0.5))
lines(gerdate,gerhedge2,col="blue",type="l")
lines(gerdate,gerhedge3,col="green",type="l")
legend("topright",cex=0.6,c("MTM hedge","Static hedge","EWMA hedge"),
       lty=c(1,1,1),col=c("red", "blue","green"))

############################Spain#####################################
### MtM Hedge
spdate<-as.Date(Spaindata[2:2590,1])
sphedge1<-0.6*shock4

### Static Hedge
spindex=0.6*((Spdefaultprob[,1]+Spdefaultprob[,2]+Spdefaultprob[,3]+Spdefaultprob[,4])/4)
sphedge2<-alphahat4*spindex*10

### EWMA Hedge
spE1=shock4*spindex
spE2=spindex^2
for(i in 2:2589){
  spE1[i]=0.94*spE1[i-1]+0.06*spE1[i]
  spE2[i]=0.94*spE2[i-1]+0.06*spE2[i]
}
sphedge3=(spE1/spE2)/100

#plot
plot(spdate,sphedge1,col="red",type="l",xlab="date",ylab="Spain hedge",ylim=c(0, 0.5))
lines(spdate,sphedge2,col="blue",type="l")
lines(spdate,sphedge3,col="green",type="l")
legend("topright",cex=0.6,c("MTM hedge","Static hedge","EWMA hedge"),
       lty=c(1,1,1),col=c("red", "blue","green"))

############################Italy#####################################
### MtM Hedge
itdate<-as.Date(Italydata[2:2515,1])
ithedge1<-0.6*shock5

### Static Hedge
itindex=0.6*((Itdefaultprob[,1]+Itdefaultprob[,2]+Itdefaultprob[,3]+Itdefaultprob[,4])/4)
ithedge2<-alphahat5*itindex*10

### EWMA Hedge
itE1=shock5*itindex
itE2=itindex^2
for(i in 2:2514){
  itE1[i]=0.94*itE1[i-1]+0.06*itE1[i]
  itE2[i]=0.94*itE2[i-1]+0.06*itE2[i]
}
ithedge3=(itE1/itE2)/100

#plot
plot(itdate,ithedge1,col="red",type="l",xlab="date",ylab="Italy hedge",ylim=c(0, 0.5))
lines(itdate,ithedge2,col="blue",type="l")
lines(itdate,ithedge3,col="green",type="l")
legend("topright",cex=0.6,c("MTM hedge","Static hedge","EWMA hedge"),
       lty=c(1,1,1),col=c("red", "blue","green"))