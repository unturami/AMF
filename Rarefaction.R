
# Rodriguez et al. Arbuscular mycorrhizal fungi of the Gurupi Biological 
# Reserve, Eastern Brazilian Amazon
#
#############################################################################
#############################################################################
#############################################################################

#                       RAREFACTION

#############################################################################
#############################################################################
#############################################################################

setwd("D:/R/Data/Ney")#set the work directory()
library(readxl)
data <- read_excel("D:/A/Papers andamento/Lesbia/RBCS/data/data.xlsx")
names(data)
data<-data[,c(9:10,16:65)]
data<-as.data.frame(data)

library (rich)
library (iNEXT)
library(vegan)
#############################################################################
#############################################################################
#############################################################################
######################
#####################       landUSE
######################
#############################################################################
#############################################################################
#############################################################################

##### FOREST
f<-subset(data,landuse=='F')
names(f)
spf<-f[,3:ncol(data)]
spsum<-apply(spf,2,sum)###apply total sum by col
spfin<-spf[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
spf<-spfin
names(f)
f<-cbind(f[,1:2],spf)

##### LOGGED FOREST
lf<-subset(data,landuse=='LF')#6;28 sp
splf<-lf[,3:ncol(data)]
spsum<-apply(splf,2,sum)###apply total sum by col
spfin<-splf[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
splf<-spfin
names(lf)
lf<-cbind(lf[,1:2],splf)

##### SECONDARY FOREST
sf<-subset(data,landuse=='SF')#6;28 sp
spsf<-sf[,3:ncol(data)]
spsum<-apply(spsf,2,sum)###apply total sum by col
spfin<-spsf[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
spsf<-spfin
names(sf)
sf<-cbind(sf[,1:2],spsf)

##### PASTURE
p<-subset(data,landuse=='P')#6;28 sp
spp<-p[,3:ncol(data)]
spsum<-apply(spp,2,sum)###apply total sum by col
spfin<-spp[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
spp<-spfin
names(p)
p<-cbind(p[,1:2],spp)

#############################################################################
#############################################################################
#############################################################################
#############################################################################
######################
#####################       LANDSCAPE
######################
#############################################################################
#############################################################################
#############################################################################
cl<-subset(data,landscape=='CL')#6;28 sp
sp<-cl[,3:ncol(data)]
spsum<-apply(sp,2,sum)###apply total sum by col
spfin<-sp[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
spcl<-spfin
cl<-cbind(cl[,1:2],spcl)


dl<-subset(data,landscape=='DL')#6;28 sp
sp<-dl[,3:ncol(data)]
spsum<-apply(sp,2,sum)###apply total sum by col
spfin<-sp[,!spsum<0.001]### delete sp < 1
#check sp values for spfin again
spsumfin<-apply(spfin,2,sum)###apply total sum by col
spsumfin
spdl<-spfin
dl<-cbind(dl[,1:2],spdl)

cl<-specaccum(spcl)
dl<-specaccum(spdl)


#############################################################################
######################################################################
######################################################################
#
#             S AND JACKKNIFE 2 INDEX
#
######################################################################
######################################################################
######################################################################

######################################################################
#
#             LANDUSE
#
######################################################################

#######  f
poolf <- poolaccum(spf)
#######  lf
poollf <- poolaccum(splf)
#######  sf
poolsf <- poolaccum(spsf)
#######  p
poolp <- poolaccum(spp)

sm<-apply(poolf$S,1,mean)
ssd<-apply(poolf$S,1,sd)
ja2m<-apply(poolf$jack2,1,mean)
ja2sd<-apply(poolf$jack2,1,sd)

lfsm<-apply(poollf$S,1,mean)
lfssd<-apply(poollf$S,1,sd)
lfja2m<-apply(poollf$jack2,1,mean)
lfja2sd<-apply(poollf$jack2,1,sd)

sfsm<-apply(poolsf$S,1,mean)
sfssd<-apply(poolsf$S,1,sd)
sfja2m<-apply(poolsf$jack2,1,mean)
sfja2sd<-apply(poolsf$jack2,1,sd)

psm<-apply(poolp$S,1,mean)
pssd<-apply(poolp$S,1,sd)
pja2m<-apply(poolp$jack2,1,mean)
pja2sd<-apply(poolp$jack2,1,sd)

rarelu<-as.data.frame(cbind(sm,ssd,ja2m,ja2sd,
                            lfsm,lfssd,lfja2m,lfja2sd,
                            sfsm,sfssd,sfja2m,sfja2sd,
                            psm,pssd,pja2m,pja2sd))

######################################################################
#
#             LANDSCAPE
#
######################################################################
#top
#######  top
poolCL <- poolaccum(spcl)# DEJARE CL Y DL COMO T E LW POR FACILIDAD
#######  top
poolDL <- poolaccum(spdl)

CLsm<-apply(poolCL$S,1,mean)
CLssd<-apply(poolCL$S,1,sd)
CLja2m<-apply(poolCL$jack2,1,mean)
CLja2sd<-apply(poolCL$jack2,1,sd)

DLsm<-apply(poolDL$S,1,mean)
DLssd<-apply(poolDL$S,1,sd)
DLja2m<-apply(poolDL$jack2,1,mean)
DLja2sd<-apply(poolDL$jack2,1,sd)

rarels<-as.data.frame(cbind(CLsm,CLssd,CLja2m,CLja2sd,
                            DLsm,DLssd,DLja2m,DLja2sd))

#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
#
#                  Plot
#
#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
#tiff("rarefaction.tiff", width = 15, height =15, units = 'cm', res = 300, compression = 'lzw')
#setwd("D:/fev2021/R/Data/Ney/Figures")#Set directory to save

#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
attach(rarelu)
xi<-as.numeric(rownames(rarelu))
#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
#x11()
par(mfrow=c(2,2))

###########################  S
plot(sm,type = "o",lty=1,col="black",bty='l',pch=2,
     xlab = "",ylab = "Riqueza",main = "S",
     xlim = c(1,11),
     ylim = c(5,40))
arrows(xi, sm - ssd, xi, sm + ssd,lty=1,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,40))
######
par(new=T)
plot(lfsm,type = "o",lty=2,col="black",bty='l',pch=16,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,40))
arrows(xi, lfsm - lfssd, xi, lfsm + lfssd,lty=2,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,40))
######
par(new=T)
plot(sfsm,type = "o",lty=3,col="black",bty='l',pch=1,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,40))
arrows(xi, sfsm - sfssd, xi, sfsm + sfssd,lty=3,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,40))
######
par(new=T)
plot(psm,type = "o",lty=4,col="black",bty='l',pch=18,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,40))
arrows(xi, psm - pssd, xi, psm + pssd,lty=4,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,40))

legend("topleft",inset = 0.0, legend=c("BA","BES","BS","P"),
       pch = c(2,16,1,18),
       horiz = F,box.lty=0,y.intersp =  0.7,lty=c(1:4),
       col="black",bg=NULL,
       cex = 0.7)
text(10.7,39,"A",cex = 1.2)
######       jack2

plot(ja2m,type = "o",lty=1,col="black",bty='l',pch=2,
     xlab = "",ylab = "",main = "Jack2",
     xlim = c(1,11),
     ylim = c(5,60))
arrows(xi, ja2m - ja2sd, xi, ja2m + ja2sd,lty=1,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,60))
######
par(new=T)
plot(lfja2m,type = "o",lty=2,col="black",bty='l',pch=16,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,60))
arrows(xi, lfja2m - lfja2sd, xi, lfja2m + lfja2sd,lty=2,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,60))
######
par(new=T)
plot(sfja2m,type = "o",lty=3,col="black",bty='l',pch=1,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,60))
arrows(xi, sfja2m - sfja2sd, xi, sfja2m + sfja2sd,lty=3,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,60))
######
par(new=T)
plot(pja2m,type = "o",lty=4,col="black",bty='l',pch=18,
     xlab = "",ylab = "",main = "",xaxt="n",yaxt="n",
     xlim = c(1,11),
     ylim = c(5,60))
arrows(xi, pja2m - pja2sd, xi, pja2m + pja2sd,lty=4,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,11),
       ylim = c(5,60))
text(10.7,58,"B",cex = 1.2)


#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
detach(rarelu)
attach(rarels)
xi<-as.numeric(rownames(rarels))
#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333
#########################################################################333

######       S

plot(CLsm,type = "o",lty=1,col="black",bty='l',pch=2,
     xlab = "Parcela",ylab = "Riqueza",main = "S",
     xlim = c(1,25),
     ylim = c(5,60))
arrows(xi, CLsm - CLssd, xi, CLsm + CLssd,lty=1,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,25),
       ylim = c(5,60))
######
par(new=T)
plot(DLsm,type = "o",lty=1,col="black",bty='l',pch=17,
     xlab = "",ylab = "",main = "",
     xlim = c(1,25),
     ylim = c(5,60))
arrows(xi, DLsm - DLssd, xi, DLsm + DLssd,lty=2,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,25),
       ylim = c(5,60))

legend("topleft",inset = 0.0, legend=c("RC","RD"), pch=c(2,17),
       horiz = F,box.lty=0,y.intersp =  0.7,lty=1,
       col="black",bg=NULL,
       cex = 0.7)
text(24.7,58,"C",cex = 1.2)
######       Jack2

plot(CLja2m,type = "o",lty=1,col="black",bty='l',pch=2,
     xlab = "Parcela",ylab = "",main = "Jack2",
     xlim = c(1,25),
     ylim = c(5,70))
arrows(xi, CLja2m - CLja2sd, xi, CLja2m + CLja2sd,lty=1,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,25),
       ylim = c(5,70))
######
par(new=T)
plot(DLja2m,type = "o",lty=1,col="black",bty='l',pch=17,
     xlab = "",ylab = "",main = "",
     xlim = c(1,25),
     ylim = c(5,70))
arrows(xi, DLja2m - DLja2sd, xi, DLja2m + DLja2sd,lty=2,
       code = 3, col = "black", angle = 0, length = .1,
       xlab = "",ylab = "",
       xlim = c(1,25),
       ylim = c(5,70))
text(24.7,68,"D",cex = 1.2)

#dev.off()
######################################################################
######################################################################
######################################################################
######################################################################
#
#              MODEL TO TEST DIFFERENCES OF TRENDS
#
######################################################################
######################################################################
######################################################################
######################################################################
names(rarelu)
ndat<-as.data.frame(cbind(rarelu$sm,rarelu$lfsm,rarelu$sfsm,rarelu$psm))
ndat2<-as.data.frame(cbind(rarelu$ja2m,rarelu$lfja2m,rarelu$sfja2m,rarelu$pja2m))

table<-matrix(data=NA,ncol=3,nrow=40)
colnames(table)<-c("LU","S","J2")

sequ<-c("f","lf","sf","p")

table[,1]<-rep(sequ,each=10)

table[1:10,2]<-ndat$V1
table[11:20,2]<-ndat$V2
table[21:30,2]<-ndat$V3
table[31:40,2]<-ndat$V4

table[1:10,3]<-ndat2$V1
table[11:20,3]<-ndat2$V2
table[21:30,3]<-ndat2$V3
table[31:40,3]<-ndat2$V4

table<-as.data.frame(table)
table$LU<- as.factor(table$LU)
table$S<-as.numeric(table$S)
table$J2<-as.numeric(table$J2)


#########################################################################3
names(rarels)
ndat3<-as.data.frame(cbind(rarels$CLsm,rarels$DLsm))
ndat4<-as.data.frame(cbind(rarels$CLja2m,rarels$DLja2m))

table2<-matrix(data=NA,ncol=3,nrow=44)
colnames(table2)<-c("LS","S","J2")

sequ<-c("RC","RD")
table2[,1]<-rep(sequ,each=22)

table2[1:22,2]<-ndat3$V1
table2[23:44,2]<-ndat3$V2

table2[1:22,3]<-ndat4$V1
table2[23:44,3]<-ndat4$V2


table2<-as.data.frame(table2)
table2$LS<- as.factor(table2$LS)
table2$S<-as.numeric(table2$S)
table2$J2<-as.numeric(table2$J2)

M1 <- glm(S~LU,family = poisson,data=table)
summary(M1)#lf=f
M2 <- glm(J2~LU,family = poisson,data=table)
summary(M2)# F** LF***  P*

M3 <- glm(S~LS,family = poisson,data=table2)
summary(M3)#***
M4 <- glm(J2~LS,family = poisson,data=table2)
summary(M4)#***
