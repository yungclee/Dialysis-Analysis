data=read.csv("G:/SC DATA/new_data.csv")
data=data[,-1]
set.seed(10)

data.H.1=data[which(data$H==1),]
data.H.0=data[which(data$H==0),]


#### H=1 targeting selected index for H=1 ####
loc.1.1=sample(which(data.H.1$age_c==1),ceiling(sum(data.H.1$age_c==1)/10),rep=F)
loc.1.2=sample(which(data.H.1$age_c==2),ceiling(sum(data.H.1$age_c==2)/10),rep=F)
loc.1.3=sample(which(data.H.1$age_c==3),ceiling(sum(data.H.1$age_c==3)/10),rep=F)

#### H=0 targeting selected index for H=0 ####
loc.0.1=sample(which(data.H.0$age_c==1),ceiling(sum(data.H.0$age_c==1)/10),rep=F)
loc.0.2=sample(which(data.H.0$age_c==2),ceiling(sum(data.H.0$age_c==2)/10),rep=F)
loc.0.3=sample(which(data.H.0$age_c==3),ceiling(sum(data.H.0$age_c==3)/10),rep=F)

#### combine index ####
out.from.H1=c(loc.1.1,loc.1.2,loc.1.3)+274552
out.from.H0=c(loc.0.1,loc.0.2,loc.0.3)
out.from=c(out.from.H0,out.from.H1)

#### 
names(data)
data$age_c=as.factor(data$age_c)
for(i in 5:37){data[,i]=as.factor(data[,i])}
test.data=data[out.from,]
training.data=data[-out.from,]

fit2.glm=glm(H~age_c+avg_dg_amt+avg_dg_d+trt1+
               trt1+trt2+trt3+trt4+
               trt6+trt9+trt11+trt13+
               trt16+trt17+trt24+trt25+
               trt34+trt35+trt36+trt38+
               trt39+trt40+trt50+trt54+
               trt56+trt58+trt61+trt62+
               trt63+trt66+trt68+trt74+
               trt79+trt80+trt82+trt94,
             data=training.data,family=binomial)
summary(fit.glm)
str(training.data)
names(fit)
1-pchisq(fit$null.deviance-fit$deviance,26)
####plotting fitted prob####
p <- sum(training.data$H==1)/nrow(training.data)
pch_v <-c(rep("¡C",nrow(test.data)))
col_v <-c(rep("black",nrow(test.data)))

data.try=read.csv("G:/SC DATA/TRY.csv")
data.try$age_c=factor(data.try$age_c)
try_p=predict(fit,newdata=data.try,type="response")
fit_p=predict(fit.glm,newdata=test.data,type="response")

table(fit_p>p)
(27548-931)/length(fit_p)
for (i in 1:nrow(test.data)){
  if(fit_p[i]>p){
    #pch_v[i] <- "¡C"
    col_v[i] <- "red"
  }
}

plot(fit_p,col=col_v,pch=pch_v,cex=.5,ylim=c(-.03,.65),
     xlab="Individual",ylab="Predicted Probability",main="Predicted Probability via LR model")
text(x=8000,y=0.1,
     expression(paste("above ",hat(p)," : ","3.4%(931)",sep=" "))
     ,pos=4)

text(x=8000,y=-0.03,expression(paste("below ",hat(p)," : "
                                     ,"96.6%(26617)",sep=" ")),pos=4)
text(x=27457.5,y=0.55,"Patient undergo dialysis",srt=90,pos=2)
abline(v=sum(test.data$H==0)+.5)
abline(h=p,lty=2)


plot(fit_p,col=col_v,pch=pch_v,cex=1.5,ylim=c(-.02,.65),xlim=c(27410,nrow(test.data))
     ,xlab="Individual",ylab="Predicted Probability",main="Predicted Probability via LR model")

text(x=27430,y=0.03,expression(paste("Sample Proportion ",hat(p),sep=" ")))
text(x=27457+.2,y=0.6,"Patient undergo dialysis",srt=90,pos=2)
polygon(x=c(280000,280000,27457+.5,27457+.5),y=c(-.1,1,1,-.1),col="aliceblue")
box()
points(fit_p,col=col_v,pch=pch_v,ylim=c(-.005,.15),xlim=c(274100,nrow(data)))
abline(v=sum(test.data$H==0)+.5)
abline(h=p)

h0.pre.1=sum(fit_p[1:27457]>p)
h0.pre.0=sum(fit_p[1:27457]<p)
h1.pre.1=sum(fit_p[27458:length(fit_p)]>p)
h1.pre.0=sum(fit_p[27458:length(fit_p)]<p)

TPrate=round(h1.pre.1/sum(test.data$H==1),digit=4)
FNrate=round(h1.pre.0/sum(test.data$H==1),digit=4)
TNrate=round(h0.pre.0/sum(test.data$H==0),digit=4)
FPrate=round(h0.pre.1/sum(test.data$H==0),digit=4)

text(x=27430,y=0.4,    paste("FPrate= ",FPrate),cex=1.2)
text(x=27430,y=-0.022, paste("TNrate= ",TNrate),cex=1.2)
text(x=27502,y=0.4,    paste("TPrate= ",TPrate),cex=1.2)
text(x=27502,y=-0.022, paste("FNrate= ",FNrate),cex=1.2)
#### ROC ####
install.packages("pROC")
library(pROC)


glm.roc<-roc(test.data$H,fit_p)
names(glm.roc)
glm.roc$auc
sensitivities=(plot.roc(glm.roc,col="black",main="ROC curve",lwd=3,print.thres=T,add=F,
                        legacy.axes=T,identity.col="black",auc.polygon=T,auc.polygon.col="gray60"))$sensitivities
specificities=(plot.roc(glm.roc,col="black",main="ROC curve",lwd=3,print.thres=T,add=F,
                        legacy.axes=T,identity.col="black",auc.polygon=T,auc.polygon.col="gray60"))$specificities
par(xaxs="i",yaxs="i")
plot(1-specificities,sensitivities,xlim=c(0,1),type="l",lwd=3,
     main="ROC Curve",ylab="Sensitivity",xlab="1-Specificty")
polygon(c(1-specificities[1] ,1-specificities,0,1-specificities[1] )
        ,c(0,sensitivities  , 0,0),col="royalblue1")
grid()
text(0.2,0.7,"AUC=0.9796",cex=1.8,col="black")
lines(x=c(0,1),y=c(0,1),col="black",lty=3,lwd=3)

#######