
data=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002).csv")

table(data$drug_day)
install.packages("psych")
library(psych)

describe(data$age)

names(data)
attach(data)
id_list=names(table(id))

sex_1=NULL
years_1=numeric(length(table(id)))
for(i in 1:length(table(id))){
  temp=data[which(data[,"id"]==id_list[i]),]
  sex_1[i]=max(as.character(temp$id_sex))
  years_1[i]=temp$first_h[1]
  
}
table(floor(years_1/10000))

dataframe=data.frame(sex=sex_1,years=floor(years_1/10000))
table.out=table(dataframe)/c(298468,295102,287984,294792,295097,294102,294001,293319,293016,293027)*1000

bar.out=barplot(table.out,legend=levels(dataframe$sex),ylim=c(0,1),main="Proportion v.s. Year v.s. Gender",ylab="Proportion of Hemodialysis (per mil (1/1000))",xlab="Year",col=c("pink2","royalblue1"))
points(bar.out,colSums(table.out),type="o",pch=20,cex=2,lwd=3)
text(table(dataframe)[1,],x=bar.out,y=table.out[1,]/2,xpd=TRUE)
text(table(dataframe)[2,],x=bar.out,y=table.out[2,]/2+table.out[1,],xpd=TRUE)

bar.out=barplot(table.out,legend=levels(dataframe$sex),ylim=c(0,1),main="性別及年份之比例關係",ylab="洗腎患者之比例 (1/1000)",xlab="年份",col=c("pink2","royalblue1"))
points(bar.out,colSums(table.out),type="o",pch=20,cex=2,lwd=3)
text(table(dataframe)[1,],x=bar.out,y=table.out[1,]/2,xpd=TRUE)
text(table(dataframe)[2,],x=bar.out,y=table.out[2,]/2+table.out[1,],xpd=TRUE)

names(data)
C1=data$cure_item_no1
C2=data$cure_item_no2
C3=data$cure_item_no3
C4=data$cure_item_no4


C_all <- as.factor(cbind(as.character(C1),as.character(C2),as.character(C3),as.character(C4)))
C_all_sort <-sort(table(C_all),decreasing=TRUE)[c(-1,-6,-35)]
C_prop <-unlist((C_all_sort)/sum(C_all_sort))


library(plotrix)
str(C_prop)

C_prop_slct=head(C_prop,6)
C_prop_slct=c(C_prop_slct,1-sum(C_prop_slct))
colnames(C_prop_slct)=c('糖尿病','高血壓','慢性肝臟炎','心臟病','高血脂','關節炎')

pie3D(C_prop_slct,labels=c('糖尿病 \n16.5% ','高血壓 \n15.1%','慢性腎臟炎 \n13.3%','心臟病 \n5.7%','高血脂 \n4.2%','關節炎 \n3.1%','其他100+特殊治療 \n40%'),
      explode=0.5,col=2:8,theta=pi/4,radius=1.3,labelrad=1,labelcex=1,shade=.3,labelcol='black')

pie3D(C_prop_slct,labels=c('Diabetes \n16.5% ','Hypertension \n15.1%','Chronic Pyelonephritis \n13.3%',
      'Heart Disease \n5.7%','Hyperlipidemia \n4.2%','Arthritis \n3.1%','Other 100+ treatments \n40%'),
      explode=0.3,col=2:8,main='Proportion of Treatments',theta=pi/4,radius=1.15,labelrad=1.2,labelcex=0.8,
      shade=.4)




col.list=c(rep("red",6),rep("black",length(C_prop-6)))
plot((C_all_sort)/sum(C_all_sort),col=col.list,pch=20,xlab='治療',ylab='比例',main='特殊治療比例')
for (i in 1:6){
  text(x=i+15,y=as.vector((C_all_sort)/sum(C_all_sort))[i],
       label=c('糖尿病 16.5%　　','高血壓 15.1%　　','慢性肝臟炎 13.3%',
               '心臟病 5.7%　　','高血脂 4.2%　　','關節炎 3.1%　　',cex=1)[i])
}

col.list=c(rep("red",6),rep("black",length(C_prop-6)))
plot((C_all_sort)/sum(C_all_sort),col=col.list,pch=20,xlab='Treatments',ylab='Proportion',main='Proportions of Treatments')
for (i in 1:6){
  text(x=i+15,y=as.vector((C_all_sort)/sum(C_all_sort))[i],
       label=c('Diabetes 16.5% ','Hypertension 15.1%','Chronic Hepatitis 13.3%','Heart Disease 5.7%','Hyperlipidemia 4.2%','Arthritis 3.1%')[i],
       cex=0.8)
}
#lines(c(0,7),c(0.2,0),col="red",lty=3)
cor(data1[c(10,11)])
cor(data[c(10,11)])
cor(data2[c(10,11)])
cor(data2)

data1=subset(data,drug_amt!=0)
data2=subset(data1,drug_day<=35)

library(ggplot2)
qplot(data=data,drug_day,drug_amt,color=id_sex,colour=c("red","blue"))+
  scale_colour_hue(l=50) +
  geom_smooth(method='lm',   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=T) +# Extend regression lines
  ylab('Drug Amount')+
  xlab('Drug Day')


ggplot(drug_amt,drug_day,data=data)
ggplot((x=drug_day),data=data)
+
  geom_histogram(aes(y=..density..),binwidth=0.2,color="black",fill="white")+
  geom_density(alpha=.2,color="green",size=1)
