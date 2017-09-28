total.id.harea.id=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002_id).csv")
data=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002).csv")

func.date=floor(total.id.harea.id$func_date/10000)
id.birth=floor(total.id.harea.id$id_birthday/10000)
sex=total.id.harea.id$id_sex
age=(func.date-id.birth)

data_age_sex=data.frame(sex=sex,age=age)
table.out=table(data_age_sex)

hist(age,ylim=c(0,250),xlim=c(0,100),col="royalblue1",labels=TRUE,main="Age of First Time Doing Dialysis ",ylab="Counts",xlab='Age')
par(new=TRUE)
hist(data_age_sex[which(data_age_sex[,"sex"]=="F"),]$age,ylim=c(0,250),xlim=c(0,100)
     ,xlab="",ylab="",main="",col="pink2")
par(new=TRUE)
barplot(0,ylim=c(0,250),legend=c("F","M"),col=c("pink2","royalblue1"),axe=F)

hist(age,ylim=c(0,250),xlim=c(0,100),col="royalblue1",labels=TRUE,main="年齡及性別之次數關係 ",ylab="人次",xlab="年齡")
par(new=TRUE)
hist(data_age_sex[which(data_age_sex[,"sex"]=="F"),]$age,ylim=c(0,250),xlim=c(0,100)
     ,xlab="",ylab="",main="",col="pink2")
par(new=TRUE)
barplot(0,ylim=c(0,250),legend=c("女","男"),col=c("pink2","royalblue1"),axe=F)

summary(total.id.harea.id)
summary(data)
121128/142442
hist(floor(total.id.harea.id$first_h/10000))
