total.id.harea=read.csv("D:/data1/temp/total_id_hosparea.csv")
total.id.harea
summary(total.id.harea)
sort(total.id.harea$id_birthday)
subset(total.id.harea,by='id_birthday'==18000101)

cd=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002).csv")
total.id.harea=cd


total.id.harea.id=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002_id).csv")
func.date=floor(total.id.harea.id$func_date/10000)

id.birth=floor(total.id.harea.id$id_birthday/10000)
table(func.date-id.birth)

hist(func.date-id.birth)

id.pop.2000=read.csv("D:/data1/temp/20141218/ID_population.csv")
id.pop.2002=read.csv("D:/data1/id_population/id_population2002.csv")
id.pop.2003=read.csv("D:/data1/id_population/id_population2003.csv")
id.pop.2004=read.csv("D:/data1/id_population/id_population2004.csv")
id.pop.2005=read.csv("D:/data1/id_population/id_population2005.csv")
id.pop.2006=read.csv("D:/data1/id_population/id_population2006.csv")
id.pop.2007=read.csv("D:/data1/id_population/id_population2007.csv")
id.pop.2008=read.csv("D:/data1/id_population/id_population2008.csv")
id.pop.2009=read.csv("D:/data1/id_population/id_population2009.csv")
barplot(table(id.pop.2000$AREA_NO_I)[-1])
barplot(table(id.pop.2002$AREA_NO_I)[-1],main='2002跋羆计',xlab='跋(絏)',ylab="计",col='royalblue2')
barplot(table(id.pop.2003$AREA_NO_I)/10000,main='2003跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))
barplot(table(id.pop.2004$AREA_NO_I)/10000,main='2004跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))
barplot(table(id.pop.2005$AREA_NO_I)/10000,main='2005跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))
barplot(table(id.pop.2006$AREA_NO_I)/10000,main='2006跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))
barplot(table(id.pop.2007$AREA_NO_I),main='2007跋羆计',xlab='跋(絏)',ylab="计",col='royalblue2')
barplot(table(id.pop.2008$AREA_NO_I)/10000,main='2008跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))
barplot(table(id.pop.2009$AREA_NO_I)[-length(table(id.pop.2008$AREA_NO_I))]/10000,main='2009跋羆计',xlab='跋(絏)',ylab="计(窾)",col='royalblue2',ylim=c(0,7))

table(id.pop$AREA_NO_I)
names(id.pop)

describe(cd$drug_amt)
describe(subset(cd$drug_amt, drug_amt!=0))

cd=read.csv("D:/data1/temp/20141227/corrected data(total_cd_list_hosparea_2002).csv")
table.out=table(cd$drug_day)
bar.out=barplot(table(cd$drug_day))
points(bar.out,c(0:99),type="o",pch=20,cex=2,lwd=3)
plot.new()
plot(y=table.out[1:50],x=c(0:49),col='royalblue3',ylim=c(0,80000),xlab='矪よぱ计',ylab='Ω计',main='矪よぱ计ч絬瓜',pch=20)
lines(y=table.out[1:50],x=c(0:49),col='royalblue2')
for(i in 1:50){
  if (table.out[i]>1000){text(x=i-1,y=table.out[i]+4000,i-1)}
}

plot(y=table.out[1:50],x=c(0:49),col='royalblue3',ylim=c(0,80000),xlab='Drug Day',ylab='Count',main='Line Chart of Drug Day',pch=20)
lines(y=table.out[1:50],x=c(0:49),col='royalblue2')
for(i in 1:50){
  if (table.out[i]>1000){text(x=i-1,y=table.out[i]+4000,i-1)}
}
points(table.out[1:100])
