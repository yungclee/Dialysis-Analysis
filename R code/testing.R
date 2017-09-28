data_0_r=read.csv("G:/SC DATA/H=0revise.csv")
data_1_r=read.csv("G:/SC DATA/H=1revise.csv")
names(data_0_r)

####Test for propotion for the different sex
sex.0.r=data_0_r$id_sex
sex.1.r=data_1_r$id_sex

m.1.num=sum(sex.1.r=="M")
f.1.num=sum(sex.1.r=="F")

m.0.num=sum(sex.0.r=="M")
f.0.num=sum(sex.0.r=="F")

sextesting=matrix(c(m.1.num,f.1.num,
                    m.0.num,f.0.num),
                  nrow=2,dimnames=list("¬~µÇ"=c("¦³","¨S¦³"),
                                        SEX=c("M","F")),byrow=T)
fisher.test(sextesting)

#Fisher's Exact Test for Count Data

#data:  sextesting
#p-value = 0.8928
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
# 0.8657983 1.1328417
#sample estimates:
#odds ratio 
# 0.9903345 

####test for area in a decade

data_0_r$loca=floor(data_0_r$"AREA_NO_H"/100)
table.out=table(data_0_r$loca)

taipei.0=sum(table.out[c(1,6,11,14,27,28)])
north.0=sum(table.out[c(7,12,13,15)])
center.0=sum(table.out[c(3,8,16,17,18)])
south.0=sum(table.out[c(4,9,10,19,20,21)])
hitdog.0=sum(table.out[c(2,5,22,23,24)])
east.0=sum(table.out[c(25,26)])

data_1_r$loca=floor(data_1_r$"AREA_NO_H"/100)
table.out=table(data_1_r$loca)

taipei.1=sum(table.out[c(1,3,8,11,24)])
north.1=sum(table.out[c(9,10,4,12)])
center.1=sum(table.out[c(5,13,14,15)])
south.1=sum(table.out[c(6,7,16,17,18)])
hitdog.1=sum(table.out[c(2,19,20,21)])
east.1=sum(table.out[c(22,23)])


localtesting=matrix(c(taipei.1,north.1,center.1,south.1,hitdog.1,east.1,
                      taipei.0,north.0,center.0,south.0,hitdog.0,east.0),
                  nrow=2,dimnames=list("¬~µÇ"=c("¦³","¨S¦³"),
                                       loc=c("taipei","north","center",
                                             "south","hitdog","east"))
                  ,byrow=T)

fisher.test(localtesting,workspace=9^9)
prop.test(localtesting[1,],colSums(localtesting))
chisq.test(localtesting)

#Fisher's Exact Test for Count Data

#data:  localtesting
#p-value = 2.406e-06
#alternative hypothesis: two.sided


#test for different years
data_1_c=read.csv("G:/SC DATA/H=1complete.csv")
names(data_1_c)
table.out=table(floor(data_1_c$first_h/10000))
c(table.out,298468,295102,287984,294792,295097,294102,294001,293319,293016,293027)
yeartesting=matrix(c(table.out,
                     298468,295102,287984,294792,295097,294102,294001,293319,293016,293027),
                    nrow=2,dimnames=list("¬~µÇ"=c("¦³","¨S¦³"),
                                         year=c("2002","2003","2004",
                                               "2005","2006","2007",
                                               "2008","2009","2010",
                                               "2011"))
                    ,byrow=T)

chisq.test(yeartesting)

#Pearson's Chi-squared test

#data:  yeartesting
#X-squared = 16.0974, df = 9, p-value = 0.06488


#####test for tainan

names(data_1_r)
tainan_1=c(which(floor(data_1_r$AREA_NO_H/100)==21),
           which(floor(data_1_r$AREA_NO_H/100)==41))

tainan_0=c(which(floor(data_0_r$AREA_NO_H/100)==21),
           which(floor(data_0_r$AREA_NO_H/100)==41),
           which(floor(data_0_r$AREA_NO_H/100)==05))

tainan.data.1.r=data_1_r[tainan_1,]
tainan.data.0.r=data_0_r[tainan_0,]

which(tainan.data.1.r==4130)
which(tainan.data.0.r==4130)
table.out.0=table(tainan.data.0.r$AREA_NO_H)


table.out.0[c(24,28,30,35,37,39,46,54,16,17,23,19,21,22)]=
  table.out.0[c(24,28,30,35,37,39,46,54,16,17,23,19,21,22)]+
  c(7,9,2,2,1,6,9,5,16,9,11,4,5,2)

table.out.0=table.out.0[c(-1:-15)]
table.out.0

table.out.1=table(factor(tainan.data.1.r$AREA_NO_H,
                   levels=as.numeric(names(table.out.0))))

tainantesting=matrix(c(sum(table.out.1[c(1:8,15,22,19,28,35,39)]),
                   sum(table.out.1[c(14,23,30:34,36:38)]),
                   sum(table.out.1[c(9,11,12,17,18,25,29)]),
                   sum(table.out.1[c(10,13,16,20,21,24,26,27)]),
                   sum(table.out.0[c(1:8,15,22,19,28,35,39)]),
                   sum(table.out.0[c(14,23,30:34,36:38)]),
                   sum(table.out.0[c(9,11,12,17,18,25,29)]),
                   sum(table.out.0[c(10,13,16,20,21,24,26,27)])
                   ),
                   nrow=2,dimnames=list("¬~µÇ"=c("¦³","¨S¦³"),
                                      loc=c("blue","green","red","yellow"))
                 ,byrow=T)

fisher.test(tainantesting)

#Fisher's Exact Test for Count Data

#data:  tainantesting
#p-value = 0.6077
#alternative hypothesis: two.sided



#####test for age
age.1=data_1_r$age
age.0=data_0_r$age


a11=length(age.1[which(age.1<45)])
a12=length(age.1[which(age.1<65)])
a13=length(age.1[which(age.1>=65)])

a01=length(age.0[which(age.0<45)])
a02=length(age.0[which(age.0<65)])
a03=length(age.0[which(age.0>=65)])

agetesting=matrix(c(a11,a12,a13,
                    a01,a02,a03),
                     nrow=2,dimnames=list("¬~µÇ"=c("¦³","¨S¦³"),
                                          loc=c("<45","45-65",">65"))
                     ,byrow=T)

chisq.test(agetesting)

#Pearson's Chi-squared test

#data:  agetesting
#X-squared = 974.3324, df = 2, p-value < 2.2e-16


fisher.test(agetesting[,c(1,2)])
#Fisher's Exact Test for Count Data

#data:  agetesting[, c(1, 2)]
#p-value < 2.2e-16
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
#0.2747604 0.4210566
#sample estimates:
#odds ratio 
#0.3414139 

fisher.test(agetesting[,c(1,3)])
#Fisher's Exact Test for Count Data

#data:  agetesting[, c(1, 3)]
#p-value < 2.2e-16
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
#0.06577181 0.10174050
#sample estimates:
#odds ratio 
#0.08209287 

fisher.test(agetesting[,c(2,3)])

#Fisher's Exact Test for Count Data

#data:  agetesting[, c(2, 3)]
#p-value < 2.2e-16
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval:
#0.2100290 0.2753314
#sample estimates:
#odds ratio 
#0.2404312 

