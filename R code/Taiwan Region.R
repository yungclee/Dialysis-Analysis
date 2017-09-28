data <- read.csv("C:/Users/user/Desktop/SC/H=1complete.csv",header=T)
head(data,20)
attach(data)
str(data)

#### number of patients of each year & region####

id_list<-names(table(data$id))
area <- numeric(length(id_list))
year <- numeric(length(id_list))

for (i in 1:length(id_list)){
  tmp <- data[which(id==id_list[i]),]
  area[i]<-names(which.max(table(tmp$AREA_NO_H)))
  year[i]<-max(tmp$func_date)
}

area_code <- c(as.character(c(1,2,11,12,17,21,22,31,32,33,34,35,36,37,38,39,40,41,
                                        42,43,44,45,46,90,91,99)),"XX")
area_code2 <- c("01","02",area_code[3:27])
length(area_code)
plot.data <- data.frame(year=factor(trunc(year/10000),levels=c(2002:2011)),
                        area=factor(trunc(as.numeric(area)/100),levels=area_code))
plot.data

barplot(table(plot.data))
##### total number of patients of each region ####
area_data <- read.csv("C:/Users/user/Downloads/ID_population.csv")
head(area_data)
area_count <- table(factor(area_data$AREA_NO_I,levels=area_code2))
#各區域年度發病比率(需要三十萬人的ID_AREA)



#####generating prob of each year by region####
g.area_count <- c(area_count["02"],0,area_count["01"],area_count["37"],area_count["22"]+area_count["40"],
                  area_count["12"]+area_count["33"],area_count["45"],area_count["34"],area_count["42"],
                  area_count["11"],area_count["35"],area_count["38"],area_count["44"],area_count["43"],
                  area_count["17"],area_count["36"],area_count["21"],area_count["41"],area_count["31"],
                  area_count["46"],area_count["32"],area_count["39"]) 
v.g.area_count <- as.numeric(g.area_count); v.g.area_count
g.pat_count <- matrix(c(table(plot.data)[,2],rep(0,10),table(plot.data)[,1],table(plot.data)[,14],
                      table(plot.data)[,7]+table(plot.data)[,17],table(plot.data)[,4]+table(plot.data)[,10],
                      table(plot.data)[,22],table(plot.data)[,11],table(plot.data)[,19],table(plot.data)[,3],
                      table(plot.data)[,12],table(plot.data)[,15],table(plot.data)[,21],table(plot.data)[,20],
                      table(plot.data)[,5],table(plot.data)[,13],table(plot.data)[,6],table(plot.data)[,18],
                      table(plot.data)[,8],table(plot.data)[,23],table(plot.data)[,9],table(plot.data)[,16]),
                      10,22,byrow=FALSE) ;g.pat_count

prob <- matrix(0,10,22)
for (i in 1:10){
  prob[i,] <- g.pat_count[i,]/v.g.area_count
}
prob_All <- colSums(prob)
####prob (matrix) is the prob of each region in gadm order from 2002~2011###

prob[,2] <- max(prob,na.rm=T)
prob[10,5]<-0 ## to ajust the y-axis mapping not real data
##### ploting taiwan map with the prob. of occurance by region#####
install.packages("sp")
library(sp)
library(RColorBrewer)
load("C:/Users/user/Downloads/TWN_adm2.RData")

x <- c(118.9,122.5)
y <- c(21.5,25.6)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

  png(filename="C:/Users/user/Pictures/Taiwan Region/2002_2011_cumprob.png")
  i=1
  gadm$NAME_2 <- as.numeric(prob[i,])*10000
  spplot(gadm, "NAME_2", col.regions=sort(heat.colors(22,1.0),decreasing=TRUE),
         main=paste(2001+i,"Taiwan Regions"," "), colorkey = TRUE, lwd=1.0, col="black",
         ylab="Probability (in 1/10000)",xlim=bbox(S)[1,],ylim=bbox(S)[2,])
 
  dev.off()

##overall
gadm$NAME_2 <- as.numeric(prob_All)*10000
spplot(gadm, "NAME_2", col.regions=colorRampPalette(c("White","navy"))(100),
       main="2002~2011 Cumulative Probability", colorkey = TRUE, lwd=1.0, col="black",
       ylab="Probability (in 1/10000)",xlim=bbox(S)[1,],ylim=bbox(S)[2,])


colorRampPalette(c("White","Blue"))(100)
install.packages("grDevices")
library(grDevices)
install.packages("colorRamps")
library(colorRamps)
ygobb(50)

####plot-seperating 5 regions####
#> gadm$NAME_2
#[1] "Kaohsiung City" ""               "Taipei City"    "Changhwa"      
#[5] "Chiayi"         "Hsinchu"        "Hualien"        "Ilan"          
#[9] "Kaohsiung"      "Keelung City"   "Miaoli"         "Nantou"        
#[13] "Penghu"         "Pingtung"       "Taichung City"  "Taichung"      
#[17] "Tainan City"    "Tainan"         "Taipei"         "Taitung"       
#[21] "Taoyuan"        "Yunlin" 
area_col <- c(7,NA,2,3,6,5,4,2,7,2,5,3,5,7,3,3,6,6,2,4,5,6)-1
gadm$NAME_2 <- area_col
spplot(gadm, "NAME_2", col.regions=rainbow(20)[16:1],
       main="6 Taiwan Regions", colorkey = TRUE, lwd=1.0, col="gray",
       ylab="",xlim=bbox(S)[1,],ylim=bbox(S)[2,])