# data <- textConnection(centennial_Y2K$V1)
fmt <- c("A6", "A1", "A5", "I4", "2I3", "1X", "2I3", "F6.0", "1X", "2F8.0", "F6.0",
         "2I4", rep(c("F4.0", "1X", "A2", "1X", "A5"),12))
a <- read.fortran("centennial_Y2K.CAT", fmt)
n <- unlist(lapply(1:12,function(k) {
  c(paste("mag",k,sep="."), paste("msc",k,sep="."), paste("mdo",k,sep="."))
  }))
n <- c("icat", "asol", "isol", "yr", "mon", "day", "hr", "min", "sec", "glat", "glon", "dep", "greg", "ntel", n)
colnames(a) <- n

long <- a$glon+360*(a$glon<0)
lat <- a$glat
depth <- cut(a$dep,c(-Inf,70,350,Inf))
big_ones <- a$mag.1>8

#' If necesary install these packages using `install.packages(c("maps", "maptools", "mapdata"))`
library(maps)
library(mapdata)
map("worldHires",mar=c(0,0,0,0))
points(a[,c("glon","glat")], cex=0.5, col="red")

map("worldHires",mar=c(0,0,0,0))
points(a[a$mag.1>8,c("glon","glat")], cex=0.5, col="red")

map("worldHires",mar=c(0,0,0,0))
points(a[,c("glon","glat")],cex=(a$mag.1-5)/4)

map("world2Hires",mar=c(0,0,0,0),fill=F)
color <- c("red","yellow","blue")[depth]
points(long, a$glat, col=color)


energy <- exp(3*log(10)*(a$mag.1 + 2.9)/2)

plot(sort(energy)/sum(energy))
a[energy>sum(energy)*0.3, 4:15]
map("worldHires",mar=c(0,0,0,0))
points(a[,c("glon","glat")],cex=energy*9/sum(energy))

norm_e <- energy/max(energy)
map("world2Hires")
points(long, lat, col=color, cex=2*norm_e+0.1, pch=19)
points(long[big_ones], lat[big_ones], col="black", cex=2*norm_e[big_ones]+0.1, pch=1)

points(long, lat, bg=color, col=ifelse(big_ones,"black",color), cex=2*norm_e+0.1, pch=21)


cnt <- table((mdy.date(a$mon,a$day,a$yr)-mdy.date(1,1,1900)) %% 28)
sort(pbinom(q = cnt, size = nrow(a),prob = 1/28))
which(pbinom(q = cnt, size = nrow(a),prob = 1/28)<0.05)
sort(p.adjust(pbinom(q = cnt, size = nrow(a),prob = 1/28), "fdr"))

