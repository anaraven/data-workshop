data <- textConnection(centennial_Y2K$V1)
fmt <- c("A6", "A1", "A5", "I4", "2I3", "1X", "2I3", "F6.0", "1X", "2F8.0", "F6.0",
         "2I4", rep(c("F4.0", "1X", "A2", "1X", "A5"),12))
a <- read.fortran(data, fmt)
n <- unlist(lapply(1:12,function(k) {
  c(paste("mag",k,sep="."), paste("msc",k,sep="."), paste("mdo",k,sep="."))
  }))
n <- c("icat", "asol", "isol", "yr", "mon", "day", "hr", "min", "sec", "glat", "glon", "dep", "greg", "ntel", n)
colnames(a) <- n

plot(a[,c("glon","glat")],cex=(a$mag.1-5)/4)
energy <- exp(3*log(10)*(a$mag.1 + 2.9)/2)
plot(sort(energy)/sum(energy))
a[energy>sum(energy)*0.3,4:15]
plot(a[,c("glon","glat")],cex=energy*9/sum(energy))
