m <- read.csv("micro array Raw data 2015 son.csv")
mix <- sample.int(nrow(m))
m[,3:26] <- m[mix, 3:26]
n <- colnames(m)
n <- sub("X32GyHeLa",  "T1", n)
n <- sub("X16GyC4I",   "T2", n)
n <- sub("KontrolHeLa","C1", n)
n <- sub("KontrolC4I", "C2", n)
colnames(m) <- n
write.csv(m, "microarray.csv")
