m <- read.csv("microarray.csv")

lfc1 <- log2(m$X32GyHeLa.AVG_Signal)   - log2(m$KontrolHeLa.AVG_Signal)
lfc2 <- log2(m$X32GyHeLa.AVG_Signal.1) - log2(m$KontrolHeLa.AVG_Signal.1)
lfc3 <- log2(m$X32GyHeLa.AVG_Signal.2) - log2(m$KontrolHeLa.AVG_Signal.2)

avg <- rep(NA, length(lfc1))
for(i in 1:length(lfc1)) {
  avg[i] <- mean(lfc1[i], lfc2[i], lfc3[i])
}

get.p.value <- function(x,y,z) {
  return( t.test(c(x,y,z), alternative = "two.sided")$p.value )
}

p.val <- rep(NA, length(lfc1))
for(i in 1:length(lfc1)) {
  p.val[i] <- get.p.value(lfc1[i], lfc2[i], lfc3[i])
}

avg <- mapply(mean, lfc1, lfc2, lfc3)
p.val <- mapply(get.p.value, lfc1, lfc2, lfc3)
q.val <- p.adjust(p.val, method = "fdr")

plot(avg, -log10(p.val), pch=".")
plot(avg, -log10(p.val), pch=16, cex=0.4)

r <- data.frame(PROBE_ID=m$PROBE_ID, SYMBOL= m$SYMBOL, lfc1, lfc2, lfc3, avg, p.val, q.val)
