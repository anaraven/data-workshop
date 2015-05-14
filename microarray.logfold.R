m <- read.csv("microarray.csv")

lfc.1 <- log2(m$T1.AVG_Signal)   - log2(m$C1.AVG_Signal)
lfc.2 <- log2(m$T1.AVG_Signal.1) - log2(m$C1.AVG_Signal.1)
lfc.3 <- log2(m$T1.AVG_Signal.2) - log2(m$C1.AVG_Signal.2)

m.1 <-  log2(m$C2.AVG_Signal.1) - log2(m$C2.AVG_Signal.2)
a.1 <- (log2(m$C2.AVG_Signal.1) + log2(m$C2.AVG_Signal.2))/2

avg.1 <- (log2(m$T1.AVG_Signal)   + log2(m$C1.AVG_Signal))/2
avg.2 <- (log2(m$T1.AVG_Signal.1) + log2(m$C1.AVG_Signal.1))/2
avg.3 <- (log2(m$T1.AVG_Signal.2) + log2(m$C1.AVG_Signal.2))/2


# lfc <- rep(NA, length(lfc.1))
# for(i in 1:length(lfc.1)) {
  # lfc[i] <- mean(lfc.1[i], lfc.2[i], lfc.3[i])
# }

plot(m$C2.AVG_Signal,m$C2.AVG_Signal.1,pch=".", log="xy")
abline(0,1,col="red")

lfc <- mapply(mean, lfc.1, lfc.2, lfc.3)
avg <- mapply(mean, avg.1, avg.2, avg.3)

get.p.value <- function(x,y,z) {
  return( t.test(c(x,y,z), alternative = "two.sided")$p.value )

p.val <- rep(NA, length(lfc1))
for(i in 1:length(lfc1)) {
  p.val[i] <- get.p.value(lfc1[i], lfc2[i], lfc3[i])
}

p.val <- mapply(get.p.value, lfc.1, lfc.2, lfc.3)
q.val <- p.adjust(p.val, method = "fdr")

plot(p.val, q.val, pch=16, cex=0.4); abline(0,1,col="red")

## Adjusted Volcano Plot

plot(lfc, -log10(q.val), pch=16, cex=0.4)
abline(h=-log10(0.05), col="red")
abline(v=1, col="red")
abline(v=-1, col="red")

# r <- data.frame(PROBE_ID=m$PROBE_ID, SYMBOL= m$SYMBOL, lfc1, lfc2, lfc3, avg, p.val, q.val)

