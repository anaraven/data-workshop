m <- read.csv("micro array Raw data 2015 son.csv")

d1 <- m$X32GyHeLa.AVG_Signal   - m$KontrolHeLa.AVG_Signal
d2 <- m$X32GyHeLa.AVG_Signal.1 - m$KontrolHeLa.AVG_Signal.1
d3 <- m$X32GyHeLa.AVG_Signal.2 - m$KontrolHeLa.AVG_Signal.2

r <- data.frame(PROBE_ID=m$PROBE_ID, SYMBOL= m$SYMBOL, d1, d2, d3)


get.p.value <- function(i, r) {
  return( t.test(r[i, 3:5])$p.value )
}

p <- rep(NA, nrow(r))
for(i in 1:nrow(r)){ p[i] <- get.p.value(i, r)}
plot(p)
r$p.val <- p

avg <- rep(NA, nrow(r))
for(i in 1:nrow(r)){ avg[i] <- mean(r[i, 3:5])}

r$avg <- avg
