contrast.condition <- function(A,B,m) {
  col1.A <- paste(A,"AVG_Signal", sep=".")
  col1.B <- paste(B,"AVG_Signal", sep=".")
  col2.A <- paste(A,"AVG_Signal", "1", sep=".")
  col2.B <- paste(B,"AVG_Signal", "1", sep=".")
  col3.A <- paste(A,"AVG_Signal", "2", sep=".")
  col3.B <- paste(B,"AVG_Signal", "2", sep=".")
  
  d1 <- log2(m[,col1.A]) - log2(m[,col1.B])
  d2 <- log2(m[,col2.A]) - log2(m[,col2.B])
  d3 <- log2(m[,col3.A]) - log2(m[,col3.B])

  get.p.value <- function(x,y,z) t.test(c(x,y,z))$p.value
  
  avg   <- mapply(mean, d1, d2, d3)
  p.val <- mapply(get.p.value, d1, d2, d3)
  q.val <- p.adjust(p.val, method = "fdr")

  data.frame(PROBE_ID=m$PROBE_ID, SYMBOL= m$SYMBOL, d1, d2, d3, avg, p.val, q.val)
}

volcano.plot <- function(x, y, lfc=1, thr=0.05) {
  plot(x, -log10(y), pch=16, cex=0.4)
  abline(h=-log10(thr), col="red")
  abline(v=lfc, col="red")
  abline(v=-lfc, col="red")
}