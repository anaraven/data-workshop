fw <- read.delim("HNAYA2S401R-Alignment.txt", header=FALSE, comment.char = "#")
rv <- read.delim("HNB75UBE016-Alignment.txt", header=FALSE, comment.char = "#")
colnames(fw) <- c("q.id", "s.id", "identity", "length", "mismatches", "gap", "q.start", "q.end", "s.start", "s.end", "evalue", "score")
colnames(rv) <- c("q.id", "s.id", "identity", "length", "mismatches", "gap", "q.start", "q.end", "s.start", "s.end", "evalue", "score")

amplicon_in_chromosome <- function(chrom) {
  ans <- NULL
  for(forward in fw[fw$s.id==chrom & fw$s.end>fw$s.start, "s.start"]) {
    for(reverse in rv[rv$s.id==chrom & rv$s.end<rv$s.start, "s.start"]) {
      if(reverse - forward>50 & reverse-forward<2000) {
        ans <- paste(forward,reverse)
      }
    }
  }
  return(ans)
}

for(chrom in levels(fw$s.id)) {
  result <- amplicon_in_chromosome(chrom)
  if(is.null(result)) {
#    print(paste("nothing found in",chrom))
  } else {
    print(paste(result, chrom))
  }
}
