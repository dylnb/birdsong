r <- rmultinom(18, size=10,
               prob=c(0.85, 0.075, 0.05, 0.0125, 0.00625, .00625))
r <- apply(r, 2, function(x) sort(x, dec=T))
r <- r[,order(r[1,],r[2,],r[3,],r[4,],r[5,],r[6,], decreasing=T)]
co <- makecol(r)[-1]
df <- data.frame(bird=as.factor(rep(1:18, each=10)), song=co)
makecol <- function(r) {
  s <- c(NA)  
  for (i in 1:ncol(r)) {
    colu <- do.call(function(A,B,C,D,E,F) {
                     c(rep("A", A), rep("B",B), rep("C",C),
                       rep("D",D), rep("E",E), rep("F",F))
                     }, as.list(r[,i]))
    s <- c(s, colu)  
  }
  return(s)
}