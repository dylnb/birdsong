# Markov model of song after all clusters learned
# ===============================================

library(qgraph)     # provides functions for markov diagramming
library(animation)
library(miscTools)  # provides insertRow
library(data.table) # makes aggregating faster
library(pheatmap)

source("bird-prologue.r")

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "cluster")]
# split clusters into songs, individuated by serial_number,
# and add Start ("<S>") and Stop ("</S>") tags
songs <- unname(dlply(allnotes, .(serial_number),
                      function(df) c("<S>", as.vector(df$cluster), "</S>")))


# start 1/3 of the way through the bird's life
# (note that cluster 4 has come and gone by this point)
mature_songs <- songs[(length(songs) * 0.33):length(songs)]

# bind all songs into single stream to get bigram stats
notes_seq <- do.call(c, mature_songs)
# break stream into bigrams
bigrams <- embed(notes_seq, 2)
# ignore Stop->Start transitions, as these were created artificially
bigrams_df <- data.frame(bigrams[bigrams[ ,1] != "<S>", ])
colnames(bigrams_df) <- c("to","from")
counts <- data.table(dcast(bigrams_df, from + to ~ ., length, value.var="from"))
setnames(counts, "V3", "count")
#normed_by_from <- ddply(counts, .(from), transform, percent=count/sum(count))
normed_by_from <- counts[, list(to, count, percent=count/sum(count)), by=from]
#normed_by_to <- ddply(counts, .(to), transform, percent=count/sum(count))
normed_by_to <- counts[, list(from, count, percent=count/sum(count)), by=to]
cluster_counts <- counts[, list(cluster=from, count=sum(count)), by=from]
# hacky way of getting Stop counts in there in the right order
cluster_counts <- rbind(cluster_counts[1,list(cluster, count)],
                        data.table(cluster="</S>", count=cluster_counts[[1,3]]),
                        cluster_counts[-1,list(cluster, count)])

# use bigram counts to view relative frequency of transitions out of
# each cluster (scale="row"), or into each cluster (scale="column"), or
# overall (scale="none")
z <- data.matrix(dcast(normed_by_from, from ~ to, value.var="count")[,-1])
row.names(z) <- c("<S>", colnames(z)[-1])
z[is.na(z)] <- 0
zheat <- pheatmap(z, scale="row", cluster_rows=FALSE, cluster_cols=FALSE,
                  display_numbers=TRUE, fontsize_row=12, fontsize_col=12, fontsize_number=12)

makegraph <- function(table) {
  graph <- data.matrix(dcast(table, from ~ to, value.var="percent")[,-1])
  graph <- insertCol(insertRow(graph, 2, 0), 1, 0)
  colnames(graph)[1] <- "<S>"
  rownames(graph) <- colnames(graph)
  graph[is.na(graph)] <- 0
  graph <- round(graph, 2)
  return(graph)
}

make3graph <- function(table) {
  graph <- data.matrix(dcast(table, from ~ to, value.var="percent")[,-1])
  graph <- insertCol(insertRow(graph, 2, 0), 1, 0)
  graph <- insertCol(insertRow(graph, 6, 0), 6, 0)
  colnames(graph)[1] <- "<S>"
  colnames(graph)[6] <- "3"
  rownames(graph) <- colnames(graph)
  graph[is.na(graph)] <- 0
  graph <- round(graph, 2)
  return(graph)
}

make4graph <- function(table) {
  graph <- data.matrix(dcast(table, from ~ to, value.var="percent")[,-1])
  graph <- insertCol(insertRow(graph, 2, 0), 1, 0)
  graph <- insertCol(insertRow(graph, 7, 0), 7, 0)
  colnames(graph)[1] <- "<S>"
  colnames(graph)[7] <- "4"
  rownames(graph) <- colnames(graph)
  graph[is.na(graph)] <- 0
  graph <- round(graph, 2)
  return(graph)
}

outgraph <- makegraph(normed_by_from)
ingraph <- makegraph(normed_by_to)

# plot the markov model, with arrows normalized by outgoing probabilities
qout <- qgraph(outgraph, edge.labels=TRUE, edge.label.cex=0.5,
               labels=cluster_counts$cluster, label.norm="OOOO",
               vsize=cluster_counts$count/100000*4)
title("Mature song, weight by 1st order outgoing probabilities")
# plot the same markov model, with arrows normalized by incoming probabilities
qin <- qgraph(ingraph, edge.labels=TRUE, edge.label.cex=0.5,
              labels=cluster_counts$cluster, label.norm="OOOO",
              vsize=cluster_counts$count/100000*4)
title("Mature song, weight by 1st order incoming probabilities")

allfroms <- c("<S>","0","1","2","3","4","5","6","7","9")
alltos <- c("0","1","2","3","4","5","6","7","9","</S>")
mats <- vector("list", 10)
temp <- matrix(NA, 11,11)
for (i in 0:9) {
  song_sec <- songs[(length(songs) * i/10):(length(songs) * (i+1)/10)]
  # bind all songs into single stream to get bigram stats
  notes_seq <- do.call(c, song_sec)
  # break stream into bigrams
  bigrams <- embed(notes_seq, 2)
  # ignore Stop->Start transitions, as these were created artificially
  bigrams_df <- data.frame(bigrams[bigrams[ ,1] != "<S>", ])
  colnames(bigrams_df) <- c("to","from")
  counts <- data.table(dcast(bigrams_df, from + to ~ ., length, value.var="from"))
  setnames(counts, "V3", "count")
  #normed_by_from <- ddply(counts, .(from), transform, percent=count/sum(count))
  nbf <- counts[, list(to, count, percent=count/sum(count)), by=from]
  nbt <- counts[, list(from, count, percent=count/sum(count)), by=to]
  ccs <- counts[, list(cluster=from, count=sum(count)), by=from]
  # hacky way of getting Stop counts in there in the right order
  ccs <- rbind(ccs[1,list(cluster, count)],
                   data.table(cluster="</S>", count=ccs[[1,3]]),
                   ccs[-1,list(cluster, count)])
  #missingfs <- allfroms[!(allfroms %in% unique(nbf[,from]))]
  #missingts <- alltos[!(alltos %in% unique(nbf[,to]))]
  #cat(i, "missingfs:", missingfs, "; missingts:", missingts, "  ")
#   setkey(nbf, from, to)
#   complete_nbf <- nbf[CJ(allfroms, alltos, sorted=FALSE)]
#   if (i < 0.2) {
#     for (j in c("from","to")) {complete_nbf[is.na(get(j)),j:="3",with=FALSE]}
#   } else if (i > 0.3) {
#     for (j in c("from","to")) {complete_nbf[is.na(get(j)),j:="4",with=FALSE]}
#   }
#   for (j in c("count","percent")) {complete_nbf[is.na(get(j)),j:=0,with=FALSE]}
  if (i < 2) {
    mat1 <- make3graph(nbf)
    mat2 <- make3graph(nbt)
    ccs <- rbind(ccs[1:5], data.table(cluster="3",count=1), ccs[-(1:5)])
  }
  else if (i == 2 | i == 3) {
    mat1 <- makegraph(nbf)
    mat2 <- makegraph(nbt)
  }
  else {
    mat1 <- make4graph(nbf)
    mat2 <- make4graph(nbt)
    ccs <- rbind(ccs[1:6], data.table(cluster="4",count=1), ccs[-(1:6)])
  }
#   print(ccs)
#   print(mat)
  mats[[i+1]] <- list(mat1, ccs, mat2)
}

saveHTML({
  par(mfrow=c(1,2))
  ani.options(interval = 2)
  lapply(mats,
         function(m) {
           q_out <- qgraph(m[[1]], edge.labels=TRUE, edge.label.cex=0.5,
                           labels=m[[2]]$cluster, label.norm="OOOO",
                           vsize=m[[2]]$count/12000*4, vTrans=50,
                           layout="circle", diag="col", DoNotPlot=T)
           q_in <- qgraph(m[[3]], edge.labels=TRUE, edge.label.cex=0.5,
                          labels=m[[2]]$cluster, label.norm="OOOO",
                          vsize=m[[2]]$count/12000*4, vTrans=50,
                          layout="circle", diag="col", DoNotPlot=T)
           plot(q_out, main="Out")
           plot(q_in, main="In")
         })
  }, ani.height = 650, ani.width = 1400,
     outdir=paste0(getwd(),"/ms-tests"),
     htmlfile="ms-tests.html")