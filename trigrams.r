# Markov model of song after all clusters learned
# ===============================================

library(qgraph)     # provides functions for markov diagramming
library(miscTools)  # provides insertRow
library(data.table) # makes aggregating faster
library(pheatmap)

source("bird-prologue.r")

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "cluster")]
allnotes$cluster <- as.character(allnotes$cluster)

# use last 2/3 of data, and convert to data.table for faster grouping
mature_notes <- data.table(allnotes[(nrow(allnotes) * 0.33):nrow(allnotes),])
setkey(mature_notes, serial_number)
mature_notes <- mature_notes[mature_notes[, .N, by=serial_number][N > 2], cluster]
# provide two previous notes for trigram context
mature_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                    last=cluster,
                    blast=c("<S>", cluster[1:.N-1])),
             by=serial_number]
setkey(mature_notes, blast, last, syl)
# aggregate counts over triagrams
mncounts <- copy(mature_notes)[, tricount := .N,
                               by=c("blast","last","syl")]
mncounts <- unique(mncounts)[, !c("cluster","serial_number"), with=FALSE]
mncounts <- mncounts[CJ(c("<S>","0","1","2","3","5","6","7","9"),
                        c("0","1","2","3","5","6","7","9"),
                        c("</S>","0","1","2","3","5","6","7","9"), sorted=FALSE)]
setkey(mncounts, blast, last, syl)
for (i in names(mncounts))
  mncounts[is.na(get(i)),i:=0,with=FALSE]
# normalize the probabilities in a few different ways
# -- max(1,sum(tricount)) is a hack to avoid dividing by 0 for unattested bigrams
# p(w_{i+1},w_{i},w_{i-1})
mncounts[, allprob := round(tricount/max(1,sum(tricount)), 2)]
# p(w_{i+1} | w_{i},w_{i-1})
mncounts[, bgprob := round(tricount/max(1,sum(tricount)), 2), by=c("blast", "last")]
# p(w_{i},w_{i-1} | w_{i+1})
mncounts[, inprob := round(tricount/max(1,sum(tricount)), 2), by=syl]

# bgprob edges in factor loadings layout
el <- mncounts[, list(bigram = paste(blast,last,sep=";"), syl, bgprob)]
elf <- el[, as.list(bgprob), by=bigram]
elm <- as.matrix(elf[,!"bigram", with=FALSE])
dimnames(elm) <- list(elf[,bigram], unique(el[,syl]))
fs <- as.numeric(1:nrow(elm))
elmgroups <- split(fs, ceiling(fs/length(unique(mncounts[,last]))))
names(elmgroups) <- unique(mncounts[,syl])
loadingsbybg <- qgraph.loadings(elm, groups=elmgroups,
                                layout="circle", vsize=c(1,5))

# trigrams displayed in clustered "circular" layouts
# --------------------------------------------------
makemat <- function(tab, tabf) {
  t <- copy(tab)
  t[, `:=`(bigram = rep(1:72, each=9), syl = rep(73:81, 72))]
  elm <- as.matrix(t)
  x <- matrix(1:72, ncol=9)
  xgs <- c(split(x, rep(1:ncol(x), each=nrow(x))), list(`10`=73:81))
  names(xgs) <- c(unique(mncounts[,blast]), "Next")
  return(list(mat=elm, fs=xgs))
}

# edges determined by bgprob 
bgel <- mncounts[, list(bigram = paste(blast,last,sep=";"), syl, bgprob)]
bgelf <- bgel[, as.list(bgprob), by=bigram]
bgmat <- makemat(bgel, bgelf)
bybg <- qgraph(bgmat$mat, groups=bgmat$fs, layout="circular", vsize=c(3,3),
               edgelist=TRUE, cut=0.9, min=0.2, legend=FALSE,
               asize=0.5, rotation=c(rep(0, 9), pi/4),
               vTrans=200, GLratio=4, labels=c(bgelf$bigram[-(65:72)],
                                               paste(c("S"),unique(mncounts[,last]),sep=";"),
                                               unique(mncounts[,syl])))
title("Mature song, weight by 2nd order outgoing probabilities")

# edges determined by inprob
inel <- mncounts[, list(bigram = paste(blast,last,sep=";"), syl, inprob)]
inelf <- inel[, as.list(inprob), by=bigram]
inmat <- makemat(inel, inelf)
byin <- qgraph(inmat$mat, groups=inmat$fs, layout="circular", vsize=c(3,3),
               edgelist=TRUE, cut=0.4, min=0.1, legend=FALSE,
               asize=0.5, rotation=c(rep(0, 9), pi/4),
               vTrans=200, GLratio=4, labels=c(inelf$bigram[-(65:72)],
                                               paste(c("S"),unique(mncounts[,last]),sep=";"),
                                               unique(mncounts[,syl])))
title("Mature song, weight by 2nd order incoming probabilities")

# edges determined by allprob
allel <- mncounts[, list(bigram = paste(blast,last,sep=";"), syl, allprob)]
allelf <- allel[, as.list(allprob), by=bigram]
allmat <- makemat(allel, allelf)
byall <- qgraph(allmat$mat, groups=allmat$fs, layout="circular", vsize=c(3,3),
               edgelist=TRUE, cut=0.4, min=0.1, legend=FALSE,
               asize=0.5, rotation=c(rep(0, 9), pi/4),
               vTrans=200, GLratio=4, labels=c(allelf$bigram[-(65:72)],
                                               paste(c("S"),unique(mncounts[,last]),sep=";"),
                                               unique(mncounts[,syl])))
title("Mature song, weight by 2nd order overall probability")