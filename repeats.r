# Are repeat transition Markovian?
# ================================

library(data.table) # makes aggregating faster

source("bird-prologue.r")

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "cluster")]
allnotes$cluster <- as.character(allnotes$cluster)

t_notes <- data.table(allnotes)
setkey(t_notes, serial_number)
t_notes <- t_notes[t_notes[, .N, by=serial_number][N > 2], cluster]
# provide two previous notes for trigram context
t_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                     last=cluster,
                     blast=c("<S>", cluster[1:.N-1])),
        by=serial_number]
setkey(t_notes, blast, last, syl)
# aggregate counts over trigrams
tncounts <- copy(t_notes)[, tricount := .N,
                          by=c("blast","last","syl")]
tncounts <- unique(tncounts)[, !c("cluster","serial_number"), with=FALSE]
tncounts <- tncounts[CJ(c("<S>","0","1","2","3","5","6","7","9"),
                        c("0","1","2","3","5","6","7","9"),
                        c("</S>","0","1","2","3","5","6","7","9"), sorted=FALSE)]
setkey(tncounts, blast, last, syl)
for (i in names(tncounts))
  tncounts[is.na(get(i)),i:=0,with=FALSE]
# normalize the probabilities in a few different ways
# -- max(1,sum(tricount)) is a hack to avoid dividing by 0 for unattested bigrams
# p(w_{i+1},w_{i},w_{i-1})
tncounts[, allprob := round(tricount/max(1,sum(tricount)), 2)]
# p(w_{i+1} | w_{i},w_{i-1})
tncounts[, bgprob := round(tricount/max(1,sum(tricount)), 2), by=c("blast", "last")]
# p(w_{i},w_{i-1} | w_{i+1})
tncounts[, inprob := round(tricount/max(1,sum(tricount)), 2), by=syl]


# use just the last 2/3 of data
# -----------------------------
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






# split clusters into songs, individuated by serial_number,
# and add Start ("<S>") and Stop ("</S>") tags
octograms_by_song <- unname(dlply(allnotes, .(serial_number),
                                  function(df) embed(c("<S>", as.vector(df$cluster), "</S>"), 8)))

# start 1/3 of the way through the bird's life
# (note that cluster 4 has come and gone by this point)
mature_songs <- songs[(length(songs) * 0.33):length(songs)]

# bind all songs into single stream to get bigram stats
notes_seq <- do.call(c, songs)
eightgrams <- data.table(embed(notes_seq,8))
just2s <- 8grams[is]
