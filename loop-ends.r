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

countloops <- function(song) {
  songnotes <- c("1","2","3")
  df <- data.frame(starts=rep(0,3), stops=rep(0,3))
  state <- 0
  for (i in 1:length(song)) {
    if (state == 0) { # not yet in a song
      if (! song[i] %in% songnotes) next # still not in a song
      else {
        df[as.integer(song[i]),1] <- df[as.integer(song[i]),1] + 1
        state <- 1
      }  
    } else if (state == 1) { # in a song
      if (song[i] %in% songnotes) next # still in a song
      else {
        df[as.integer(song[i-1]),2] <- df[as.integer(song[i-1]),2] + 1
        state <- 0
      }
    }
  }
  return(df)
}

song_bins <- split(songs, cut(seq_along(songs), 10, labels=FALSE))
sscounts_bins <- lapply(song_bins, function(x) lapply(x, countloops))
sscounts_bins <- lapply(sscounts_bins,
                        function(x) Reduce("+", x, data.frame(starts=c(0,0,0),stops=0)))
ssarray <- as.array(sscounts_bins)
ssdf <- cbind(cluster=rep(c("1","2","3"),10), adply(ssarray,1))
colnames(ssdf)[2] <- "bin"
ssdfm <- melt(ssdf, id=c("cluster","bin"), variable.name="ss", value.name="count")
ggplot(ssdfm, aes(x=bin,y=count,color=cluster,group=cluster)) + geom_line() + facet_wrap(~ss)

# bind all songs into single stream to get bigram stats
notes_seq <- do.call(c, mature_songs)
# break stream into bigrams
octograms <- data.table(embed(notes_seq, 8))
setnames(octograms, c("eigth","seventh","sixth","fifth","fourth","third","second","first"))
octograms[!(first %in% c("1","2","3")]
bigrams_df <- data.frame(bigrams[bigrams[ ,1] != "<S>", ])
colnames(bigrams_df) <- c("to","from")
counts <- data.table(dcast(bigrams_df, from + to ~ ., length, value.var="from"))