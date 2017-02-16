library(data.table) # makes aggregating faster

source("bird-prologue.r")

numbins <- 60

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "cluster")]
# split clusters into songs, individuated by serial_number,
# and add Start ("<S>") and Stop ("</S>") tags
songs <- unname(dlply(allnotes, .(serial_number),
                      function(df) c("<S>", as.vector(df$cluster), "</S>")))
notes_seq <- do.call(c, songs)
bigrams <- embed(notes_seq, 2)
bigrams_df <- data.frame(bigrams[bigrams[ ,1] != "<S>", ])
colnames(bigrams_df) <- c("to","from")
bigrams_df$bin <- cut(1:nrow(bigrams_df), numbins, labels=FALSE)
bigrams_df <- bigrams_df[,c(2,1,3)]
tprobs <- dcast(bigrams_df, from + to ~ bin, length)
tprobs_df <- data.frame(prop.table(as.matrix(tprobs[-(1:2)]), margin=2))
tprobs_df$from <- tprobs$from
tprobs_df$to <- tprobs$to
colnames(tprobs_df)[1:numbins] <- 1:numbins
tprobs_dt <- data.table(tprobs_df, key=c("from","to"))
tprobs_dt[, bigram := paste(from, to, sep=".")]
tprobs_dt <- tprobs_dt[,!c("from","to"), with=FALSE]
tprobs_dt <- data.table(melt(tprobs_dt, id.var="bigram", variable.name="bin"), key="bigram")
ggplot(tprobs_dt[c("1.2","2.1")], aes(x=as.numeric(bin),y=value,color=bigram,group=bigram)) +
  geom_line() + xlim(2,10)