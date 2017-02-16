# Basic Song Statistics
# =====================

library(data.table)
library(ggplot2)

source("bird-prologue.r")

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "start_on", "cluster")]
allnotes$cluster <- as.character(allnotes$cluster)
# split clusters into songs, individuated by serial_number,
# and add Start ("<S>") and Stop ("</S>") tags
songs <- unname(dlply(allnotes, .(serial_number),
                      function(df) c("<S>", as.vector(df$cluster), "</S>")))

# check whether song-length varies over time (it probably doesn't)
lengths <- data.frame(cbind(1:length(songs), do.call(rbind, lapply(songs, length))))
colnames(lengths) <- c("song", "length")
lp <- ggplot(lengths, aes(song, length)) + stat_bin2d()

# length between song notes, song/call notes, and call notes
bgtype <- function(c, nc) {
  if (c == nc) {
    if (c %in% c("1","2","3")) return("song.repeat")
    else return("call.repeat")
  }  
  else if (c=="1" & nc=="2") return("1.2")
  else if (c=="2" & nc=="3") return("2.3")
  else if (c=="3" & nc=="1") return("3.1")
  else if (c=="3" & nc=="</S>") return("3.S")
  else if (!(nc %in% c("1","2","3","</S>"))) {
    if (c %in% c("1","2","3")) return("song.call")
    else return("call.call")
  }
  else if (c %in% c("1","2","3")) return("wrong.song")
  else if (nc=="</S>") return("call.S")
  else return("call.song")
}
vbgtype <- Vectorize(bgtype)
notes <- data.table(allmeans[,c("serial_number", "start_on", "duration", "cluster")])
notes$cluster <- as.character(notes$cluster)
notes[, `:=`(end = start_on + duration,
             next_start_on = c(start_on[-1],NA),
             nextcluster = c(cluster[-1],"</S>")), by=serial_number]

delays <- notes[start_on > 0, list(serial_number,
                                   delay = next_start_on - end,
                                   bg = paste(cluster,nextcluster,sep="."),
                                   bgtype = vbgtype(cluster,nextcluster),
                                   epoch = as.factor(cut(1:.N, 3, labels=FALSE)))]
delays <- delays[complete.cases(notes)]
ggplot(delays, aes(x=delay, color=epoch)) +
  geom_line(stat="density") +
  scale_x_continuous(limits=c(0,2000)) +
  coord_cartesian(xlim=c(0,300)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count..,color=epoch),
#             position=position_dodge(width=50), 
#             stat="bin", binwidth=10000, origin=3500+0) +
  facet_wrap(~bgtype)
summary(delays)

# calculate average songs per bout at different stages of learning
perbouts <- notes[, list(serial_number,
                         cluster,
                         nextcluster,
                         bg = paste(cluster,nextcluster,sep="."),
                         bgtype = vbgtype(cluster,nextcluster))]
perbouts[, song_num := .GRP, by=serial_number]
songsperbout <- perbouts[bgtype=="call.song", .N, by=song_num]
songsperbout <- merge(songsperbout, data.table(song_num = 1:75473),
                      by="song_num", all=TRUE)
songsperbout[is.na(songsperbout)] <- 0
spb <- songsperbout[, list(song_num, N, songbin=cut(song_num, 150, labels=FALSE),
                           epoch=as.factor(cut(song_num, 5, labels=FALSE)))]
spbsum <- spb[, list(songbin, songmean=mean(N)), by=songbin]
ggplot(spbsum, aes(x=songbin, y=songmean)) + geom_line()
ggplot(spb, aes(x=epoch, y=N)) + geom_violin()

notes2 <- data.table(allmeans[,c("serial_number", "start_on", "duration", "cluster")])
notes2 <- notes2[, list(N=.N, start_on, duration, cluster), by=serial_number]
notes2 <- subset(notes2, N > 2 & start_on > 0, c(serial_number, start_on, duration, cluster))
notes2[, `:=`(song = .GRP,
              songpos = c(1:(.N-1), -1)), by=serial_number]

# get row indices of final bout notes
finals <- cumsum(rle(notes2$serial_number)$lengths)
# bind these rows with their following rows (the initial bout notes of the next bout)
transitions <- notes2[head(as.vector(rbind(finals, finals+1)), -1)]
# extract times of initial notes of following bouts
# 86400000 is the number of ms in a day
starttimes <- transitions[songpos > 0, list(start=serial_number + start_on/86400000, song)]
# extract times of final notes of preceding bouts
finaltimes <- transitions[songpos < 0, list(end=serial_number + (start_on + duration)/86400000, song)]
transitiontimes <- merge(starttimes, finaltimes, by="song")
transitiontimes[, nextstart := c(tail(start, -1),NA)]
# pause times between bouts in SECONDS
transitiontimes[, followingpause := (nextstart - end)*86400]
ggplot(transitiontimes, aes(x=followingpause)) +
  geom_density() + scale_x_continuous(limits=c(1,200))
summary(transitiontimes$followingpause)
