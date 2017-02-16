# Does a bird know when it will song breaks?
# =========================================

library(data.table)

source("bird-prologue.r")

# retrieve clusters over time
allnotes <- allmeans[,c("serial_number", "duration", "cluster")]
allnotes$cluster <- as.character(allnotes$cluster)

ctest <- function(feature) {
  sf <- substitute(feature)
  f <- as.character(sf)
  a_notes <- allmeans[,c("serial_number", f, "cluster")]
  a_notes$cluster <- as.character(allnotes$cluster)
  t_notes <- data.table(a_notes)
  setkey(t_notes, serial_number)
  t_notes <- t_notes[t_notes[, .N, by=serial_number][N > 2],
                     c(f, "cluster"), with=FALSE]
  t_notes  
  # provide two previous notes for trigram context
  t_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                 last=cluster,
                 blast=c("<S>", cluster[1:.N-1])),
          by=serial_number]
  # deleting redundant cluster field (replaced by last)
  t_notes <- t_notes[, !"cluster", with=FALSE]
  
  #### NOTE THAT f CORRESPONDS TO last, NOT syl ###
  
  mature_notes <- data.table(allnotes[(nrow(allnotes) * 0.33):nrow(allnotes),])
  setkey(mature_notes, serial_number)
  mature_notes <- mature_notes[mature_notes[, .N, by=serial_number][N > 2],
                               c("serial_number", f, "cluster"), with=FALSE]
  # provide two previous notes for trigram context
  mature_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                      last=cluster,
                      blast=c("<S>", cluster[1:.N-1])),
               by=serial_number]
  setkey(mature_notes, blast, last, syl)
  
  
  allplot <- ggplot(t_notes, aes(x=sf, color=syl)) +
    geom_density() +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    facet_wrap(~last) +
    ggtitle(paste("all notes over all time:", f, "x next-syllable"))
  
  
  # concentrate on the song (not the alarms)
  # look for differences before or after song breaks
  setkey(t_notes, blast, last, syl)
  t_123 <- t_notes[CJ(c("1", "2", "3"),
                      c("<S>", "</S>", "1", "2", "3"),
                      c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
                   f, nomatch=0, with=FALSE]
  setkey(t_123, last, blast, syl)
  t_123_means <- copy(t_123)
  t_123_means <- unique(t_123_means[, `:=`(count=.N, mean_dur=mean(sf),
                                    by=key(t_123_means)])[, !f, with=FALSE]
  
  # concentrate on the song (not the alarms)
  # look for differences before or after song breaks
  setkey(mature_notes, blast, last, syl)
  m_123 <- mature_notes[CJ(c("1", "2", "3"),
                           c("<S>", "</S>", "1", "2", "3"),
                           c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
                        f, nomatch=0, with=FALSE]
  setkey(m_123, last, blast, syl)
  m_123_means <- copy(m_123)
  m_123_means <- unique(m_123_means[, `:=`(count=.N, mean_dur=mean(sf),
                                    by=key(m_123_means)])[, !f, with=FALSE]
  
  
  # see if the difference b/t, say, 2-3 is different than 2-else
  t123gt_tests <- copy(t_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
                                     twonext = syl==2, threenext = syl==3)]
  t123cf_tests <- copy(t_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
                                     aftertwo = blast==2, afterthree = blast==3)]
  
  m123gt_tests <- copy(m_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
                                     twonext = syl==2, threenext = syl==3)]
  m123cf_tests <- copy(m_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
                                     aftertwo = blast==2, afterthree = blast==3)]
  
  t.test(sf ~ stopnext, data=t123gt_tests["1"])
}
  
  
  ggplot(t_123, aes(x=duration, color=syl)) +
    geom_line(stat="density") +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over all time: duration x next-syllable")
  
  ggplot(t_123, aes(x=duration, color=blast)) +
    geom_line(stat="density") +
    stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over all time: duration x last-syllable")
  
  ggplot(m_123, aes(x=duration, color=syl)) +
    geom_line(stat="density") +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over mature time: duration x next-syllable")
  
  ggplot(m_123, aes(x=duration, color=blast)) +
    geom_line(stat="density") +
    stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over mature time: duration x last-syllable")
  
  mcheck <- m_123[syl==as.integer(last)+1 | syl=="1" & last=="3" | syl=="</S>"]
  ggplot(mcheck, aes(x=duration, color=syl)) +
    geom_line(stat="density") +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over mature time: duration x (stopnext vs songnext)")
  
  
  
  # convert to data.table for faster grouping
  t_skip_123 <- data.table(allnotes)
  # provide forward and backward looking bigram info
  setkey(t_skip_123, serial_number)
  t_skip_123 <- t_skip_123[t_skip_123[, .N, by=serial_number][N > 2],
                           list(duration, cluster)]
  # provide two previous notes for trigram context
  t_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
                    syl=c(cluster[-1], "</S>"),
                    last=cluster,
                    blast=c("<S>", cluster[1:.N-1])),
             by=serial_number]
  # deleting redundand cluster field (replaced by last)
  t_skip_123 <- t_skip_123[, !"cluster", with=FALSE]
  
  #### NOTE THAT duration CORRESPONDS TO last, NOT syl ###
  
  setkey(t_skip_123, last, syl, anext)
  t_skip_123 <- t_skip_123[CJ(c("1", "2", "3"),
                              c("1", "2", "3"),
                              c("</S>", "1", "2", "3"), sorted=FALSE),
                           duration, nomatch=0]
  setkey(t_skip_123, last, syl, anext)
  
  # convert to data.table for faster grouping
  m_skip_123 <- data.table(allnotes[(nrow(allnotes) * 0.33):nrow(allnotes),])
  # provide forward and backward looking bigram info
  setkey(m_skip_123, serial_number)
  m_skip_123 <- m_skip_123[m_skip_123[, .N, by=serial_number][N > 2],
                           list(duration, cluster)]
  # provide two previous notes for trigram context
  m_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
                    syl=c(cluster[-1], "</S>"),
                    last=cluster,
                    blast=c("<S>", cluster[1:.N-1])),
             by=serial_number]
  # deleting redundand cluster field (replaced by last)
  m_skip_123 <- m_skip_123[, !"cluster", with=FALSE]
  
  #### NOTE THAT duration CORRESPONDS TO last, NOT syl ###
  
  setkey(m_skip_123, last, syl, anext)
  m_skip_123 <- m_skip_123[CJ(c("1", "2", "3"),
                              c("1", "2", "3"),
                              c("</S>", "1", "2", "3"), sorted=FALSE),
                           duration, nomatch=0]
  setkey(m_skip_123, last, syl, anext)
  
  tskip123_tests <- copy(t_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
                                            twocoming = anext==2, threecoming = anext==3)]
  
  mskip123_tests <- copy(m_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
                                            twocoming = anext==2, threecoming = anext==3)]
  
  t.test(duration ~ stopcoming, data=tskip123_tests["1"])
  
  ggplot(t_skip_123, aes(x=duration, color=anext)) +
    geom_line(stat="density") +
    stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over all time: duration x postnext-syllable")
    
  
  ggplot(m_skip_123, aes(x=duration, color=anext)) +
    geom_line(stat="density") +
    stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
    geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
              position=position_dodge(width=200), 
              stat="bin", binwidth=10000, origin=-4900+0) +
    facet_wrap(~last) +
    ggtitle("1,2,3 over mature time: duration x postnext-syllable")
}
# 
# 
# # provide forward and backward looking bigram info
# t_notes <- data.table(allnotes)
# setkey(t_notes, serial_number)
# t_notes <- t_notes[t_notes[, .N, by=serial_number][N > 2],
#                    list(duration, cluster)]
# # provide two previous notes for trigram context
# t_notes[, `:=`(syl=c(cluster[-1], "</S>"),
#                last=cluster,
#                blast=c("<S>", cluster[1:.N-1])),
#         by=serial_number]
# # deleting redundand cluster field (replaced by last)
# t_notes <- t_notes[, !"cluster", with=FALSE]
# 
# #### NOTE THAT duration CORRESPONDS TO last, NOT syl ###
# 
# mature_notes <- data.table(allnotes[(nrow(allnotes) * 0.33):nrow(allnotes),])
# setkey(mature_notes, serial_number)
# mature_notes <- mature_notes[mature_notes[, .N, by=serial_number][N > 2],
#                              list(serial_number, duration, cluster)]
# # provide two previous notes for trigram context
# mature_notes[, `:=`(syl=c(cluster[-1], "</S>"),
#                     last=cluster,
#                     blast=c("<S>", cluster[1:.N-1])),
#              by=serial_number]
# setkey(mature_notes, blast, last, syl)
# 
# 
# allplot <- ggplot(t_notes, aes(x=duration, color=syl)) +
#   geom_density() +
#   stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#   facet_wrap(~last) +
#   ggtitle("all notes over all time: duration x next-syllable")
# 
# 
# # concentrate on the song (not the alarms)
# # look for differences before or after song breaks
# setkey(t_notes, blast, last, syl)
# t_123 <- t_notes[CJ(c("1", "2", "3"),
#                     c("<S>", "</S>", "1", "2", "3"),
#                     c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
#                  duration, nomatch=0]
# setkey(t_123, last, blast, syl)
# t_123_means <- copy(t_123)
# t_123_means <- unique(t_123_means[, `:=`(count=.N, mean_dur=mean(duration)),
#                                   by=key(t_123_means)])[, !"duration", with=FALSE]
# 
# # concentrate on the song (not the alarms)
# # look for differences before or after song breaks
# setkey(mature_notes, blast, last, syl)
# m_123 <- mature_notes[CJ(c("1", "2", "3"),
#                          c("<S>", "</S>", "1", "2", "3"),
#                          c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
#                       duration, nomatch=0]
# setkey(m_123, last, blast, syl)
# m_123_means <- copy(m_123)
# m_123_means <- unique(m_123_means[, `:=`(count=.N, mean_dur=mean(duration)),
#                                   by=key(m_123_means)])[, !"duration", with=FALSE]
# 
# 
# # see if the difference b/t, say, 2-3 is different than 2-else
# t123gt_tests <- copy(t_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
#                                    twonext = syl==2, threenext = syl==3)]
# t123cf_tests <- copy(t_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
#                                    aftertwo = blast==2, afterthree = blast==3)]
# 
# m123gt_tests <- copy(m_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
#                                    twonext = syl==2, threenext = syl==3)]
# m123cf_tests <- copy(m_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
#                                    aftertwo = blast==2, afterthree = blast==3)]
# 
# t.test(duration ~ stopnext, data=t123gt_tests["1"])
# 
# 
# ggplot(t_123, aes(x=duration, color=syl)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over all time: duration x next-syllable")
# 
# ggplot(t_123, aes(x=duration, color=blast)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over all time: duration x last-syllable")
# 
# ggplot(m_123, aes(x=duration, color=syl)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over mature time: duration x next-syllable")
# 
# ggplot(m_123, aes(x=duration, color=blast)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over mature time: duration x last-syllable")
# 
# mcheck <- m_123[syl==as.integer(last)+1 | syl=="1" & last=="3" | syl=="</S>"]
# ggplot(mcheck, aes(x=duration, color=syl)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over mature time: duration x (stopnext vs songnext)")
# 
# 
# 
# # convert to data.table for faster grouping
# t_skip_123 <- data.table(allnotes)
# # provide forward and backward looking bigram info
# setkey(t_skip_123, serial_number)
# t_skip_123 <- t_skip_123[t_skip_123[, .N, by=serial_number][N > 2],
#                          list(duration, cluster)]
# # provide two previous notes for trigram context
# t_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
#                   syl=c(cluster[-1], "</S>"),
#                   last=cluster,
#                   blast=c("<S>", cluster[1:.N-1])),
#            by=serial_number]
# # deleting redundand cluster field (replaced by last)
# t_skip_123 <- t_skip_123[, !"cluster", with=FALSE]
# 
# #### NOTE THAT duration CORRESPONDS TO last, NOT syl ###
# 
# setkey(t_skip_123, last, syl, anext)
# t_skip_123 <- t_skip_123[CJ(c("1", "2", "3"),
#                             c("1", "2", "3"),
#                             c("</S>", "1", "2", "3"), sorted=FALSE),
#                          duration, nomatch=0]
# setkey(t_skip_123, last, syl, anext)
# 
# # convert to data.table for faster grouping
# m_skip_123 <- data.table(allnotes[(nrow(allnotes) * 0.33):nrow(allnotes),])
# # provide forward and backward looking bigram info
# setkey(m_skip_123, serial_number)
# m_skip_123 <- m_skip_123[m_skip_123[, .N, by=serial_number][N > 2],
#                          list(duration, cluster)]
# # provide two previous notes for trigram context
# m_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
#                   syl=c(cluster[-1], "</S>"),
#                   last=cluster,
#                   blast=c("<S>", cluster[1:.N-1])),
#            by=serial_number]
# # deleting redundand cluster field (replaced by last)
# m_skip_123 <- m_skip_123[, !"cluster", with=FALSE]
# 
# #### NOTE THAT duration CORRESPONDS TO last, NOT syl ###
# 
# setkey(m_skip_123, last, syl, anext)
# m_skip_123 <- m_skip_123[CJ(c("1", "2", "3"),
#                             c("1", "2", "3"),
#                             c("</S>", "1", "2", "3"), sorted=FALSE),
#                          duration, nomatch=0]
# setkey(m_skip_123, last, syl, anext)
# 
# tskip123_tests <- copy(t_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
#                                           twocoming = anext==2, threecoming = anext==3)]
# 
# mskip123_tests <- copy(m_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
#                                           twocoming = anext==2, threecoming = anext==3)]
# 
# t.test(duration ~ stopcoming, data=tskip123_tests["1"])
# 
# ggplot(t_skip_123, aes(x=duration, color=anext)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over all time: duration x postnext-syllable")
#   
# 
# ggplot(m_skip_123, aes(x=duration, color=anext)) +
#   geom_line(stat="density") +
#   stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
#   coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#   geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
#             position=position_dodge(width=200), 
#             stat="bin", binwidth=10000, origin=-4900+0) +
#   facet_wrap(~last) +
#   ggtitle("1,2,3 over mature time: duration x postnext-syllable")