# Does a bird know what's come before / coming next?
# ==================================================

library(data.table)

source("bird-prologue.r")

plotter <- function(df, f, c, w="last",
                    xlimits=c(0,300), ylimits=c(0,8e-2),
                    textheight=6e-2, dodge=200, origin=-4900,
                    title="") {
  ggplot(df, aes_string(x=f, color=c)) +
    geom_line(stat="density") +
    stat_vline(aes_string(color=c), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=xlimits, ylim=ylimits) +
    geom_text(y=textheight, aes_string(label="..count..", ymax="..count..", color=c),
              position=position_dodge(width=dodge), 
              stat="bin", binwidth=10000, origin=origin) +
    facet_wrap(as.formula(paste("~", w))) +
    ggtitle(title)
}

ctest <- function(feature, xs=c(0,300), ys=c(0,8e-2),
                  th=6e-2, dodge=200, origin=-4900,
                  quit=-2,save=FALSE) {
  f <- as.character(feature)
  if (save) pdf(file=paste0("env-tests/env-",f,".pdf"),
                pointsize=8, width=12, height=6)
  a_notes <- allmeans[,c("serial_number", f, "cluster")]
  a_notes$cluster <- as.character(a_notes$cluster)
  t_notes <- data.table(a_notes)
  setkey(t_notes, serial_number)
  t_notes <- t_notes[t_notes[, .N, by=serial_number][N > 2],
                     c("serial_number", f, "cluster"), with=FALSE]
  # provide two previous notes for trigram context
  t_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                 last=cluster,
                 blast=c("<S>", cluster[1:.N-1])),
          by=serial_number]
  # deleting redundant cluster field (replaced by last)
  t_notes <- t_notes[, !"cluster", with=FALSE]
  
  #### NOTE THAT f CORRESPONDS TO last, NOT syl ###
  
  mature_notes <- data.table(a_notes[(nrow(a_notes) * 0.33):nrow(a_notes),])
  setkey(mature_notes, serial_number)
  mature_notes <- mature_notes[mature_notes[, .N, by=serial_number][N > 2],
                               c("serial_number", f, "cluster"), with=FALSE]
  # provide two previous notes for trigram context
  mature_notes[, `:=`(syl=c(cluster[-1], "</S>"),
                      last=cluster,
                      blast=c("<S>", cluster[1:.N-1])),
               by=serial_number]
  setkey(mature_notes, blast, last, syl)
  
  if (quit == 0) {
    p0 <- ggplot(t_notes, aes_string(x=f, color="syl")) +
      geom_density() +
      stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
      facet_wrap(~last) +
      ggtitle(paste("all notes over all time:", f, "x next-syllable"))
    if (save) {print(p0); dev.off(); return()}
    else {return(p0)}
  }
  properxs <- xs
  if (f == "duration") properxs <- c(0,450)
  p0 <- ggplot(t_notes, aes_string(x=f, color="syl")) +
    geom_density() +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=properxs, ylim=ys) +
    facet_wrap(~last) +
    ggtitle(paste("all notes over all time:", f, "x next-syllable"))
  p0.5 <- ggplot(mature_notes, aes_string(x=f, color="syl")) +
    geom_density() +
    stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
    coord_cartesian(xlim=properxs, ylim=ys) +
    geom_text(y=th, aes(label=..count.., ymax=..count.., color=syl),
              position=position_dodge(width=0.95*diff(properxs)),
              stat="bin", binwidth=10000, origin=-5000-1.5*(-5000-origin)) +
    facet_wrap(~last) +
    ggtitle(paste("all notes over mature time:", f, "x next-syllable"))
  if (quit==0.5) {
    if (save) {print(p0.5); dev.off(); return()}
    else {return(p0.5)}
  }
    
  
  # concentrate on the song (not the alarms)
  # look for differences before or after song breaks
  setkey(t_notes, blast, last, syl)
  t_123 <- t_notes[CJ(c("1", "2", "3"),
                      c("<S>", "</S>", "1", "2", "3"),
                      c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
                   c("blast", "last", "syl", f), nomatch=0, with=FALSE]
  setkey(t_123, last, blast, syl)
#   t_123_means <- copy(t_123)
#   t_123_means <- unique(t_123_means[, `:=`(count=.N, mean_dur=mean(eval(feature))),
#                                     by=key(t_123_means)])[, !f, with=FALSE]
  
  # concentrate on the song (not the alarms)
  # look for differences before or after song breaks
  setkey(mature_notes, blast, last, syl)
  m_123 <- mature_notes[CJ(c("1", "2", "3"),
                           c("<S>", "</S>", "1", "2", "3"),
                           c("<S>", "</S>", "1", "2", "3"), sorted=FALSE),
                        c("blast","last","syl",f), nomatch=0, with=FALSE]
  setkey(m_123, last, blast, syl)
#   m_123_means <- copy(m_123)
#   m_123_means <- unique(m_123_means[, `:=`(count=.N, mean_dur=mean(eval(feature))),
#                                     by=key(m_123_means)])[, !f, with=FALSE]
  
  # see if the difference b/t, say, 2-3 is different than 2-else
  t123gt_tests <- copy(t_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
                                     twonext = syl==2, threenext = syl==3)]
  t123cf_tests <- copy(t_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
                                     aftertwo = blast==2, afterthree = blast==3)]
  
  m123gt_tests <- copy(m_123)[, `:=`(stopnext = syl=="</S>", onenext = syl==1,
                                     twonext = syl==2, threenext = syl==3)]
  m123cf_tests <- copy(m_123)[, `:=`(afterstart = blast=="<S>", afterone = blast==1,
                                     aftertwo = blast==2, afterthree = blast==3)]
  
#   t.test(eval(feature) ~ stopnext, data=t123gt_tests["1"])
  p1 <- plotter(t_123, f, "syl", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over all time:", f, "x next-syllable"))
#   p1 <- ggplot(t_123, aes_string(x=f, color="syl")) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over all time:", f, "x next-syllable"))

  if (quit == 1) {
    if (save) {print(p0); print(p1); dev.off(); return()}
    else {return(p1)}
  }

  p2 <- plotter(t_123, f, "blast", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over all time:", f, "x last-syllable"))
#   p2 <- ggplot(t_123, aes_string(x=f, color="blast")) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over all time:", f, "x last-syllable"))

  p3 <- plotter(m_123, f, "syl", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over mature time:", f, "x next-syllable"))
#   p3 <- ggplot(m_123, aes(x=duration, color=syl)) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over mature time:", f, "x next-syllable"))
  
  p4 <- plotter(m_123, f, "blast", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over mature time:", f, "x last-syllable"))
#   p4 <- ggplot(m_123, aes(x=duration, color=blast)) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=blast), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=blast),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over mature time:", f, "x last-syllable"))
  
  mcheck <- m_123[syl==as.integer(last)+1 | syl=="1" & last=="3" | syl=="</S>"]
  p5 <- plotter(mcheck, f, "syl", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over mature time:", f, "x (stopnext vs songnext)"))
#   p5 <- ggplot(mcheck, aes(x=duration, color=syl)) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=syl),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over mature time:", f, "x (stopnext vs songnext)"))
  

  # Same tests, but now for skip-1 effects
  # ======================================
  
  # convert to data.table for faster grouping
  t_skip_123 <- data.table(a_notes)
  # provide forward and backward looking bigram info
  setkey(t_skip_123, serial_number)
  t_skip_123 <- t_skip_123[t_skip_123[, .N, by=serial_number][N > 2],
                           c("serial_number", f, "cluster"), with=FALSE]
  # provide two previous notes for trigram context
  t_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
                    syl=c(cluster[-1], "</S>"),
                    last=cluster,
                    blast=c("<S>", cluster[1:.N-1])),
             by=serial_number]
  # deleting redundand cluster field (replaced by last)
  t_skip_123 <- t_skip_123[, !"cluster", with=FALSE]
  
  #### NOTE THAT f CORRESPONDS TO last, NOT syl ###
  
  setkey(t_skip_123, last, syl, anext)
  t_skip_123 <- t_skip_123[CJ(c("1", "2", "3"),
                              c("1", "2", "3"),
                              c("</S>", "1", "2", "3"), sorted=FALSE),
                           c("blast","last","syl","anext",f), nomatch=0, with=FALSE]
  setkey(t_skip_123, last, syl, anext)
  
  # convert to data.table for faster grouping
  m_skip_123 <- data.table(a_notes[(nrow(a_notes) * 0.33):nrow(a_notes),])
  # provide forward and backward looking bigram info
  setkey(m_skip_123, serial_number)
  m_skip_123 <- m_skip_123[m_skip_123[, .N, by=serial_number][N > 2],
                           c("serial_number", f, "cluster"), with=FALSE]
  # provide two previous notes for trigram context
  m_skip_123[, `:=`(anext=c(cluster[-(1:2)], "</S>", "</S>"),
                    syl=c(cluster[-1], "</S>"),
                    last=cluster,
                    blast=c("<S>", cluster[1:.N-1])),
             by=serial_number]
  # deleting redundand cluster field (replaced by last)
  m_skip_123 <- m_skip_123[, !"cluster", with=FALSE]
  
  #### NOTE THAT f CORRESPONDS TO last, NOT syl ###
  
  setkey(m_skip_123, last, syl, anext)
  m_skip_123 <- m_skip_123[CJ(c("1", "2", "3"),
                              c("1", "2", "3"),
                              c("</S>", "1", "2", "3"), sorted=FALSE),
                           c("blast","last","syl","anext",f), nomatch=0, with=FALSE]
  setkey(m_skip_123, last, syl, anext)
  
  tskip123_tests <- copy(t_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
                                            twocoming = anext==2, threecoming = anext==3)]
  
  mskip123_tests <- copy(m_skip_123)[, `:=`(stopcoming = anext=="</S>", onecoming = anext==1,
                                            twocoming = anext==2, threecoming = anext==3)]
  
#   t.test(eval(feature) ~ stopcoming, data=tskip123_tests["1"])
  
  p6 <- plotter(t_skip_123, f, "anext", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over all time:", f, "x postnext-syllable"))
#   p6 <- ggplot(t_skip_123, aes_string(x=f, color="anext")) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over all time:", f, "x postnext-syllable"))
    
  p7 <- plotter(m_skip_123, f, "anext", "last", xs, ys, th, dodge, origin,
                paste("1,2,3 over mature time:", f, "x postnext-syllable"))  
#   p7 <- ggplot(m_skip_123, aes_string(x=f, color="anext")) +
#     geom_line(stat="density") +
#     stat_vline(aes(color=anext), xintercept="mean", linetype="dotted") +
#     coord_cartesian(xlim=c(0,300), ylim=c(0, 8e-2)) +
#     geom_text(y=6e-2, aes(label=..count.., ymax=..count.., color=anext),
#               position=position_dodge(width=200), 
#               stat="bin", binwidth=10000, origin=-4900+0) +
#     facet_wrap(~last) +
#     ggtitle(paste("1,2,3 over mature time:", f, "x postnext-syllable"))
  
  l <- list(p0,p0.5,p1,p2,p3,p4,p5,p6,p7)

  if (save) {
    lapply(l, print)
    dev.off()
  }
  else {
    return(l)
  }
}

# ctest(quote(duration), save=TRUE)
# ctest(quote(mean_pitch), xs=c(0,4000), ys=c(0,3e-3),
#       th=2e-3, dodge=2500, origin=-3500, save=TRUE)
# ctest(quote(mean_FM), xs=c(0,75), ys=c(0,.15),
#       th=10e-2, dodge=50, origin=-4975, save=TRUE)
# # ggplot(t_notes, aes_string(x="mean_entropy", color="syl")) +
# #   geom_line(stat="density") +
# #   stat_vline(aes(color=syl), xintercept="mean", linetype="dotted") +
# #   coord_cartesian(xlim=c(-5,0), ylim=c(0,3)) +
# #   geom_text(y=2, aes(label=..count.., ymax=..count.., color=syl),
# #             position=position_dodge(width=5),
# #             stat="bin", binwidth=10000, origin=-5002.5) +
# #   facet_wrap(~last) +
# #   ggtitle("all notes over all time: mean_entropy x next-syl")
# ctest(quote(mean_entropy), xs=c(-3,0), ys=c(0,3),
#       th=2, dodge=2, origin=-5002, save=TRUE)
# ctest(quote(mean_amplitude), xs=c(0,60), ys=c(0,0.3),
#       th=0.2, dodge=40, origin=-4975, save=TRUE)
# ctest(quote(mean_goodness_of_pitch), xs=c(0,800), ys=c(0,0.02),
#       th=0.015, dodge=600, origin=-4600, save=TRUE)
# ctest(quote(mean_mean_frequency), xs=c(0,7000), ys=c(0,0.0015),
#       th=0.001, dodge=4500, origin=-2700, save=TRUE)