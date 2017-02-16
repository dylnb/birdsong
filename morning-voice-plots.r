source("bird-prologue.r")

# use entropy, pitch, hour, and cluster data
morning_voice <- allmeans[,c("mean_entropy","mean_pitch","hour","cluster")]
# add a column with a binary switch for am/pm
morning_voice$ampm <- apply(entropy_ts,1,function(row) if (row["hour"] < 10) "early" else "later")

# scatter plots, split by cluster, comparing pitch and entropy, colored by am/pm
mv_plots <- ggplot(morning_voice, aes(x=mean_pitch, y=mean_entropy, color=ampm)) +
  geom_point(alpha=.11) + facet_wrap( ~ cluster) +
  scale_colour_manual(values=c("early"="black","later"="red"))

