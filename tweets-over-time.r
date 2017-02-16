source("bird-prologue.r")

# clusters over time, nothing else
ta <- allmeans[,c("recnum","cluster")]

# divide data into 100 bins; add bin number as column
ta$bin <- cut(ta$recnum, 100, labels=1:100)
#   recnum cluster bin
# 1      1       0   1
# 2      2       0   1
# 3      3       0   1
# 4      4       0   2
# 5      5       0   2
# 6      6       0   2
# ...

# aggregate clusters by bin
tacast <- dcast(ta, value.var="cluster", cluster ~ bin, length)
#   cluster 1    2    3    4    5    6    7    8    9
# 1       0 3 1104 1105 1104 1105 1104 1105 1105 1104
# 2       1 0    0    0    0    0    0    0    0    0
# 3       2 0    0    0    0    0    0    0    0    0
# 4       3 0    0    0    0    0    0    0    0    0
# ...

# switch data to "long" format to make plotting easier
mta <- melt(tacast, id="cluster", variable.name="bin")
#   cluster bin value
# 1       0   1     3
# 2       1   1     0
# 3       2   1     0
# 4       3   1     0
# 5       4   1     0
# 6       5   1     0
# 7       6   1     0
# 8       7   1     0
# 9       9   1     0
# 10      0   2  1104
# 11      1   2     0
# 12      2   2     0
# 13      3   2     0
# ...

# split the data into separate frames for each bin, then add a column
# tracking the local percentage of the bin represented by each cluster
mta_prop <- ddply(mta, "bin", transform, percent = value / sum(value) * 100)
#   cluster bin value percent
# 1       0   1     3     100
# 2       1   1     0       0
# 3       2   1     0       0
# 4       3   1     0       0
# 5       4   1     0       0
# 6       5   1     0       0
# ...

# make bin numeric (because it's representing time)
mta_prop$bin <- as.numeric(mta_prop$bin)

# for each cluster, create a plot object: bins by percent of bin occupied by cluster
mta_prop_plot <- ggplot(mta_prop, aes(x=bin, y=percent, fill=cluster))
# geom_area creates stacked lines from the plot objects:
#   "colour" and "size" style the lines, "alpha" transparencizes the fills,
#   "fill_brewer" sets the fill colors, "breaks" arranges the legend,
#   "theme_bw" makes the background white
mta_prop_plot + geom_area(colour="black", size=.2, alpha=0.6) +
  scale_fill_brewer(palette="Set1", breaks=rev(levels(mta_prop$cluster))) + 
  theme_bw()