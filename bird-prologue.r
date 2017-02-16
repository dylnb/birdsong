library(plyr)
library(reshape2)
library(ggplot2)
library(RMySQL)

if (!exists("con")) {
  con <- dbConnect(MySQL(), user='root', password='', dbname='birds')
}
if (!exists("allmeans")) {
  allmeans <- dbGetQuery(con, paste("SELECT recnum, serial_number, start_on, ",
                                    "duration, month, day, hour, minute, second, ", 
                                    "mean_pitch, mean_FM, mean_entropy, mean_goodness_of_pitch, ",
                                    "mean_mean_frequency, mean_am2, mean_amplitude, ",
                                    "cluster FROM clustered109", sep=""))
}
if (!exists("allvars")) {
  allvars <- dbGetQuery(con, paste("SELECT recnum, serial_number, start_on, ",
                                   "duration, month, day, hour, minute, second, ",
                                   "var_pitch, var_FM, var_entropy, var_goodness_of_pitch, ",
                                   "var_mean_frequency, var_am, ",
                                   "cluster FROM clustered109", sep=""))
}
# cluster is categorical
allmeans$cluster <- factor(allmeans$cluster)
allvars$cluster <- factor(allvars$cluster)