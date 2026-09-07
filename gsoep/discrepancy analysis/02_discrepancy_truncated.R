# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Relationship Life Course' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
# kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'
kim.server <- '/home/kmcerlea/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim); .libPaths("G:/Other computers/My Laptop/Documents/R/R library") }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea); .libPaths("G:/My Drive/R Library")  }
if (Sys.getenv(c("HOME" )) == "/home/lpessin") { setwd(lea.server) }
if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") { setwd(kim.server) }
getwd() # check it worked

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load and install packages for whomever is running the script
## the server doesn't let you install packages
## the server doesn't have ggseqplot for now (package incompatibility issue)

if (Sys.getenv(c("HOME" )) == "/home/lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("USERNAME")) == "lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot","dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("educational differences/R data/gsoep-setupsequence-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import data and small things needed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data$couple_educ_type <- factor(
  data$couple_educ_type,
  levels = c(1,2,3,4),
  labels = c(
    "Neither College",
    "Him College",
    "Her College",
    "Both College"
  )
)


data$parent_info <- factor(
  data$parent_info,
  levels = c(0,1,2),
  labels = c(
    "Always CF",
    "Become Parent",
    "Always Parent"
  )
)

table(data$couple_educ_type) 
table(data$one_college) # which is better? I think 4 groups is ideal but might be a lot to focus on? 
# neither college is def distinct; it really depends on channel if three colleges are.
table(data$either_birth_pre_rel)
table(data$parent_info)

subset.cf0 <- data$either_birth_pre_rel %in% c(0)
subset.par1 <- data$either_birth_pre_rel %in% c(1)

subset.cf <- data$parent_info %in% c("Always CF")
subset.trans <- data$parent_info %in% c("Become Parent")
subset.par <- data$parent_info %in% c("Always Parent")+

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First look at descriptive details about the sequences by education
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Index plots by group
seqIplot(seq.fam, group = data$couple_educ_type)
seqIplot(seq.work.ow, group = data$couple_educ_type)
seqIplot(seq.hw.hrs, group = data$couple_educ_type)

# okay there is MUCH less variation in Germany than in the US GAH (I mean, the R2 tell me that, but these visuals are like...the same)
# this is particularly true for family. MAYBE for paid work / HW, the both college are more egal (can't decide if for Germany, BOTH college is more distinct?)
# like her college and both college similar. him college more like neither (which makes sense and THIS is why I like the 4-category)
seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)

seqIplot(seq.fam, group = data$one_college, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$one_college, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$one_college, sortv = "from.end",  with.missing = FALSE)

seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)

#actually maybe start works well for the TWO GROUPS specifically
seqIplot(seq.fam, group = data$one_college,  sortv = "from.start", with.missing = FALSE) # here the main diff is more start with kids, that's like all I can see
seqIplot(seq.work.ow, group = data$one_college, sortv = "from.start", with.missing = FALSE) # so you do see more egal at start. this is an interesting narrative
seqIplot(seq.hw.hrs, group = data$one_college,  sortv = "from.start", with.missing = FALSE)

# State distro by group
seqdplot(seq.fam, group = data$couple_educ_type)
seqdplot(seq.work.ow, group = data$couple_educ_type)
seqdplot(seq.hw.hrs, group = data$couple_educ_type)

seqdplot(seq.fam, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.work.ow, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.hw.hrs, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)

#I actually really like these. even though they AREN'T index plots, i feel like these convey diffs best (bc honestly, without complete sequences, it's hard to ascertain trends at the end anyway)
seqdplot(seq.fam, group = data$one_college, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.work.ow, group = data$one_college, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.hw.hrs, group = data$one_college, yaxis=FALSE, xaxis=FALSE)

# representative seq by group
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "dist")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "density")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "freq")

seqrplot(seq.work.ow, group = data$couple_educ_type, diss=dist.work.min)
seqrplot(seq.hw.hrs, group = data$couple_educ_type, diss=dist.hw.min)

## Multi-channel plots by education and parental status
pdf("educational differences/results/gsoep/GSOEP_MCIndex_4Groups.pdf",
    width=8,
    height=11)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7)

dev.off()

pdf("educational differences/results/gsoep/GSOEP_MCIndex_Childfree.pdf",
    width=8,
    height=11)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.cf, ],
                        Housework=seq.hw.hrs[subset.cf, ],
                        Family=seq.fam[subset.cf, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.cf],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=1,
          cex.legend=0.7)

dev.off()

pdf("educational differences/results/gsoep/GSOEP_MCIndex_BecomeParents.pdf",
    width=8,
    height=11)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.trans, ],
                        Housework=seq.hw.hrs[subset.trans, ],
                        Family=seq.fam[subset.trans, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.trans],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=1,
          cex.legend=0.7)

dev.off()

pdf("educational differences/results/gsoep/GSOEP_MCIndex_AlwaysParents.pdf",
    width=8,
    height=11)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.par, ],
                        Housework=seq.hw.hrs[subset.par, ],
                        Family=seq.fam[subset.par, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.par],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=1,
          cex.legend=0.7)

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempt discrepancy analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
da.mcsa.4gp<-dissassoc(mcdist.det.min, group = data$couple_educ_type)
view(da.mcsa.4gp$groups)
view(da.mcsa.4gp$stat)

da.mcsa.stat<-data.frame(da.mcsa.4gp$stat)
da.mcsa.stat$channel<-c("mcsa","mcsa","mcsa","mcsa","mcsa")

da.mcsa.gp.discrep<-data.frame(da.mcsa.4gp$groups)
da.mcsa.gp.discrep$channel<-c("mcsa","mcsa","mcsa","mcsa","mcsa")

da.mcsa.2gp<-dissassoc(mcdist.det.min, group = data$one_college)

# higher discrepancy = more internally diverse trajectories
# lower discrepancy = more homogeneous trajectories
# so makes sense college = more homogenous (but quite small)
# Barlett and Levene more formally test if groups differ on their internal heterogeneity (so they do) - but none of these are pairwise
# concern is that all of this - R2 is quite low.
# I wonder if HERE, doing NON-MCSA is better because with like what 6x8x5 states, are they going to be different always (bc could be like male BW, HWA, male BW HW B?)
# (versus within one channel - easier to see similariies or differs
# splitting might ALSO make it easier to say like -- okay FAMILY patterns same, but gendered DoL is NOT (or vice versa). or even like paid work
# Wait this is prob MORE interesting, and I already have the diss matrices anyway?
# R-squared is literally exp / total

# Four group education
da.fam.4gp<-dissassoc(dist.fam.min, group = data$couple_educ_type)
da.fam.stat<-data.frame(da.fam.4gp$stat)
da.fam.stat$channel<-c("fam","fam","fam","fam","fam")

da.fam.gp.discrep<-data.frame(da.fam.4gp$groups)
da.fam.gp.discrep$channel<-c("fam","fam","fam","fam","fam")

da.work.4gp<-dissassoc(dist.work.min, group = data$couple_educ_type)
da.work.stat<-data.frame(da.work.4gp$stat)
da.work.stat$channel<-c("work","work","work","work","work")

da.work.gp.discrep<-data.frame(da.work.4gp$groups)
da.work.gp.discrep$channel<-c("work","work","work","work","work")

da.hw.4gp<-dissassoc(dist.hw.min, group = data$couple_educ_type)
da.hw.stat<-data.frame(da.hw.4gp$stat)
da.hw.stat$channel<-c("hw","hw","hw","hw","hw")

da.hw.gp.discrep<-data.frame(da.hw.4gp$groups)
da.hw.gp.discrep$channel<-c("hw","hw","hw","hw","hw")

# export overall stats
da.stat.combined.x <- bind_rows(list(df1 = da.mcsa.stat, df2 = da.fam.stat, 
                                     df3 = da.work.stat, df4 = da.hw.stat)) #, .id = "source")
da.stat.combined <- cbind(stat = row.names(da.stat.combined.x), da.stat.combined.x)
write_csv(da.stat.combined, "educational differences/results/gsoep/GSOEP_discrepancy_overall_stats.csv")

#write.table(da.stat.combined, file="educational differences/results/gsoep/GSOEP_discrepancy_overall_stats.csv",
#            sep = ",", row.names = TRUE, col.names = TRUE)

# export group-level discrepancies
da.gp.discrep.combined.x <- bind_rows(list(df1 = da.mcsa.gp.discrep, df2 = da.fam.gp.discrep,
                                           df3 = da.work.gp.discrep, df4 = da.hw.gp.discrep)) # , .id = "source")
da.gp.discrep.combined <- cbind(stat = row.names(da.gp.discrep.combined.x), da.gp.discrep.combined.x)
write_csv(da.gp.discrep.combined, "educational differences/results/gsoep/GSOEP_discrepancy_by_group.csv")

# Two  group education (not exporting for now)
da.fam.2gp<-dissassoc(dist.fam.min, group = data$one_college)
da.work.2gp<-dissassoc(dist.work.min, group = data$one_college)
da.hw.2gp<-dissassoc(dist.hw.min, group = data$one_college)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Oh yeah do I want to try the moving window thing?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# worried this will not work as well in truncated, but let's see...

# takeaway for the Pseudo R2 / Levene - trends across time are similar regardless of whether 2 or 4-cat
# discrepancy obviously similar because 2 is just reduced version of 4 - probably more compelling with 2.
# i think there are some nuances (esp HIM college) - let's see if emerge in other countries (think that is also a way to decide)
# if it's a US-specific thing v. global - worth calling out if global. otherwise, maybe too much?
# Okay, I think it is global

# Germany is interesting because all of the action for work (both) seems to happen in middle of life course (like high R2)
# family is most explanatory at beginning (is this kids?) then end is standardized

### Four groups ###
# Fam
educ.diff.fam <- seqdiff(seq.fam, data$couple_educ_type)

plot(educ.diff.fam, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.fam, stat="discrepancy")
educ.diff.fam$discrepancy # just displays the whole table (educ x duration)
educ.diff.fam$stat

fam.diff.dur<-data.frame(educ.diff.fam$stat)
fam.diff.dur$channel<-c("fam")
fam.diff.dur$duration<-c(1,2,3,4,5,6,7,8,9)

# Work
educ.diff.work <- seqdiff(seq.work.ow, data$couple_educ_type)
plot(educ.diff.work, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.work, stat="discrepancy")

work.diff.dur<-data.frame(educ.diff.work$stat)
work.diff.dur$channel<-c("work")
work.diff.dur$duration<-c(1,2,3,4,5,6,7,8,9)

# HW
educ.diff.hw <- seqdiff(seq.hw.hrs, data$couple_educ_type)
plot(educ.diff.hw, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.hw, stat="discrepancy")

hw.diff.dur<-data.frame(educ.diff.hw$stat)
hw.diff.dur$channel<-c("hw")
hw.diff.dur$duration<-c(1,2,3,4,5,6,7,8,9)

# Does this work in MC approach?
educ.diff.mcsa <- seqdiff(mcsa, data$couple_educ_type)
plot(educ.diff.mcsa, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.mcsa, stat="discrepancy")

mcsa.diff.dur<-data.frame(educ.diff.mcsa$stat)
mcsa.diff.dur$channel<-c("mcsa")
mcsa.diff.dur$duration<-c(1,2,3,4,5,6,7,8,9)

# export channel x duration
diff.dur.combined <- bind_rows(list(df1 = mcsa.diff.dur, df2 = fam.diff.dur, 
                                    df3 = work.diff.dur, df4 = hw.diff.dur)) #, .id = "source")
write_csv(diff.dur.combined, "educational differences/results/gsoep/GSOEP_discrepancy_by_duration.csv")

# Want to combine all plots
# See: https://r-charts.com/base-r/axes/

pdf("educational differences/results/gsoep/GSOEP_R2_by_duration.pdf",
    width=20,
    height=8)

layout.fig1 <- layout(matrix(c(1,2,3,4), nrow=1, ncol=4, byrow = TRUE)) #,
#heights = c(1,1,1,1,1))
layout.show(layout.fig1)

# par(mar = c(5, 5, 3, 3))
par(mar = c(4, 4, 3, 1))

# MCSA
#plot(educ.diff.mcsa, stat=c("Pseudo R2"),
#     xaxis=FALSE)
#axis(1, at = c(1,2,3,4,5,6,7,8,9))
#axis(side=2, at = c(0,0.005,0.010,0.015,0.020,0.025,0.030))

plot(educ.diff.mcsa$stat[, "Pseudo R2"],
     type = "l",
     ylim = c(0, 0.015),
     xaxt = "n",
     xlab = "",
     ylab = "Pseudo R2",,
     main = "Multi-Channel")

axis(1, at = 1:9)
axis(2, at = seq(0, 0.015, 0.005))

# Paid Work Channel: With Overwork
plot(educ.diff.work$stat[, "Pseudo R2"],
     type = "l",
     ylim = c(0, 0.015),
     xaxt = "n",
     xlab = "",
     ylab = "Pseudo R2",
     main = "Paid Work")

axis(1, at = 1:9)
axis(2, at = seq(0, 0.015, 0.005))

# Housework Channel: Hours with Group-specific thresholds
plot(educ.diff.hw$stat[, "Pseudo R2"],
     type = "l",
     ylim = c(0, 0.015),
     xaxt = "n",
     xlab = "",
     ylab = "Pseudo R2",
     main = "Housework")

axis(1, at = 1:9)
axis(2, at = seq(0, 0.015, 0.005))

# Family channel
plot(educ.diff.fam$stat[, "Pseudo R2"],
     type = "l",
     ylim = c(0, 0.015),
     xaxt = "n",
     xlab = "",
     ylab = "Pseudo R2",
     main = "Family")

axis(1, at = 1:9)
axis(2, at = seq(0, 0.015, 0.005))

dev.off()

## Alt: just put on 1 figure
pdf("educational differences/results/gsoep/GSOEP_R2_by_duration_combined.pdf")

plot(educ.diff.mcsa$stat[, "Pseudo R2"],
     type = "l",
     lwd = 2,
     ylim = c(0, 0.015),
     xaxt = "n",
     xlab = "Relationship duration",
     ylab = "Pseudo R2")

lines(educ.diff.work$stat[, "Pseudo R2"], lwd = 2, col="seagreen3") # lty = 2)
lines(educ.diff.hw$stat[, "Pseudo R2"], lwd = 2, col="mediumpurple1") # lty = 3)
lines(educ.diff.fam$stat[, "Pseudo R2"], lwd = 2, col="steelblue1") # lty = 4)

axis(1, at = 1:9)
axis(2, at = seq(0, 0.015, 0.005))

legend("topright",
       legend = c("Multi-channel", "Paid work", "Housework", "Family"),
       col = c("black", "seagreen3", "mediumpurple1", "steelblue1"),
       lwd = 2,
       bty = "n")

dev.off()

### Two groups ###
coll.diff.fam <- seqdiff(seq.fam, data$one_college)
plot(coll.diff.fam, stat=c("Pseudo R2", "Levene"))
plot(coll.diff.fam, stat="discrepancy")

coll.diff.work <- seqdiff(seq.work.ow, data$one_college)
plot(coll.diff.work, stat=c("Pseudo R2", "Levene")) 
plot(coll.diff.work, stat="discrepancy")

coll.diff.hw <- seqdiff(seq.hw.hrs, data$one_college)
plot(coll.diff.hw, stat=c("Pseudo R2", "Levene"))
plot(coll.diff.hw, stat="discrepancy")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I want to change the colors on these discrepancy plots by education
# and it's chaos so making separate section (not actually sure using atm)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Family
df.fam.discrepancy <- as.data.frame(educ.diff.fam$discrepancy)

matplot(
  df.fam.discrepancy,
  type = "l",
  lty = 1,
  lwd = 2,
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
)

legend(
  "bottomright",
  legend = c(levels(data$couple_educ_type), "Total"),
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
  lty = 1,
  lwd = 2,
  cex = 0.7,
  y.intersp = 0.6,   # vertical spacing
  x.intersp = 0.5,   # line-to-text spacing
  seg.len = 1.5,     # legend line length
  bty = "n"
)


# Paid Work
df.work.discrepancy <- as.data.frame(educ.diff.work$discrepancy)

matplot(
  df.work.discrepancy,
  type = "l",
  lty = 1,
  lwd = 2,
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
)

legend(
  "bottomright",
  legend = c(levels(data$couple_educ_type), "Total"),
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
  lty = 1,
  lwd = 2,
  cex = 0.7,
  y.intersp = 0.6,   # vertical spacing
  x.intersp = 0.5,   # line-to-text spacing
  seg.len = 1.5,     # legend line length
  bty = "n"
)

# Housework
df.hw.discrepancy <- as.data.frame(educ.diff.hw$discrepancy)

matplot(
  df.hw.discrepancy,
  type = "l",
  lty = 1,
  lwd = 2,
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
)

legend(
  "bottomleft",
  legend = c(levels(data$couple_educ_type), "Total"),
  col = c(
    "#E97132",
    "#0F9ED5",
    "#7570b3",
    "#e7298a",
    "black"
  ),
  lty = 1,
  lwd = 2,
  cex = 0.7,
  y.intersp = 0.6,   # vertical spacing
  x.intersp = 0.5,   # line-to-text spacing
  seg.len = 1.5,     # legend line length
  bty = "n"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Is it education or parenthood?
# Here is where you can see which covariates matter [could even do more]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not exporting these for now.

dissmfacw(
  dist.fam.min ~ couple_educ_type + first_birth_pre_rel_man + first_birth_pre_rel_woman, 
  data = data, R = 100)

dissmfacw(
  dist.fam.min ~ couple_educ_type + parent_info, # oh, family is kind of stupid because this is literally defined by family states. This is why this would be not a domain but a stratifier
  data = data, R = 100)

dissmfacw(
  dist.work.min ~ couple_educ_type + first_birth_pre_rel_man + first_birth_pre_rel_woman, 
  data = data, R = 100)

dissmfacw(
  dist.work.min ~ couple_educ_type + parent_info, 
  data = data, R = 100)

dissmfacw(
  dist.hw.min ~ couple_educ_type + first_birth_pre_rel_man + first_birth_pre_rel_woman, 
  data = data, R = 100)

dissmfacw(
  dist.hw.min ~ couple_educ_type + parent_info, 
  data = data, R = 100)

