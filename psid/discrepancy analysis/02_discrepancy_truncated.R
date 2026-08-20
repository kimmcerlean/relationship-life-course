# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Relationship Life Course' #leas folder
kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course' # Kim
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
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
# Import data and small things needed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid-setupsequence-truncated.RData")

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
table(data$either_birth_pre_rel)
table(data$parent_info)

subset.cf0 <- data$either_birth_pre_rel %in% c(0)
subset.par1 <- data$either_birth_pre_rel %in% c(1)

subset.cf <- data$parent_info %in% c("Always CF")
subset.trans <- data$parent_info %in% c("Become Parent")
subset.par <- data$parent_info %in% c("Always Parent")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempt discrepancy analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# neither college is def distinct; it really depends on channel if three colleges are.

dissassoc(mcdist.det.min, group = data$couple_educ_type)
dissassoc(mcdist.det.min, group = data$one_college)

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

dissassoc(dist.fam.min, group = data$couple_educ_type)
dissassoc(dist.work.min, group = data$couple_educ_type)
dissassoc(dist.hw.min, group = data$couple_educ_type)

dissassoc(dist.fam.min, group = data$one_college)
dissassoc(dist.work.min, group = data$one_college)
dissassoc(dist.hw.min, group = data$one_college)

# Index plots by group
seqIplot(seq.fam, group = data$couple_educ_type)
seqIplot(seq.work.ow, group = data$couple_educ_type)
seqIplot(seq.hw.hrs, group = data$couple_educ_type)

# adding sort end brings missing back in? okay no all have missing just easier / harder to see in some views, I am dumb
seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE) # I think PAID WORK is the most interesting for FOUR groups specifically
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)

seqIplot(seq.fam, group = data$one_college, sortv = "from.end",  with.missing = FALSE) # this visual is compelling
# family is the primary plot that makes sense to order bc there is a progression. HW is the oddest in this sort order tbh
seqIplot(seq.work.ow, group = data$one_college, sortv = "from.end",  with.missing = FALSE) # here harder to say if it's also because more people AT end in college
seqIplot(seq.hw.hrs, group = data$one_college, sortv = "from.end",  with.missing = FALSE)

seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)

#actually maybe start works well for the TWO GROUPS specifically
seqIplot(seq.fam, group = data$one_college,  sortv = "from.start", with.missing = FALSE) # okay but THIS is interesting bc shows MARRIAGE prevalence AND CF at start prevalence
seqIplot(seq.work.ow, group = data$one_college, sortv = "from.start", with.missing = FALSE) # here we see more egal at start, but I think you lose the eND state (bc I think parenthood operates differently)
seqIplot(seq.hw.hrs, group = data$one_college,  sortv = "from.start", with.missing = FALSE) # okay this also shows that more educated START more egal, so goes well with above actually

# State distro by group
seqdplot(seq.fam, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.work.ow, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.hw.hrs, group = data$couple_educ_type, yaxis=FALSE, xaxis=FALSE)

seqdplot(seq.work.ow, group = data$either_birth_pre_rel, yaxis=FALSE, xaxis=FALSE) # parenthood or educ?
seqdplot(seq.hw.hrs, group = data$either_birth_pre_rel, yaxis=FALSE, xaxis=FALSE) # parenthood or educ?

seqdplot(seq.work.ow[subset.cf, ], group = data$couple_educ_type[subset.cf], yaxis=FALSE, xaxis=FALSE) 
seqdplot(seq.work.ow[subset.par, ], group = data$couple_educ_type[subset.par], yaxis=FALSE, xaxis=FALSE) 

#I actually really like these
seqdplot(seq.fam, group = data$one_college, yaxis=FALSE, xaxis=FALSE)
seqdplot(seq.work.ow, group = data$one_college, yaxis=FALSE, xaxis=FALSE) # this is also compelling (but these aren't index plots so not within couples, but I think it actually captures trends well)
seqdplot(seq.hw.hrs, group = data$one_college, yaxis=FALSE, xaxis=FALSE)

# representative seq by group
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "dist")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "density")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.min, criterion = "freq")

seqrplot(seq.work.ow, group = data$couple_educ_type, diss=dist.work.min)
seqrplot(seq.hw.hrs, group = data$couple_educ_type, diss=dist.hw.min)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Oh yeah do I want to try the moving window thing?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# worried this will not work as well in truncated, but let's see...

# takeaway for the Pseudo R2 / Levene - trends across time are similar regardless of whether 2 or 4-cat
# discrepancy obviously similar because 2 is just reduced version of 4 - probably more compelling with 2.
# i think there are some nuances (esp HIM college) - let's see if emerge in other countries (think that is also a way to decide)
# if it's a US-specific thing v. global - worth calling out if global. otherwise, maybe too much?

educ.diff.fam <- seqdiff(seq.fam, data$couple_educ_type)

# this looks almost exactly the same as complete
plot(educ.diff.fam, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.fam, stat="discrepancy")
educ.diff.fam$discrepancy # just displays the whole table

educ.diff.work <- seqdiff(seq.work.ow, data$couple_educ_type)
plot(educ.diff.work, stat=c("Pseudo R2", "Levene")) # this is quite different to complete?
plot(educ.diff.work, stat="discrepancy")

#this also seems similar to complete
educ.diff.hw <- seqdiff(seq.hw.hrs, data$couple_educ_type)
plot(educ.diff.hw, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.hw, stat="discrepancy")

# Two groups
coll.diff.fam <- seqdiff(seq.fam, data$one_college)
plot(coll.diff.fam, stat=c("Pseudo R2", "Levene"))
plot(coll.diff.fam, stat="discrepancy")

coll.diff.work <- seqdiff(seq.work.ow, data$one_college)
plot(coll.diff.work, stat=c("Pseudo R2", "Levene")) 
plot(coll.diff.work, stat="discrepancy") # these changes become dramatic and I *think* it's because patterns among college educated are consistent at start and diverge if parent or not

coll.diff.hw <- seqdiff(seq.hw.hrs, data$one_college)
plot(coll.diff.hw, stat=c("Pseudo R2", "Levene"))
plot(coll.diff.hw, stat="discrepancy") # also interesting - do these all decline because everyone ends up in specialized?

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I want to change the colors on these discrepancy plots and it's chaos so making separate section
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
  "topright",
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
  dist.work.min ~ couple_educ_type + either_birth_pre_rel, 
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

#
work.tree <- seqtree(
  seq.work.ow ~ couple_educ_type + either_birth_pre_rel,
  data = data, R = 100, diss = dist.work.min, pval= 0.01) ## , min.size = 30, maxdepth = 5

seqtreedisplay(work.tree, type = "I",
               sortv = cmdscale(sqrt(dist.work.min), k = 1))

seqtreedisplay(work.tree, type = "d")

#
work.tree1 <- seqtree(
  seq.work.ow ~ couple_educ_type + parent_info,
  data = data, R = 100, diss = dist.work.min, pval= 0.01) ## , min.size = 30, maxdepth = 5

seqtreedisplay(work.tree1, type = "I",
               sortv = cmdscale(sqrt(dist.work.min), k = 1))

seqtreedisplay(work.tree1, type = "d")

#
hw.tree <- seqtree(
  seq.hw.hrs ~ couple_educ_type + either_birth_pre_rel,
  data = data, R = 100, diss = dist.hw.min, pval= 0.01) ## , min.size = 30, maxdepth = 5

seqtreedisplay(hw.tree, type = "I",
               sortv = cmdscale(sqrt(dist.hw.min), k = 1))

seqtreedisplay(hw.tree, type = "d")

#
hw.tree1 <- seqtree(
  seq.hw.hrs ~ couple_educ_type + parent_info,
  data = data, R = 100, diss = dist.hw.min, pval= 0.01) ## , min.size = 30, maxdepth = 5

seqtreedisplay(hw.tree1, type = "I",
               sortv = cmdscale(sqrt(dist.hw.min), k = 1))

seqtreedisplay(hw.tree1, type = "d")

# WILL this work in MC framework?!
mcsa.tree <- seqtree(
  mcsa ~ couple_educ_type + either_birth_pre_rel,
  data = data, R = 100, diss = mcdist.det.min, pval= 0.01) 

seqtreedisplay(mcsa.tree, type = "I",
               sortv = cmdscale(sqrt(mcdist.det.min), k = 1)) ## okay wait this is kind of interesting...

seqIplot(mcsa, group = data$one_college,  sortv = "from.start", with.missing = FALSE) 
## is it this easy to graph like this?! okay yes but okay quite hard to examine actually...
seqdplot(mcsa, group = data$one_college, with.missing = FALSE, cex.legend=0.8, ncol=3)
dev.new(width = 20, height = 5)
par(mar = c(0,0,0,0))
seqlegend(mcsa, ncol = 3, cex = 0.6, x.intersp = 0.1, y.intersp = 0.6)
seqlegend(mcsa, ncol=5, cex=0.6)

png("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/legend.png", width = 3000, height = 2000, res = 300)

par(mar = c(0,0,0,0))
seqlegend(mcsa, ncol = 5, cex = 0.6, x.intersp = 0.1)

dev.off()

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs, Family=seq.fam),
          type="d", xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  
#           group = data$mc.factor, 

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=500,sortv="from.end",dom.crit=3,
          cex.legend=0.7)

# OKAY WAIT THIS IS INTERESTING
seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min, group = data$one_college,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min, group = data$one_college,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=3,
          cex.legend=0.7)

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_4Groups.pdf",
    width=8,
    height=11)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7)

dev.off()

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_Childfree.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_BecomeParents.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_AlwaysParents.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_Childfree_alt.pdf",
    width=12,
    height=8)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.cf, ],
                        Housework=seq.hw.hrs[subset.cf, ],
                        Family=seq.fam[subset.cf, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.cf],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=TRUE,k=100,sortv="from.start",dom.crit=1,
          cex.legend=0.7)

dev.off()

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_BecomeParents_alt.pdf",
    width=12,
    height=8)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.trans, ],
                        Housework=seq.hw.hrs[subset.trans, ],
                        Family=seq.fam[subset.trans, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.trans],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=TRUE,k=100,sortv="from.start",dom.crit=1,
          cex.legend=0.7)

dev.off()

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_MCIndex_AlwaysParents_alt.pdf",
    width=12,
    height=8)


seqplotMD(channels=list('Paid Work'=seq.work.ow[subset.par, ],
                        Housework=seq.hw.hrs[subset.par, ],
                        Family=seq.fam[subset.par, ]),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type[subset.par],
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=TRUE,k=100,sortv="from.start",dom.crit=1,
          cex.legend=0.7)

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Try the Liao and Fasang 2021 approach
# SeqCompare: https://search.r-project.org/CRAN/refmans/TraMineRextras/html/seqCompare.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Small drawback here is technically built to compare two groups at a time
# In some ways, answers DIFF question than DA - like, which groups are most different and are ALL different?

# Let's explore first with the two group college because handled easily, then figure out if I can easily subet by education and compare 2x2
# Will add to export for funsies to compare
one.fam <- seqCompare(seq.fam, seqdata2=NULL, group=data$one_college, stat="all",
           set=NULL,  with.missing = TRUE, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)
          #  s=100

one.work <- seqCompare(seq.work.ow, seqdata2=NULL, group=data$one_college, stat="all",
          set=NULL,  with.missing = TRUE, seed=36963, 
          squared="LRTonly", weighted=FALSE, opt=NULL, 
          BFopt=NULL, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel)
          # s=100

one.hw <- seqCompare(seq.hw.hrs, seqdata2=NULL, group=data$one_college, stat="all",
           set=NULL,  with.missing = TRUE, s=100, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel)
# s=100

# Diss matrices for reference:
# dist.work.om <- seqdist(seq.work.ow, method="OM", indel=work.miss.indel, 
#                        sm= work.miss.cost$sm, with.missing=TRUE)
# dist.hw.om <- seqdist(seq.hw.hrs, method="OM", indel=hw.miss.indel, 
#                      sm= hw.miss.cost$sm, with.missing=TRUE)
# dist.fam.om <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
#                       sm= fam.miss.cost$sm, with.missing=TRUE)

# Are these difference or just one or the other? I think they are just one or the other
seqBIC(seq.fam, seqdata2=NULL, group=data$one_college,
           set=NULL,  with.missing = TRUE, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)

seqLRT(seq.fam, seqdata2=NULL, group=data$one_college,
       set=NULL,  with.missing = TRUE, seed=36963, 
       squared="LRTonly", weighted=FALSE, opt=NULL, 
       BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)


### Is this how i create subsequences for each group?
lev <- levels(data$couple_educ_type)
l <- length(lev)
seq.list.fam <- list()
for (i in 1:l){
  seq.list.fam[[i]] <- seq.fam[data$couple_educ_type==lev[i],]
}

sapply(seq.list.fam, nrow) # confirm counts match educ counts

## Then I have to do ALL pairwise? how do I also even know this makes sense / is correct?
seqCompare(list(seq.list.fam[[1]]),list(seq.list.fam[[2]]), stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

# Can also do it this way; okay let's do this
# ~~~~~~~~~~~~~~~
# Family
# ~~~~~~~~~~~~~~~

fam.neither.him <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Him College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.neither.her <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Her College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.neither.both <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.him.her <- seqCompare(seq.fam[data$couple_educ_type=="Him College",],seq.fam[data$couple_educ_type=="Her College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

fam.him.both <- seqCompare(seq.fam[data$couple_educ_type=="Him College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

fam.her.both <- seqCompare(seq.fam[data$couple_educ_type=="Her College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Paid Work
# ~~~~~~~~~~~~~~~

work.neither.him <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Him College",], stat="all", 
                               with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                               s=100, seed=36963, squared="LRTonly")

work.neither.her <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Her College",], stat="all", 
                               with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                               s=100, seed=36963, squared="LRTonly")

work.neither.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                                with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                                s=100, seed=36963, squared="LRTonly")

work.him.her <- seqCompare(seq.work.ow[data$couple_educ_type=="Him College",],seq.work.ow[data$couple_educ_type=="Her College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

work.him.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Him College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                            with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                            s=100, seed=36963, squared="LRTonly")

work.her.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Her College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                            with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                            s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Housework
# ~~~~~~~~~~~~~~~
hw.neither.him <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Him College",], stat="all", 
                             with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                             s=100, seed=36963, squared="LRTonly")

hw.neither.her <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Her College",], stat="all", 
                             with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                             s=100, seed=36963, squared="LRTonly")

hw.neither.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                              with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                              s=100, seed=36963, squared="LRTonly")

hw.him.her <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Him College",],seq.hw.hrs[data$couple_educ_type=="Her College",], stat="all", 
                         with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                         s=100, seed=36963, squared="LRTonly")

hw.him.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Him College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

hw.her.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Her College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~
one.fam.df <- as.data.frame(one.fam)
one.work.df <- as.data.frame(one.work)
one.hw.df <- as.data.frame(one.hw)
fam.neither.him.df <- as.data.frame(fam.neither.him)
fam.neither.her.df <- as.data.frame(fam.neither.her)
fam.neither.both.df <- as.data.frame(fam.neither.both)
fam.him.her.df <- as.data.frame(fam.him.her)
fam.him.both.df <- as.data.frame(fam.him.both)
fam.her.both.df <- as.data.frame(fam.her.both)
work.neither.him.df <- as.data.frame(work.neither.him)
work.neither.her.df <- as.data.frame(work.neither.her)
work.neither.both.df <- as.data.frame(work.neither.both)
work.him.her.df <- as.data.frame(work.him.her)
work.him.both.df <- as.data.frame(work.him.both)
work.her.both.df <- as.data.frame(work.her.both)
hw.neither.him.df <- as.data.frame(hw.neither.him)
hw.neither.her.df <- as.data.frame(hw.neither.her)
hw.neither.both.df <- as.data.frame(hw.neither.both)
hw.him.her.df <- as.data.frame(hw.him.her)
hw.him.both.df <- as.data.frame(hw.him.both)
hw.her.both.df <- as.data.frame(hw.her.both)

combined_results <- rbind(fam.neither.him.df, fam.neither.her.df, fam.neither.both.df, 
                          fam.him.her.df, fam.him.both.df, fam.her.both.df,
                          work.neither.him.df, work.neither.her.df, work.neither.both.df, 
                          work.him.her.df, work.him.both.df, work.her.both.df,
                          hw.neither.him.df, hw.neither.her.df, hw.neither.both.df, 
                          hw.him.her.df, hw.him.both.df, hw.her.both.df,
                          one.fam.df,one.work.df,one.hw.df)

combined_results <- cbind(comparison = c("Fam: Neither v. Him", "Fam: Neither v. Her",
                                         "Fam: Neither v. Both", "Fam: Him v. Her",
                                         "Fam: Him v. Both", "Fam: Her v. Both",
                                         "Work: Neither v. Him", "Work: Neither v. Her",
                                         "Work: Neither v. Both", "Work: Him v. Her",
                                         "Work: Him v. Both", "Work: Her v. Both",
                                         "HW: Neither v. Him", "HW: Neither v. Her",
                                         "HW: Neither v. Both", "HW: Him v. Her",
                                         "HW: Him v. Both", "HW: Her v. Both",
                                         "One College: Fam", "One College:Work", 
                                         "One College: HW"), combined_results)

write_xlsx(combined_results, "G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_BIC_LRT_tests.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Think it'd be cool to also look at sequence METRICS by group
# (e.g. volatility, those integrative potential, etc.)
# Think this would add value to above
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Metrics by individual channel (these don't have the valence ones yet)
seqindic.fam<-seqindic(seq.fam, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                        "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.work<-seqindic(seq.work.ow, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                             "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.hw<-seqindic(seq.hw.hrs, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                          "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add to data frame and create object to export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(seqindic.fam)  <- paste0("fam_",  tolower(names(seqindic.fam)))
names(seqindic.work) <- paste0("work_", tolower(names(seqindic.work)))
names(seqindic.hw)   <- paste0("hw_",   tolower(names(seqindic.hw)))

data <- bind_cols(data,
                   seqindic.fam,
                   seqindic.work,
                   seqindic.hw)

# so, this just makes a smaller table with these columns
desctable <- data %>%
select(couple_educ_type,
         starts_with("fam_"),
         starts_with("work_"),
         starts_with("hw_"))

# oh, this is just a new column
# desctable$educ <- as.character(desctable$couple_educ_type)

sumtable(desctable, digits = 4, fixed.digits = TRUE, numformat = NA, group = 'couple_educ_type', group.test = TRUE, 
         out='csv', file="G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid_educ_desctable_truncated.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at relationship transitions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Marriage - gah, this is hard bc will have to do for all #s of kids?
data$time_m0 <- seqfpos(seq.fam, state="MARc0")
data$time_m1 <- seqfpos(seq.fam, state="MARc1")
data$time_m2 <- seqfpos(seq.fam, state="MARc2")
data$time_m3 <- seqfpos(seq.fam, state="MARc3")

data$time_marriage <- with(data, pmin(time_m0, time_m1, time_m2, time_m3, na.rm = TRUE)) 

# Childbearing
data$time_child.mar <- seqfpos(seq.fam, state="MARc1")
data$time_child.coh <- seqfpos(seq.fam, state="COHc1")

data$time_child <- with(data, pmin(time_child.mar, time_child.coh, na.rm = TRUE)) 

# want to do integral potential of each category
# Marriage
data$fam_integr_marr <- seqipos(seq.fam, dss=NULL, pos.states=c('MARc0','MARc1','MARc2','MARc3'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Cohabitation
data$fam_integr_coh <- seqipos(seq.fam, dss=NULL, pos.states=c('COHc0','COHc1','COHc2','COHc3'), 
                               neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore valence-based metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

######## Considering equality "positive"

## Work
posindic.work<-seqindic(seq.work.ow, indic=c("ppos", "nvolat", "integr"),
                        ipos.args=list(pos.states=c('dualFT','dualFT-anyOW')))

data$work_ppos <- posindic.work$Ppos
data$work_nvolat <- posindic.work$Nvolat
data$work_integr <- posindic.work$Integr


# want to do integral potential of each category
# Dual FT
data$work_integr_dual <- seqipos(seq.work.ow, dss=NULL, pos.states=c('dualFT','dualFT-anyOW'), 
                                 neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Male BW
data$work_integr_mbw <- seqipos(seq.work.ow, dss=NULL, pos.states=c('MBW','1.5MBW'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Female BW
data$work_integr_fbw <- seqipos(seq.work.ow, dss=NULL, pos.states=c('FBW'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Underwork
data$work_integr_under <- seqipos(seq.work.ow, dss=NULL, pos.states=c('underWK'), 
                                  neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)



    ## something else I want to explore, you can reweight the integrative potential by recency
    ## I wonder if something like *that* can account better for people consistently in a state v. transition into a state and stay
    data$w_integr_dual_wgt <- seqipos(seq.work.ow, dss=NULL, pos.states=c('dualFT','dualFT-anyOW'), 
                                     neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

    data$w_integr_mbw_wgt <- seqipos(seq.work.ow, dss=NULL, pos.states=c('MBW','1.5MBW'), 
                                      neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

## Housework
posindic.hw<-seqindic(seq.hw.hrs, indic=c("ppos", "nvolat", "integr"),
                      ipos.args=list(pos.states=c('equal:high', 'equal:low')))

#state.hw.hrs <- c('W-most:high', 'W-most:low',
#                  'equal:high', 'equal:low', 'M-most:all')

data$hw_ppos <- posindic.hw$Ppos
data$hw_nvolat <- posindic.hw$Nvolat
data$hw_integr <- posindic.hw$Integr

# want to do integral potential of each category
# Equal
data$hw_integr_eq <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('equal:high', 'equal:low'), 
                             neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Her Most or All
data$hw_integr_her <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('W-most:high','W-most:low'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Him Most or All
data$hw_integr_him <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('M-most:all'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

    ## Try reweighting here
    data$hw_integr_eq_wgt <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('equal:high', 'equal:low'), 
                             neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

    data$hw_integr_her_wgt <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('W-most:high','W-most:low'), 
                                  neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)
  
    educ_integr_test <- subset(data,
                               select = c(couple_educ_type,
                                          work_integr_dual, work_integr_mbw,
                                          w_integr_dual_wgt, w_integr_mbw_wgt,
                                          hw_integr_eq, hw_integr_her,
                                          hw_integr_eq_wgt, hw_integr_her_wgt)
    )
    
    sumtable(educ_integr_test, group = 'couple_educ_type') ## Okay so this is VERY interesting, because it actually narrows the education differences between neither / both
    # bc more specialized at end, it actually makes it HIGHER for both college (but not true of dual FT because less educated have OTHER arrangements like underwork)

######## Attemption degradation - seems cool because quantifies movement between "good" and "bad" states
######## so again thinking egal / specialized

## Work

# go from most positive to least positive
work.state.order=c('dualFT','dualFT-anyOW','1.5MBW','MBW')

# I did neg before - but thinking - do I want to either distinguish
# between neg and pos (to show neg happens more?)
# or - what does both do? (does it exacerbate negative?)
# I actually think all are useful for different reasons, so let's keep all
data$work_degrad_neg <- seqidegrad(seq.work.ow, 
                                   state.order=work.state.order,
                                   penalized="NEG")

data$work_degrad_pos <- seqidegrad(seq.work.ow, 
                                   state.order=work.state.order,
                                   penalized="POS")

data$work_degrad_both <- seqidegrad(seq.work.ow, 
                                    state.order=work.state.order,
                                    penalized="BOTH")

## Housework

# go from most positive to least positive
hw.state.order=c('equal:low','equal:high','W-most:low','W-most:high')

data$hw_degrad_neg <- seqidegrad(seq.hw.hrs, 
                                 state.order=hw.state.order,
                                 penalized="NEG")

data$hw_degrad_pos <- seqidegrad(seq.hw.hrs, 
                                 state.order=hw.state.order,
                                 penalized="POS")

data$hw_degrad_both <- seqidegrad(seq.hw.hrs, 
                                  state.order=hw.state.order,
                                  penalized="BOTH")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export metrics at overall level jic
# (should match original paper?)
# This is actually better list and gets all together?
# Going to just use this for by education also
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

educ_metric_table <- subset(data,
                       select = c(couple_educ_type,
                                  fam_lgth, fam_nonm, fam_dlgth, fam_visited, 
                                  fam_recu, fam_trans, fam_transp, fam_entr, 
                                  fam_meand, fam_volat, fam_cplx, fam_turb2n, 
                                  work_lgth, work_nonm, work_dlgth, work_visited, 
                                  work_recu, work_trans, work_transp, work_entr, 
                                  work_meand, work_volat, work_cplx, work_turb2n, 
                                  hw_lgth, hw_nonm, hw_dlgth,
                                  hw_visited, hw_recu, hw_trans, hw_transp,
                                  hw_entr, hw_meand, hw_volat, hw_cplx, hw_turb2n,
                                  time_marriage, time_child, time_child.mar,
                                  time_child.coh, fam_integr_marr, fam_integr_coh,
                                  work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
                                  work_integr_fbw, work_integr_under, work_degrad_neg,
                                  work_degrad_pos, work_degrad_both, hw_ppos,
                                  hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
                                  hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
)

sumtable(educ_metric_table, digits = 4, numformat = NA, group = 'couple_educ_type', group.test = TRUE, 
         out='csv', file="G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid_educ_metrics_truncated.csv")


#overall_metrics <- subset(data,
#                          select = c(fam_lgth, fam_lgth_nomiss, fam_spells, fam_states, 
#                                     fam_recu, fam_trans, fam_transp, fam_entr, 
#                                     fam_meand, fam_volat, fam_cplx, fam_turb2n, 
#                                     work_lgth, work_lgth_nomiss, work_spells, work_states, 
#                                     work_recu, work_trans, work_transp, work_entr, 
#                                     work_meand, work_volat, work_cplx, work_turb2n, 
#                                     hw_lgth, hw_lgth_nomiss, hw_spells,
#                                     hw_states, hw_recu, hw_trans, hw_transp,
#                                     hw_entr, hw_meand, hw_volat, hw_cplx, hw_turb2n,
#                                     time_marriage, time_child, time_child.mar,
#                                     time_child.coh, fam_integr_marr, fam_integr_coh,
#                                     work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
#                                     work_integr_fbw, work_integr_under, work_degrad_neg,
#                                     work_degrad_pos, work_degrad_both, hw_ppos,
#                                     hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
#                                     hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
#)


#sumtable(overall_metrics, digits = 4, fixed.digits = TRUE, numformat = NA, 
#         out='csv', file='results/PSID/tables/PSID_Overall_Sequence_Metrics.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Okay TRY this implicative statistics?
# I think I can actually set missing to TRUE OR FALSE - which might get over my concerns?
# Because this seems to use SEQ object NOT Diss Matrix
# Let's prob explore both
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sequence of typical states
implic.fam.nomiss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                        weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.fam.miss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.miss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                             weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.miss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                             weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

##Plotting the typical states
x_lab <- c("1","2","3","4","5","6","7","8","9","10")

plot(implic.fam.nomiss, lwd=3, conf.level=c(0.95, 0.99))
plot(implic.fam.miss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab) ## okay, this actually is OKAY and actually probably BETTER HIGHLIGHTS the dissolution
# OR maybe we use that for FAM state because really dissolution is a family state and then remove from other graphs? Let's see...

plot(implic.work.nomiss, lwd=2, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.work.miss, lwd=2, conf.level=c(0.95, 0.99))

plot(implic.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.hw.miss, lwd=3, conf.level=c(0.95, 0.99))

## Test just binary education
implic.one.fam.miss <- seqimplic(seq.fam, group=data$one_college, with.missing = TRUE,  
                             weighted = FALSE, na.rm = TRUE)

implic.one.work.nomiss <- seqimplic(seq.work.ow, group=data$one_college, with.missing = FALSE,
                                weighted = FALSE, na.rm = TRUE)

implic.one.hw.nomiss <- seqimplic(seq.hw.hrs, group=data$one_college, with.missing = FALSE,
                              weighted = FALSE, na.rm = TRUE)

plot(implic.one.fam.miss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.one.work.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.one.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

##Plotting the typical states (Example from Fauser to make pretty maybe) ##
# need to revisit this.
tiff("Z:/your path/graphs/Figure2.tif", width = 4800,
     height = 3600, units = "px", res = 300, compression = "lzw")
plot(implA, lwd=2.5, ylim=c(0,9), conf.level=c(0.95, 0.99, 0.999), 
     sub = "Age of child in months", cex.legend = 2, cex.main=2, 
     cex.axis = 1.5,  cex.lab = 1.5, cex.sub = 1.5)
dev.off()

