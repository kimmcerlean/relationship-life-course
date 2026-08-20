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
# First look at descriptive details about the sequences by education
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

## Multi-channel plots by education and parental status

## is it this easy to graph like this?! okay yes but okay quite hard to examine actually...
#seqIplot(mcsa, group = data$one_college,  sortv = "from.start", with.missing = FALSE) 
#seqdplot(mcsa, group = data$one_college, with.missing = FALSE, cex.legend=0.8, ncol=3)
#dev.new(width = 20, height = 5)
#par(mar = c(0,0,0,0))
#seqlegend(mcsa, ncol = 3, cex = 0.6, x.intersp = 0.1, y.intersp = 0.6)
#seqlegend(mcsa, ncol=5, cex=0.6)

#png("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/legend.png", width = 3000, height = 2000, res = 300)

#par(mar = c(0,0,0,0))
#seqlegend(mcsa, ncol = 5, cex = 0.6, x.intersp = 0.1)

#dev.off()

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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_4Groups.pdf",
    width=8,
    height=11)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          type="rf", diss=mcdist.det.min, group = data$couple_educ_type,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7)

dev.off()

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_Childfree.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_BecomeParents.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_AlwaysParents.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_Childfree_alt.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_BecomeParents_alt.pdf",
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

pdf("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/results/PSID_MCIndex_AlwaysParents_alt.pdf",
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

# Does this work in MC approach?
educ.diff.mcsa <- seqdiff(mcsa, data$couple_educ_type)
plot(educ.diff.mcsa, stat=c("Pseudo R2", "Levene"))

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
