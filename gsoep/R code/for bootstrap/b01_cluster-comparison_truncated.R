# ---------------------------------------------------------------------
#    Program: cluster-comparison_truncated
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: October 8 2026
#    Goal: compare clusters for SC v. MC solution,
#     all sequences, including truncated (GSOEP)
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'G:/My Drive/(2) McErlean and Pessin - Life-course division of labor/' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
# kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'
kim.server <- '/home/kmcerlea/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim); .libPaths("G:/Other computers/My Laptop/Documents/R/R library") }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea); .libPaths("G:/My Drive/Software/R Library")  }
if (Sys.getenv(c("HOME" )) == "/home/lpessin") { setwd(lea.server) }
if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") { setwd(kim.server) }
getwd() # check it worked

# ~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~

# load and install packages for whomever is running the script
## the server doesn't let you install packages
## the server doesn't have ggseqplot for now (package incompatibility issue)

if (Sys.getenv(c("HOME" )) == "/home/lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "gridExtra",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "gridExtra",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "gridExtra",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "gridExtra",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load sequences created in step 00 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("temp data/gsoep/gsoep-setupsequence-test-data.RData")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now, turn to multichannel sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Preparatory work required for rendering the plot


# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the combined clustering

mcsa<-seqMD(channels=list(seq.work, seq.hw.hrs, seq.fam),
            with.missing=TRUE,
            what="MDseq") ##, right=NA)
seqlength(mcsa)
seqlength(mcsa, with.missing = FALSE) # okay, this isn't working, 
# but all sequences are same length across domains, so can just use 1 domain

## Now create multi-channel distance
mcdist.det.om <- seqdistmc(channels=list(seq.work, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                           method="OM", 
                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                           with.missing=TRUE) 

## Divide by length matrix (created in step 0)
mcdist.det.min <- mcdist.det.om / fam.min.len

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intermission to follow this:
# https://sequenceanalysis.org/2023/10/19/validating-sequence-analysis-typologies-using-parametric-bootstrap/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## These come from step 2, but the example uses hclust
# Hierarchical cluster analysis, non-squared dissimilarities
mc.det.ward1 <- hclust(as.dist(mcdist.det.min), 
                       method = "ward.D")

clustqual <- as.clustrange(mc.det.ward1, diss=mcdist.det.min, ncluster=10)
clustqual
# I am using obviously not full data but these really don't match real output.
# let's see if match below

# Apply PAM clustering + Ward starting point (this is what we ultimately use)
# Ward's clustering is explained in Chapter 4
mcdist.om.pam.ward <- wcKMedRange(mcdist.det.min, kvals = 2:10,
                                initialclust = mc.det.ward1)

mc.min.val.ward<-mcdist.om.pam.ward[[4]]
print(mc.min.val.ward)
# This matches below

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get metrics to compare across cluster solutions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Now, go through next steps - but use the normalized matrix instead
mcdist.det.min.pam <- wcKMedRange(mcdist.det.min, 
                                 kvals = 2:15)

mc.min.val<-mcdist.det.min.pam[[4]] # see, this is the full list
# We just plot two, but this table has ALL
print(mc.min.val) # these metrics DO match other output

clustqual2 <- as.clustrange(mcdist.det.min.pam$clustering, diss=mcdist.det.min, ncluster=10)
clustqual2
# okay, THIS works

clustqual3 <- as.clustrange(mcdist.om.pam.ward$clustering, diss=mcdist.det.min, ncluster=10)
clustqual3

mc.min.asw <- mc.min.val[,4]

mc.min.r2 <- mc.min.val[,7]

## Compare to original

mcdist.det.om.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15)

mc.om.val<-mcdist.det.om.pam[[4]]

mc.om.asw <- mc.om.val[,4]

mc.om.r2 <- mc.om.val[,7]

## Now, get the truncated sequence information to compare
# Paid work
work.pam <- wcKMedRange(dist.work.min, 
                                kvals = 2:15)

work.val<-work.pam[[4]]

work.asw <- work.val[,4]

work.r2 <- work.val[,7]

# Family
fam.pam <- wcKMedRange(dist.fam.min, 
                            kvals = 2:15)

fam.val<-fam.pam[[4]]

fam.asw <- fam.val[,4]

fam.r2 <- fam.val[,7]


# Housework
hw.hrs.pam <- wcKMedRange(dist.hw.min, 
                                     kvals = 2:15)

hw.hrs.val<-hw.hrs.pam[[4]]

hw.hrs.asw <- hw.hrs.val[,4]

hw.hrs.r2 <- hw.hrs.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save for later ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("temp data/gsoep/gsoep_cluster-comparison-test-data.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exploring that code
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Is this going to work with NORMALIZED Matrix? Maybe *this* is the problem?
# Converting multichannel data to single-channel data
#sc_data <- mc_to_sc_data(list(marr_seq, child_seq, left_seq))

# 10 combined states
#alphabet(sc_data)

# Can I get combined cost structure?
#cost_output <- seqdistmc(channels=list(seq.work, seq.hw.hrs, seq.fam), what = "cost")
cost_output <- seqdistmc(channels=list(seq.work, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                         method="OM", 
                         indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                         sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                         with.missing=TRUE, what="cost") 
print(cost_output)
view(cost_output)

# Core problem here is this is just the SEQUENCE OBJECT and we really need the DISTANCE MATRIX
#bcq.combined <- seqnullcqi(mcsa, clustqual2, R=100, model="combined", 
#                           seqdist.args=list(channels=list(seq.work, seq.hw.hrs, seq.fam),
                                              #method="OM", 
                                             #indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                                             #sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                                             #with.missing=TRUE), 
                           #hclust.method="ward.D", kmedoid = TRUE,
                           #parallel = FALSE, progressbar=TRUE)


#mcdist.det.om <- seqdistmc(channels=list(seq.work, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
#                           method="OM", 
#                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
#                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
#                           with.missing=TRUE) 