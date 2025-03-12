# ---------------------------------------------------------------------
#    Program: 00a_unique-sequences.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: March 2025
#    Modified: March 7 2025
#    Goal: identify unique sequences to use for cluster analysis (Rather than all sequences)
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

#note to put this on github otherwise this script is not usable for Kim. Think I updated this below?
#.libPaths("G:/My Drive/R Library") #leas library

options(repos=c(CRAN="https://cran.r-project.org"))

# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'
kim.server <- '/home/kmcerlea/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim); .libPaths("G:/Other computers/My Laptop/Documents/R/R library") }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea); .libPaths("G:/My Drive/R Library")  }
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","WeightedCluster")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","WeightedCluster")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","WeightedCluster")
  
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","WeightedCluster")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import previously created sequence information ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("created data/ukhls/ukhls_setupsequence.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sequence objects for reference

# Couple Paid Work - no OW
# seq.work <- seqdef(data[,col_work], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

# Couple Paid Work - OW
# seq.work.ow <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

# Couple HW - no amounts
# seq.hw <- seqdef(data[,col_hw], cpal = colspace.hw, labels=longlab.hw, states= shortlab.hw)

# Couple HW - amounts v1
# seq.hw.hrs <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, states= shortlab.hw.hrs)

# Couple HW - amounts v2
# seq.hw.hrs.alt <- seqdef(data[,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt, labels=longlab.hw.hrs.alt, 
#                      states= shortlab.hw.hrs.alt)

# Family channel
# seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get unique sequence information
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Couple Paid Work - no OW
ac.work <- wcAggregateCases(data[,col_work])
ac.work

# Couple Paid Work - OW
ac.work.ow <- wcAggregateCases(data[,col_work.ow])
ac.work.ow

# Couple HW - no amounts
ac.hw <- wcAggregateCases(data[,col_hw])
ac.hw

# Couple HW - amounts v1
ac.hw.hrs <- wcAggregateCases(data[,col_hw.hrs])
ac.hw.hrs

# Couple HW - amounts v2
ac.hw.hrs.alt <- wcAggregateCases(data[,col_hw.hrs.alt])
ac.hw.hrs.alt

# Family channel
ac.fam <- wcAggregateCases(data[,col_fam])
ac.fam

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create unique sequence objects
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Couple Paid Work - no OW
unique.work <- seqdef(data[ac.work$aggIndex,col_work],cpal = colspace.work, labels=longlab.work, states= shortlab.work)

# ggseqdplot(seq.work) +
#  scale_x_discrete(labels = 1:10) +
#  labs(x = "Year")

# these look quite different, but I guess that is expected?
# ggseqdplot(unique.work) +
#  scale_x_discrete(labels = 1:10) +
#  labs(x = "Year")

# Couple Paid Work - OW
unique.work.ow <- seqdef(data[ac.work.ow$aggIndex,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

# Couple HW - no amounts
unique.hw <- seqdef(data[ac.hw$aggIndex,col_hw], cpal = colspace.hw, labels=longlab.hw, states= shortlab.hw)

# Couple HW - amounts v1
unique.hw.hrs <- seqdef(data[ac.hw.hrs$aggIndex,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, states= shortlab.hw.hrs)

# Couple HW - amounts v2
unique.hw.hrs.alt <- seqdef(data[ac.hw.hrs.alt$aggIndex,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt, labels=longlab.hw.hrs.alt, 
                     states= shortlab.hw.hrs.alt)

# Family channel
unique.fam <- seqdef(data[ac.fam$aggIndex,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM distance matrices for each domain ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist.unique.work <- seqdist(unique.work, method="OM", indel=1, sm= "CONSTANT")
dist.unique.work.ow <- seqdist(unique.work.ow, method="OM", indel=1, sm= "CONSTANT")

dist.unique.hw <- seqdist(unique.hw, method="OM", indel=1, sm= "CONSTANT")
dist.unique.hw.hrs <- seqdist(unique.hw.hrs, method="OM", indel=1, sm= "CONSTANT")
dist.unique.hw.hrs.alt <- seqdist(unique.hw.hrs.alt, method="OM", indel=1, sm= "CONSTANT")

dist.unique.fam <- seqdist(unique.fam, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/ukhls/ukhls_unique-sequences.RData")
