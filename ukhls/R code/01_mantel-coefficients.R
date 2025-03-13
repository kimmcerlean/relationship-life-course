# ---------------------------------------------------------------------
#    Program: 01_mantel-coefficients.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 12 2025
#    Goal: run mantel coefficients on one imputed datasets
# --------------------------------------------------------------------
# --------------------------------------------------------------------

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
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse")
  
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse")
  
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
# Preliminary analysis for MCSA 
## set up sequence objects on one imputed dataset
## Compute standard OM distance matrices for each domain one imputed dataset
## Compute mantel coefficients across domains one imputed dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import created data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# Load previously created sequences
load("created data/ukhls/ukhls_setupsequence.RData")

# Import non-imputed data
data <- read_dta("created data/ukhls/ukhls_couples_imputed_wide.dta")
data <- data%>%filter(`_mi_m`== 1) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create sequences just with mi of 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
# Couple Paid Work - no OW
seq.work <- seqdef(data[,col_work], cpal = colspace.work, labels=longlab.work, states= shortlab.work)
 
# Couple Paid Work - OW
seq.work.ow <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

# Couple HW - no amounts
seq.hw <- seqdef(data[,col_hw], cpal = colspace.hw, labels=longlab.hw, states= shortlab.hw)

# Couple HW - amounts v1
seq.hw.hrs <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, states= shortlab.hw.hrs)

# Couple HW - amounts v2
seq.hw.hrs.alt <- seqdef(data[,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt, labels=longlab.hw.hrs.alt, 
                         states= shortlab.hw.hrs.alt)
# Family channel
seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM distance matrices for each domain ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist.work.om <- seqdist(seq.work, method="OM", indel=1, sm= "CONSTANT") ## , full.matrix="FALSE")
dist.work.ow.om <- seqdist(seq.work.ow, method="OM", indel=1, sm= "CONSTANT")

dist.hw.om <- seqdist(seq.hw, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.om <- seqdist(seq.hw.hrs, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.alt.om <- seqdist(seq.hw.hrs.alt, method="OM", indel=1, sm= "CONSTANT")

dist.fam.om <- seqdist(seq.fam, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute mantel coefficients across domains----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mantel_w.hw = mantel(dist.work.om, dist.hw.om)
mantel_ow.hw = mantel(dist.work.ow.om, dist.hw.om)

mantel_w.hw.hrs = mantel(dist.work.om, dist.hw.hrs.om)
mantel_ow.hw.hrs = mantel(dist.work.ow.om, dist.hw.hrs.om)

mantel_w.hw.hrs.alt = mantel(dist.work.om, dist.hw.hrs.alt.om)
mantel_ow.hw.hrs.alt = mantel(dist.work.ow.om, dist.hw.hrs.alt.om)

mantel_w.fam = mantel(dist.work.om, dist.fam.om)
mantel_ow.fam = mantel(dist.work.ow.om, dist.fam.om)
mantel_hw.fam = mantel(dist.hw.om, dist.fam.om)
mantel_hw.hrs.fam = mantel(dist.hw.hrs.om, dist.fam.om)
mantel_hw.hrs.alt.fam = mantel(dist.hw.hrs.alt.om, dist.fam.om)

mantel_w.hw$statistic
mantel_w.hw$signif

mantel_ow.hw$statistic
mantel_ow.hw$signif

mantel_w.hw.hrs$statistic
mantel_w.hw.hrs$signif

mantel_ow.hw.hrs$statistic
mantel_ow.hw.hrs$signif

mantel_w.hw.hrs.alt$statistic
mantel_w.hw.hrs.alt$signif

mantel_ow.hw.hrs.alt$statistic
mantel_ow.hw.hrs.alt$signif

mantel_w.fam$statistic
mantel_w.fam$signif

mantel_ow.fam$statistic
mantel_ow.fam$signif

mantel_hw.fam$statistic
mantel_hw.fam$signif

mantel_hw.hrs.fam$statistic
mantel_hw.hrs.fam$signif

mantel_hw.hrs.alt.fam$statistic
mantel_hw.hrs.alt.fam$signif

## create and export data frame with mantel coefficients
association <- c('w.hw', 'ow.hw', 'w.hw.hrs', 'ow.hw.hrs', 'w.hw.hrs.alt', 'ow.hw.hrs.alt',
                 'w.fam', 'ow.fam', 'hw.fam', 'hw.hrs.fam', 'hw.hrs.alt.fam')
stats <- c(mantel_w.hw$statistic, mantel_ow.hw$statistic, mantel_w.hw.hrs$statistic,
           mantel_ow.hw.hrs$statistic, mantel_w.hw.hrs.alt$statistic, mantel_ow.hw.hrs.alt$statistic,
           mantel_w.fam$statistic, mantel_ow.fam$statistic, mantel_hw.fam$statistic,
           mantel_hw.hrs.fam$statistic,mantel_hw.hrs.alt.fam$statistic)
sig <- c(mantel_w.hw$signif, mantel_ow.hw$signif, mantel_w.hw.hrs$signif,
         mantel_ow.hw.hrs$signif, mantel_w.hw.hrs.alt$signif, mantel_ow.hw.hrs.alt$signif,
         mantel_w.fam$signif, mantel_ow.fam$signif, mantel_hw.fam$signif,
         mantel_hw.hrs.fam$signif,mantel_hw.hrs.alt.fam$signif)
mantel <- data.frame(association, stats, sig)

print(mantel)

write.xlsx(mantel, "results/UKHLS/ukhls_mantel_coefficients_mi1.xlsx")

## Save
save.image("created data/ukhls/ukhls_mantel_mi1.RData")
