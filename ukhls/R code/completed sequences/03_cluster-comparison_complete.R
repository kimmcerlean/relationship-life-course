# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: April 23 2025
#    Goal: compare clusters for SC v. MC solution - complete sequences only
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
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
# Skipping steps 1 and 2 for now
# (step 2 has a lot of duplicated steps anyway)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load ("created data/ukhls/ukhls_setupsequence-complete.RData") ## This loads the sequences so we don't need to recreate

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM distance matrices for each domain ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist.work.ow.om <- seqdist(seq.work.ow, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.alt.om <- seqdist(seq.hw.hrs.alt, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om <- seqdist(seq.fam, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparatory work required for rendering the plot ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the combined clustering

## More detailed sequence alphabets
mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam), ## Seq states NOT om matrix
                           method="OM", indel=1, sm="CONSTANT") 

mcdist.det.om.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15)

mc.det.val<-mcdist.det.om.pam[[4]]

mc.det.asw <- mc.det.val[,4]

mc.det.r2 <- mc.det.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of family trajectories

fam.pam.test <- wcKMedRange(dist.fam.om, 
                            kvals = 2:15)

fam.val<-fam.pam.test[[4]]

fam.asw <- fam.val[,4]

fam.r2 <- fam.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of paid labor trajectories
## With Overwork
work.ow.pam.test <- wcKMedRange(dist.work.ow.om, 
                             kvals = 2:15)

work.ow.val<-work.ow.pam.test[[4]]

work.ow.asw <- work.ow.val[,4]

work.ow.r2 <- work.ow.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of housework trajectories

## V2
hw.hrs.alt.pam.test <- wcKMedRange(dist.hw.hrs.alt.om, 
                               kvals = 2:15)

hw.hrs.alt.val<-hw.hrs.alt.pam.test[[4]]

hw.hrs.alt.asw <- hw.hrs.alt.val[,4]

hw.hrs.alt.r2 <- hw.hrs.alt.val[,7]

save.image("created data/ukhls/ukhls_cluster-comparison-complete.RData")
# in case it fails here

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/UKHLS/UKHLS_cluster_comparison_complete_sequences.pdf", # doesn't need to be cairo so removed for now (might not work)
    width=20,
    height=10)

layout.fig1 <- layout(matrix(c(1,2,3,4), nrow=1, ncol=1, byrow = TRUE),
                      heights = c(1,1,1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# MCSA: Detailed
plot(x, mc.det.asw, type = "b", frame = FALSE, pch = 19, main="(1a) MCSA: Detailed Sequences", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.det.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: With Overwork
plot(x, work.ow.asw, type = "b", frame = FALSE, pch = 19, main="(2a) Paid Work (with Overwork)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, work.ow.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Housework Channel: Hours with Group-specific thresholds
plot(x, hw.hrs.asw, type = "b", frame = FALSE, pch = 19, main="(3a) Housework (with Hours)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.hrs.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Family channel
plot(x, fam.asw, type = "b", frame = FALSE, pch = 19, main="(4) Family formation",
     col = "blue", xlab = "N. clusters", ylab = "ASW and R2 value", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, fam.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

dev.off()

# pdf convert doesn't work, so just will export pdf
# pdf_convert("results/Fig5-1_UKHLS.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save.image("created data/ukhls/ukhls_cluster-comparison-complete.RData")
