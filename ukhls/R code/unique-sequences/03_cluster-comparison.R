# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 7 2025
#    Goal: compare clusters for SC v. MC solution
#    (weighting the UKHLS unique sequences)
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load sequence states and distance matrices created in step 00a ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load ("created data/ukhls/ukhls_unique-sequences.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create multi-channel sequences so can get unique sequences ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Following: 
# https://stackoverflow.com/questions/79245090/how-to-use-weightedcluster-to-aggregate-sequences-and-apply-on-multichannel-sequ

# Simple
channels<-(list(work=seq.work, hw=seq.hw, fam=seq.fam))
mc.simp<-seqMD(channels)
alphabet(mc.simp)
plot(mc.simp)

ac.mc.simp <- wcAggregateCases(mc.simp)
print(ac.mc.simp)

unique.mc.simp <- list(work=seq.work[ac.mc.simp$aggIndex, ], hw=seq.hw[ac.mc.simp$aggIndex, ], 
                       fam=seq.fam[ac.mc.simp$aggIndex, ])

mcdist.simp.om <- seqdistmc(unique.mc.simp, method="OM", indel=1, sm="CONSTANT") 

# Detailed
mc.det<-seqMD(channels=list(work=seq.work.ow, hw=seq.hw.hrs.alt, fam=seq.fam))
alphabet(mc.det)

ac.mc.det <- wcAggregateCases(mc.det)
ac.mc.det

unique.mc.det <- list(work=seq.work.ow[ac.mc.det$aggIndex, ], hw=seq.hw.hrs.alt[ac.mc.det$aggIndex, ], 
                       fam=seq.fam[ac.mc.det$aggIndex, ])

mcdist.det.om <- seqdistmc(unique.mc.det, method="OM", indel=1, sm="CONSTANT") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparatory work required for rendering the plot ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the combined clustering

## More detailed sequence alphabets, with weights
mcdist.det.wt.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15,
                                 weights = ac.mc.det$aggWeights)

mc.det.wt.val<-mcdist.det.wt.pam[[4]]

mc.det.wt.asw <- mc.det.wt.val[,4]

mc.det.wt.r2 <- mc.det.wt.val[,7]

## Simpler sequence alphabets, with weights
mcdist.simp.wt.pam <- wcKMedRange(mcdist.simp.om, 
                                  kvals = 2:15,
                                  weights = ac.mc.simp$aggWeights)

mc.simp.wt.val<-mcdist.simp.wt.pam[[4]]

mc.simp.wt.asw <- mc.simp.wt.val[,4]

mc.simp.wt.r2 <- mc.simp.wt.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of family trajectories
fam.pam.wt.test <- wcKMedRange(dist.unique.fam, 
                               kvals = 2:15,
                               weights = ac.fam$aggWeights)

fam.wt.val<-fam.pam.wt.test[[4]]

fam.wt.asw <- fam.wt.val[,4]

fam.wt.r2 <- fam.wt.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of paid labor trajectories

## No Overwork
work.pam.wt.test <- wcKMedRange(dist.unique.work, 
                            kvals = 2:15,
                            weights = ac.work$aggWeights)

work.wt.val<-work.pam.wt.test[[4]]

work.wt.asw <- work.wt.val[,4]

work.wt.r2 <- work.wt.val[,7]

## With Overwork
work.ow.pam.wt.test <- wcKMedRange(dist.unique.work.ow, 
                             kvals = 2:15,
                             weights = ac.work.ow$aggWeights)

work.ow.wt.val<-work.ow.pam.wt.test[[4]]

work.ow.wt.asw <- work.ow.wt.val[,4]

work.ow.wt.r2 <- work.ow.wt.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of housework trajectories

## No hours
hw.pam.wt.test <- wcKMedRange(dist.unique.hw, 
                           kvals = 2:15,
                           weights = ac.hw$aggWeights)

hw.wt.val<-hw.pam.wt.test[[4]]

hw.wt.asw <- hw.wt.val[,4]

hw.wt.r2 <- hw.wt.val[,7]

## V1
hw.hrs.pam.wt.test <- wcKMedRange(dist.unique.hw.hrs, 
                            kvals = 2:15,
                            weights = ac.hw.hrs$aggWeights)

hw.hrs.wt.val<-hw.hrs.pam.wt.test[[4]]

hw.hrs.wt.asw <- hw.hrs.wt.val[,4]

hw.hrs.wt.r2 <- hw.hrs.wt.val[,7]

## V2
hw.hrs.alt.pam.wt.test <- wcKMedRange(dist.unique.hw.hrs.alt, 
                               kvals = 2:15,
                               weights = ac.hw.hrs.alt$aggWeights)

hw.hrs.alt.wt.val<-hw.hrs.alt.pam.wt.test[[4]]

hw.hrs.alt.wt.asw <- hw.hrs.alt.wt.val[,4]

hw.hrs.alt.wt.r2 <- hw.hrs.alt.wt.val[,7]

# Save here in case images fail
save.image("created data/ukhls/ukhls_cluster-comparison.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/Fig5-1_UKHLS.pdf",
          width=15,
          height=20)

layout.fig1 <- layout(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2, byrow = TRUE),
                      heights = c(1,1,1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# MCSA: Detailed
plot(x, mc.det.wt.asw, type = "b", frame = FALSE, pch = 19, main="(1a) MCSA: Detailed Sequences", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.det.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# MCSA: Simple
plot(x, mc.simp.wt.asw, type = "b", frame = FALSE, pch = 19, main="(1b) MCSA: SImple Sequences", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.simp.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Paid Work Channel: With Overwork
plot(x, work.ow.wt.asw, type = "b", frame = FALSE, pch = 19, main="(2a) Paid Work (with Overwork)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, work.ow.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: no Overwork
plot(x, work.wt.asw, type = "b", frame = FALSE, pch = 19, main="(2b) Paid Work (no Overwork)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, work.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: Hours with Group-specific thresholds
plot(x, hw.hrs.wt.asw, type = "b", frame = FALSE, pch = 19, main="(3a) Housework (with Hours)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.hrs.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: No Hours
plot(x, hw.wt.asw, type = "b", frame = FALSE, pch = 19, main="(3b) Housework (No Hours)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Family channel
plot(x, fam.wt.asw, type = "b", frame = FALSE, pch = 19, main="(4) Family formation",
     col = "blue", xlab = "N. clusters", ylab = "ASW and R2 value", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, fam.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

dev.off()

# pdf convert doesn't work at the moment
# pdf_convert("results/Fig5-1_UKHLS.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare two paid work options)

pdf("results/Fig5-1_UKHLS_Work-Options.pdf",
          width=12,
          height=5)

layout.fig1 <- layout(matrix(c(1,2), 1, 2, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# Paid Work Channel: Option 1 (used in MCSA)
plot(x, work.ow.wt.asw, type = "b", frame = FALSE, pch = 19, main="(a) Paid Work (with Overwork)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, work.ow.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: Option 2
plot(x, work.wt.asw, type = "b", frame = FALSE, pch = 19, main="(b) Paid Work", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, work.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


dev.off()

# pdf_convert("results/Fig5-1_UKHLS_Work-Options.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS_Work-Options.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare three housework options)

pdf("results/Fig5-1_UKHLS_HW-Options.pdf",
          width=15,
          height=5)

layout.fig1 <- layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))


# Housework Channel: Option 1 (used in MCSA)
plot(x, hw.hrs.wt.asw, type = "b", frame = FALSE, pch = 19, main="(a) Housework Hours", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.hrs.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: Option 2
plot(x, hw.wt.asw, type = "b", frame = FALSE, pch = 19, main="(b) Housework (no Hours)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: Option 3
plot(x, hw.hrs.alt.wt.asw, type = "b", frame = FALSE, pch = 19, main="(c) Housework Hours (Option 2)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, hw.hrs.alt.wt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


dev.off()

# pdf_convert("results/Fig5-1_UKHLS_HW-Options.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS_HW-Options.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save.image("created data/ukhls/ukhls_cluster-comparison.RData")
