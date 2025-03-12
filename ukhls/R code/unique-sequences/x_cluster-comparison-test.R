# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 7 2025
#    Goal: compare clusters for SC v. MC solution (just using unique sequences)
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
# are these the same? Confirmed yes
mcdist.simp.om2 <- seqMD(unique.mc.simp, method="OM", indel=1, sm="CONSTANT", what="diss") 

# Detailed
mc.det<-seqMD(channels=list(work=seq.work.ow, hw=seq.hw.hrs.alt, fam=seq.fam))
alphabet(mc.det)

ac.mc.det <- wcAggregateCases(mc.det)
ac.mc.det

unique.mc.det <- list(work=seq.work.ow[ac.mc.det$aggIndex, ], hw=seq.hw.hrs.alt[ac.mc.det$aggIndex, ], 
                      fam=seq.fam[ac.mc.det$aggIndex, ])

mcdist.det.om <- seqdistmc(unique.mc.det, method="OM", indel=1, sm="CONSTANT") 
# are these the same?
mcdist.det.om2 <- seqMD(unique.mc.det, method="OM", indel=1, sm="CONSTANT", what="diss") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparatory work required for rendering the plot ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the combined clustering

## More detailed sequence alphabets: v1
mcdist.det.om.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15)

mc.det.val<-mcdist.det.om.pam[[4]]

mc.det.asw <- mc.det.val[,4]

mc.det.r2 <- mc.det.val[,7]

## More detailed sequence alphabets: v1 with weights
mcdist.det.wt.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15,
                                 weights = ac.mc.det$aggWeights)

mc.det.wt.val<-mcdist.det.wt.pam[[4]]

mc.det.wt.asw <- mc.det.wt.val[,4]

mc.det.wt.r2 <- mc.det.wt.val[,7]

## More detailed sequence alphabets: v2
mcdist.det2.om.pam <- wcKMedRange(mcdist.det.om2, 
                                  kvals = 2:15)

mc.det2.val<-mcdist.det2.om.pam[[4]]

mc.det2.asw <- mc.det2.val[,4]

mc.det2.r2 <- mc.det2.val[,7]

# save.image("created data/ukhls/ukhls_cluster-comparison-test.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of family trajectories

fam.pam.test <- wcKMedRange(dist.unique.fam, 
                            kvals = 2:15)

fam.val<-fam.pam.test[[4]]

fam.asw <- fam.val[,4]

fam.r2 <- fam.val[,7]

# with weights
fam.pam.wt.test <- wcKMedRange(dist.unique.fam, 
                            kvals = 2:15,
                            weights = ac.fam$aggWeights)

fam.wt.val<-fam.pam.wt.test[[4]]

fam.wt.asw <- fam.wt.val[,4]

fam.wt.r2 <- fam.wt.val[,7]

# Save here in case images fail
save.image("created data/ukhls/ukhls_cluster-comparison-test.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare two family options (weighted and unweighted)

pdf("results/Fig5-1_UKHLS_Fam-test.pdf",
    width=12,
    height=5)

layout.fig1 <- layout(matrix(c(1,2), 1, 2, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# Family: without weights
plot(x, fam.asw, type = "b", frame = FALSE, pch = 19, main="(a) Family (no Weights)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
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

# Family: with weights
plot(x, fam.wt.asw, type = "b", frame = FALSE, pch = 19, main="(b) Family (weighted)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
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

# pdf_convert("results/Fig5-1_UKHLS_Fam-test.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS_Fam-test.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare three MCSA options)

pdf("results/Fig5-1_UKHLS_MCSA-test.pdf",
    width=15,
    height=5)

layout.fig1 <- layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))


#  Detailed V1
plot(x, mc.det.asw, type = "b", frame = FALSE, pch = 19, main="(a) Detailed (Option 1)", 
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


# Detailed v1 - with weights
plot(x, mc.det.wt.asw, type = "b", frame = FALSE, pch = 19, main="(b) Detailed (Opt 1 weighted)", 
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


# Detailed v2
plot(x, mc.det2.asw, type = "b", frame = FALSE, pch = 19, main="(c) Detailed (Option 2)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.det2.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


dev.off()

# pdf_convert("results/Fig5-1_UKHLS_MCSA-test.pdf",
#            format = "png", dpi = 300, pages = 2,
#            "results/Fig5-1_UKHLS_MCSA-test.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save.image("created data/ukhls/ukhls_cluster-comparison.RData")
