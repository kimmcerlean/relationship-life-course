# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: June 16 2025
#    Goal: compare clusters for SC v. MC solution - all sequences, including truncated
# --------------------------------------------------------------------
# --------------------------------------------------------------------

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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("created data/setupsequence-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparatory work required for rendering the plot ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mcsa<-seqMD(channels=list(seq.work.ow, seq.hw.hrs, seq.fam),
            with.missing=TRUE,
            what="MDseq") ##, right=NA)

# Extract r2 and silhouette for the combined clustering

## More detailed sequence alphabets
mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                           method="OM", 
                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                           with.missing=TRUE) 

## Create length matrix - happens in step 0
#fam.min.len <- matrix(NA,ncol=length(seq.len.fam),nrow=length(seq.len.fam))
#for (i in 1:length(seq.len.fam)){
#  for (j in 1:length(seq.len.fam)){
#    fam.min.len[i,j] <- min(c(seq.len.fam[i],seq.len.fam[j]))
#  }
#}

## Divide by length matrix
mcdist.det.min <- mcdist.det.om / fam.min.len

## Now, go through next steps - but use the normalized matrix instead
mcdist.det.min.pam <- wcKMedRange(mcdist.det.min, 
                                  kvals = 2:15)

mc.min.val<-mcdist.det.min.pam[[4]]

mc.min.asw <- mc.min.val[,4]

mc.min.r2 <- mc.min.val[,7]

## Compare to original

mcdist.det.om.pam <- wcKMedRange(mcdist.det.om, 
                                 kvals = 2:15)

mc.om.val<-mcdist.det.om.pam[[4]]

mc.om.asw <- mc.om.val[,4]

mc.om.r2 <- mc.om.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Now, get the truncated sequence information by channel to compare
# Paid work
work.ow.pam <- wcKMedRange(dist.work.min, 
                           kvals = 2:15)

work.ow.val<-work.ow.pam[[4]]

work.ow.asw <- work.ow.val[,4]

work.ow.r2 <- work.ow.val[,7]

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

save.image("created data/psid_cluster-comparison-truncated.RData")
# in case it fails here

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing normalized v. not (for MC)  ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/PSID/psid_truncated_sequences_cluster_mcsa.pdf")

# MCSA: Normalized
p1<-plot(x, mc.min.asw, type = "b", frame = FALSE, pch = 19, main="MCSA: Normalized", 
         col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8), xlim=c(2,16),
         cex.main=2,
         cex.lab=1.6,
         cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.min.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("topright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# MCSA: Not Normalized (for length)
p2<-plot(x, mc.om.asw, type = "b", frame = FALSE, pch = 19, main="MCSA: Not Normalized", 
         col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8), xlim=c(2,16),
         cex.main=2,
         cex.lab=1.6,
         cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.om.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("topright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# grid.arrange(p1, p2, ncol=2, nrow=1)
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/PSID/PSID_truncated_sequences_cluster.pdf",
    width=20,
    height=10)

layout.fig1 <- layout(matrix(c(1,2,3,4), nrow=1, ncol=4, byrow = TRUE),
                      heights = c(1,1,1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# MCSA
plot(x, mc.min.asw, type = "b", frame = FALSE, pch = 19, main="MCSA", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray85", lwd = 1)
# Add a second line
lines(x, mc.min.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: With Overwork
plot(x, work.ow.asw, type = "b", frame = FALSE, pch = 19, main="Paid Work (with Overwork)", 
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
plot(x, hw.hrs.asw, type = "b", frame = FALSE, pch = 19, main="Housework (with Hours)", 
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
plot(x, fam.asw, type = "b", frame = FALSE, pch = 19, main="Family formation",
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
