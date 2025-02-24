# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: February 24 2025
#    Goal: compare clusters for SC v. MC solution
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))

# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' # Kim


if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim) }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea) }
getwd() # check it worked

# ~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~

required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                       "colorspace","ggplot2","ggpubr","ggseqplot", 
                       "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                       "labelled", "readxl", "openxlsx","tidyverse","pdftools")

install_if_missing <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

install_if_missing(required_packages)
lapply(required_packages, library, character.only = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load sequence states created in step 00 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load ("created data/setupsequence.RData")

data <- data%>%filter(`_mi_m`== 1) # test for now with just one imputation
# (bc I don't think I have the processing power for more)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Have to recreate the sequences --------------
# and OM matrices using just the mi 1 data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating the sequence objects for each channel
# Just using the mi 1 data

# Couple Paid Work - no OW
seq.work.mi1 <- seqdef(data[,col_work], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

# Couple Paid Work - OW
seq.work.ow.mi1 <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

# Couple HW - no amounts
seq.hw.mi1 <- seqdef(data[,col_hw], cpal = colspace.hw, labels=longlab.hw, states= shortlab.hw)

# Couple HW - amounts v1
seq.hw.hrs.mi1 <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, states= shortlab.hw.hrs)

# Couple HW - amounts v2
seq.hw.hrs.alt.mi1 <- seqdef(data[,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt, labels=longlab.hw.hrs.alt, 
                         states= shortlab.hw.hrs.alt)

# Family channel
seq.fam.mi1 <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM distance matrices

dist.work.om.mi1 <- seqdist(seq.work.mi1, method="OM", indel=1, sm= "CONSTANT")
dist.work.ow.om.mi1 <- seqdist(seq.work.ow.mi1, method="OM", indel=1, sm= "CONSTANT")

dist.hw.om.mi1 <- seqdist(seq.hw.mi1, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.om.mi1 <- seqdist(seq.hw.hrs.mi1, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.alt.om.mi1 <- seqdist(seq.hw.hrs.alt.mi1, method="OM", indel=1, sm= "CONSTANT")

dist.fam.om.mi1 <- seqdist(seq.fam.mi1, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparatory work required for rendering the plot ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the label for the x-axis 

xtlab<-seq(1,10, by = 1) ## Think this is for number of states


# Defining the range of the x axis 

x <- 2:15 ## this is number of clusters

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the combined clustering

mcdist.om <- seqdistmc(channels=list(seq.work.mi1, seq.hw.hrs.mi1, seq.fam.mi1), ## Seq states NOT om matrix
                       method="OM", indel=1, sm="CONSTANT") 

mcdist.om.pam <- wcKMedRange(mcdist.om, 
                             kvals = 2:15)

mc.val<-mcdist.om.pam[[4]]

mc.asw <- mc.val[,4]

mc.r2 <- mc.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of family trajectories

fam.pam.test <- wcKMedRange(dist.fam.om.mi1, 
                            kvals = 2:15)

fam.val<-fam.pam.test[[4]]

fam.asw <- fam.val[,4]

fam.r2 <- fam.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of paid labor trajectories

## No Overwork
work.pam.test <- wcKMedRange(dist.work.om.mi1, 
                            kvals = 2:15)

work.val<-work.pam.test[[4]]

work.asw <- work.val[,4]

work.r2 <- work.val[,7]

## With Overwork
work.ow.pam.test <- wcKMedRange(dist.work.ow.om.mi1, 
                             kvals = 2:15)

work.ow.val<-work.ow.pam.test[[4]]

work.ow.asw <- work.ow.val[,4]

work.ow.r2 <- work.ow.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract r2 and silhouette for the clustering of housework trajectories

## No hours
hw.pam.test <- wcKMedRange(dist.hw.om.mi1, 
                           kvals = 2:15)

hw.val<-hw.pam.test[[4]]

hw.asw <- hw.val[,4]

hw.r2 <- hw.val[,7]

## V1
hw.hrs.pam.test <- wcKMedRange(dist.hw.hrs.om.mi1, 
                            kvals = 2:15)

hw.hrs.val<-hw.hrs.pam.test[[4]]

hw.hrs.asw <- hw.hrs.val[,4]

hw.hrs.r2 <- hw.hrs.val[,7]

## V2
hw.hrs.alt.pam.test <- wcKMedRange(dist.hw.hrs.alt.om.mi1, 
                               kvals = 2:15)

hw.hrs.alt.val<-hw.hrs.alt.pam.test[[4]]

hw.hrs.alt.asw <- hw.hrs.alt.val[,4]

hw.hrs.alt.r2 <- hw.hrs.alt.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cairo_pdf("results/test - mi1/Fig5-1_PSID.pdf",
          width=15,
          height=5)

layout.fig1 <- layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# Family channel
plot(x, fam.asw, type = "b", frame = FALSE, pch = 19, main="(a) Family formation",
     col = "blue", xlab = "N. clusters", ylab = "ASW and R2 value", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, fam.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: Option 1 (used in MCSA)
plot(x, work.asw, type = "b", frame = FALSE, pch = 19, main="(b) Paid Work", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, work.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Housework Channel: Option 1 (used in MCSA)
plot(x, hw.hrs.asw, type = "b", frame = FALSE, pch = 19, main="(c) Housework", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, hw.hrs.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# MCSA
plot(x, mc.asw, type = "b", frame = FALSE, pch = 19, main="(d) MCSA", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, mc.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

dev.off()


pdf_convert("results/test - mi1/Fig5-1_PSID.pdf",
            format = "png", dpi = 300, pages = 2,
            "results/test - mi1/Fig5-1_PSID.png")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare two paid work options)

cairo_pdf("results/test - mi1/Fig5-1_PSID_Work-Options.pdf",
          width=12,
          height=5)

layout.fig1 <- layout(matrix(c(1,2), 1, 2, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))

# Paid Work Channel: Option 1 (used in MCSA)
plot(x, work.asw, type = "b", frame = FALSE, pch = 19, main="(b) Paid Work", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, work.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

# Paid Work Channel: Option 2 (used in MCSA)
plot(x, work.ow.asw, type = "b", frame = FALSE, pch = 19, main="(b) Paid Work (with Overwork)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, work.ow.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)

dev.off()


pdf_convert("results/test - mi1/Fig5-1_PSID_Work-Options.pdf",
            format = "png", dpi = 300, pages = 2,
            "results/test - mi1/Fig5-1_PSID_Work-Options.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional figures (compare three housework options)

cairo_pdf("results/test - mi1/Fig5-1_PSID_HW-Options.pdf",
          width=15,
          height=5)

layout.fig1 <- layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

par(mar = c(5, 5, 3, 3))


# Housework Channel: Option 1 (used in MCSA)
plot(x, hw.hrs.asw, type = "b", frame = FALSE, pch = 19, main="(a) Housework Hours", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, hw.hrs.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: Option 2
plot(x, hw.asw, type = "b", frame = FALSE, pch = 19, main="(b) Housework (no Hours)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, hw.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


# Housework Channel: Option 3
plot(x, hw.hrs.alt.asw, type = "b", frame = FALSE, pch = 19, main="(c) Housework Hours (Option 2)", 
     col = "blue", xlab = "N. clusters", ylab = "", ylim = c(0,0.8),
     cex.main=2,
     cex.lab=1.6,
     cex.axis=1.2)
# Add a second line
lines(x, hw.hrs.alt.r2, pch = 19, col = "black", type = "b", lty = 2)
# Add a legend to the plot
legend("bottomright", legend=c("ASW", "R2"),
       col=c("blue", "black"), lty = 1:2, cex=1.2)


dev.off()


pdf_convert("results/test - mi1/Fig5-1_PSID_HW-Options.pdf",
            format = "png", dpi = 300, pages = 2,
            "results/test - mi1/Fig5-1_PSID_HW-Options.png")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/cluster-comparison.RData")
