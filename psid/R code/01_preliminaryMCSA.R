# ---------------------------------------------------------------------
#    Program: 01_preliminaryMCSA.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: February 10 2025
#    Goal: preliminary analysis to justify analyzing different domains
# --------------------------------------------------------------------
# --------------------------------------------------------------------

options(repos=c(CRAN="https://cran.r-project.org"))

#note to put this on github otherwise this script is not usable for Kim
.libPaths("G:/My Drive/R Library") #leas library
setwd("C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course") #leas folder

# ~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~

#### installing traminer package. 
#install.packages("TraMineR", dependencies = TRUE)
library(TraMineR)
#install.packages("TraMineRextras")
library(TraMineRextras)

#packages for color palettes
#install.packages("RColorBrewer", dependencies= TRUE)
library(RColorBrewer)
#install.packages("paletteer", dependencies= TRUE)
library(paletteer) 
library(colorspace)

#ggplot
#install.packages("ggplot2", dependencies= TRUE)
library(ggplot2)
#install.packages("ggsignif")
#install.packages("rstatix")
#install.packages("ggpubr")
library(ggpubr)
library(ggseqplot)
library(patchwork)

#Weighted cluster
#Cluster
#install.packages("cluster", dependencies= TRUE)
library(cluster)
#install.packages("WeightedCluster", dependencies= TRUE)
library(WeightedCluster)
#install.packages("dendextend")
library(dendextend) 

#seqHMM
#install.packages("seqHMM")
library(seqHMM)

#importing .dta into R
#install.packages("haven", dependencies= TRUE)
library(haven)

#labeling variables
#install.packages("labelled", dependencies= TRUE)
library(labelled)

#Excel
#install.packages("readxl", dependencies= TRUE)
library(readxl)
#install.packages("openxlsx", dependencies= TRUE)
library(openxlsx)

#Data Functions
#install.packages("tidyverse", dependencies= TRUE)
library(tidyverse)

#clear graph window
graphics.off()

#number display
options(scipen=999)
#library(MASS)

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import created data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# Import imputed datasets using haven 
load("created data/setupsequence.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary analysis for MCSA 
## Compute standard OM distance matrices for each domain
## Compute mantel coefficients across domains
## Compute standard OM multichannel distance
## Compare r2 and silhouette across single SA and MC SA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM distance matrices for each domain ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist.work.om <- seqdist(seq.work, method="OM", indel=1, sm= "CONSTANT")
dist.work.ow.om <- seqdist(seq.work.ow, method="OM", indel=1, sm= "CONSTANT")

dist.hw.om <- seqdist(seq.hw, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.om <- seqdist(seq.hw.hrs, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.alt.om <- seqdist(seq.hw.hrs.alt, method="OM", indel=1, sm= "CONSTANT")

dist.fam.om <- seqdist(seq.fam, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute mantel coefficients across domains----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mantel.rtest(dist.work.om, dist.hw.om, nrepet = 100)
mantel.rtest(dist.work.ow.om, dist.hw.om, nrepet = 100)

mantel.rtest(dist.work.om, dist.hw.hrs.om, nrepet = 100)
mantel.rtest(dist.work.ow.om, dist.hw.hrs.om, nrepet = 100)

mantel.rtest(dist.work.om, dist.hw.hrs.alt.om, nrepet = 100)
mantel.rtest(dist.work.ow.om, dist.hw.hrs.alt.om, nrepet = 100)

mantel.rtest(dist.work.om, dist.fam.om, nrepet = 100)
mantel.rtest(dist.work.ow.om, dist.fam.om, nrepet = 100)
mantel.rtest(dist.work.hw.om, dist.fam.om, nrepet = 100)
mantel.rtest(dist.work.hw.hrs.om, dist.fam.om, nrepet = 100)
mantel.rtest(dist.work.hw.hrs.alt.om, dist.fam.om, nrepet = 100)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute standard OM multichannel distance-----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mcdist.om <- seqdistmc(channels=list(seq.work, seq.hw, seq.fam),
                       method="OM", indel=1, sm="CONSTANT", 
                       cweight=c(1,1,1))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare r2 and silhouette across single SA and MC SA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defining the range of the x axis 
x <- 2:15

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract r2 and silhouette for the clustering of paid work trajectories

work.pam.test <- wcKMedRange(dist.work.om, 
                            kvals = 2:15)

work.val<-work.pam.test[[4]]

work.asw <- work.val[,4]

work.r2 <- work.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract r2 and silhouette for the clustering of housework trajectories

hw.pam.test <- wcKMedRange(dist.hw.om, 
                            kvals = 2:15)

hw.val<-hw.pam.test[[4]]

hw.asw <- hw.val[,4]

hw.r2 <- hw.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract r2 and silhouette for the clustering of family trajectories

fam.pam.test <- wcKMedRange(dist.fam.om, 
                            kvals = 2:15)

fam.val<-fam.pam.test[[4]]

fam.asw <- fam.val[,4]

fam.r2 <- fam.val[,7]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract r2 and silhouette for the combined clustering

mcdist.om.pam <- wcKMedRange(mcdist.om, 
                             kvals = 2:15)

mc.val<-mcdist.om.pam[[4]]

mc.asw <- mc.val[,4]

mc.r2 <- mc.val[,7]


save.image("created data/singledist.RData")