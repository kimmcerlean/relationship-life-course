# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: May 15 2025
#    Goal: compare clusters for SC v. MC solution - just complete sequences
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data and restrict to subset of sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- read_dta("created data/psid_couples_wide_truncated.dta")
data <- data%>%filter(`_mi_m`!=0)

# Filter to just 6 individuals (some with complete seq, some without)
# this matches the test file I've been using
data <- data%>%filter(unique_id==4032 | unique_id==6032 | unique_id==7005 | 
                        unique_id==4039 | unique_id==88171 | unique_id==297030)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Then create sequence objects just for these sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t = 1:10 #Number of time units (10 years - don't want to use year 11)

# ------------------------------------------------------------------------------
#Couple Paid Work - WITH OW: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_ow_trunc",i, sep="")
}
col_work.ow=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_alt_trunc",i, sep="")
}
col_hw.hrs.alt =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Family type: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_trunc",i, sep="")
}
col_fam =which(colnames(data)%in%lab_t) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating short and long labels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
#Couple Paid Work - WITH OW: labels

shortlab.work.ow <- c("MBW", "1.5MBW", 
                      "dualFT", "dualFT-hisOW", 
                      "dualFT-herOW", "dualOW",
                      "FBW", "underWK")

longlab.work.ow <- c("male breadwinner", "1.5 male breadwinner", 
                     "dual full-time", "dual full-time & his overwork", 
                     "dual full-time & her overwork", "dual overwork",
                     "female breadwinner", "under work")


# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

shortlab.hw.hrs.alt <- c("W-all:high", "W-all:low",
                         "W-most:high", "W-most:med", "W-most:low",
                         "equal:high", "equal:low", 
                         "M-most:high","M-most:low")

longlab.hw.hrs.alt <- c("woman does all: high", "woman does all: low",
                        "woman does most: high", "woman does most: med", "woman does most: low", 
                        "equal:high", "equal:low", 
                        "man does most: high", "man does most: low")

# ------------------------------------------------------------------------------
#Family type: labels

shortlab.fam <- c("MARc0", "MARc1", "MARc2", "MARc3",
                  "COHc0", "COHc1", "COHc2", "COHc3")

longlab.fam <- c("married, 0 Ch", 
                 "married, 1 Ch",
                 "married, 2 Ch",
                 "married, 3 Ch",
                 "cohab, 0 Ch",
                 "cohab, 1 Ch",
                 "cohab, 2 Ch",
                 "cohab, 3 Ch ")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define different color palettes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/

# ------------------------------------------------------------------------------
#Couple Paid Work - OW: labels

# Work colors
col1 <- sequential_hcl(5, palette = "BuGn") [1:2] #Male BW
col2 <- sequential_hcl(5, palette = "Purples")[1:4] #Dual FT
col3 <- sequential_hcl(5, palette = "PuRd")[c(2)] #Female BW
col4 <- sequential_hcl(5, palette = "PuRd")[c(1)]  #UnderWork
# col5 <- sequential_hcl(5, palette = "Grays")[c(3)] # Right-censored states

# Combine to full color palette
colspace.work.ow <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

#Housework colors
col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col2 <- sequential_hcl(5, palette = "PurpOr")[1:3] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[2:3] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[1:2] #M-most
# col5 <- sequential_hcl(5, palette = "Grays")[c(3)] # Right-censored states

# Combine to full color palette
colspace.hw.hrs.alt <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:1]   # Married states
col2 <- sequential_hcl(15, palette = "Inferno")[15:12]   # Cohabitation states
# col3 <- sequential_hcl(5, palette = "Grays")[c(3)] # Right-censored states

# Combine to full color palette
colspace.fam <- c(col1, col2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating the sequence objects for each channel
# Here, treating missing as NA to facilitate OM later
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Couple Paid Work - OW
seq.work.ow <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, 
                      labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.hw.hrs.alt <- seqdef(data[,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt,
                         labels=longlab.hw.hrs.alt, states= shortlab.hw.hrs.alt,right=NA)

seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam,
                  states= shortlab.fam,right=NA)

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

mcsa<-seqMD(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),
            with.missing=TRUE,
            what="MDseq") ##, right=NA)
seqlength(mcsa)
seqlength(mcsa, with.missing = FALSE) # okay, this isn't working, 
# but all sequences are same length across domains, so can just use 1 domain

seq.len.hw<-seqlength(seq.hw.hrs.alt, with.missing = FALSE)
seq.len.fam<-seqlength(seq.fam, with.missing = FALSE)

# Now create costs: set sm to 0 for missing
fam.miss.cost <- seqcost(seq.fam, method="CONSTANT", 
                         miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

work.miss.cost <- seqcost(seq.work.ow, method="CONSTANT", 
                          miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

hw.miss.cost <- seqcost(seq.hw.hrs.alt, method="CONSTANT", 
                        miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

# Then make indel costs very high
fam.miss.indel<- rep(1,ncol(fam.miss.cost$sm))
fam.miss.indel[length(fam.miss.indel)] <- 99999
fam.miss.indel

work.miss.indel<- rep(1,ncol(work.miss.cost$sm))
work.miss.indel[length(work.miss.indel)] <- 99999
work.miss.indel

hw.miss.indel<- rep(1,ncol(hw.miss.cost$sm))
hw.miss.indel[length(hw.miss.indel)] <- 99999
hw.miss.indel


## Now create multi-channel distance
mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam), ## Seq states NOT om matrix
                           method="OM", 
                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                           with.missing=TRUE) 

## Create length matrix
fam.min.len <- matrix(NA,ncol=length(seq.len.fam),nrow=length(seq.len.fam))
for (i in 1:length(seq.len.fam)){
  for (j in 1:length(seq.len.fam)){
    fam.min.len[i,j] <- min(c(seq.len.fam[i],seq.len.fam[j]))
  }
}

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create figure comparing separate channels and MCSA ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/PSID/truncated_sequences_cluster_test.pdf")

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

# MCSA: Not Normalized
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

## okay, this truncated behavior is diff to the state distribution plot
dist.fam.om <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)

dist.fam.min<-dist.fam.om / fam.min.len

ggseqrfplot(seq.fam, diss=dist.fam.min, sortv="from.start",
            which.plot="medoids") + theme(legend.position="none")

seqrfplot(seq.fam, diss=dist.fam.min, sortv="from.end",
          which.plot="medoids", with.missing=FALSE) + theme(legend.position="none")

seqiplot(seq.fam)
