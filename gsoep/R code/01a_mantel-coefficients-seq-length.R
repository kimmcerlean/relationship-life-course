# ---------------------------------------------------------------------
#    Program: 01_mantel-coefficients.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: Jan 22, 2026
#    Goal: Compare mantel coefficients of different sequence length configurations
#    (Never did this for Germany before, so this is new, actual sequence states)
# --------------------------------------------------------------------
# --------------------------------------------------------------------

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

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import created data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

data <- read_dta("created data/gsoep/gsoep_couples_imputed_wide_complete.dta")

# Filter to just 1 imputation
# think we did this instead of Rubin's rules bc we didn't know. For our purposes, this works for uncertainty iMO
# Really, we also picked 3 bc of US cohabitation - so this is a check that 3 is at least mostly correlated with 10.
data <- data%>%filter(`_mi_m`== 1) 
table(data$couple_work_ow_end1)
table(data$couple_hw_hrs_weekly_end1)
table(data$family_type_end1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary analysis for MCSA 
## set up sequence objects on one imputed dataset
## Compute standard OM distance matrices for multi-channel sequence objects of different lengths
## Compute mantel coefficients across different sequence lengths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables
## couple_work_ow_end: Couple-level work indicator (overwork split out)
## couple_hw_hrs_weekly_end:	Couple-level housework indicator, split by time spent (Germany uses weekly created)
##on HW, percentiles created within a specific subgroup (e.g. she does most)
## family_type_end:	Type of family based on relationship type + number of 
##children
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setting up the data ----------------------------------------------------------
## Identifying the columns with the sequence states
## Creating short and long labels
## Choosing colors
## Creating sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating short and long labels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
#Couple Paid Work - WITH OW: labels

shortlab.work.ow <- c("MBW", "1.5MBW", 
                      "dualFT", "dualFT-anyOW", 
                      "FBW", "underWK")

longlab.work.ow <- c("male breadwinner", "1.5 male breadwinner", 
                     "dual full-time", "dual full-time & any overwork", 
                     "female breadwinner", "under work")

# ------------------------------------------------------------------------------
#Couple HW - with amounts (group-specific ptiles): labels 

shortlab.hw.hrs.combo <- c("W-most:high", "W-most:low",
                           "equal:high", "equal:low", 
                           "M-most:all")

longlab.hw.hrs.combo <- c("woman does most/all: high", "woman does most/all: low",
                          "equal:high", "equal:low", 
                          "man does most: all")

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

# ------------------------------------------------------------------------------
#Couple Paid Work - OW: labels

# Work colors
col1 <- sequential_hcl(5, palette = "BuGn") [1:2] #Male BW
col2 <- sequential_hcl(5, palette = "Purples")[1:2] #Dual FT
col3 <- sequential_hcl(5, palette = "PuRd")[c(2)] #Female BW
col4 <- sequential_hcl(5, palette = "PuRd")[c(1)]  #UnderWork

# Combine to full color palette
colspace.work.ow <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

#Housework colors
# col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col1 <- sequential_hcl(5, palette = "PurpOr")[c(1)] #W-most
col2 <- sequential_hcl(5, palette = "PurpOr")[c(3)] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[2:3] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most

# Combine to full color palette
colspace.hw.hrs.combo <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:1]   # Married states
col2 <- sequential_hcl(15, palette = "Inferno")[15:12]   # Cohabitation states
#col3 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.fam <- c(col1, col2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Struggling to make loops work)
# Create sequence options at each duration 1:10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~
# Duration 2 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.2=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.2 <- seqdef(data[,col_work.ow.2], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.2 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.2 <- seqdef(data[,col_hw.hrs.weekly.2], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.2 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.2 <- seqdef(data[,col_fam.2], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 3 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.3=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.3 <- seqdef(data[,col_work.ow.3], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.3 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.3 <- seqdef(data[,col_hw.hrs.weekly.3], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.3 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.3 <- seqdef(data[,col_fam.3], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 4 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.4=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.4 <- seqdef(data[,col_work.ow.4], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.4 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.4 <- seqdef(data[,col_hw.hrs.weekly.4], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.4 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.4 <- seqdef(data[,col_fam.4], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 5 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.5=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.5 <- seqdef(data[,col_work.ow.5], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.5 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.5 <- seqdef(data[,col_hw.hrs.weekly.5], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.5 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.5 <- seqdef(data[,col_fam.5], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 6 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.6=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.6 <- seqdef(data[,col_work.ow.6], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.6 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.6 <- seqdef(data[,col_hw.hrs.weekly.6], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.6 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.6 <- seqdef(data[,col_fam.6], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 7 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.7=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.7 <- seqdef(data[,col_work.ow.7], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.7 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.7 <- seqdef(data[,col_hw.hrs.weekly.7], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.7 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.7 <- seqdef(data[,col_fam.7], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 8 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.8=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.8 <- seqdef(data[,col_work.ow.8], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.8 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.8 <- seqdef(data[,col_hw.hrs.weekly.8], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.8 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.8 <- seqdef(data[,col_fam.8], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 9 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.9=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.9 <- seqdef(data[,col_work.ow.9], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.9 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.9 <- seqdef(data[,col_hw.hrs.weekly.9], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.9 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.9 <- seqdef(data[,col_fam.9], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 10 --
# ~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
## Couple Paid Work - WITH OW

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow.10=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.ow.10 <- seqdef(data[,col_work.ow.10], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - amounts v2 (group-specific ptiles)

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_weekly_end",i, sep="")
}
col_hw.hrs.weekly.10 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.10 <- seqdef(data[,col_hw.hrs.weekly.10], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                            states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam.10 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.fam.10 <- seqdef(data[,col_fam.10], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute multichannel OM distance matrices at each duration--
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Duration 2
mcdist.om.2 <- seqdistmc(channels=list(seq.work.ow.2, seq.hw.hrs.2, seq.fam.2), ## Seq states NOT om matrix
                           method="OM", indel=1, sm="CONSTANT") 

# Duration 3
mcdist.om.3 <- seqdistmc(channels=list(seq.work.ow.3, seq.hw.hrs.3, seq.fam.3), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 4
mcdist.om.4 <- seqdistmc(channels=list(seq.work.ow.4, seq.hw.hrs.4, seq.fam.4), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 5
mcdist.om.5 <- seqdistmc(channels=list(seq.work.ow.5, seq.hw.hrs.5, seq.fam.5), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 6
mcdist.om.6 <- seqdistmc(channels=list(seq.work.ow.6, seq.hw.hrs.6, seq.fam.6), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 7
mcdist.om.7 <- seqdistmc(channels=list(seq.work.ow.7, seq.hw.hrs.7, seq.fam.7), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 8
mcdist.om.8 <- seqdistmc(channels=list(seq.work.ow.8, seq.hw.hrs.8, seq.fam.8), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 9
mcdist.om.9 <- seqdistmc(channels=list(seq.work.ow.9, seq.hw.hrs.9, seq.fam.9), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 10
mcdist.om.10 <- seqdistmc(channels=list(seq.work.ow.10, seq.hw.hrs.10, seq.fam.10), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute mantel coefficients across durations ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all are compared to 10
mantel_comp.2 = mantel(mcdist.om.10, mcdist.om.2)
mantel_comp.3 = mantel(mcdist.om.10, mcdist.om.3)
mantel_comp.4 = mantel(mcdist.om.10, mcdist.om.4)
mantel_comp.5 = mantel(mcdist.om.10, mcdist.om.5)
mantel_comp.6 = mantel(mcdist.om.10, mcdist.om.6)
mantel_comp.7 = mantel(mcdist.om.10, mcdist.om.7)
mantel_comp.8 = mantel(mcdist.om.10, mcdist.om.8)
mantel_comp.9 = mantel(mcdist.om.10, mcdist.om.9)

## create and export data frame with mantel coefficients
length <- c(2,3,4,5,6,7,8,9)

stats_comp <- c(mantel_comp.2$statistic,mantel_comp.3$statistic,mantel_comp.4$statistic,
                mantel_comp.5$statistic,mantel_comp.6$statistic,mantel_comp.7$statistic,
                mantel_comp.8$statistic,mantel_comp.9$statistic)

sig_comp <- c(mantel_comp.2$signif,mantel_comp.3$signif,mantel_comp.4$signif,
              mantel_comp.5$signif,mantel_comp.6$signif,mantel_comp.7$signif,
              mantel_comp.8$signif,mantel_comp.9$signif)

mantel_comp <- data.frame(length, stats_comp, sig_comp)

print(mantel_comp)

write.xlsx(mantel_comp, "results/GSOEP/gsoep_mantel_seqlength-complete.xlsx")

