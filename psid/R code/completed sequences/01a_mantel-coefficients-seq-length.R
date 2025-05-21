# ---------------------------------------------------------------------
#    Program: 01_mantel-coefficients.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: April 19 2025
#    Goal: Compare mantel coefficients of different sequence length configurations
#         Just using the detailed sequence states for ease
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
                         "labelled", "readxl", "openxlsx","tidyverse","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "vegan",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","ecodist")
  
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
                         "labelled", "readxl", "openxlsx","tidyverse","ecodist")
  
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

data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to just complete sequences to start
data <- data%>%filter(complete_seq== 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary analysis for MCSA 
## set up sequence objects on one imputed dataset
## Compute standard OM distance matrices for multi-channel sequence objects of different lengths
## Compute mantel coefficients across different sequence lengths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables
## couple_work_ow_end: Couple-level work indicator (overwork split out)
## couple_hw_hrs_combo_end:	Couple-level housework indicator, split by time spent 
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
                      "FBW", "underWK",
                      "DISS", "ATT")

longlab.work.ow <- c("male breadwinner", "1.5 male breadwinner", 
                     "dual full-time", "dual full-time & any overwork", 
                     "female breadwinner", "under work",
                     "dissolved", "attrited")


# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

shortlab.hw.hrs.combo <- c("W-most:high", "W-most:low",
                           "equal:high", "equal:low", 
                           "M-most:all",
                           "DISS", "ATT")

longlab.hw.hrs.combo <- c("woman does most/all: high", "woman does most/all: low",
                          "equal:high", "equal:low", 
                          "man does most: all",
                          "dissolved", "attrited")

# ------------------------------------------------------------------------------
#Family type: labels

shortlab.fam <- c("MARc0", "MARc1", "MARc2", "MARc3+",
                  "COHc0", "COHc1", "COHc2", "COHc3+",
                  "DISS", "ATT")

longlab.fam <- c("married, 0 Ch", 
                 "married, 1 Ch",
                 "married, 2 Ch",
                 "married, 3+ Ch",
                 "cohab, 0 Ch",
                 "cohab, 1 Ch",
                 "cohab, 2 Ch",
                 "cohab, 3+ Ch ",
                 "dissolved", "attrited")

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
col5 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.work.ow <- c(col1, col2, col3, col4, col5)

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

#Housework colors
# col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col1 <- sequential_hcl(5, palette = "PurpOr")[c(1)] #W-most
col2 <- sequential_hcl(5, palette = "PurpOr")[c(3)] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[2:3] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most
col5 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.hw.hrs.combo <- c(col1, col2, col3, col4, col5)

# ------------------------------------------------------------------------------
# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:1]   # Married states
col2 <- sequential_hcl(5, palette = "Oranges")[4:1] # Cohabitation states
col3 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.fam <- c(col1, col2, col3)

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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.2 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.2 <- seqdef(data[,col_hw.hrs.combo.2], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.2) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.3 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.3 <- seqdef(data[,col_hw.hrs.combo.3], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.3) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.4 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.4 <- seqdef(data[,col_hw.hrs.combo.4], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.4) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.5 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.5 <- seqdef(data[,col_hw.hrs.combo.5], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.5) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.6 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.6 <- seqdef(data[,col_hw.hrs.combo.6], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.6) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.7 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.7 <- seqdef(data[,col_hw.hrs.combo.7], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.7) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.8 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.8 <- seqdef(data[,col_hw.hrs.combo.8], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.8) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.9 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.9 <- seqdef(data[,col_hw.hrs.combo.9], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.9) +
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
  lab_t[i]=paste("couple_hw_hrs_combo_end",i, sep="")
}
col_hw.hrs.combo.10 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.combo.10 <- seqdef(data[,col_hw.hrs.combo.10], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                            states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.10) +
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
mcdist.om.2 <- seqdistmc(channels=list(seq.work.ow.2, seq.hw.hrs.combo.2, seq.fam.2), ## Seq states NOT om matrix
                           method="OM", indel=1, sm="CONSTANT") 

# Duration 3
mcdist.om.3 <- seqdistmc(channels=list(seq.work.ow.3, seq.hw.hrs.combo.3, seq.fam.3), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 4
mcdist.om.4 <- seqdistmc(channels=list(seq.work.ow.4, seq.hw.hrs.combo.4, seq.fam.4), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 5
mcdist.om.5 <- seqdistmc(channels=list(seq.work.ow.5, seq.hw.hrs.combo.5, seq.fam.5), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 6
mcdist.om.6 <- seqdistmc(channels=list(seq.work.ow.6, seq.hw.hrs.combo.6, seq.fam.6), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 7
mcdist.om.7 <- seqdistmc(channels=list(seq.work.ow.7, seq.hw.hrs.combo.7, seq.fam.7), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 8
mcdist.om.8 <- seqdistmc(channels=list(seq.work.ow.8, seq.hw.hrs.combo.8, seq.fam.8), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 9
mcdist.om.9 <- seqdistmc(channels=list(seq.work.ow.9, seq.hw.hrs.combo.9, seq.fam.9), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# Duration 10
mcdist.om.10 <- seqdistmc(channels=list(seq.work.ow.10, seq.hw.hrs.combo.10, seq.fam.10), ## Seq states NOT om matrix
                         method="OM", indel=1, sm="CONSTANT") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute mantel coefficients across durations ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all are compared to 10
# mantel_comp.2 = mantel(mcdist.om.10, mcdist.om.2) # I switched which Mantel I am using to get CIs
mantel_comp.2 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.2), nperm=1000)
mantel_comp.3 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.3), nperm=1000)
mantel_comp.4 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.4), nperm=1000)
mantel_comp.5 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.5), nperm=1000)
mantel_comp.6 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.6), nperm=1000)
mantel_comp.7 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.7), nperm=1000)
mantel_comp.8 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.8), nperm=1000)
mantel_comp.9 = mantel(lower(mcdist.om.10) ~ lower(mcdist.om.9), nperm=1000)

mantel_comp.2.df <- data.frame(mantel_comp.2)
mantel_comp.3.df <- data.frame(mantel_comp.3)
mantel_comp.4.df <- data.frame(mantel_comp.4)
mantel_comp.5.df <- data.frame(mantel_comp.5)
mantel_comp.6.df <- data.frame(mantel_comp.6)
mantel_comp.7.df <- data.frame(mantel_comp.7)
mantel_comp.8.df <- data.frame(mantel_comp.8)
mantel_comp.9.df <- data.frame(mantel_comp.9)

mantel_comp.2.df[1, ]

## create and export data frame with mantel coefficients
length <- c(2,3,4,5,6,7,8,9)

stats_comp <- c(mantel_comp.2.df[1, ],mantel_comp.3.df[1, ],mantel_comp.4.df[1, ],
                mantel_comp.5.df[1, ],mantel_comp.6.df[1, ],mantel_comp.7.df[1, ],
                mantel_comp.8.df[1, ],mantel_comp.9.df[1, ])

sig_comp <- c(mantel_comp.2.df[2, ],mantel_comp.3.df[2, ],mantel_comp.4.df[2, ],
              mantel_comp.5.df[2, ],mantel_comp.6.df[2, ],mantel_comp.7.df[2, ],
              mantel_comp.8.df[2, ],mantel_comp.9.df[2, ])

mantel_comp <- data.frame(length, stats_comp, sig_comp)

print(mantel_comp)

write.xlsx(mantel_comp, "results/PSID/psid_mantel_sequence-complete.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now doing this for all data, complete UP to a given duration ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First create sequence objects
# Think can use all the things created above,
# just restrict the data to given durations?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~
# Duration 2 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")
table(data$sequence_length)

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 2
data <- data%>%filter(sequence_length==2 | sequence_length==3 | sequence_length==4 | 
                        sequence_length==5 | sequence_length==6 | sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc2 <- seqdef(data[,col_work.ow.2], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc2 <- seqdef(data[,col_hw.hrs.combo.2], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                           states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc2 <- seqdef(data[,col_fam.2], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 3 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 3
data <- data%>%filter(sequence_length==3 | sequence_length==4 | 
                        sequence_length==5 | sequence_length==6 | sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

# Sequence objects
seq.work.ow.trunc3 <- seqdef(data[,col_work.ow.3], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc3 <- seqdef(data[,col_hw.hrs.combo.3], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc3 <- seqdef(data[,col_fam.3], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 4 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 4
data <- data%>%filter(sequence_length==4 | 
                        sequence_length==5 | sequence_length==6 | sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc4 <- seqdef(data[,col_work.ow.4], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc4 <- seqdef(data[,col_hw.hrs.combo.4], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc4 <- seqdef(data[,col_fam.4], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")


# ~~~~~~~~~~~~~~
# Duration 5 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 5
data <- data%>%filter(sequence_length==5 | sequence_length==6 | sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc5 <- seqdef(data[,col_work.ow.5], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc5 <- seqdef(data[,col_hw.hrs.combo.5], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc5 <- seqdef(data[,col_fam.5], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 6 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 6
data <- data%>%filter(sequence_length==6 | sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc6 <- seqdef(data[,col_work.ow.6], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc6 <- seqdef(data[,col_hw.hrs.combo.6], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc6 <- seqdef(data[,col_fam.6], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 7 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 7
data <- data%>%filter(sequence_length==7 | 
                        sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc7 <- seqdef(data[,col_work.ow.7], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc7 <- seqdef(data[,col_hw.hrs.combo.7], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc7 <- seqdef(data[,col_fam.7], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 8 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 8
data <- data%>%filter(sequence_length==8 | sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc8 <- seqdef(data[,col_work.ow.8], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc8 <- seqdef(data[,col_hw.hrs.combo.8], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc8 <- seqdef(data[,col_fam.8], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~
# Duration 9 --
# ~~~~~~~~~~~~~~
data <- read_dta("created data/psid_couples_imputed_wide.dta")

# Filter to just 1 imputation
data <- data%>%filter(`_mi_m`== 1) 

# Filter to sequences with minimum duration of 9
data <- data%>%filter(sequence_length==9 | sequence_length==10)

table(data$sequence_length)

# Sequence objects
seq.work.ow.trunc9 <- seqdef(data[,col_work.ow.9], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow.trunc9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# Sequence object
seq.hw.hrs.combo.trunc9 <- seqdef(data[,col_hw.hrs.combo.9], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                                states= shortlab.hw.hrs.combo)

ggseqdplot(seq.hw.hrs.combo.trunc9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# Sequence object
seq.fam.trunc9 <- seqdef(data[,col_fam.9], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam.trunc9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute multichannel OM distance matrices
# at each duration
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Duration 2
mcdist.om.trunc2 <- seqdistmc(channels=list(seq.work.ow.trunc2, seq.hw.hrs.combo.trunc2, seq.fam.trunc2), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 3
mcdist.om.trunc3 <- seqdistmc(channels=list(seq.work.ow.trunc3, seq.hw.hrs.combo.trunc3, seq.fam.trunc3), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 4
mcdist.om.trunc4 <- seqdistmc(channels=list(seq.work.ow.trunc4, seq.hw.hrs.combo.trunc4, seq.fam.trunc4), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 5
mcdist.om.trunc5 <- seqdistmc(channels=list(seq.work.ow.trunc5, seq.hw.hrs.combo.trunc5, seq.fam.trunc5), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 6
mcdist.om.trunc6 <- seqdistmc(channels=list(seq.work.ow.trunc6, seq.hw.hrs.combo.trunc6, seq.fam.trunc6), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 7
mcdist.om.trunc7 <- seqdistmc(channels=list(seq.work.ow.trunc7, seq.hw.hrs.combo.trunc7, seq.fam.trunc7), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 8
mcdist.om.trunc8 <- seqdistmc(channels=list(seq.work.ow.trunc8, seq.hw.hrs.combo.trunc8, seq.fam.trunc8), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# Duration 9
mcdist.om.trunc9 <- seqdistmc(channels=list(seq.work.ow.trunc9, seq.hw.hrs.combo.trunc9, seq.fam.trunc9), ## Seq states NOT om matrix
                              method="OM", indel=1, sm="CONSTANT") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute mantel coefficients across durations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all are compared to 10 - aka the complete sequences
# OH, I think the reason we can't do this is because the distance matrices aren't the same size
# This is why you have to restrict to only complete sequences, so then the sample sizes are the same

# mantel_comp.trunc2 = mantel(mcdist.om.10, mcdist.om.trunc2)
# mantel_comp.trunc3 = mantel(mcdist.om.10, mcdist.om.trunc3)
# mantel_comp.trunc4 = mantel(mcdist.om.10, mcdist.om.trunc4)
# mantel_comp.trunc5 = mantel(mcdist.om.10, mcdist.om.trunc5)
# mantel_comp.trunc6 = mantel(mcdist.om.10, mcdist.om.trunc6)
# mantel_comp.trunc7 = mantel(mcdist.om.10, mcdist.om.trunc7)
# mantel_comp.trunc8 = mantel(mcdist.om.10, mcdist.om.trunc8)
# mantel_comp.trunc9 = mantel(mcdist.om.10, mcdist.om.trunc9)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Okay, so instead, will compare the cross-domain mantel coefficients at 
# a. different durations of complete sequences
# b. different durations of incomplete (should work bc sequences same size)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Complete sequences: length of 4
dist.work.ow.om.c4 <- seqdist(seq.work.ow.4, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.c4 <- seqdist(seq.hw.hrs.combo.4, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.c4 <- seqdist(seq.fam.4, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.c4 = mantel(lower(dist.work.ow.om.c4) ~ lower(dist.hw.hrs.combo.om.c4), nperm=1000)
mantel_ow.fam.c4 = mantel(lower(dist.work.ow.om.c4) ~ lower(dist.fam.om.c4), nperm=1000)
mantel_hw.hrs.combo.fam.c4 = mantel(lower(dist.hw.hrs.combo.om.c4) ~ lower(dist.fam.om.c4), nperm=1000)

mantel_ow.hw.hrs.combo.c4 <- data.frame(mantel_ow.hw.hrs.combo.c4)
mantel_ow.fam.c4 <- data.frame(mantel_ow.fam.c4)
mantel_hw.hrs.combo.fam.c4 <- data.frame(mantel_hw.hrs.combo.fam.c4)

# Complete sequences: length of 7
dist.work.ow.om.c7 <- seqdist(seq.work.ow.7, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.c7 <- seqdist(seq.hw.hrs.combo.7, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.c7 <- seqdist(seq.fam.7, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.c7 = mantel(lower(dist.work.ow.om.c7) ~ lower(dist.hw.hrs.combo.om.c7), nperm=1000)
mantel_ow.fam.c7 = mantel(lower(dist.work.ow.om.c7) ~ lower(dist.fam.om.c7), nperm=1000)
mantel_hw.hrs.combo.fam.c7 = mantel(lower(dist.hw.hrs.combo.om.c7) ~ lower(dist.fam.om.c7), nperm=1000)

mantel_ow.hw.hrs.combo.c7 <- data.frame(mantel_ow.hw.hrs.combo.c7)
mantel_ow.fam.c7 <- data.frame(mantel_ow.fam.c7)
mantel_hw.hrs.combo.fam.c7 <- data.frame(mantel_hw.hrs.combo.fam.c7)

# Complete sequences: length of 10
dist.work.ow.om.c10 <- seqdist(seq.work.ow.10, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.c10 <- seqdist(seq.hw.hrs.combo.10, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.c10 <- seqdist(seq.fam.10, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.c10 = mantel(lower(dist.work.ow.om.c10) ~ lower(dist.hw.hrs.combo.om.c10), nperm=1000)
mantel_ow.fam.c10 = mantel(lower(dist.work.ow.om.c10) ~ lower(dist.fam.om.c10), nperm=1000)
mantel_hw.hrs.combo.fam.c10 = mantel(lower(dist.hw.hrs.combo.om.c10) ~ lower(dist.fam.om.c10), nperm=1000)

mantel_ow.hw.hrs.combo.c10 <- data.frame(mantel_ow.hw.hrs.combo.c10)
mantel_ow.fam.c10 <- data.frame(mantel_ow.fam.c10)
mantel_hw.hrs.combo.fam.c10 <- data.frame(mantel_hw.hrs.combo.fam.c10)

# Truncated sequences: length of 4
dist.work.ow.om.t4 <- seqdist(seq.work.ow.trunc4, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.t4 <- seqdist(seq.hw.hrs.combo.trunc4, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.t4 <- seqdist(seq.fam.trunc4, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.t4 = data.frame(mantel(lower(dist.work.ow.om.t4) ~ lower(dist.hw.hrs.combo.om.t4), nperm=1000))
mantel_ow.fam.t4 = data.frame(mantel(lower(dist.work.ow.om.t4) ~ lower(dist.fam.om.t4), nperm=1000))
mantel_hw.hrs.combo.fam.t4 = data.frame(mantel(lower(dist.hw.hrs.combo.om.t4) ~ lower(dist.fam.om.t4), nperm=1000))

# Truncated sequences: length of 7
dist.work.ow.om.t7 <- seqdist(seq.work.ow.trunc7, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.t7 <- seqdist(seq.hw.hrs.combo.trunc7, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.t7 <- seqdist(seq.fam.trunc7, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.t7 = data.frame(mantel(lower(dist.work.ow.om.t7) ~ lower(dist.hw.hrs.combo.om.t7), nperm=1000))
mantel_ow.fam.t7 = data.frame(mantel(lower(dist.work.ow.om.t7) ~ lower(dist.fam.om.t7), nperm=1000))
mantel_hw.hrs.combo.fam.t7 = data.frame(mantel(lower(dist.hw.hrs.combo.om.t7) ~ lower(dist.fam.om.t7), nperm=1000))

# Truncated sequences: length of 9
dist.work.ow.om.t9 <- seqdist(seq.work.ow.trunc9, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.combo.om.t9 <- seqdist(seq.hw.hrs.combo.trunc9, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om.t9 <- seqdist(seq.fam.trunc9, method="OM", indel=1, sm= "CONSTANT")

mantel_ow.hw.hrs.combo.t9 = data.frame(mantel(lower(dist.work.ow.om.t9) ~ lower(dist.hw.hrs.combo.om.t9), nperm=1000))
mantel_ow.fam.t9 = data.frame(mantel(lower(dist.work.ow.om.t9) ~ lower(dist.fam.om.t9), nperm=1000))
mantel_hw.hrs.combo.fam.t9 = data.frame(mantel(lower(dist.hw.hrs.combo.om.t9) ~ lower(dist.fam.om.t9), nperm=1000))

## create and export data frame with mantel coefficients
comparison <- c('Complete 4: work/hw', 'Complete 4: work/fam',  'Complete 4: hw/fam',
                'Complete 7: work/hw', 'Complete 7: work/fam',  'Complete 7: hw/fam',
                'Complete 10: work/hw', 'Complete 10: work/fam',  'Complete 10: hw/fam',
                'Truncated 4: work/hw', 'Truncated 4: work/fam',  'Truncated 4: hw/fam',
                'Truncated 7: work/hw', 'Truncated 7: work/fam',  'Truncated 7: hw/fam',
                'Truncated 9: work/hw', 'Truncated 9: work/fam',  'Truncated 9: hw/fam')

stats <- c(mantel_ow.hw.hrs.combo.c4[1, ], mantel_ow.fam.c4[1, ], mantel_hw.hrs.combo.fam.c4[1, ],
           mantel_ow.hw.hrs.combo.c7[1, ], mantel_ow.fam.c7[1, ], mantel_hw.hrs.combo.fam.c7[1, ],
           mantel_ow.hw.hrs.combo.c10[1, ], mantel_ow.fam.c10[1, ], mantel_hw.hrs.combo.fam.c10[1, ],
           mantel_ow.hw.hrs.combo.t4[1, ], mantel_ow.fam.t4[1, ], mantel_hw.hrs.combo.fam.t4[1, ],
           mantel_ow.hw.hrs.combo.t7[1, ], mantel_ow.fam.t7[1, ], mantel_hw.hrs.combo.fam.t7[1, ],
           mantel_ow.hw.hrs.combo.t9[1, ], mantel_ow.fam.t9[1, ], mantel_hw.hrs.combo.fam.t9[1, ])

sig <- c(mantel_ow.hw.hrs.combo.c4[2, ], mantel_ow.fam.c4[2, ], mantel_hw.hrs.combo.fam.c4[2, ],
         mantel_ow.hw.hrs.combo.c7[2, ], mantel_ow.fam.c7[2, ], mantel_hw.hrs.combo.fam.c7[2, ],
         mantel_ow.hw.hrs.combo.c10[2, ], mantel_ow.fam.c10[2, ], mantel_hw.hrs.combo.fam.c10[2, ],
         mantel_ow.hw.hrs.combo.t4[2, ], mantel_ow.fam.t4[2, ], mantel_hw.hrs.combo.fam.t4[2, ],
         mantel_ow.hw.hrs.combo.t7[2, ], mantel_ow.fam.t7[2, ], mantel_hw.hrs.combo.fam.t7[2, ],
         mantel_ow.hw.hrs.combo.t9[2, ], mantel_ow.fam.t9[2, ], mantel_hw.hrs.combo.fam.t9[2, ])

mantel_length <- data.frame(comparison, stats, sig)

write.xlsx(mantel_length, "results/PSID/psid_mantel_length_comparison.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~
## Save
# ~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/mantel-seq-length.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test bootstrapping for confidence intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complete sequences: length of 10
mantel_ow.hw.hrs.combo.c10 = mantel(dist.work.ow.om.c10, dist.hw.hrs.combo.om.c10)
mantel_ow.fam.c10 = mantel(dist.work.ow.om.c10, dist.fam.om.c10)
mantel_hw.hrs.combo.fam.c10 = mantel(dist.hw.hrs.combo.om.c10, dist.fam.om.c10)

mantel(dist.work.ow.om.c10, dist.hw.hrs.combo.om.c10, permutations=1000)

# Vegan
test.work.dist <- as.matrix(dist.work.ow.om.c10)
test.fam.dist <- as.matrix(dist.fam.om.c10)
mantel(dist.work.ow.om.c10 ~ dist.fam.om.c10, nperm=100)

# Ecodist
# https://stat.ethz.ch/pipermail/r-help/2011-January/266062.html
test.work.dist <- dist(dist.work.ow.om.c10)
test.fam.dist <- dist(dist.fam.om.c10)
mantel(lower(dist.work.ow.om.c10) ~ lower(dist.fam.om.c10), nperm=100)

# need these to match, and do these match ecodist
mantel(dist.work.ow.om.c10, dist.fam.om.c10, permutations=999)
mantel_ow.fam.c10$statistic
quantile(mantel_ow.fam.c10$perm, probs=c(.90, .95, .975, .99))

mantel.test.work.fam <- mantel(lower(dist.work.ow.om.c10) ~ lower(dist.fam.om.c10), nperm=100)
mantel.test.work.hw <- mantel(lower(dist.work.ow.om.c10) ~ lower(dist.hw.hrs.combo.om.c10), nperm=100)
mantel.test.hw.fam <- mantel(lower(dist.hw.hrs.combo.om.c10) ~ lower(dist.fam.om.c10), nperm=100)

mantel.df.work.fam <- data.frame(mantel.test.work.fam)
mantel.df.work.hw <- data.frame(mantel.test.work.hw)
mantel.df.hw.fam <- data.frame(mantel.test.hw.fam)
mantel.col <- c('mantelr','pval1','pval2','pval3','llim.2.5%','ulim.97.5%')
mantel.df <- data.frame(mantel.col, mantel.df.work.fam, mantel.df.hw.fam, mantel.df.work.hw)

write.xlsx(mantel.df, "results/PSID/tables/psid_mantel_ci_mi1.xlsx")

# compare to previous estimates
mantel_ow.fam.c10$statistic
mantel_ow.fam.c10$signif

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exporting figure of sequences for just complete sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# realizing - these are just for 1 imputation; have migrated this to new setup file

pdf("results/PSID/PSID_Base_Sequences_complete.pdf",
    width=12,
    height=3)

s1<-ggseqdplot(seq.work.ow.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Paid Work") + 
  theme(plot.title=element_text(hjust=0.5))

s2<-ggseqdplot(seq.hw.hrs.combo.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Housework") + 
  theme(plot.title=element_text(hjust=0.5))

s3<-ggseqdplot(seq.fam.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration") + 
  theme(legend.position="none") +
  ggtitle("Family") + 
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(s3,s1,s2, ncol=3, nrow=1)
dev.off()

pdf("results/PSID/PSID_Base_Index_complete.pdf",
    width=12,
    height=5)

i1<-ggseqiplot(seq.fam.10, sortv="from.start")
i2<-ggseqiplot(seq.work.ow.10, sortv="from.start")
i3<-ggseqiplot(seq.hw.hrs.combo.10, sortv="from.start")

grid.arrange(i1,i2,i3, ncol=3, nrow=1)
dev.off()

pdf_convert("results/PSID/PSID_Base_Index_complete.pdf",
            format = "png", dpi = 300, pages = 1,
            "results/PSID/PSID_Base_Index_complete.png")

pdf("results/PSID/PSID_Base_RF_complete.pdf",
    width=12,
    height=5)

rf1<-ggseqrfplot(seq.fam.10, diss=dist.work.ow.om.c10, k=500, sortv="from.start",which.plot="medoids") + theme(legend.position="none")
rf2<-ggseqrfplot(seq.work.ow.10, diss=dist.hw.hrs.combo.om.c10, k=500, sortv="from.start",which.plot="medoids") + theme(legend.position="none")
rf3<-ggseqrfplot(seq.hw.hrs.combo.10, diss=dist.fam.om.c10, k=500, sortv="from.start",which.plot="medoids") + theme(legend.position="none")

grid.arrange(rf1,rf2,rf3, ncol=3, nrow=1)
dev.off()
