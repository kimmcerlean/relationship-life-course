# ---------------------------------------------------------------------
#    Program: 07_mantel-coefficients.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: Nov 25, 2026
#    Goal: Compare mantel coefficients of different sequence length configurations
#     to determine minimum length needed to remain in sample
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))

# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Relationship Life Course' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
# kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course' # Kim
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

data <- read_dta("created data/ukhls/ukhls_couples_imputed_wide_complete.dta")

# Filter to just 1 imputation
# think we did this instead of Rubin's rules bc we didn't know. For our purposes, this works for uncertainty iMO
# Really, we also picked 3 bc of US cohabitation - so this is a check that 3 is at least mostly correlated with 10.
data <- data%>%filter(`_mi_m`== 1) 

table(data$couple_work_end10)
table(data$couple_hw_hrs_4cat_end10)
table(data$family_type_5cat_end10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary analysis for MCSA 
## set up sequence objects on one imputed dataset
## Compute standard OM distance matrices for multi-channel sequence objects of different lengths
## Compute mantel coefficients across different sequence lengths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables
## couple_work_end: Couple-level work indicator
## couple_hw_hrs_4cat_end:	Couple-level housework indicator, split by time spent
##on HW, percentiles created within a specific subgroup (e.g. she does most)
## family_type_5cat_end:	Type of family based on relationship type + number of 
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
#Couple Paid Work: labels

shortlab.work <- c("MBW", "1.5MBW", 
                   "dualFT", 
                   "FBW", "underWK")

longlab.work <- c("male breadwinner", "1.5 male breadwinner", 
                  "dual full-time", 
                  "female breadwinner", "under work")

# ------------------------------------------------------------------------------
#Couple HW: labels - equal combined

shortlab.hw.hrs <- c("W-most:high", "W-most:low",
                     "equal:all", 
                     "M-most:all")

longlab.hw.hrs <- c("woman does most/all: high", "woman does most/all: low",
                    "equal:all", 
                    "man does most: all")

# ------------------------------------------------------------------------------
#Family type: labels

shortlab.fam <- c("MARc0", "MARc1", "MARc2",
                  "COHc0", "COHc1")

longlab.fam <- c("married, 0 Ch", 
                 "married, 1 Ch",
                 "married, 2 or more Ch",
                 "cohab, 0 Ch",
                 "cohab, 1 or more Ch")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define different color palettes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
#Couple Paid Work: colors

# Work colors
col1 <- sequential_hcl(5, palette = "BuGn") [1:2] #Male BW
col2 <- sequential_hcl(5, palette = "Purples")[c(1)] #Dual FT
col3 <- sequential_hcl(5, palette = "PuRd")[c(2)] #Female BW
col4 <- sequential_hcl(5, palette = "PuRd")[c(1)]  #UnderWork

# Combine to full color palette
colspace.work <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
#Couple HW - less detailed: labels 

#Housework colors
# col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col1 <- sequential_hcl(5, palette = "PurpOr")[c(1)] #W-most
col2 <- sequential_hcl(5, palette = "PurpOr")[c(3)] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[c(2)] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most

# Combine to full color palette
colspace.hw.hrs <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:2]   # Married states [4:1] 
col2 <- sequential_hcl(15, palette = "Inferno")[c(15,13)]   # Cohabitation states [15:12] 
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.2=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.2 <- seqdef(data[,col_work.2], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.2 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.2 <- seqdef(data[,col_hw.hrs.2], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                         states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.2) +
  scale_x_discrete(labels = 1:2) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:2){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.3=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.3 <- seqdef(data[,col_work.3], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.3 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.3 <- seqdef(data[,col_hw.hrs.3], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.3) +
  scale_x_discrete(labels = 1:3) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:3){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.4=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.4 <- seqdef(data[,col_work.4], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.4 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.4 <- seqdef(data[,col_hw.hrs.4], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.4) +
  scale_x_discrete(labels = 1:4) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:4){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.5=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.5 <- seqdef(data[,col_work.5], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.5 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.5 <- seqdef(data[,col_hw.hrs.5], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.5) +
  scale_x_discrete(labels = 1:5) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:5){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.6=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.6 <- seqdef(data[,col_work.6], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.6 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.6 <- seqdef(data[,col_hw.hrs.6], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.6) +
  scale_x_discrete(labels = 1:6) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:6){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.7=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.7 <- seqdef(data[,col_work.7], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.7 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.7 <- seqdef(data[,col_hw.hrs.7], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.7) +
  scale_x_discrete(labels = 1:7) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:7){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.8=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.8 <- seqdef(data[,col_work.8], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.8 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.8 <- seqdef(data[,col_hw.hrs.8], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.8) +
  scale_x_discrete(labels = 1:8) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:8){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.9=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.9 <- seqdef(data[,col_work.9], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.9 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.9 <- seqdef(data[,col_hw.hrs.9], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                           states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.9) +
  scale_x_discrete(labels = 1:9) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:9){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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
## Couple Paid Work

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work.10=which(colnames(data)%in%lab_t) 

# Sequence object
seq.work.10 <- seqdef(data[,col_work.10], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Couple HW - less detailed

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_4cat_end",i, sep="")
}
col_hw.hrs.10 =which(colnames(data)%in%lab_t) 

# Sequence object
seq.hw.hrs.10 <- seqdef(data[,col_hw.hrs.10], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                            states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs.10) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ------------------------------------------------------------------------------
## Family type

# Columns
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_5cat_end",i, sep="")
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First create custom cost structures, BUT don't need for missing states
# because complete sequences, so should be easier
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create custom substitution matrices for non-missing states

sw <- c("MBW", "1.5MBW", "dualFT", "FBW", "underWK")
work.raw <- matrix(c(0, 2, 3, 5, 3,
                     2, 0, 1, 3, 3,
                     3, 1, 0, 2, 4,
                     5, 3, 2, 0, 2,
                     3, 3, 4, 2, 0), 5, byrow = TRUE, dimnames = list(sw, sw))

sh <- c("W-most:high", "W-most:low", "equal:all", "M-most:all")
hw.raw <- matrix(c(0, 1, 2, 4,
                   1, 0, 2, 4,
                   2, 2, 0, 2,
                   4, 4, 2, 0), 4, byrow = TRUE, dimnames = list(sh, sh))

sf <- c("MARc0", "MARc1", "MARc2", "COHc0", "COHc1")
fam.raw <- matrix(c(0, 4, 6, 3, 8,
                    4, 0, 2, 7, 4,
                    6, 2, 0, 9, 4,
                    3, 7, 9, 0, 5,
                    8, 4, 4, 5, 0), 5, byrow = TRUE, dimnames = list(sf, sf))


build_sm <- function(raw, seqobj, cval = 2) {
  alph <- alphabet(seqobj)
  stopifnot(setequal(alph, rownames(raw)), isSymmetric(raw), all(diag(raw) == 0))
  sm  <- raw[alph, alph] * cval / max(raw)
  #sm <- rbind(cbind(m, 0), 0)
  #sm
}

## Run functions to create costs for SM.
##I think I can just do it once, because the states are the same, just the lengths differ?
work.cost <- list(sm = build_sm(work.raw, seq.work.10))
hw.cost   <- list(sm = build_sm(hw.raw,   seq.hw.hrs.10))
fam.cost  <- list(sm = build_sm(fam.raw,  seq.fam.10))

print(work.cost$sm)
print(hw.cost$sm)
print(fam.cost$sm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now get distances
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Duration 2
mcdist.om.2 <- seqdistmc(channels=list(seq.work.2, seq.hw.hrs.2, seq.fam.2), ## Seq states NOT om matrix
                           method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm))

# Duration 3
mcdist.om.3 <- seqdistmc(channels=list(seq.work.3, seq.hw.hrs.3, seq.fam.3), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm))

# Duration 4
mcdist.om.4 <- seqdistmc(channels=list(seq.work.4, seq.hw.hrs.4, seq.fam.4), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm)) 

# Duration 5
mcdist.om.5 <- seqdistmc(channels=list(seq.work.5, seq.hw.hrs.5, seq.fam.5), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm))

# Duration 6
mcdist.om.6 <- seqdistmc(channels=list(seq.work.6, seq.hw.hrs.6, seq.fam.6), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm))

# Duration 7
mcdist.om.7 <- seqdistmc(channels=list(seq.work.7, seq.hw.hrs.7, seq.fam.7), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm))

# Duration 8
mcdist.om.8 <- seqdistmc(channels=list(seq.work.8, seq.hw.hrs.8, seq.fam.8), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm)) 

# Duration 9
mcdist.om.9 <- seqdistmc(channels=list(seq.work.9, seq.hw.hrs.9, seq.fam.9), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm)) 

# Duration 10
mcdist.om.10 <- seqdistmc(channels=list(seq.work.10, seq.hw.hrs.10, seq.fam.10), ## Seq states NOT om matrix
                         method="OM", indel=1,
                         sm=list(work.cost$sm, hw.cost$sm, fam.cost$sm)) 

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

write.xlsx(mantel_comp, "results/UKHLS/ukhls_mantel_seqlength-complete.xlsx")

