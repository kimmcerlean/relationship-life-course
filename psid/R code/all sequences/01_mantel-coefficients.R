# ---------------------------------------------------------------------
#    Program: 01_mantel-coefficients.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 4 2025
#    Goal: run mantel coefficients on one imputed datasets
# --------------------------------------------------------------------
# --------------------------------------------------------------------

options(repos=c(CRAN="https://cran.r-project.org"))

# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim) }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea) }
if (Sys.getenv(c("HOME" )) == "/home/lpessin") { setwd(lea.server) }
getwd() # check it worked

# set library path for whomever is running the script
if (Sys.getenv(c("USERNAME")) == "lpessin") { .libPaths("G:/My Drive/R Library") }

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

# Import non-imputed data
data <- read_dta("created data/psid_couples_imputed_wide.dta")
data <- data%>%filter(`_mi_m`== 1) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary analysis for MCSA 
## set up sequence objects on one imputed dataset
## Compute standard OM distance matrices for each domain one imputed dataset
## Compute mantel coefficients across domains one imputed dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setting up the data ----------------------------------------------------------
## Identifying the columns with the sequence states
## Creating short and long labels
## Choosing colors
## Creating sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identifying the columns in which we have sequence variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## couple_work_end: Couple-level work indicator (overwork not split out)
## couple_work_ow_end: Couple-level work indicator (overwork split out)
## couple_hw_end:	Couple-level housework indicator, no indicator of time spent 
## on HW
## couple_hw_hrs_end:	Couple-level housework indicator, split by time spent on 
##HW, percentiles created at total sample level
## couple_hw_hrs_alt_end:	Couple-level housework indicator, split by time spent 
##on HW, percentiles created within a specific subgroup (e.g. she does most)
## family_type_end:	Type of family based on relationship type + number of 
##children

# ------------------------------------------------------------------------------
### We identify columns that contain our sequence analysis input variables

t = 1:10 #Number of time units (10 years - don't want to use year 11)

# ------------------------------------------------------------------------------
#Couple Paid Work - no OW: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_end",i, sep="")
}
col_work=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple Paid Work - WITH OW: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_ow_end",i, sep="")
}
col_work.ow=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - no amounts: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_end",i, sep="")
}
col_hw=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - amounts v1 (universal ptiles): columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_end",i, sep="")
}
col_hw.hrs=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_alt_end",i, sep="")
}
col_hw.hrs.alt =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Family type: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_end",i, sep="")
}
col_fam =which(colnames(data)%in%lab_t) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating short and long labels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
#Couple Paid Work - no OW: labels

shortlab.work <- c("MBW", "1.5MBW", 
                   "dualFT", "FBW", 
                   "underWK",
                   "DISS", "ATT")

longlab.work <- c("male breadwinner", "1.5 male breadwinner", 
                  "dual full-time", "female breadwinner", 
                  "under work",
                  "dissolved", "attrited")

# ------------------------------------------------------------------------------
#Couple Paid Work - WITH OW: labels

shortlab.work.ow <- c("MBW", "1.5MBW", 
                      "dualFT", "dualFT-hisOW", 
                      "dualFT-herOW", "dualOW",
                      "FBW", "underWK",
                      "DISS", "ATT")

longlab.work.ow <- c("male breadwinner", "1.5 male breadwinner", 
                     "dual full-time", "dual full-time & his overwork", 
                     "dual full-time & her overwork", "dual overwork",
                     "female breadwinner", "under work", 
                     "dissolved", "attrited")

# ------------------------------------------------------------------------------
#Couple HW - no amounts: labels

shortlab.hw <- c("W-all", "W-most", 
                 "equal", "M-most", 
                 "DISS", "ATT")

longlab.hw <- c("woman does all", "woman does most", 
                "equal", "man does most", 
                "dissolved", "attrited")

# ------------------------------------------------------------------------------
#Couple HW - amounts v1 (universal ptiles): labels

shortlab.hw.hrs <- c("W-all:high", "W-all:low",
                     "W-most:high", "W-most:med", "W-most:low",
                     "equal:high", "equal:low", 
                     "M-most:high","M-most:low", 
                     "DISS", "ATT")

longlab.hw.hrs <- c("woman does all: high", "woman does all: low",
                    "woman does most: high", "woman does most: med", "woman does most: low", 
                    "equal:high", "equal:low", 
                    "man does most: high", "man does most: low",  
                    "dissolved", "attrited")

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

shortlab.hw.hrs.alt <- c("W-all:high", "W-all:low",
                         "W-most:high", "W-most:med", "W-most:low",
                         "equal:high", "equal:low", 
                         "M-most:high","M-most:low", 
                         "DISS", "ATT")

longlab.hw.hrs.alt <- c("woman does all: high", "woman does all: low",
                        "woman does most: high", "woman does most: med", "woman does most: low", 
                        "equal:high", "equal:low", 
                        "man does most: high", "man does most: low",  
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
#Couple Paid Work - no OW: colors

# Work colors
col1 <- diverging_hcl(5, palette = "Purple-Green")
col2 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.work <- c(col1, col2)

# ------------------------------------------------------------------------------
#Couple Paid Work - OW: labels

# Work colors
col1 <- diverging_hcl(8, palette = "Purple-Green")
col2 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.work.ow <- c(col1, col2)

# ------------------------------------------------------------------------------
#Couple HW - no amounts: labels

#Housework colors
col1 <- sequential_hcl(5, palette = "OrYel") [c(2)] #W-all
col2 <- sequential_hcl(5, palette = "Greens")[c(2)] #W-most
col3 <- sequential_hcl(5, palette = "Reds")[c(2)] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most
col5 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.hw <- c(col1, col2, col3, col4, col5)

# ------------------------------------------------------------------------------
#Couple HW - amounts v1 (universal ptiles): labels

#Housework colors
col1 <- sequential_hcl(5, palette = "OrYel") [2:1] #W-all
col2 <- sequential_hcl(5, palette = "Greens")[3:1] #W-most
col3 <- sequential_hcl(5, palette = "Reds")[2:1] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[2:1] #M-most
col5 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.hw.hrs <- c(col1, col2, col3, col4, col5)

# ------------------------------------------------------------------------------
#Couple HW - amounts v2 (group-specific ptiles): labels 

#Housework colors
col1 <- sequential_hcl(5, palette = "OrYel") [2:1] #W-all
col2 <- sequential_hcl(5, palette = "Greens")[3:1] #W-most
col3 <- sequential_hcl(5, palette = "Reds")[2:1] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[2:1] #Equal
col5 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.hw.hrs.alt <- c(col1, col2, col3, col4, col5)

# ------------------------------------------------------------------------------
# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:1]   # Married states
col2 <- sequential_hcl(5, palette = "Oranges")[4:1] # Cohabitation states
col3 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.fam <- c(col1, col2, col3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating the sequence objects for each channel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Couple Paid Work - no OW
seq.work <- seqdef(data[,col_work], cpal = colspace.work, labels=longlab.work, states= shortlab.work)

ggseqdplot(seq.work) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple Paid Work - OW
seq.work.ow <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow)

ggseqdplot(seq.work.ow) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - no amounts
seq.hw <- seqdef(data[,col_hw], cpal = colspace.hw, labels=longlab.hw, states= shortlab.hw)

ggseqdplot(seq.hw) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - amounts v1
seq.hw.hrs <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, states= shortlab.hw.hrs)

ggseqdplot(seq.hw.hrs) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - amounts v2
seq.hw.hrs.alt <- seqdef(data[,col_hw.hrs.alt], cpal = colspace.hw.hrs.alt, labels=longlab.hw.hrs.alt, 
                         states= shortlab.hw.hrs.alt)

ggseqdplot(seq.hw.hrs.alt) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Family channel
seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam)

ggseqdplot(seq.fam) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

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

mantel_w.hw = mantel(dist.work.om, dist.hw.om)
mantel_ow.hw = mantel(dist.work.ow.om, dist.hw.om)

mantel_w.hw.hrs = mantel(dist.work.om, dist.hw.hrs.om)
mantel_ow.hw.hrs = mantel(dist.work.ow.om, dist.hw.hrs.om)

mantel_w.hw.hrs.alt = mantel(dist.work.om, dist.hw.hrs.alt.om)
mantel_ow.hw.hrs.alt = mantel(dist.work.ow.om, dist.hw.hrs.alt.om)

mantel_w.fam = mantel(dist.work.om, dist.fam.om)
mantel_ow.fam = mantel(dist.work.ow.om, dist.fam.om)
mantel_hw.fam = mantel(dist.hw.om, dist.fam.om)
mantel_hw.hrs.fam = mantel(dist.hw.hrs.om, dist.fam.om)
mantel_hw.hrs.alt.fam = mantel(dist.hw.hrs.alt.om, dist.fam.om)

mantel_w.hw$statistic
mantel_w.hw$signif

mantel_ow.hw$statistic
mantel_ow.hw$signif

mantel_w.hw.hrs$statistic
mantel_w.hw.hrs$signif

mantel_ow.hw.hrs$statistic
mantel_ow.hw.hrs$signif

mantel_w.hw.hrs.alt$statistic
mantel_w.hw.hrs.alt$signif

mantel_ow.hw.hrs.alt$statistic
mantel_ow.hw.hrs.alt$signif

mantel_w.fam$statistic
mantel_w.fam$signif

mantel_ow.fam$statistic
mantel_ow.fam$signif

mantel_hw.fam$statistic
mantel_hw.fam$signif

mantel_hw.hrs.fam$statistic
mantel_hw.hrs.fam$signif

mantel_hw.hrs.alt.fam$statistic
mantel_hw.hrs.alt.fam$signif

## create and export data frame with mantel coefficients
association <- c('w.hw', 'ow.hw', 'w.hw.hrs', 'ow.hw.hrs', 'w.hw.hrs.alt', 'ow.hw.hrs.alt',
                 'w.fam', 'ow.fam', 'hw.fam', 'hw.hrs.fam', 'hw.hrs.alt.fam')
stats <- c(mantel_w.hw$statistic, mantel_ow.hw$statistic, mantel_w.hw.hrs$statistic,
           mantel_ow.hw.hrs$statistic, mantel_w.hw.hrs.alt$statistic, mantel_ow.hw.hrs.alt$statistic,
           mantel_w.fam$statistic, mantel_ow.fam$statistic, mantel_hw.fam$statistic,
           mantel_hw.hrs.fam$statistic,mantel_hw.hrs.alt.fam$statistic)
sig <- c(mantel_w.hw$signif, mantel_ow.hw$signif, mantel_w.hw.hrs$signif,
         mantel_ow.hw.hrs$signif, mantel_w.hw.hrs.alt$signif, mantel_ow.hw.hrs.alt$signif,
         mantel_w.fam$signif, mantel_ow.fam$signif, mantel_hw.fam$signif,
         mantel_hw.hrs.fam$signif,mantel_hw.hrs.alt.fam$signif)
mantel <- data.frame(association, stats, sig)

print(mantel)

write.xlsx(mantel, "results/psid_mantel_coefficients.xlsx")

## Save
save.image("created data/mantel.RData")
