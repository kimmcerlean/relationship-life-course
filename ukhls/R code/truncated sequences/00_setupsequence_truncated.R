# ---------------------------------------------------------------------
#    Program: 00_setupsequence.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 4 2025
#    Goal: setup UKHLS for multichannel sequence analysis of couples' life courses;
#   This file focuses on all sequences, including incomplete ones   
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
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
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
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~
# Import data ----
# ~~~~~~~~~~~~~~~~

# Import imputed datasets using haven 
data <- read_dta("created data/ukhls/ukhls_couples_wide_truncated.dta")
data <- data%>%filter(`_mi_m`!=0)

## testing with 5 imputations for now to avoid using unique sequences
## (maybe) once we figure this out, can try to add all
data <- data%>%filter(`_mi_m`==1 | `_mi_m`==2 | `_mi_m`==3 | `_mi_m`==4 | `_mi_m`==5)
table(data$`_mi_m`)

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

## couple_work_ow_trunc: Couple-level work indicator (overwork split out, but not detailed)
## couple_hw_hrs_combo_trunc:	Couple-level housework indicator, split by time spent 
##on HW for some categories (consolidated)
## family_type_trunc:	Type of family based on relationship type + number of 
##children

# ------------------------------------------------------------------------------
### We identify columns that contain our sequence analysis input variables

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
  lab_t[i]=paste("couple_hw_hrs_combo_trunc",i, sep="")
}
col_hw.hrs =which(colnames(data)%in%lab_t) 

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

# https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/

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
# Creating the sequence objects for each channel
# Here, treating missing as NA to facilitate OM later
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Couple Paid Work - OW
seq.work.ow <- seqdef(data[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)
seqlength(seq.work.ow)
seq.len.work<-seqlength(seq.work.ow, with.missing = FALSE)

ggseqdplot(seq.work.ow) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# So default is actually already missing=false
# for reference (confirm I understand what is happening):
ggseqdplot(seq.work.ow, with.missing=TRUE) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - amounts v2
seq.hw.hrs <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw<-seqlength(seq.hw.hrs, with.missing = FALSE)

ggseqdplot(seq.hw.hrs) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Family channel
seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam<-seqlength(seq.fam, with.missing = FALSE)

ggseqdplot(seq.fam) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dissimilarity matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First set costs of sm to 0 for missing
fam.miss.cost <- seqcost(seq.fam, method="CONSTANT", 
                              miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

work.miss.cost <- seqcost(seq.work.ow, method="CONSTANT", 
                         miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

hw.miss.cost <- seqcost(seq.hw.hrs, method="CONSTANT", 
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

# Now use these costs to create NON-normalized matrices
dist.work.om <- seqdist(seq.work.ow, method="OM", indel=work.miss.indel, 
                           sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om <- seqdist(seq.hw.hrs, method="OM", indel=hw.miss.indel, 
                              sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
                           sm= fam.miss.cost$sm, with.missing=TRUE)

# Then create matrices of shortest length 
fam.min.len <- matrix(NA,ncol=length(seq.len.fam),nrow=length(seq.len.fam))
for (i in 1:length(seq.len.fam)){
  for (j in 1:length(seq.len.fam)){
    fam.min.len[i,j] <- min(c(seq.len.fam[i],seq.len.fam[j]))
  }
}

work.min.len <- matrix(NA,ncol=length(seq.len.work),nrow=length(seq.len.work))
for (i in 1:length(seq.len.work)){
  for (j in 1:length(seq.len.work)){
    work.min.len[i,j] <- min(c(seq.len.work[i],seq.len.work[j]))
  }
}

hw.min.len <- matrix(NA,ncol=length(seq.len.hw),nrow=length(seq.len.hw))
for (i in 1:length(seq.len.hw)){
  for (j in 1:length(seq.len.hw)){
    hw.min.len[i,j] <- min(c(seq.len.hw[i],seq.len.hw[j]))
  }
}

# Then normalize based on that length
dist.fam.min<-dist.fam.om / fam.min.len
dist.work.min<-dist.work.om / work.min.len
dist.hw.min<-dist.hw.om / hw.min.len

# Temp save in case figures fail

save.image("created data/ukhls/ukhls-setupsequence-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exporting figures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("results/UKHLS/UKHLS_Base_Sequences_truncated.pdf",
    width=12,
    height=3)

s1<-ggseqdplot(seq.work.ow) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Paid Work") + 
  theme(plot.title=element_text(hjust=0.5))

s2<-ggseqdplot(seq.hw.hrs) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Housework") + 
  theme(plot.title=element_text(hjust=0.5))

s3<-ggseqdplot(seq.fam) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Family") + 
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(s1,s3,s2, ncol=3, nrow=1)
dev.off()


#pdf("results/UKHLS/UKHLS_Base_Index_truncated.pdf",
#    width=12,
#    height=5)

#i1<-ggseqiplot(seq.fam, sortv="from.start")
#i2<-ggseqiplot(seq.work.ow, sortv="from.start")
#i3<-ggseqiplot(seq.hw.hrs.alt, sortv="from.start")

#grid.arrange(i1,i2,i3, ncol=3, nrow=1)
#dev.off()

#pdf_convert("results/UKHLS/UKHLS_Base_Index_truncated.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "results/UKHLS/UKHLS_Base_Index_truncated.png")


pdf("results/UKHLS/UKHLS_Base_RF_truncated.pdf",
    width=12,
    height=5)

rf1<-ggseqrfplot(seq.fam, diss=dist.fam.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

rf2<-ggseqrfplot(seq.work.ow, diss=dist.work.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

rf3<-ggseqrfplot(seq.hw.hrs, diss=dist.hw.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

grid.arrange(rf1,rf2,rf3, ncol=3, nrow=1)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/ukhls/ukhls-setupsequence-truncated.RData")
#load("created data/ukhls/ukhls-setupsequence-truncated.RData")

