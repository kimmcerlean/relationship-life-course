# ---------------------------------------------------------------------
#    Program: setupsequence_truncated.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: October 8 2026
#    Goal: setup GSOEP for multichannel sequence analysis of couples' life courses;
#   This file focuses on all sequences, including incomplete ones   
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
data <- read_dta("created data/gsoep/gsoep_couples_wide_truncated.dta")
data <- data%>%filter(`_mi_m`!=0)

# Also need to keep people with a minimum sequence length of 3
table(data$sequence_length)
data <- data%>%filter(sequence_length>=3)

## testing with 5 imputations for now to avoid using unique sequences
## (maybe) once we figure this out, can try to add all
# For reference, it's 2^31-1 (which is 46340)
# we have 56290 with 10 imputations, so that drops to 28145
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

## couple_work_trunc: Couple-level work indicator
## HW, let's explore several variables for now:
  # couple_hw_hrs_weekday_trunc (Core HW, Weekday)
  # couple_hw_hrs_4cat_trunc (Core HW, weekday * 5 + sat + sun)
  # couple_hw_hrs_combined_trunc (Core HW + repairs, weekday)
## family_type_5cat_trunc:	Type of family based on relationship type + number of children

# ------------------------------------------------------------------------------
### We identify columns that contain our sequence analysis input variables

t = 1:10 #Number of time units (10 years - don't want to use year 11)

# ------------------------------------------------------------------------------
#Couple Paid Work: columns


lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_trunc",i, sep="")
}
col_work=which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - weekday: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_weekday_trunc",i, sep="")
}
col_hw.hrs.weekday =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
#Couple HW - weekly: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_4cat_trunc",i, sep="")
}
col_hw.hrs =which(colnames(data)%in%lab_t)

# ------------------------------------------------------------------------------
#Couple HW - Core HW + Repairs (weekday): columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_combined_trunc",i, sep="")
}
col_hw.hrs.combined =which(colnames(data)%in%lab_t) 


# ------------------------------------------------------------------------------
#Family type: columns

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_5cat_trunc",i, sep="")
}
col_fam =which(colnames(data)%in%lab_t) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#Couple HW - with amounts (group-specific ptiles): labels 
#Can use same labels for all

shortlab.hw.hrs.combo <- c("W-most:high", "W-most:low",
                           "equal:high", "equal:low", 
                           "M-most:all")

longlab.hw.hrs.combo <- c("woman does most/all: high", "woman does most/all: low",
                          "equal:high", "equal:low", 
                          "man does most: all")

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

# https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/

# ------------------------------------------------------------------------------
#Couple Paid Work: labels

# Work colors
col1 <- sequential_hcl(5, palette = "BuGn") [1:2] #Male BW
col2 <- sequential_hcl(5, palette = "Purples")[c(1)] #Dual FT
col3 <- sequential_hcl(5, palette = "PuRd")[c(2)] #Female BW
col4 <- sequential_hcl(5, palette = "PuRd")[c(1)]  #UnderWork

# Combine to full color palette
colspace.work <- c(col1, col2, col3, col4)

# ------------------------------------------------------------------------------
#Couple HW - detailed (group-specific ptiles): labels 

#Housework colors
# col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col1 <- sequential_hcl(5, palette = "PurpOr")[c(1)] #W-most
col2 <- sequential_hcl(5, palette = "PurpOr")[c(3)] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[2:3] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most

# Combine to full color palette
colspace.hw.hrs.combo <- c(col1, col2, col3, col4)

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
# Creating the sequence objects for each channel
# Here, treating missing as NA to facilitate OM later
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Couple Paid Work
seq.work <- seqdef(data[,col_work], cpal = colspace.work, labels=longlab.work, states= shortlab.work,right=NA)
seqlength(seq.work)
seq.len.work<-seqlength(seq.work, with.missing = FALSE)

ggseqdplot(seq.work) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# So default is actually already missing=false
# for reference (confirm I understand what is happening):
ggseqdplot(seq.work, with.missing=TRUE) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - Weekdays
seq.hw.hrs.weekday <- seqdef(data[,col_hw.hrs.weekday], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.weekday<-seqlength(seq.hw.hrs.weekday, with.missing = FALSE)

ggseqdplot(seq.hw.hrs.weekday) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - Weekly
seq.hw.hrs <- seqdef(data[,col_hw.hrs], cpal = colspace.hw.hrs, labels=longlab.hw.hrs, 
                     states= shortlab.hw.hrs,right=NA)

seq.len.hw<-seqlength(seq.hw.hrs, with.missing = FALSE)

ggseqdplot(seq.hw.hrs) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Couple HW - Combined Weekday
seq.hw.hrs.combined <- seqdef(data[,col_hw.hrs.combined], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                            states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.combined<-seqlength(seq.hw.hrs.combined, with.missing = FALSE)

ggseqdplot(seq.hw.hrs.combined) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Family channel
seq.fam <- seqdef(data[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam<-seqlength(seq.fam, with.missing = FALSE)

ggseqdplot(seq.fam) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cost setting
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


# raw matrix -> sm in the sequence object's alphabet order, rescaled to max cval,
# with the censored state "*" appended at zero cost (what seqcost(with.missing = TRUE) produced)
build_sm <- function(raw, seqobj, cval = 2) {
  alph <- alphabet(seqobj)
  stopifnot(setequal(alph, rownames(raw)), isSymmetric(raw), all(diag(raw) == 0))
  m  <- raw[alph, alph] * cval / max(raw)
  sm <- rbind(cbind(m, 0), 0)
  dimnames(sm) <- list(c(alph, "*"), c(alph, "*"))
  sm
}

build_indel <- function(seqobj) c(rep(1, length(alphabet(seqobj))), 99999)

## Run functions to create costs for both substitutions and indels
work.miss.cost <- list(sm = build_sm(work.raw, seq.work))
hw.miss.cost   <- list(sm = build_sm(hw.raw,   seq.hw.hrs))
fam.miss.cost  <- list(sm = build_sm(fam.raw,  seq.fam))

view(work.miss.cost$sm)
print(work.miss.cost$sm)
view(hw.miss.cost$sm)
print(hw.miss.cost$sm)
view(fam.miss.cost$sm)
print(fam.miss.cost$sm)

work.miss.indel <- build_indel(seq.work)
hw.miss.indel   <- build_indel(seq.hw.hrs)
fam.miss.indel  <- build_indel(seq.fam)

print(work.miss.indel)
print(hw.miss.indel)
print(fam.miss.indel)

# Check: each matrix ends in a zero row/column labelled "*", largest valid cost is 2
round(work.miss.cost$sm, 2); work.miss.indel
round(hw.miss.cost$sm, 2);   hw.miss.indel
round(fam.miss.cost$sm, 2);  fam.miss.indel

# Appendix tables: the rescaled matrices without the censored state
write.csv(round(work.miss.cost$sm[sw, sw], 2), "results/GSOEP/costs_paidwork.csv")
write.csv(round(hw.miss.cost$sm[sh, sh], 2),   "results/GSOEP/costs_housework.csv")
write.csv(round(fam.miss.cost$sm[sf, sf], 2),  "results/GSOEP/costs_family.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dissimilarity matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now use these costs to create NON-normalized matrices
dist.work.om <- seqdist(seq.work, method="OM", indel=work.miss.indel, 
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
save.image("created data/gsoep/gsoep-setupsequence-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exporting figures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All HW options
pdf("results/GSOEP/GSOEP_Base_Sequences_truncated.pdf",
    width=12,
    height=8)

s1<-ggseqdplot(seq.work) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Paid Work") + 
  theme(plot.title=element_text(hjust=0.5))

s2a<-ggseqdplot(seq.hw.hrs.weekday) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Housework (Weekdays)") + 
  theme(plot.title=element_text(hjust=0.5))

s2b<-ggseqdplot(seq.hw.hrs) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Housework (Weekly)") + 
  theme(plot.title=element_text(hjust=0.5))

s2c<-ggseqdplot(seq.hw.hrs.combined) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Housework (Combined Weekday)") + 
  theme(plot.title=element_text(hjust=0.5))

s3<-ggseqdplot(seq.fam) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Relationship Duration", y=NULL) + 
  theme(legend.position="none") +
  ggtitle("Family") + 
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(s2a,s2b,s2c,s1,s3, ncol=3, nrow=2)
dev.off()

# Preferred HW option
pdf("results/GSOEP/GSOEP_Base_Sequences_truncated_v2.pdf",
    width=12,
    height=3)

s1<-ggseqdplot(seq.work) +
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

#pdf("results/GSOEP/GSOEP_Base_Index_truncated.pdf",
#    width=12,
#    height=5)

#i1<-ggseqiplot(seq.fam, sortv="from.start")
#i2<-ggseqiplot(seq.work, sortv="from.start")
#i3<-ggseqiplot(seq.hw.hrs.alt, sortv="from.start")

#grid.arrange(i1,i2,i3, ncol=3, nrow=1)
#dev.off()

#pdf_convert("results/GSOEP/GSOEP_Base_Index_truncated.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "results/GSOEP/GSOEP_Base_Index_truncated.png")


pdf("results/GSOEP/GSOEP_Base_RF_truncated.pdf",
    width=12,
    height=5)

rf1<-ggseqrfplot(seq.fam, diss=dist.fam.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

rf2<-ggseqrfplot(seq.work, diss=dist.work.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

rf3<-ggseqrfplot(seq.hw.hrs, diss=dist.hw.min, k=500, sortv="from.start",
                 which.plot="medoids") + theme(legend.position="none")

grid.arrange(rf1,rf2,rf3, ncol=3, nrow=1)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save objects for further usage in other scripts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/gsoep/gsoep-setupsequence-truncated.RData")
#load("created data/gsoep/gsoep-setupsequence-truncated.RData")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prelim robustness. What would substitution costs be with TRATE?
# (so not even running with this, just exploring)
# One Q: do I do by domain, or using the multichannel sequences?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#seqtrate(seq.work)
#seqtrate(seq.hw.hrs)
#seqtrate(seq.fam)

#work.trate.subcosts <- seqsubm(seq.work, method = "TRATE")
#hw.trate.subcosts <- seqsubm(seq.hw.hrs, method = "TRATE")
#fam.trate.subcosts <- seqsubm(seq.fam, method = "TRATE")

