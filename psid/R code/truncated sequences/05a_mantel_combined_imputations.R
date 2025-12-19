# ---------------------------------------------------------------------
#    Program: 05a_mantel_combined_imputations.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: December 19, 2025
#    Goal: Get Mantel coefficients for each imputation, then combine using Rubin's rules
#     (manual workaround to handle permutations)
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo", "mice",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo","mice",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo","mice",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo","mice",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  
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
data <- read_dta("created data/psid_couples_wide_truncated.dta")
# data <- data%>%filter(`_mi_m`!=0) ## Ah - for mids to work, think I need to retain the unimputed data

# Also need to keep people with a minimum sequence length of 3
table(data$sequence_length)
data <- data%>%filter(sequence_length>=3)

## testing with 5 imputations for now to avoid using unique sequences
## it's 2^31-1, so currently too many couples (5828, but 4685 with min seq length)
## so close, we could have 46340 max
data <- data%>%filter(`_mi_m`==0 | `_mi_m`==1 | `_mi_m`==2 | `_mi_m`==3 | `_mi_m`==4 | `_mi_m`==5)
table(data$`_mi_m`)

## Convert to R imputed object
names(data)[names(data) == "_mi_m"] <- ".imp"
names(data)[names(data) == "_mi_id"] <- ".id"

mids_object <- as.mids(data) # got a warning that number of logged events was 1
mids_object$loggedEvents # it relates to variable _mi_miss (this makes sense), think only needed above 2 variables
is.mids(mids_object) # want it to say True

# okay but what is a mids_object lol - is a list of 23

# Analyze the data: Once you have the data in a mids object, you can run analyses (e.g., regressions) on all imputed datasets simultaneously using the with() function and pool the results using pool()
# okay but based on google, this works for the actual Mantel coefficient, but will be harder to do for the confidence intervals...(this is because thhose come from permutations, which will always differ...)
# Should I first manually do for each imputation and see how different they are?
# this will require running all of step 1 and step 5 for each imputation. can I do a loop and store the results somewhere?!

# --------------------------------------------------------------------
# Going to see if I can do some things shared across imputations and
# some that are on specific subsets of data? Let's try this...
# --------------------------------------------------------------------
#data <- read_dta("created data/psid_couples_wide_truncated.dta")
#data <- data%>%filter(sequence_length>=3)
data_mi1 <- data%>%filter(`.imp`==1)
data_mi2 <- data%>%filter(`.imp`==2)
data_mi3 <- data%>%filter(`.imp`==3)
data_mi4 <- data%>%filter(`.imp`==4)
data_mi5 <- data%>%filter(`.imp`==5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sequence prep
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
### We identify columns that contain our sequence analysis input variables

# Let's see if this will be the same across dataframes?

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_work_ow_trunc",i, sep="")
}
col_work.ow=which(colnames(data)%in%lab_t) 
#col_work.ow.m1=which(colnames(data_mi1)%in%lab_t)  # all match
#col_work.ow.m2=which(colnames(data_mi2)%in%lab_t) 

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("couple_hw_hrs_combo_trunc",i, sep="")
}
col_hw.hrs =which(colnames(data)%in%lab_t)
#col_hw.hrs.m3 =which(colnames(data_mi3)%in%lab_t)  # all match
#col_hw.hrs.m4 =which(colnames(data_mi4)%in%lab_t)


lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("family_type_trunc",i, sep="")
}
col_fam =which(colnames(data)%in%lab_t) 
#col_fam.m1 =which(colnames(data_mi1)%in%lab_t)   # all match
#col_fam.m5 =which(colnames(data_mi5)%in%lab_t) 

# ------------------------------------------------------------------------------
### Creating short and long labels
shortlab.work.ow <- c("MBW", "1.5MBW", 
                      "dualFT", "dualFT-anyOW", 
                      "FBW", "underWK")

longlab.work.ow <- c("male breadwinner", "1.5 male breadwinner", 
                     "dual full-time", "dual full-time & any overwork", 
                     "female breadwinner", "under work")


shortlab.hw.hrs.combo <- c("W-most:high", "W-most:low",
                           "equal:high", "equal:low", 
                           "M-most:all")

longlab.hw.hrs.combo <- c("woman does most/all: high", "woman does most/all: low",
                          "equal:high", "equal:low", 
                          "man does most: all")

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

# ------------------------------------------------------------------------------
### Color palette

# Work colors
col1 <- sequential_hcl(5, palette = "BuGn") [1:2] #Male BW
col2 <- sequential_hcl(5, palette = "Purples")[1:2] #Dual FT
col3 <- sequential_hcl(5, palette = "PuRd")[c(2)] #Female BW
col4 <- sequential_hcl(5, palette = "PuRd")[c(1)]  #UnderWork

# Combine to full color palette
colspace.work.ow <- c(col1, col2, col3, col4)

#Housework colors
# col1 <- sequential_hcl(5, palette = "Reds") [1:2] #W-all
col1 <- sequential_hcl(5, palette = "PurpOr")[c(1)] #W-most
col2 <- sequential_hcl(5, palette = "PurpOr")[c(3)] #W-most
col3 <- sequential_hcl(5, palette = "OrYel")[2:3] #Equal
col4 <- sequential_hcl(5, palette = "Teal")[c(2)] #M-most

# Combine to full color palette
colspace.hw.hrs.combo <- c(col1, col2, col3, col4)

# Family colors
col1 <- sequential_hcl(5, palette = "Blues")[4:1]   # Married states
col2 <- sequential_hcl(15, palette = "Inferno")[15:12]   # Cohabitation states
#col3 <- sequential_hcl(5, palette = "Grays")[c(2,4)] # Right-censored states

# Combine to full color palette
colspace.fam <- c(col1, col2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create sequence objects for each imputed dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# just noting, I created lengths for all categories and datasets, but they are all the same, so can just use 1 for all i think
# (because the imputation doesn't affect the length DUH)

#### Imputation 1 ####
# Couple Paid Work - OW
seq.work.ow.mi1 <- seqdef(data_mi1[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.len.work.mi1<-seqlength(seq.work.ow.mi1, with.missing = FALSE)

# Couple HW - amounts v2
seq.hw.hrs.mi1 <- seqdef(data_mi1[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                     states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.mi1<-seqlength(seq.hw.hrs.mi1, with.missing = FALSE)

# Family channel
seq.fam.mi1 <- seqdef(data_mi1[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam.mi1<-seqlength(seq.fam.mi1, with.missing = FALSE)

#### Imputation 2 ####
# Couple Paid Work - OW
seq.work.ow.mi2 <- seqdef(data_mi2[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.len.work.mi2<-seqlength(seq.work.ow.mi2, with.missing = FALSE)

# Couple HW - amounts v2
seq.hw.hrs.mi2 <- seqdef(data_mi2[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.mi2<-seqlength(seq.hw.hrs.mi2, with.missing = FALSE)

# Family channel
seq.fam.mi2 <- seqdef(data_mi2[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam.mi2<-seqlength(seq.fam.mi2, with.missing = FALSE)

#### Imputation 3 ####
# Couple Paid Work - OW
seq.work.ow.mi3 <- seqdef(data_mi3[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.len.work.mi3<-seqlength(seq.work.ow.mi3, with.missing = FALSE)

# Couple HW - amounts v2
seq.hw.hrs.mi3 <- seqdef(data_mi3[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.mi3<-seqlength(seq.hw.hrs.mi3, with.missing = FALSE)

# Family channel
seq.fam.mi3 <- seqdef(data_mi3[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam.mi3<-seqlength(seq.fam.mi3, with.missing = FALSE)

#### Imputation 4 ####
# Couple Paid Work - OW
seq.work.ow.mi4 <- seqdef(data_mi4[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.len.work.mi4<-seqlength(seq.work.ow.mi4, with.missing = FALSE)

# Couple HW - amounts v2
seq.hw.hrs.mi4 <- seqdef(data_mi4[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.mi4<-seqlength(seq.hw.hrs.mi4, with.missing = FALSE)

# Family channel
seq.fam.mi4 <- seqdef(data_mi4[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam.mi4<-seqlength(seq.fam.mi4, with.missing = FALSE)

#### Imputation 5 ####
# Couple Paid Work - OW
seq.work.ow.mi5 <- seqdef(data_mi5[,col_work.ow], cpal = colspace.work.ow, labels=longlab.work.ow, states= shortlab.work.ow,right=NA)

seq.len.work.mi5<-seqlength(seq.work.ow.mi5, with.missing = FALSE)

# Couple HW - amounts v2
seq.hw.hrs.mi5 <- seqdef(data_mi5[,col_hw.hrs], cpal = colspace.hw.hrs.combo, labels=longlab.hw.hrs.combo, 
                         states= shortlab.hw.hrs.combo,right=NA)

seq.len.hw.mi5<-seqlength(seq.hw.hrs.mi5, with.missing = FALSE)

# Family channel
seq.fam.mi5 <- seqdef(data_mi5[,col_fam], cpal = colspace.fam, labels=longlab.fam, states= shortlab.fam,right=NA)

seq.len.fam.mi5<-seqlength(seq.fam.mi5, with.missing = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dissimilarity matrices
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------------------------------------------------
### First define costs (can I think create one set of costs for all? but let's validate?)
# First set costs of sm to 0 for missing
fam.miss.cost <- seqcost(seq.fam.mi1, method="CONSTANT", 
                         miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

work.miss.cost <- seqcost(seq.work.ow.mi1, method="CONSTANT", 
                          miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

hw.miss.cost <- seqcost(seq.hw.hrs.mi1, method="CONSTANT", 
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

# ------------------------------------------------------------------------------
# Then create matrices of shortest length - think can do once and use for all?
# is that risky? can def just do for one domain so let's do 5 jic...
min.len.mi1 <- matrix(NA,ncol=length(seq.len.fam.mi1),nrow=length(seq.len.fam.mi1))
for (i in 1:length(seq.len.fam.mi1)){
  for (j in 1:length(seq.len.fam.mi1)){
    min.len.mi1[i,j] <- min(c(seq.len.fam.mi1[i],seq.len.fam.mi1[j]))
  }
}

min.len.mi2 <- matrix(NA,ncol=length(seq.len.fam.mi2),nrow=length(seq.len.fam.mi2))
for (i in 1:length(seq.len.fam.mi2)){
  for (j in 1:length(seq.len.fam.mi2)){
    min.len.mi2[i,j] <- min(c(seq.len.fam.mi2[i],seq.len.fam.mi2[j]))
  }
}

min.len.mi3 <- matrix(NA,ncol=length(seq.len.fam.mi3),nrow=length(seq.len.fam.mi3))
for (i in 1:length(seq.len.fam.mi3)){
  for (j in 1:length(seq.len.fam.mi3)){
    min.len.mi3[i,j] <- min(c(seq.len.fam.mi3[i],seq.len.fam.mi3[j]))
  }
}

min.len.mi4 <- matrix(NA,ncol=length(seq.len.fam.mi4),nrow=length(seq.len.fam.mi4))
for (i in 1:length(seq.len.fam.mi4)){
  for (j in 1:length(seq.len.fam.mi4)){
    min.len.mi4[i,j] <- min(c(seq.len.fam.mi4[i],seq.len.fam.mi4[j]))
  }
}

min.len.mi5 <- matrix(NA,ncol=length(seq.len.fam.mi5),nrow=length(seq.len.fam.mi5))
for (i in 1:length(seq.len.fam.mi5)){
  for (j in 1:length(seq.len.fam.mi5)){
    min.len.mi5[i,j] <- min(c(seq.len.fam.mi5[i],seq.len.fam.mi5[j]))
  }
}

# ------------------------------------------------------------------------------
# Now use these costs to create NON-normalized matrices BY imputation

#### Imputation 1 ####
dist.work.om.mi1 <- seqdist(seq.work.ow.mi1, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om.mi1 <- seqdist(seq.hw.hrs.mi1, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om.mi1 <- seqdist(seq.fam.mi1, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)

#### Imputation 2 ####
dist.work.om.mi2 <- seqdist(seq.work.ow.mi2, method="OM", indel=work.miss.indel, 
                            sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om.mi2 <- seqdist(seq.hw.hrs.mi2, method="OM", indel=hw.miss.indel, 
                          sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om.mi2 <- seqdist(seq.fam.mi2, method="OM", indel=fam.miss.indel, 
                           sm= fam.miss.cost$sm, with.missing=TRUE)

#### Imputation 3 ####
dist.work.om.mi3 <- seqdist(seq.work.ow.mi3, method="OM", indel=work.miss.indel, 
                            sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om.mi3 <- seqdist(seq.hw.hrs.mi3, method="OM", indel=hw.miss.indel, 
                          sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om.mi3 <- seqdist(seq.fam.mi3, method="OM", indel=fam.miss.indel, 
                           sm= fam.miss.cost$sm, with.missing=TRUE)

#### Imputation 4 ####
dist.work.om.mi4 <- seqdist(seq.work.ow.mi4, method="OM", indel=work.miss.indel, 
                            sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om.mi4 <- seqdist(seq.hw.hrs.mi4, method="OM", indel=hw.miss.indel, 
                          sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om.mi4 <- seqdist(seq.fam.mi4, method="OM", indel=fam.miss.indel, 
                           sm= fam.miss.cost$sm, with.missing=TRUE)

#### Imputation 5 ####
dist.work.om.mi5 <- seqdist(seq.work.ow.mi5, method="OM", indel=work.miss.indel, 
                            sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om.mi5 <- seqdist(seq.hw.hrs.mi5, method="OM", indel=hw.miss.indel, 
                          sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om.mi5 <- seqdist(seq.fam.mi5, method="OM", indel=fam.miss.indel, 
                           sm= fam.miss.cost$sm, with.missing=TRUE)

# ------------------------------------------------------------------------------
# Now normalize all

#### Imputation 1 ####
dist.fam.min.mi1<-dist.fam.om.mi1 / min.len.mi1
dist.work.min.mi1<-dist.work.om.mi1 / min.len.mi1
dist.hw.min.mi1<-dist.hw.om.mi1 / min.len.mi1

#### Imputation 2 ####
dist.fam.min.mi2<-dist.fam.om.mi2 / min.len.mi2
dist.work.min.mi2<-dist.work.om.mi2 / min.len.mi2
dist.hw.min.mi2<-dist.hw.om.mi2 / min.len.mi2

#### Imputation 3 ####
dist.fam.min.mi3<-dist.fam.om.mi3 / min.len.mi3
dist.work.min.mi3<-dist.work.om.mi3 / min.len.mi3
dist.hw.min.mi3<-dist.hw.om.mi3 / min.len.mi3

#### Imputation 4 ####
dist.fam.min.mi4<-dist.fam.om.mi4 / min.len.mi4
dist.work.min.mi4<-dist.work.om.mi4 / min.len.mi4
dist.hw.min.mi4<-dist.hw.om.mi4 / min.len.mi4

#### Imputation 5 ####
dist.fam.min.mi5<-dist.fam.om.mi5 / min.len.mi5
dist.work.min.mi5<-dist.work.om.mi5 / min.len.mi5
dist.hw.min.mi5<-dist.hw.om.mi5 / min.len.mi5

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mantels (might need to go to HPC now) - 100 permutations, by imputation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Imputation 1 ####
mantel.work.hw.ci.mi1 <- mantel(lower(dist.work.min.mi1) ~ lower(dist.hw.min.mi1), nperm=100)
mantel.work.fam.ci.mi1 <- mantel(lower(dist.work.min.mi1) ~ lower(dist.fam.min.mi1), nperm=100)
mantel.hw.fam.ci.mi1 <- mantel(lower(dist.hw.min.mi1) ~ lower(dist.fam.min.mi1), nperm=100)

mantel.df.work.hw.ci.mi1 <- data.frame(mantel.work.hw.ci.mi1)
mantel.df.work.fam.ci.mi1 <- data.frame(mantel.work.fam.ci.mi1)
mantel.df.hw.fam.ci.mi1 <- data.frame(mantel.hw.fam.ci.mi1)

mantel.col <- c('mantelr','pval1','pval2','pval3','llim.2.5%','ulim.97.5%')
# will just use this for all

mantel.df.mi1 <- data.frame(mantel.col, mantel.df.work.hw.ci.mi1, 
                            mantel.df.work.fam.ci.mi1, mantel.df.hw.fam.ci.mi1)

write.xlsx(mantel.df.mi1, "results/PSID/psid_mantel_truncated_100p_mi1.xlsx")

#### Imputation 2 ####
mantel.work.hw.ci.mi2 <- mantel(lower(dist.work.min.mi2) ~ lower(dist.hw.min.mi2), nperm=100)
mantel.work.fam.ci.mi2 <- mantel(lower(dist.work.min.mi2) ~ lower(dist.fam.min.mi2), nperm=100)
mantel.hw.fam.ci.mi2 <- mantel(lower(dist.hw.min.mi2) ~ lower(dist.fam.min.mi2), nperm=100)

mantel.df.work.hw.ci.mi2 <- data.frame(mantel.work.hw.ci.mi2)
mantel.df.work.fam.ci.mi2 <- data.frame(mantel.work.fam.ci.mi2)
mantel.df.hw.fam.ci.mi2 <- data.frame(mantel.hw.fam.ci.mi2)

mantel.df.mi2 <- data.frame(mantel.col, mantel.df.work.hw.ci.mi2, 
                            mantel.df.work.fam.ci.mi2, mantel.df.hw.fam.ci.mi2)

write.xlsx(mantel.df.mi2, "results/PSID/psid_mantel_truncated_100p_mi2.xlsx")

#### Imputation 3 ####
mantel.work.hw.ci.mi3 <- mantel(lower(dist.work.min.mi3) ~ lower(dist.hw.min.mi3), nperm=100)
mantel.work.fam.ci.mi3 <- mantel(lower(dist.work.min.mi3) ~ lower(dist.fam.min.mi3), nperm=100)
mantel.hw.fam.ci.mi3 <- mantel(lower(dist.hw.min.mi3) ~ lower(dist.fam.min.mi3), nperm=100)

mantel.df.work.hw.ci.mi3 <- data.frame(mantel.work.hw.ci.mi3)
mantel.df.work.fam.ci.mi3 <- data.frame(mantel.work.fam.ci.mi3)
mantel.df.hw.fam.ci.mi3 <- data.frame(mantel.hw.fam.ci.mi3)

mantel.df.mi3 <- data.frame(mantel.col, mantel.df.work.hw.ci.mi3, 
                            mantel.df.work.fam.ci.mi3, mantel.df.hw.fam.ci.mi3)

write.xlsx(mantel.df.mi3, "results/PSID/psid_mantel_truncated_100p_mi3.xlsx")

#### Imputation 4 ####
mantel.work.hw.ci.mi4 <- mantel(lower(dist.work.min.mi4) ~ lower(dist.hw.min.mi4), nperm=100)
mantel.work.fam.ci.mi4 <- mantel(lower(dist.work.min.mi4) ~ lower(dist.fam.min.mi4), nperm=100)
mantel.hw.fam.ci.mi4 <- mantel(lower(dist.hw.min.mi4) ~ lower(dist.fam.min.mi4), nperm=100)

mantel.df.work.hw.ci.mi4 <- data.frame(mantel.work.hw.ci.mi4)
mantel.df.work.fam.ci.mi4 <- data.frame(mantel.work.fam.ci.mi4)
mantel.df.hw.fam.ci.mi4 <- data.frame(mantel.hw.fam.ci.mi4)

mantel.df.mi4 <- data.frame(mantel.col, mantel.df.work.hw.ci.mi4, 
                            mantel.df.work.fam.ci.mi4, mantel.df.hw.fam.ci.mi4)

write.xlsx(mantel.df.mi4, "results/PSID/psid_mantel_truncated_100p_mi4.xlsx")

#### Imputation 5 ####
mantel.work.hw.ci.mi5 <- mantel(lower(dist.work.min.mi5) ~ lower(dist.hw.min.mi5), nperm=100)
mantel.work.fam.ci.mi5 <- mantel(lower(dist.work.min.mi5) ~ lower(dist.fam.min.mi5), nperm=100)
mantel.hw.fam.ci.mi5 <- mantel(lower(dist.hw.min.mi5) ~ lower(dist.fam.min.mi5), nperm=100)

mantel.df.work.hw.ci.mi5 <- data.frame(mantel.work.hw.ci.mi5)
mantel.df.work.fam.ci.mi5 <- data.frame(mantel.work.fam.ci.mi5)
mantel.df.hw.fam.ci.mi5 <- data.frame(mantel.hw.fam.ci.mi5)

mantel.df.mi5 <- data.frame(mantel.col, mantel.df.work.hw.ci.mi5, 
                            mantel.df.work.fam.ci.mi5, mantel.df.hw.fam.ci.mi5)

write.xlsx(mantel.df.mi5, "results/PSID/psid_mantel_truncated_100p_mi5.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempting to pool
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Intermission to restructure in Excel - can figure out maybe how to do this in R later...

mantel_data <- read.csv("results/PSID/tables/pooled mantels/psid_mantel_truncated_100p_mis-combined.csv")

mantel <- mantel_data$mantel_r
lower <- mantel_data$ci_lower
upper <- mantel_data$ci_upper

se <- (upper - lower) / (2 * 1.96) # calculate estimated SE (have notes on the validity of doing this)
variance  <- se^2 # variance

pool_mantel <- function(mantel, lower, upper) {
  M <- length(mantel)
  se <- (upper - lower) / (2 * 1.96)
  variance  <- se^2
  
  mantel_bar <- mean(mantel)
  var_bar <- mean(variance)
  B <- var(mantel)
  
  Tvar <- var_bar + (1 + 1/M) * B
  SE_pooled <- sqrt(Tvar)
  
  nu <- (M - 1) * (1 + var_bar / ((1 + 1/M) * B))^2
  CI <- mantel_bar + c(-1, 1) * qt(0.975, df = nu) * SE_pooled
  
  list(
    pooled_mantel = mantel_bar,
    pooled_SE = SE_pooled,
    df = nu,
    CI_lower = CI[1],
    CI_upper = CI[2]
  )
}

result <- pool_mantel(mantel, lower, upper)
result

# Apply by mantel_set
results <- mantel_data %>%
  group_by(mantel_set) %>%
  summarise(pool = list(pool_mantel(mantel_r, ci_lower, ci_upper))) %>%
  tidyr::unnest_wider(pool)

results # I am ~stunned~ this worked as expected

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/psid_pooled_mantels.RData")
# let's see if this will save on my computer, might need to run in HPC to get it to save pre-Excel step
# it worked!

###### Reference code for visualizing across imputations
###### (currently based on manual input)
###### Not using at the moment

#library(ggplot2)

#work_hw <- data.frame(
#  imputation = 1:5,
#  theta = c(0.074969339, 0.073429433, 0.07262276, 0.068816612, 0.074384387),
#  se = c(0.00163, 0.00155, 0.00157, 0.00169, 0.00159)
#)

#theta_pooled <- 0.07284
#se_pooled <- 0.00313

#ggplot(work_hw, aes(x = imputation, y = theta)) +
  # Single-imputation SE (green)
#  geom_errorbar(aes(ymin = theta - se, ymax = theta + se), width = 0.2, color = "green", linewidth = 1) +
  # Imputation points (blue)
#  geom_point(size = 3, color = "blue") +
  # Labels for each point
#  geom_text(aes(label = round(theta, 4)), vjust = -1, color = "blue", size = 3.5) +
  # Pooled θ̄ line (red dashed)
#  geom_hline(yintercept = theta_pooled, color = "red", linetype = "dashed", linewidth = 1) +
  # Pooled SE band (red shaded)
#  annotate("rect", xmin = 0.5, xmax = 5.5,
#           ymin = theta_pooled - se_pooled,
#           ymax = theta_pooled + se_pooled,
#           alpha = 0.2, fill = "red") +
  # Label pooled θ̄
#  annotate("text", x = 5.5, y = theta_pooled + 0.0015, label = paste0("Pooled θ = ", round(theta_pooled, 4)), 
#           color = "red", hjust = 1, size = 4) +
#  labs(
#    x = "Imputation",
#    y = "Mantel coefficient θ",
#    title = "Work_hw: Single-Imputation vs Pooled SE"
#  ) +
#  theme_minimal()
