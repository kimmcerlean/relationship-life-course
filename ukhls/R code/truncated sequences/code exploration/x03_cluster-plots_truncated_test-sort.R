# ---------------------------------------------------------------------
#    Program: cluster-plots
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: June 9 2025
#    Goal: Create relative frequency and state distribution plots
#         comparing across cluster solutions - all sequences, including truncated
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

# set.seed(25)

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
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x")
  
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# Just one imputation so can attempt to run it here to test diff sort options
# For rendering the RF plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import imputed datasets using haven 
data <- read_dta("created data/ukhls/ukhls_couples_wide_truncated.dta")
data <- data%>%filter(`_mi_m`==1)

# Also need to keep people with a minimum sequence length of 3
table(data$sequence_length)
data <- data%>%filter(sequence_length>=3)

# GAH do I actually have to sort here?
# select(data, pidp, sequence_length)

# data <- data[order(data$sequence_length),]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First, create sequence objects for just one imputation (so can run it here)
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

## Now create multi-channel distance
mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                           method="OM", 
                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                           with.missing=TRUE) 

## Divide by length matrix
mcdist.det.min <- mcdist.det.om / fam.min.len

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now create clusters
# We will just do this for 7
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hierarchical cluster analysis, non-squared dissimilarities
mc.det.ward1 <- hclust(as.dist(mcdist.det.min), 
                       method = "ward.D")

# Apply PAM clustering + Ward starting point
# Ward's clustering is explained in Chapter 4

mcdist.om.pam.ward <- wcKMedRange(mcdist.det.min, kvals = 2:10,
                                  initialclust = mc.det.ward1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at 7
mc7 <- mcdist.om.pam.ward$clustering$cluster7 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc7,
         id2 = row_number())

# Obtain relative frequencies of the seven cluster (using weights)
# Convert relative frequencies to percentages (used for labeling the y-axes)

data <- data |>
  count(cluster) |>  # wt = weight40
  mutate(share = n/ sum(n)) |>
  arrange(desc(share)) |> 
  mutate(mc.factor = glue("Cluster {row_number()}
                            ({round(share*100,1)}%)"),
         mc.factor = factor(mc.factor)) |> 
  select(cluster, mc.factor, share) |> 
  right_join(data, by = "cluster") |> 
  arrange(id2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## https://stackoverflow.com/questions/72198066/how-to-sort-a-dataframe-in-the-same-order-as-a-sorted-plot-with-seqiplot-functio

# No sort
# sortv="from.start",dom.crit=2,
# I forgot - if you don't do this, it does the really long option which we prob want to avoid...
#### Relative frequency: 100 K, sort 1a (start, domain1)
#pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_nosort.pdf",
#    width=15,
#    height=42)

#seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
#          group = data$mc.factor, type="rf", diss=mcdist.det.min,
#          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
#          dom.byrow=FALSE,k=100,cex.legend=0.7)

#dev.off()

# Normal
#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_d2start.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
          group = data$mc.factor, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=2,
          cex.legend=0.7)

dev.off()

# Will by end retain the lengths? Oh, sort of, but then yeah, the srts of same length not nec. grouped together
pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_d2end.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
          group = data$mc.factor, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=2,
          cex.legend=0.7)

dev.off()

# Can I just set the sort variable to length?
pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_length.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
          group = data$mc.factor, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv=data$sequence_length, cex.legend=0.7)

dev.off()

# Can I create a custom sort order
# First, sort by family at time 1
# Then sort by length?
# Then create an index to use for sorting?

select(data, pidp, sequence_length, family_type_trunc1)
data <- data[order(data$family_type_trunc1, data$sequence_length),]
data$sort_var <- 1:nrow(data)
select(data, sort_var, pidp, sequence_length, family_type_trunc1, mc.factor)

pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_custom.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
          group = data$mc.factor, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv=data$sort_var, cex.legend=0.7)

dev.off()

## Do I actually have to sort each duration  to make this work?
select(data, pidp, sequence_length, family_type_trunc1,family_type_trunc2,family_type_trunc3,family_type_trunc4,
       family_type_trunc5,family_type_trunc6,family_type_trunc7,family_type_trunc8,family_type_trunc9,family_type_trunc10)
data <- data[order(data$sequence_length, data$family_type_trunc10, data$family_type_trunc9, 
                   data$family_type_trunc8, data$family_type_trunc7, data$family_type_trunc6, 
                   data$family_type_trunc5, data$family_type_trunc4,data$family_type_trunc3, 
                   data$family_type_trunc2, data$family_type_trunc1),]
data$sort_fam_length <- 1:nrow(data)
select(data, sort_fam_length, pidp, sequence_length, family_type_trunc1,family_type_trunc2,family_type_trunc3,family_type_trunc4,
       family_type_trunc5,family_type_trunc6,family_type_trunc7,family_type_trunc8,family_type_trunc9,family_type_trunc10)
# this feels even more chaos

pdf("results/UKHLS/truncated cluster options/testing sort orders/UKHLS_rf_mc7_custom2.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs),
          group = data$mc.factor, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv=data$sort_fam_length, cex.legend=0.7)

dev.off()
