# ---------------------------------------------------------------------
#    Program: 00_setupsequence.R
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: June 16 2025
#    Goal: setup UKHLS for multichannel sequence analysis of couples' life courses;
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "matrixStats",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "matrixStats",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "matrixStats",
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "matrixStats",
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

# Adding this for now to use LP added code, but we have to work out of a system to integrate code better.
# I am not changing this everywhere for now
country <- "UKHLS"          # "GSOEP", "UKHLS", "PSID"

# ~~~~~~~~~~~~~~~~
# Import data ----
# ~~~~~~~~~~~~~~~~

# Import imputed datasets using haven 
data <- read_dta("created data/ukhls/ukhls_couples_wide_truncated.dta")
data <- data%>%filter(`_mi_m`!=0)

# Also need to keep people with a minimum sequence length of 3
table(data$sequence_length)
data <- data%>%filter(sequence_length>=3)

## testing with 5 imputations for now to avoid using unique sequences
## it's 2^31-1, so currently too many couples
## so close, we could have 46340 max
# have to explore with mi of just 1 because don't think will otherwise run on this computer
# data <- data%>%filter(`_mi_m`==1)
data <- data%>%filter(`_mi_m`==1 | `_mi_m`==2 | `_mi_m`==3 | `_mi_m`==4 | `_mi_m`==5)
table(data$`_mi_m`)

# confirm variables made it here
table(data$division_of_labor_trunc1)
table(data$egal_dol_yn_trunc1)
table(data$egal_dol_yn_trunc5)
table(data$hw_egal_trunc5)
table(data$detailed_dol_trunc5)

# >>> ADDED: rows are couple x imputation; counts are divided by this to be reported as couples
n_imp <- length(unique(data$`_mi_m`))
# <<< END ADDED

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

## division_of_labor_trunc: categorical egal, male BW, female BW, other
## egal_dol_yn_trunc: the binary version of above
## egalitarian_trunc: modified egalitarian - dual FT, equal OR he does more HW
## dual_work_trunc: Dual FT or Dual PT
## dual_ft_trunc: Dual FT only
## hw_mod_egal_trunc: equal OR he does more HW
## hw_egal_trunc: Just equal HW
## detailed_dol_trunc

# ------------------------------------------------------------------------------
### We identify columns that contain our sequence analysis input variables

t = 1:10 #Number of time units (10 years - don't want to use year 11)

# ------------------------------------------------------------------------------
# Categorical Division of Labor

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("division_of_labor_trunc",i, sep="")
}
col_dol=which(colnames(data)%in%lab_t) 

# Detailed Division of Labor

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("detailed_dol_trunc",i, sep="")
}
col_det_dol=which(colnames(data)%in%lab_t)

# ------------------------------------------------------------------------------
# Binary version of above

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("egal_dol_yn_trunc",i, sep="")
}
col_dol.egal =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Modified egal: more liberal definition

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("egalitarian_trunc",i, sep="")
}
col_mod_egal =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Paid work: dual FT or PT

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("dual_work_trunc",i, sep="")
}
col_dual_work =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Paid work: Just Dual FT

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("dual_ft_trunc",i, sep="")
}
col_dual_ft =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Housework: Egal or he does more

lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("hw_mod_egal_trunc",i, sep="")
}
col_hw_mod_egal =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Housework: Just egal
lab_t=c()
for (i in 1:10){
  lab_t[i]=paste("hw_egal_trunc",i, sep="")
}
col_hw_egal =which(colnames(data)%in%lab_t) 

# ------------------------------------------------------------------------------
# Need to add individual sequence domains
col_work       <- which(colnames(data) %in% paste0("couple_work_trunc", 1:10))
col_hw.hrs <- which(colnames(data) %in% paste0("couple_hw_hrs_4cat_trunc", 1:10))
col_fam           <- which(colnames(data) %in% paste0("family_type_5cat_trunc", 1:10))
stopifnot(length(col_work) == 10, length(col_hw.hrs) == 10, length(col_fam) == 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating short and long labels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Categorical DoL

lab.dol <- c("Egalitarian", 
             "Traditional",
             "Counter-Trad", 
             "Other")

lab.dol.egal <- c("Not Egalitarian", "Classic Egalitarian")

lab.mod.egal <- c("Not Egalitarian", "Modified Egalitarian")

lab.dual.work <- c("Not Dual Work", "Dual FT or PT")

lab.dual.ft <- c("Not Dual FT", "Dual FT")

lab.hw.mod.egal <- c("Not Egalitarian", "Egal or He Does More HW")

lab.hw.egal <- c("Not Egalitarian", "Egalitarian HW")

lab.detailed.dol <- c("Egal Work x Egal HW", "Egal Work x Her HW", "Egal Work x His HW",
                      "Male BW x Egal HW", "Male BW x Her HW", "Male BW x His HW",
                      "Female BW x Egal HW", "Female BW x Her HW", "Female BW x His HW",
                      "Neither Working")

shortlab.work <- c("MBW", "1.5MBW", "dualFT", "FBW", "underWK")
shortlab.hw.hrs <- c("W-most:high", "W-most:low", "equal:all", "M-most:all")
shortlab.fam <- c("MARc0", "MARc1", "MARc2", "COHc0", "COHc1")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define different color palettes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/

# ------------------------------------------------------------------------------
# Division of Labor
colspace.dol <- sequential_hcl(5, palette = "Hawaii") [1:4]

# Binary Egalitarian Variables
col1 <- sequential_hcl(5, palette = "Grays")[c(3)] #Not Egal
col2 <- sequential_hcl(5, palette = "Hawaii")[c(1)]  #Egal
colspace.egal <- c(col1, col2)

# Detailed DoL
col1 <- sequential_hcl(5, palette = "Purp")[c(1,3,4)] # Dual Work
col2 <- sequential_hcl(5, palette = "Greens")[c(1,3,4)] # Male BW
col3 <- sequential_hcl(5, palette = "Reds")[2:4] # Female BW
col4 <- sequential_hcl(5, palette = "Grays")[c(3)] # Not Working

colspace.det.dol<- c(col1, col2, col3, col4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating the sequence objects for each channel
# Here, treating missing as NA to facilitate OM later
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Division of labor
seq.dol <- seqdef(data[,col_dol], cpal = colspace.dol, labels=lab.dol, 
                  states= lab.dol, right=NA)

seq.len.all<-seqlength(seq.dol, with.missing = FALSE)

ggseqdplot(seq.dol) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# So default is actually already missing=false
# for reference (confirm I understand what is happening):
ggseqdplot(seq.dol, with.missing=TRUE) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Detailed Division of labor
seq.det.dol <- seqdef(data[,col_det_dol], cpal = colspace.det.dol, labels=lab.detailed.dol, 
                      states= lab.detailed.dol, right=NA)


ggseqdplot(seq.det.dol) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Classic Egalitarian
seq.dol.egal <- seqdef(data[,col_dol.egal], cpal = colspace.egal, labels=lab.dol.egal, 
                       states= lab.dol.egal,right=NA)

ggseqdplot(seq.dol.egal) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Modified Egalitarian
seq.mod.egal <- seqdef(data[,col_mod_egal], cpal = colspace.egal, labels=lab.mod.egal, 
                       states= lab.mod.egal,right=NA)

ggseqdplot(seq.mod.egal) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")


# Dual FT or PT Paid Work
seq.dual.work <- seqdef(data[,col_dual_work], cpal = colspace.egal, labels=lab.dual.work, 
                        states= lab.dual.work,right=NA)

ggseqdplot(seq.dual.work) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Just Dual FT Paid Work
seq.dual.ft <- seqdef(data[,col_dual_ft], cpal = colspace.egal, labels=lab.dual.ft, 
                      states= lab.dual.ft,right=NA)

ggseqdplot(seq.dual.ft) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")


# Housework: Egal or He Does More
seq.hw.mod.egal <- seqdef(data[,col_hw_mod_egal], cpal = colspace.egal, labels=lab.hw.mod.egal, 
                          states= lab.hw.mod.egal,right=NA)

ggseqdplot(seq.hw.mod.egal) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# Housework: JUst Egal
seq.hw.egal <- seqdef(data[,col_hw_egal], cpal = colspace.egal, labels=lab.hw.egal, 
                      states= lab.hw.egal,right=NA)

ggseqdplot(seq.hw.egal) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")

# >>> ADDED: the three channel objects (no colours needed here, they are not plotted)
seq.work    <- seqdef(data[, col_work],       states = shortlab.work,       right = NA)
seq.hw.hrs <- seqdef(data[, col_hw.hrs], states = shortlab.hw.hrs, right = NA)
seq.fam           <- seqdef(data[, col_fam],           states = shortlab.fam,           right = NA)
stopifnot(all(seqlength(seq.work, with.missing = FALSE) == seq.len.all))

## Check totals + create matrix version of variables to use later
m.work <- as.matrix(seq.work);       m.work[m.work %in% c("*", "%")] <- NA
m.hw   <- as.matrix(seq.hw.hrs); m.hw[m.hw %in% c("*", "%")]     <- NA
m.fam  <- as.matrix(seq.fam);           m.fam[m.fam %in% c("*", "%")]   <- NA
m.egal <- as.matrix(seq.dol.egal) == "Classic Egalitarian";  m.egal[is.na(m.work)] <- NA
m.mod  <- as.matrix(seq.mod.egal) == "Modified Egalitarian"; m.mod[is.na(m.work)]  <- NA

#  I technically made variables for these, but in the interest of not just adding sequence objects, I think this works.
# Oh, I think I have these above? this is fine anyway...

# BUT for posterity, updating to our NEW alphabet. 
m.dualft  <- m.work %in% c("dualFT"); dim(m.dualft)  <- dim(m.work); m.dualft[is.na(m.work)] <- NA
m.equalhw <- m.hw   %in% c("equal:all");  dim(m.equalhw) <- dim(m.hw);  m.equalhw[is.na(m.hw)] <- NA
print(mean((m.dualft & m.equalhw) == m.egal, na.rm = TRUE))   # expect 1

# 99% sure it's not 1 because of dual FT v. FT + PT. Need to align on definition, though dual PT is so small. Okay, yes confirmed here
m.dual.work  <- as.matrix(seq.dual.work) == "Dual FT or PT"; m.mod[is.na(m.work)]  <- NA
print(mean((m.dual.work & m.equalhw) == m.egal, na.rm = TRUE)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exploring possibilities of extracting max time spent egal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# False is better, because otherwise, the time spent missing is last value here (which I guess isn't horrible)
# SO REALLY THOUGH - i want this but JUST for egal states

# seqdur(seq.dol, with.missing=TRUE)
seqdur(seq.dol, with.missing=FALSE)

# seqdur(seq.dol.egal, with.missing=TRUE)
seqdur(seq.dol.egal, with.missing=FALSE)
# problem is - this still doesn't get there because both have a duration

seqistatd(seq.dol)
# So this is sort of what I did in Stata but this is TOTAL not consecutive time

seqdss(seq.dol)
# I guess this technically adds cateogries to above? Can I merge in some way?

# I think this works
#~~~~~~~~~~~~~~~~~~~~~~
# this is CLASSIC egalitarian
#~~~~~~~~~~~~~~~~~~~~~~
dur.dol.egal<-seqdur(seq.dol.egal, with.missing=FALSE)
dss.dol.egal<-seqdss(seq.dol.egal)

dol_egal_state <- "Classic Egalitarian"
dol_egal_durations <- as.matrix(dur.dol.egal) * (as.matrix(dss.dol.egal) == dol_egal_state)
max_consecutive_dol_egal <- rowMaxs(dol_egal_durations, na.rm = TRUE)
print(max_consecutive_dol_egal)

# Can i add this back on to data?
data$max_dur_dol_egal <- max_consecutive_dol_egal
#data$all_durs_dol_egal <- dur.dol.egal  # not working because Traminer objects. have to d asmatrix first, let's dealw ith that later
#data$dss_dol_egal <- dss.dol.egal
#data$dol_egal_durations <- dol_egal_durations
# attr(seq.dol.egal, "max_dur_dol_egal") <- max_consecutive_dol_egal
colnames(dol_egal_durations) <- paste0("dol_egal_spell_", 1:ncol(dol_egal_durations))
data <- cbind(data, dol_egal_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.dol.egal <- mean(max_consecutive_dol_egal, na.rm = TRUE)
print(avg_all.dol.egal)

# ONLY people who entered egal
avg_experienced.dol.egal <- mean(max_consecutive_dol_egal[max_consecutive_dol_egal > 0], na.rm = TRUE)
print(avg_experienced.dol.egal)

#
dol_egal_avg_by_sequence_length <- aggregate(max_dur_dol_egal ~ sequence_length, 
                                             data = data, 
                                             FUN = mean)

print(dol_egal_avg_by_sequence_length)

#~~~~~~~~~~~~~~~~~~~~~~
# MODIFIED egalitarian
#~~~~~~~~~~~~~~~~~~~~~~
dur.mod.egal<-seqdur(seq.mod.egal, with.missing=FALSE)
dss.mod.egal<-seqdss(seq.mod.egal)

egal_state <- "Modified Egalitarian"
egal_durations <- as.matrix(dur.mod.egal) * (as.matrix(dss.mod.egal) == egal_state)
max_consecutive_mod_egal <- rowMaxs(egal_durations, na.rm = TRUE)
print(max_consecutive_mod_egal)

# Can i add this back on to data?
data$max_dur_mod_egal <- max_consecutive_mod_egal
#data$all_durs_mod_egal <- dur.mod.egal # not working because Traminer objects. have to d asmatrix first, let's dealw ith that later
#data$dss_mod_egal <- dss.mod.egal
#data$mod_egal_durations <- egal_durations
colnames(egal_durations) <- paste0("mod_egal_spell_", 1:ncol(egal_durations))
data <- cbind(data, egal_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.mod.egal <- mean(max_consecutive_mod_egal, na.rm = TRUE)
print(avg_all.mod.egal)

# ONLY people who entered egal
avg_experienced.mod.egal <- mean(max_consecutive_mod_egal[max_consecutive_mod_egal > 0], na.rm = TRUE)
print(avg_experienced.mod.egal)

#
mod_egal_avg_by_sequence_length <- aggregate(max_dur_mod_egal ~ sequence_length, 
                                             data = data, 
                                             FUN = mean)

print(mod_egal_avg_by_sequence_length)

#~~~~~~~~~~~~~~~~~~~~~~
# Dual FT OR Dual PT
#~~~~~~~~~~~~~~~~~~~~~~
dur.dual.work<-seqdur(seq.dual.work, with.missing=FALSE)
dss.dual.work<-seqdss(seq.dual.work)

dual_work_state <- "Dual FT or PT"
dual_work_durations <- as.matrix(dur.dual.work) * (as.matrix(dss.dual.work) == dual_work_state)
max_consecutive_dual_work <- rowMaxs(dual_work_durations, na.rm = TRUE)
print(max_consecutive_dual_work)

# Can i add this back on to data?
data$max_dur_dual_work <- max_consecutive_dual_work
colnames(dual_work_durations) <- paste0("dual_work_spell_", 1:ncol(dual_work_durations))
data <- cbind(data, dual_work_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.dual.work <- mean(max_consecutive_dual_work, na.rm = TRUE)
print(avg_all.dual.work)

# ONLY people who entered egal
avg_experienced.dual.work <- mean(max_consecutive_dual_work[max_consecutive_dual_work > 0], na.rm = TRUE)
print(avg_experienced.dual.work)

#
dual_work_avg_by_sequence_length <- aggregate(max_dur_dual_work ~ sequence_length, 
                                              data = data, 
                                              FUN = mean)

print(dual_work_avg_by_sequence_length)

#~~~~~~~~~~~~~~~~~~~~~~
# Dual FT
#~~~~~~~~~~~~~~~~~~~~~~
dur.dual.ft<-seqdur(seq.dual.ft, with.missing=FALSE)
dss.dual.ft<-seqdss(seq.dual.ft)

dual_ft_state <- "Dual FT"
dual_ft_durations <- as.matrix(dur.dual.ft) * (as.matrix(dss.dual.ft) == dual_ft_state)
max_consecutive_dual_ft <- rowMaxs(dual_ft_durations, na.rm = TRUE)
print(max_consecutive_dual_ft)

# Can i add this back on to data?
data$max_dur_dual_ft <- max_consecutive_dual_ft
colnames(dual_ft_durations) <- paste0("dual_ft_spell_", 1:ncol(dual_ft_durations))
data <- cbind(data, dual_ft_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.dual.ft <- mean(max_consecutive_dual_ft, na.rm = TRUE)
print(avg_all.dual.ft)

# ONLY people who entered egal
avg_experienced.dual.ft <- mean(max_consecutive_dual_ft[max_consecutive_dual_ft > 0], na.rm = TRUE)
print(avg_experienced.dual.ft)

#
dual_ft_avg_by_sequence_length <- aggregate(max_dur_dual_ft ~ sequence_length, 
                                            data = data, 
                                            FUN = mean)

print(dual_ft_avg_by_sequence_length)

#~~~~~~~~~~~~~~~~~~~~~~
# HW: Egal or He Does More
#~~~~~~~~~~~~~~~~~~~~~~
dur.hw.mod.egal<-seqdur(seq.hw.mod.egal, with.missing=FALSE)
dss.hw.mod.egal<-seqdss(seq.hw.mod.egal)

hw_mod_egal_state <- "Egal or He Does More HW"
hw_mod_egal_durations <- as.matrix(dur.hw.mod.egal) * (as.matrix(dss.hw.mod.egal) == hw_mod_egal_state)
max_consecutive_hw_mod_egal <- rowMaxs(hw_mod_egal_durations, na.rm = TRUE)
print(max_consecutive_hw_mod_egal)

# Can i add this back on to data?
data$max_dur_hw_mod_egal <- max_consecutive_hw_mod_egal
colnames(hw_mod_egal_durations) <- paste0("hw_mod_egal_spell_", 1:ncol(hw_mod_egal_durations))
data <- cbind(data, hw_mod_egal_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.hw.mod.egal <- mean(max_consecutive_hw_mod_egal, na.rm = TRUE)
print(avg_all.hw.mod.egal)

# ONLY people who entered egal
avg_experienced.hw.mod.egal <- mean(max_consecutive_hw_mod_egal[max_consecutive_hw_mod_egal > 0], na.rm = TRUE)
print(avg_experienced.hw.mod.egal)

#
hw_mod_egal_avg_by_sequence_length <- aggregate(max_dur_hw_mod_egal ~ sequence_length, 
                                                data = data, 
                                                FUN = mean)

print(hw_mod_egal_avg_by_sequence_length)

#~~~~~~~~~~~~~~~~~~~~~~
# HW: Just Egal
#~~~~~~~~~~~~~~~~~~~~~~
dur.hw.egal<-seqdur(seq.hw.egal, with.missing=FALSE)
dss.hw.egal<-seqdss(seq.hw.egal)

hw_egal_state <- "Egalitarian HW"
hw_egal_durations <- as.matrix(dur.hw.egal) * (as.matrix(dss.hw.egal) == hw_egal_state)
max_consecutive_hw_egal <- rowMaxs(hw_egal_durations, na.rm = TRUE)
print(max_consecutive_hw_egal)

# Can i add this back on to data?
data$max_dur_hw_egal <- max_consecutive_hw_egal
colnames(hw_egal_durations) <- paste0("hw_egal_spell_", 1:ncol(hw_egal_durations))
data <- cbind(data, hw_egal_durations)

# And calculate averages?
# Include people who did not enter egal
avg_all.hw.egal <- mean(max_consecutive_hw_egal, na.rm = TRUE)
print(avg_all.hw.egal)

# ONLY people who entered egal
avg_experienced.hw.egal <- mean(max_consecutive_hw_egal[max_consecutive_hw_egal > 0], na.rm = TRUE)
print(avg_experienced.hw.egal)

#
hw_egal_avg_by_sequence_length <- aggregate(max_dur_hw_egal ~ sequence_length, 
                                            data = data, 
                                            FUN = mean)

print(hw_egal_avg_by_sequence_length)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export for Stata
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write_dta(data, "created data/ukhls/ukhls_wide_truncated_Rdurs.dta")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transition rate matrix. Is this useful? Might be useful, but maybe not in this format?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# common transitions
seqtrate(seq.dol)
seqtrate(seq.det.dol)

trans.rate.det.dol<-data.frame(seqtrate(seq.det.dol, with.missing=TRUE))
write.csv(trans.rate.det.dol, "results/UKHLS/dol_transition_rates.csv", row.names = TRUE)

seqtrate(seq.dol, time.varying=TRUE) # here can do at each time point. so this is useful for after time 1, but still not as useful for FIRST transition
seqtrate(seq.det.dol, time.varying=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# would ideally like to figure out the FIRST departure from egal state, where do you go.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I think I can calculate first egal, then need to figure out state after that. Can I do using same code as like time to marriage?
# https://stackoverflow.com/questions/73443586/is-there-a-way-for-traminer-to-find-the-first-time-leaving-a-state-for-each-obse

#get first occurrence of non egal state?
# This gives position of END? This is regardless of how long. so then I want the NEXT state
seqfposend(seq.det.dol,state="Egal Work x Egal HW")

# I know how to do this in stata but not here. I would just loop and pull the State that is in this position + 1. How do I do that here
pos_end  <- seqfposend(seq.det.dol, state = "Egal Work x Egal HW") # actual position
pos_next <- seqfposend(seq.det.dol, state = "Egal Work x Egal HW", lead = 1) # add 1 to get NEXT position

det.dol.matrix <- as.matrix(seq.det.dol)        # plain character matrix of state codes
n <- nrow(det.dol.matrix); L <- ncol(det.dol.matrix)

# guard against people who never enter the state, and against spells
# that run to the end of the observation window
ok <- !is.na(pos_next) & pos_next >= 1 & pos_next <= L

next_state <- rep(NA_character_, n)
next_state[ok] <- det.dol.matrix[cbind(which(ok), pos_next[ok])]
view(next_state)
trans_output <- data.frame(det.dol.matrix, pos_end, pos_next, next_state)
# view(trans_output)

table(trans_output$next_state)
table(trans_output$next_state, useNA = "always")
table(trans_output$next_state, trans_output$pos_end)
prop.table(table(trans_output$next_state))
prop.table(table(trans_output$next_state, useNA = "always"))

transition_table <- table(trans_output$next_state, useNA = "always")

transition_export <- data.frame(
  next_state = names(transition_table),
  n          = as.numeric(transition_table),
  pct        = round(100 * as.numeric(prop.table(transition_table)), 1)
)

write.csv(transition_export, "results/UKHLS/first_egal_exit.csv", row.names = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary: what changes when a strict egalitarian spell ends.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Need to update earlier code to get this to work based on what I have run so far, so this isn't going to work right now
## Or figure out what else to pull in from other file to get this to work. 

#     exit at t = egalitarian at t, not at t+1, t+1 observed.
exit <- m.egal[, 1:9] & !m.egal[, 2:10]
exit[is.na(exit)] <- FALSE

nch     <- as.integer(sub(".*c", "", m.fam)); dim(nch) <- dim(m.fam)
married <- substr(m.fam, 1, 3) == "MAR";       dim(married) <- dim(m.fam)

work_breaks <- !m.dualft[, 2:10][exit]              # leaves dual full-time
hw_breaks   <- !m.equalhw[, 2:10][exit]             # leaves equal housework
birth       <- (nch[, 2:10] > nch[, 1:9])[exit]     # birth between t and t+1
marry       <- (married[, 2:10] & !married[, 1:9])[exit]

exit_tab <- data.frame(
  country       = country,
  n_exits       = sum(exit) / n_imp,
  work_only     = mean(work_breaks & !hw_breaks),
  hw_only       = mean(!work_breaks & hw_breaks),
  both          = mean(work_breaks & hw_breaks),
  with_birth    = mean(birth),
  with_marriage = mean(marry),
  work_only_given_birth = mean((work_breaks & !hw_breaks)[birth]),
  hw_only_given_birth   = mean((!work_breaks & hw_breaks)[birth]),
  work_only_no_birth    = mean((work_breaks & !hw_breaks)[!birth]),
  hw_only_no_birth      = mean((!work_breaks & hw_breaks)[!birth])
)

print(exit_tab)
write.csv(exit_tab, file.path("results", country, paste0("egal_exit_", country, ".csv")), row.names = FALSE)

# <<< END ADDED