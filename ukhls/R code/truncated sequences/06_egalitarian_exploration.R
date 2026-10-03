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
