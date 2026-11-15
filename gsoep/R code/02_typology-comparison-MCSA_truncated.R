# ---------------------------------------------------------------------
#    Program: typology-comparison-MCSA_truncated
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: August 12 2026
#    Goal: compare different cluster solutions of MCSA, based on truncated sequences
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
# kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' # Kim
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign")
  
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign")
  
  install_if_missing <- function(packages) {
    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  }
  install_if_missing(required_packages)
  lapply(required_packages, require, character.only = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load sequences (created in step 00)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("created data/gsoep/gsoep_cluster-comparison-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the clusters: PAM using Ward's as starting point
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# replace seq.hw.hrs.weekly with seq.hw.hrs
# replace seq.work.ow with seq.work

# For reference, normalized distance matrix created in previous step
#mcdist.det.om <- seqdistmc(channels=list(seq.work, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
#                           method="OM", 
#                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
#                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
#                           with.missing=TRUE) 

#mcdist.det.min <- mcdist.det.om / fam.min.len

# Hierarchical cluster analysis, non-squared dissimilarities
mc.det.ward1 <- hclust(as.dist(mcdist.det.min), 
                    method = "ward.D")

# Apply PAM clustering + Ward starting point
# Ward's clustering is explained in Chapter 4

mcdist.om.pam.ward <- wcKMedRange(mcdist.det.min, kvals = 2:15,
                                      initialclust = mc.det.ward1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 4 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==4

mc4 <- mcdist.om.pam.ward$clustering$cluster4 # these are sub"folders" in ward output

# Label the clusters from 1 to 4
labels4<-unique(mc4)
sort(labels4)

mc4.factor <- factor(mc4, levels = sort(labels4),
                     c("1", "2", "3", "4"))

# Separate objects for each channel and for each cluster

data$mc4.factor <- as.numeric(mc4.factor)

# Identify position of variables indicating start and end of sequences

mc4.work1.seq <- seq.work[data$mc4.factor == "1", ]
mc4.work2.seq <- seq.work[data$mc4.factor == "2", ]
mc4.work3.seq <- seq.work[data$mc4.factor == "3", ]
mc4.work4.seq <- seq.work[data$mc4.factor == "4", ]

mc4.hw.hrs1.seq <- seq.hw.hrs[data$mc4.factor == "1", ]
mc4.hw.hrs2.seq <- seq.hw.hrs[data$mc4.factor == "2", ]
mc4.hw.hrs3.seq <- seq.hw.hrs[data$mc4.factor == "3", ]
mc4.hw.hrs4.seq <- seq.hw.hrs[data$mc4.factor == "4", ]

mc4.fam1.seq <- seq.fam[data$mc4.factor == "1", ]
mc4.fam2.seq <- seq.fam[data$mc4.factor == "2", ]
mc4.fam3.seq <- seq.fam[data$mc4.factor == "3", ]
mc4.fam4.seq <- seq.fam[data$mc4.factor == "4", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the four clusters

relfreq4 <- data %>% 
  count(mc4.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq4$share)*100, 1)

# display frequencies of each cluster.
print(relfreq4)

write.csv(relfreq4,("results/GSOEP/truncated_cluster_freq_mc4.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 5 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==5

mc5 <- mcdist.om.pam.ward$clustering$cluster5 # these are sub"folders" in ward output

# Label the clusters from 1 to 5
labels5<-unique(mc5)
sort(labels5)

mc5.factor <- factor(mc5, levels = sort(labels5),
                   c("1", "2", "3", "4", "5"))

# Separate objects for each channel and for each cluster

data$mc5.factor <- as.numeric(mc5.factor)

# Identify position of variables indicating start and end of sequences

mc5.work1.seq <- seq.work[data$mc5.factor == "1", ]
mc5.work2.seq <- seq.work[data$mc5.factor == "2", ]
mc5.work3.seq <- seq.work[data$mc5.factor == "3", ]
mc5.work4.seq <- seq.work[data$mc5.factor == "4", ]
mc5.work5.seq <- seq.work[data$mc5.factor == "5", ]

mc5.hw.hrs1.seq <- seq.hw.hrs[data$mc5.factor == "1", ]
mc5.hw.hrs2.seq <- seq.hw.hrs[data$mc5.factor == "2", ]
mc5.hw.hrs3.seq <- seq.hw.hrs[data$mc5.factor == "3", ]
mc5.hw.hrs4.seq <- seq.hw.hrs[data$mc5.factor == "4", ]
mc5.hw.hrs5.seq <- seq.hw.hrs[data$mc5.factor == "5", ]

mc5.fam1.seq <- seq.fam[data$mc5.factor == "1", ]
mc5.fam2.seq <- seq.fam[data$mc5.factor == "2", ]
mc5.fam3.seq <- seq.fam[data$mc5.factor == "3", ]
mc5.fam4.seq <- seq.fam[data$mc5.factor == "4", ]
mc5.fam5.seq <- seq.fam[data$mc5.factor == "5", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the five clusters

relfreq5 <- data %>% 
  count(mc5.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq5$share)*100, 1)

# display frequencies of each cluster.
print(relfreq5)

write.csv(relfreq5,("results/GSOEP/truncated_cluster_freq_mc5.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 6 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==6

mc6 <- mcdist.om.pam.ward$clustering$cluster6 # these are sub"folders" in ward output

# Label the clusters from 1 to 6
labels6<-unique(mc6)
sort(labels6)

mc6.factor <- factor(mc6, levels = sort(labels6),
                     c("1", "2", "3", "4", "5", "6"))

# Separate objects for each channel and for each cluster

data$mc6.factor <- as.numeric(mc6.factor)

# Identify position of variables indicating start and end of sequences

mc6.work1.seq <- seq.work[data$mc6.factor == "1", ]
mc6.work2.seq <- seq.work[data$mc6.factor == "2", ]
mc6.work3.seq <- seq.work[data$mc6.factor == "3", ]
mc6.work4.seq <- seq.work[data$mc6.factor == "4", ]
mc6.work5.seq <- seq.work[data$mc6.factor == "5", ]
mc6.work6.seq <- seq.work[data$mc6.factor == "6", ]

mc6.hw.hrs1.seq <- seq.hw.hrs[data$mc6.factor == "1", ]
mc6.hw.hrs2.seq <- seq.hw.hrs[data$mc6.factor == "2", ]
mc6.hw.hrs3.seq <- seq.hw.hrs[data$mc6.factor == "3", ]
mc6.hw.hrs4.seq <- seq.hw.hrs[data$mc6.factor == "4", ]
mc6.hw.hrs5.seq <- seq.hw.hrs[data$mc6.factor == "5", ]
mc6.hw.hrs6.seq <- seq.hw.hrs[data$mc6.factor == "6", ]

mc6.fam1.seq <- seq.fam[data$mc6.factor == "1", ]
mc6.fam2.seq <- seq.fam[data$mc6.factor == "2", ]
mc6.fam3.seq <- seq.fam[data$mc6.factor == "3", ]
mc6.fam4.seq <- seq.fam[data$mc6.factor == "4", ]
mc6.fam5.seq <- seq.fam[data$mc6.factor == "5", ]
mc6.fam6.seq <- seq.fam[data$mc6.factor == "6", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the six clusters

relfreq6 <- data %>% 
  count(mc6.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq6$share)*100, 1)

# display frequencies of each cluster.
print(relfreq6)

write.csv(relfreq6,("results/GSOEP/truncated_cluster_freq_mc6.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 7 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==7

mc7 <- mcdist.om.pam.ward$clustering$cluster7 # these are sub"folders" in ward output

# Label the clusters from 1 to 7
labels7<-unique(mc7)
sort(labels7)

mc7.factor <- factor(mc7, levels = sort(labels7),
                     c("1", "2", "3", "4", "5", "6", "7"))

# Separate objects for each channel and for each cluster

data$mc7.factor <- as.numeric(mc7.factor)

# Identify position of variables indicating start and end of sequences

mc7.work1.seq <- seq.work[data$mc7.factor == "1", ]
mc7.work2.seq <- seq.work[data$mc7.factor == "2", ]
mc7.work3.seq <- seq.work[data$mc7.factor == "3", ]
mc7.work4.seq <- seq.work[data$mc7.factor == "4", ]
mc7.work5.seq <- seq.work[data$mc7.factor == "5", ]
mc7.work6.seq <- seq.work[data$mc7.factor == "6", ]
mc7.work7.seq <- seq.work[data$mc7.factor == "7", ]

mc7.hw.hrs1.seq <- seq.hw.hrs[data$mc7.factor == "1", ]
mc7.hw.hrs2.seq <- seq.hw.hrs[data$mc7.factor == "2", ]
mc7.hw.hrs3.seq <- seq.hw.hrs[data$mc7.factor == "3", ]
mc7.hw.hrs4.seq <- seq.hw.hrs[data$mc7.factor == "4", ]
mc7.hw.hrs5.seq <- seq.hw.hrs[data$mc7.factor == "5", ]
mc7.hw.hrs6.seq <- seq.hw.hrs[data$mc7.factor == "6", ]
mc7.hw.hrs7.seq <- seq.hw.hrs[data$mc7.factor == "7", ]

mc7.fam1.seq <- seq.fam[data$mc7.factor == "1", ]
mc7.fam2.seq <- seq.fam[data$mc7.factor == "2", ]
mc7.fam3.seq <- seq.fam[data$mc7.factor == "3", ]
mc7.fam4.seq <- seq.fam[data$mc7.factor == "4", ]
mc7.fam5.seq <- seq.fam[data$mc7.factor == "5", ]
mc7.fam6.seq <- seq.fam[data$mc7.factor == "6", ]
mc7.fam7.seq <- seq.fam[data$mc7.factor == "7", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the seven clusters

relfreq7 <- data %>% 
  count(mc7.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq7$share)*100, 1)

# display frequencies of each cluster.
print(relfreq7)

write.csv(relfreq7,("results/GSOEP/truncated_cluster_freq_mc7.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 8 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==8

mc8 <- mcdist.om.pam.ward$clustering$cluster8 # these are sub"folders" in ward output

# Label the clusters from 1 to 8
labels8<-unique(mc8)
sort(labels8)

mc8.factor <- factor(mc8, levels = sort(labels8),
                     c("1", "2", "3", "4", "5", "6", "7", "8"))

# Separate objects for each channel and for each cluster

data$mc8.factor <- as.numeric(mc8.factor)

# Identify position of variables indicating start and end of sequences

mc8.work1.seq <- seq.work[data$mc8.factor == "1", ]
mc8.work2.seq <- seq.work[data$mc8.factor == "2", ]
mc8.work3.seq <- seq.work[data$mc8.factor == "3", ]
mc8.work4.seq <- seq.work[data$mc8.factor == "4", ]
mc8.work5.seq <- seq.work[data$mc8.factor == "5", ]
mc8.work6.seq <- seq.work[data$mc8.factor == "6", ]
mc8.work7.seq <- seq.work[data$mc8.factor == "7", ]
mc8.work8.seq <- seq.work[data$mc8.factor == "8", ]

mc8.hw.hrs1.seq <- seq.hw.hrs[data$mc8.factor == "1", ]
mc8.hw.hrs2.seq <- seq.hw.hrs[data$mc8.factor == "2", ]
mc8.hw.hrs3.seq <- seq.hw.hrs[data$mc8.factor == "3", ]
mc8.hw.hrs4.seq <- seq.hw.hrs[data$mc8.factor == "4", ]
mc8.hw.hrs5.seq <- seq.hw.hrs[data$mc8.factor == "5", ]
mc8.hw.hrs6.seq <- seq.hw.hrs[data$mc8.factor == "6", ]
mc8.hw.hrs7.seq <- seq.hw.hrs[data$mc8.factor == "7", ]
mc8.hw.hrs8.seq <- seq.hw.hrs[data$mc8.factor == "8", ]

mc8.fam1.seq <- seq.fam[data$mc8.factor == "1", ]
mc8.fam2.seq <- seq.fam[data$mc8.factor == "2", ]
mc8.fam3.seq <- seq.fam[data$mc8.factor == "3", ]
mc8.fam4.seq <- seq.fam[data$mc8.factor == "4", ]
mc8.fam5.seq <- seq.fam[data$mc8.factor == "5", ]
mc8.fam6.seq <- seq.fam[data$mc8.factor == "6", ]
mc8.fam7.seq <- seq.fam[data$mc8.factor == "7", ]
mc8.fam8.seq <- seq.fam[data$mc8.factor == "8", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the eight clusters

relfreq8 <- data %>% 
  count(mc8.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq8$share)*100, 1)

# display frequencies of each cluster.
print(relfreq8)

write.csv(relfreq8,("results/GSOEP/truncated_cluster_freq_mc8.csv"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 9 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==9

mc9 <- mcdist.om.pam.ward$clustering$cluster9 # these are sub"folders" in ward output

# Label the clusters from 1 to 9
labels9<-unique(mc9)
sort(labels9)

mc9.factor <- factor(mc9, levels = sort(labels9),
                     c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

# Separate objects for each channel and for each cluster

data$mc9.factor <- as.numeric(mc9.factor)

# Identify position of variables indicating start and end of sequences

mc9.work1.seq <- seq.work[data$mc9.factor == "1", ]
mc9.work2.seq <- seq.work[data$mc9.factor == "2", ]
mc9.work3.seq <- seq.work[data$mc9.factor == "3", ]
mc9.work4.seq <- seq.work[data$mc9.factor == "4", ]
mc9.work5.seq <- seq.work[data$mc9.factor == "5", ]
mc9.work6.seq <- seq.work[data$mc9.factor == "6", ]
mc9.work7.seq <- seq.work[data$mc9.factor == "7", ]
mc9.work8.seq <- seq.work[data$mc9.factor == "8", ]
mc9.work9.seq <- seq.work[data$mc9.factor == "9", ]

mc9.hw.hrs1.seq <- seq.hw.hrs[data$mc9.factor == "1", ]
mc9.hw.hrs2.seq <- seq.hw.hrs[data$mc9.factor == "2", ]
mc9.hw.hrs3.seq <- seq.hw.hrs[data$mc9.factor == "3", ]
mc9.hw.hrs4.seq <- seq.hw.hrs[data$mc9.factor == "4", ]
mc9.hw.hrs5.seq <- seq.hw.hrs[data$mc9.factor == "5", ]
mc9.hw.hrs6.seq <- seq.hw.hrs[data$mc9.factor == "6", ]
mc9.hw.hrs7.seq <- seq.hw.hrs[data$mc9.factor == "7", ]
mc9.hw.hrs8.seq <- seq.hw.hrs[data$mc9.factor == "8", ]
mc9.hw.hrs9.seq <- seq.hw.hrs[data$mc9.factor == "9", ]

mc9.fam1.seq <- seq.fam[data$mc9.factor == "1", ]
mc9.fam2.seq <- seq.fam[data$mc9.factor == "2", ]
mc9.fam3.seq <- seq.fam[data$mc9.factor == "3", ]
mc9.fam4.seq <- seq.fam[data$mc9.factor == "4", ]
mc9.fam5.seq <- seq.fam[data$mc9.factor == "5", ]
mc9.fam6.seq <- seq.fam[data$mc9.factor == "6", ]
mc9.fam7.seq <- seq.fam[data$mc9.factor == "7", ]
mc9.fam8.seq <- seq.fam[data$mc9.factor == "8", ]
mc9.fam9.seq <- seq.fam[data$mc9.factor == "9", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the nine clusters

relfreq9 <- data %>% 
  count(mc9.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq9$share)*100, 1)

# display frequencies of each cluster.
print(relfreq9)

write.csv(relfreq9,("results/GSOEP/truncated_cluster_freq_mc9.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 10 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==10

mc10 <- mcdist.om.pam.ward$clustering$cluster10 # these are sub"folders" in ward output

# Label the clusters from 1 to 10
labels10<-unique(mc10)
sort(labels10)

mc10.factor <- factor(mc10, levels = sort(labels10),
                      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# Separate objects for each channel and for each cluster

data$mc10.factor <- as.numeric(mc10.factor)

# Identify position of variables indicating start and end of sequences

mc10.work1.seq <- seq.work[data$mc10.factor == "1", ]
mc10.work2.seq <- seq.work[data$mc10.factor == "2", ]
mc10.work3.seq <- seq.work[data$mc10.factor == "3", ]
mc10.work4.seq <- seq.work[data$mc10.factor == "4", ]
mc10.work5.seq <- seq.work[data$mc10.factor == "5", ]
mc10.work6.seq <- seq.work[data$mc10.factor == "6", ]
mc10.work7.seq <- seq.work[data$mc10.factor == "7", ]
mc10.work8.seq <- seq.work[data$mc10.factor == "8", ]
mc10.work9.seq <- seq.work[data$mc10.factor == "9", ]
mc10.work10.seq <- seq.work[data$mc10.factor == "10", ]

mc10.hw.hrs1.seq <- seq.hw.hrs[data$mc10.factor == "1", ]
mc10.hw.hrs2.seq <- seq.hw.hrs[data$mc10.factor == "2", ]
mc10.hw.hrs3.seq <- seq.hw.hrs[data$mc10.factor == "3", ]
mc10.hw.hrs4.seq <- seq.hw.hrs[data$mc10.factor == "4", ]
mc10.hw.hrs5.seq <- seq.hw.hrs[data$mc10.factor == "5", ]
mc10.hw.hrs6.seq <- seq.hw.hrs[data$mc10.factor == "6", ]
mc10.hw.hrs7.seq <- seq.hw.hrs[data$mc10.factor == "7", ]
mc10.hw.hrs8.seq <- seq.hw.hrs[data$mc10.factor == "8", ]
mc10.hw.hrs9.seq <- seq.hw.hrs[data$mc10.factor == "9", ]
mc10.hw.hrs10.seq <- seq.hw.hrs[data$mc10.factor == "10", ]

mc10.fam1.seq <- seq.fam[data$mc10.factor == "1", ]
mc10.fam2.seq <- seq.fam[data$mc10.factor == "2", ]
mc10.fam3.seq <- seq.fam[data$mc10.factor == "3", ]
mc10.fam4.seq <- seq.fam[data$mc10.factor == "4", ]
mc10.fam5.seq <- seq.fam[data$mc10.factor == "5", ]
mc10.fam6.seq <- seq.fam[data$mc10.factor == "6", ]
mc10.fam7.seq <- seq.fam[data$mc10.factor == "7", ]
mc10.fam8.seq <- seq.fam[data$mc10.factor == "8", ]
mc10.fam9.seq <- seq.fam[data$mc10.factor == "9", ]
mc10.fam10.seq <- seq.fam[data$mc10.factor == "10", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the ten clusters

relfreq10 <- data %>% 
  count(mc10.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq10$share)*100, 1)

# display frequencies of each cluster.
print(relfreq10)

write.csv(relfreq10,("results/GSOEP/truncated_cluster_freq_mc10.csv"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# also, export the data with the clusters attached to use for analysis in stata
write.dta(data, "created data/gsoep/GSOEP_clusters_truncated_sequences.dta")

save.image("created data/gsoep/typology-comparison-truncated-prep.RData")

# load("created data/gsoep/typology-comparison-truncated-prep.RData")
