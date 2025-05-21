# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: May 21 2025
#    Goal: compare different cluster solutions of "detailed" MCSA
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' # Kim
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

load("created data/setupsequence-complete.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the clusters: PAM using Ward's as starting point
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hierarchical cluster analysis, non-squared dissimilarities
mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.combo, seq.fam), ## Seq states NOT om matrix
                           method="OM", indel=1, sm="CONSTANT") 

mc.det.ward1 <- hclust(as.dist(mcdist.det.om), 
                    method = "ward.D")

# Apply PAM clustering + Ward starting point
# Ward's clustering is explained in Chapter 4

mcdist.om.pam.det.ward <- wcKMedRange(mcdist.det.om, kvals = 2:10,
                                      initialclust = mc.det.ward1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 6 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==6

mc6 <- mcdist.om.pam.det.ward$clustering$cluster6 # these are sub"folders" in ward output

# Label the clusters from 1 to 6
labels6<-unique(mc6)
sort(labels6)

mc6.factor <- factor(mc6, levels = sort(labels6),
                     c("1", "2", "3", "4", "5", "6"))

# Separate objects for each channel and for each cluster

data$mc6.factor <- as.numeric(mc6.factor)

# Identify position of variables indicating start and end of sequences

mc6.work.ow1.seq <- seq.work.ow[data$mc6.factor == "1", ]
mc6.work.ow2.seq <- seq.work.ow[data$mc6.factor == "2", ]
mc6.work.ow3.seq <- seq.work.ow[data$mc6.factor == "3", ]
mc6.work.ow4.seq <- seq.work.ow[data$mc6.factor == "4", ]
mc6.work.ow5.seq <- seq.work.ow[data$mc6.factor == "5", ]
mc6.work.ow6.seq <- seq.work.ow[data$mc6.factor == "6", ]

mc6.hw.hrs1.seq <- seq.hw.hrs.combo[data$mc6.factor == "1", ]
mc6.hw.hrs2.seq <- seq.hw.hrs.combo[data$mc6.factor == "2", ]
mc6.hw.hrs3.seq <- seq.hw.hrs.combo[data$mc6.factor == "3", ]
mc6.hw.hrs4.seq <- seq.hw.hrs.combo[data$mc6.factor == "4", ]
mc6.hw.hrs5.seq <- seq.hw.hrs.combo[data$mc6.factor == "5", ]
mc6.hw.hrs6.seq <- seq.hw.hrs.combo[data$mc6.factor == "6", ]

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
# This might also be useful to ensure one cluster is not extremely small?
print(relfreq6)

write.csv(relfreq6,("results/PSID/complete_cluster_freq_mc6.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 7 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==7

mc7 <- mcdist.om.pam.det.ward$clustering$cluster7 # these are sub"folders" in ward output

# Label the clusters from 1 to 7
labels7<-unique(mc7)
sort(labels7)

mc7.factor <- factor(mc7, levels = sort(labels7),
                     c("1", "2", "3", "4", "5", "6", "7"))

# Separate objects for each channel and for each cluster

data$mc7.factor <- as.numeric(mc7.factor)

# Identify position of variables indicating start and end of sequences

mc7.work.ow1.seq <- seq.work.ow[data$mc7.factor == "1", ]
mc7.work.ow2.seq <- seq.work.ow[data$mc7.factor == "2", ]
mc7.work.ow3.seq <- seq.work.ow[data$mc7.factor == "3", ]
mc7.work.ow4.seq <- seq.work.ow[data$mc7.factor == "4", ]
mc7.work.ow5.seq <- seq.work.ow[data$mc7.factor == "5", ]
mc7.work.ow6.seq <- seq.work.ow[data$mc7.factor == "6", ]
mc7.work.ow7.seq <- seq.work.ow[data$mc7.factor == "7", ]

mc7.hw.hrs1.seq <- seq.hw.hrs.combo[data$mc7.factor == "1", ]
mc7.hw.hrs2.seq <- seq.hw.hrs.combo[data$mc7.factor == "2", ]
mc7.hw.hrs3.seq <- seq.hw.hrs.combo[data$mc7.factor == "3", ]
mc7.hw.hrs4.seq <- seq.hw.hrs.combo[data$mc7.factor == "4", ]
mc7.hw.hrs5.seq <- seq.hw.hrs.combo[data$mc7.factor == "5", ]
mc7.hw.hrs6.seq <- seq.hw.hrs.combo[data$mc7.factor == "6", ]
mc7.hw.hrs7.seq <- seq.hw.hrs.combo[data$mc7.factor == "7", ]

mc7.fam1.seq <- seq.fam[data$mc7.factor == "1", ]
mc7.fam2.seq <- seq.fam[data$mc7.factor == "2", ]
mc7.fam3.seq <- seq.fam[data$mc7.factor == "3", ]
mc7.fam4.seq <- seq.fam[data$mc7.factor == "4", ]
mc7.fam5.seq <- seq.fam[data$mc7.factor == "5", ]
mc7.fam6.seq <- seq.fam[data$mc7.factor == "6", ]
mc7.fam7.seq <- seq.fam[data$mc7.factor == "7", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the six clusters

relfreq7 <- data %>% 
  count(mc7.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq7$share)*100, 1)

# display frequencies of each cluster.
# This might also be useful to ensure one cluster is not extremely small?
print(relfreq7)

write.csv(relfreq7,("results/PSID/complete_cluster_freq_mc7.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Temp save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# will try to run whole thing in HPC, but saving here just in case figures don't work
save.image("created data/typology-comparison-complete-prep.RData")

# also, export the data with the clusters attached to use for analysis in stata
write.dta(data, "created data/PSID_clusters_complete_sequences.dta")

# load("created data/typology-comparison-complete-prep.RData")
