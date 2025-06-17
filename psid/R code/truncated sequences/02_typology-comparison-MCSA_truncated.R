# ---------------------------------------------------------------------
#    Program: typology-comparison-MCSA_truncated
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: June 16 2025
#    Goal: compare different cluster solutions of MCSA, based on truncated sequences
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

load("created data/psid_cluster-comparison-truncated.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the clusters: PAM using Ward's as starting point
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For reference, normalized distance matrix created in previous step
#mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
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

mcdist.om.pam.ward <- wcKMedRange(mcdist.det.min, kvals = 2:10,
                                      initialclust = mc.det.ward1)

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

mc6.work.ow1.seq <- seq.work.ow[data$mc6.factor == "1", ]
mc6.work.ow2.seq <- seq.work.ow[data$mc6.factor == "2", ]
mc6.work.ow3.seq <- seq.work.ow[data$mc6.factor == "3", ]
mc6.work.ow4.seq <- seq.work.ow[data$mc6.factor == "4", ]
mc6.work.ow5.seq <- seq.work.ow[data$mc6.factor == "5", ]
mc6.work.ow6.seq <- seq.work.ow[data$mc6.factor == "6", ]

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

write.csv(relfreq6,("results/PSID/truncated_cluster_freq_mc6.csv"))

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

mc7.work.ow1.seq <- seq.work.ow[data$mc7.factor == "1", ]
mc7.work.ow2.seq <- seq.work.ow[data$mc7.factor == "2", ]
mc7.work.ow3.seq <- seq.work.ow[data$mc7.factor == "3", ]
mc7.work.ow4.seq <- seq.work.ow[data$mc7.factor == "4", ]
mc7.work.ow5.seq <- seq.work.ow[data$mc7.factor == "5", ]
mc7.work.ow6.seq <- seq.work.ow[data$mc7.factor == "6", ]
mc7.work.ow7.seq <- seq.work.ow[data$mc7.factor == "7", ]

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

write.csv(relfreq7,("results/PSID/truncated_cluster_freq_mc7.csv"))

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

mc8.work.ow1.seq <- seq.work.ow[data$mc8.factor == "1", ]
mc8.work.ow2.seq <- seq.work.ow[data$mc8.factor == "2", ]
mc8.work.ow3.seq <- seq.work.ow[data$mc8.factor == "3", ]
mc8.work.ow4.seq <- seq.work.ow[data$mc8.factor == "4", ]
mc8.work.ow5.seq <- seq.work.ow[data$mc8.factor == "5", ]
mc8.work.ow6.seq <- seq.work.ow[data$mc8.factor == "6", ]
mc8.work.ow7.seq <- seq.work.ow[data$mc8.factor == "7", ]
mc8.work.ow8.seq <- seq.work.ow[data$mc8.factor == "8", ]

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

write.csv(relfreq8,("results/PSID/truncated_cluster_freq_mc8.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/psid_typology-comparison-truncated-prep.RData")

# also, export the data with the clusters attached to use for analysis in stata
write.dta(data, "created data/PSID_clusters_truncated_sequences.dta")


