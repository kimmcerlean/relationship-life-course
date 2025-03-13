# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 12 2025
#    Goal: compare different cluster solutions of "simple" MCSA
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra")
  
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra")
  
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
# Load sequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load ("created data/ukhls/ukhls_setupsequence.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First compare the 4 v 5 cluster solution --
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hierarchical cluster analysis, non-squared dissimilarities
mcdist.simp.om <- seqdistmc(channels=list(seq.work, seq.hw, seq.fam), ## Seq states NOT om matrix
                            method="OM", indel=1, sm="CONSTANT") 

mc.simp.ward1 <- hclust(as.dist(mcdist.simp.om), 
                    method = "ward.D")

# Extract the 4-cluster solution

mc.ward.4cl <-cutree(mc.simp.ward1, k = 4)

# Attach the vector with the 4-cluster solution to the main data.frame

data$mc.ward.4cl<-mc.ward.4cl

# Extract the 5-cluster solution

mc.ward.5cl <-cutree(mc.simp.ward1, k = 5)

# Attach the vector with the 5-cluster solution to the main data.frame

data$mc.ward.5cl<-mc.ward.5cl

# comp.mc.45.ward<-table(mc.ward.5cl, mc.ward.4cl)
# write.csv(comp.mc.45.ward,("results/UKHLS_MC45-simple-comparison.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for  4 cluster solution --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply PAM clustering + Ward starting point
# Ward's clustering is explained in Chapter 4

mcdist.om.pam.ward <- wcKMedRange(mcdist.simp.om, kvals = 2:10,
                                  initialclust = mc.simp.ward1)

# Cut tree at cluster==4

mc4 <- mcdist.om.pam.ward$clustering$cluster4 # these are sub"folders" in ward output

# Label the clusters from 1 to 4
labels4<-unique(mc4)
sort(labels4)

mc4.factor <- factor(mc4, levels = sort(labels4),
                     c("1", "2", "3", "4"))

# Separate objects for each channel and for each cluster (=4*3)

data$mc4.factor <- as.numeric(mc4.factor)

# Identify position of variables indicating start and end of sequences

mc4.work1.seq <- seq.work[data$mc4.factor == "1", ]
mc4.work2.seq <- seq.work[data$mc4.factor == "2", ]
mc4.work3.seq <- seq.work[data$mc4.factor == "3", ]
mc4.work4.seq <- seq.work[data$mc4.factor == "4", ]

mc4.hw1.seq <- seq.hw[data$mc4.factor == "1", ]
mc4.hw2.seq <- seq.hw[data$mc4.factor == "2", ]
mc4.hw3.seq <- seq.hw[data$mc4.factor == "3", ]
mc4.hw4.seq <- seq.hw[data$mc4.factor == "4", ]

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

write.csv(relfreq4,("results/UKHLS_MC-4cluster-freq-simple.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep work for 5 cluster solution --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree at cluster==5

mc5 <- mcdist.om.pam.ward$clustering$cluster5 # these are sub"folders" in ward output

# Label the clusters from 1 to 5
labels5<-unique(mc5)
sort(labels5)

mc5.factor <- factor(mc5, levels = sort(labels5),
                     c("1", "2", "3", "4", "5"))

# Separate objects for each channel and for each cluster (=5*3)

data$mc5.factor <- as.numeric(mc5.factor)

# Identify position of variables indicating start and end of sequences

mc5.work1.seq <- seq.work[data$mc5.factor == "1", ]
mc5.work2.seq <- seq.work[data$mc5.factor == "2", ]
mc5.work3.seq <- seq.work[data$mc5.factor == "3", ]
mc5.work4.seq <- seq.work[data$mc5.factor == "4", ]
mc5.work5.seq <- seq.work[data$mc5.factor == "5", ]

mc5.hw1.seq <- seq.hw[data$mc5.factor == "1", ]
mc5.hw2.seq <- seq.hw[data$mc5.factor == "2", ]
mc5.hw3.seq <- seq.hw[data$mc5.factor == "3", ]
mc5.hw4.seq <- seq.hw[data$mc5.factor == "4", ]
mc5.hw5.seq <- seq.hw[data$mc5.factor == "5", ]

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

write.csv(relfreq5,("results/UKHLS_MC-5cluster-freq-simple.csv"))

save.image("created data/ukhls/typology-comparison-simple-prep.RData")

# save here in case graphs don't work
# load("created data/ukhls/typology-comparison-simple-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempt to compile channel charts by cluster -------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Four cluster solution: state distribution

pdf("results/UKHLS_MCSA_simp_4Cluster.pdf",
    width=12,
    height=6)

# Work

w1<- ggseqdplot(mc4.work1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w2<- ggseqdplot(mc4.work2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Work") +
  theme(legend.position="none",plot.title = element_text(size = 8))

w3 <- ggseqdplot(mc4.work3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w3a <- ggseqdplot(mc4.work3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="bottom")

w4<- ggseqdplot(mc4.work4.seq) + # Cluster 4
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

# Housework
w5<- ggseqdplot(mc4.hw1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w6<- ggseqdplot(mc4.hw2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Housework") +
  theme(legend.position="none",plot.title = element_text(size = 8))

w7<- ggseqdplot(mc4.hw3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w7a<- ggseqdplot(mc4.hw3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="bottom")

w8<- ggseqdplot(mc4.hw4.seq) + # Cluster 4
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

# Family
w9<- ggseqdplot(mc4.fam1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w10<- ggseqdplot(mc4.fam2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Family") +
  theme(legend.position="none",plot.title = element_text(size = 8))

w11<- ggseqdplot(mc4.fam3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w11a<- ggseqdplot(mc4.fam3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="bottom")

w12<- ggseqdplot(mc4.fam4.seq) + # Cluster 4
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

grid.arrange(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,ncol = 4, nrow=3)
grid.arrange(w1,w2,w3a,w4, newpage=TRUE)
grid.arrange(w5,w6,w7a,w8,newpage=TRUE)
grid.arrange(w9,w10,w11a,w12,newpage=TRUE)
grid.arrange(w1,w5,w9,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w2,w6,w10, ncol=3, nrow=1,newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w3,w7,w11, ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w4,w8,w12, ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Four cluster solution: index plot

# test, following WeightedCluster page 15, can I create silhouette to order sequenceplots?
sil <-wcSilhouetteObs(mcdist.simp.om,mc4,measure="ASW")
# oh is it doing it by group instead of individually actually a better way to do this also?

pdf("results/UKHLS_MCSA_simp_4Cluster_Index_sil.pdf")
seqIplot(seq.work, group=mc4, sortv=sil)
seqIplot(seq.hw, group=mc4, sortv=sil)
seqIplot(seq.fam, group=mc4, sortv=sil)
dev.off()

# Other option
pdf("results/UKHLS_MCSA_simp_4Cluster_Index.pdf",
          width=12,
          height=6)

# Work
w1<- ggseqiplot(mc4.work1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w2<- ggseqiplot(mc4.work2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Work") +
  theme(legend.position="none",plot.title = element_text(size = 8))
w3<- ggseqiplot(mc4.work3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w3a<- ggseqiplot(mc4.work3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="bottom")
w4<- ggseqiplot(mc4.work4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")


# HW
w5<- ggseqiplot(mc4.hw1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w6<- ggseqiplot(mc4.hw2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Housework") +
  theme(legend.position="none",plot.title = element_text(size = 8))
w7<- ggseqiplot(mc4.hw3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w7a<- ggseqiplot(mc4.hw3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="bottom")
w8<- ggseqiplot(mc4.hw4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")

# Fam
w9<- ggseqiplot(mc4.fam1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w10<- ggseqiplot(mc4.fam2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year", title = "Family") +
  theme(legend.position="none",plot.title = element_text(size = 8))
w11<- ggseqiplot(mc4.fam3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w11a<- ggseqiplot(mc4.fam3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="bottom")
w12<- ggseqiplot(mc4.fam4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")

grid.arrange(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,ncol = 4, nrow=3)
# grid.arrange(w1,w2,w3a,w4, newpage=TRUE)
# grid.arrange(w5,w6,w7a,w8,newpage=TRUE)
# grid.arrange(w9,w10,w11a,w12,newpage=TRUE)
grid.arrange(w1,w5,w9,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w2,w6,w10, ncol=3, nrow=1,newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w3,w7,w11, ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w4,w8,w12, ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Five cluster solution: state distribution

pdf("results/UKHLS_MCSA_simp_5Cluster.pdf",
          width=12,
          height=6)

# Work
w1<- ggseqdplot(mc5.work1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w2<- ggseqdplot(mc5.work2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w3<- ggseqdplot(mc5.work3.seq) + # Cluster 3
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w4<- ggseqdplot(mc5.work4.seq) + # this is actually cluster 5
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

w5<- ggseqdplot(mc5.work5.seq) + # Cluster 5
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="right")

grid.arrange(w1,w2,w3,w4,w5, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5))

# Housework
hw6<- ggseqdplot(mc5.hw1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

hw7<- ggseqdplot(mc5.hw2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

hw8<- ggseqdplot(mc5.hw3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

hw9<- ggseqdplot(mc5.hw4.seq) + # Cluster 4
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

hw10<- ggseqdplot(mc5.hw5.seq) + # Cluster 5
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="right")

grid.arrange(hw6,hw7,hw8,hw9,hw10, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5), newpage=TRUE)

# Family
f11<- ggseqdplot(mc5.fam1.seq) + # Cluster 1
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

f12<- ggseqdplot(mc5.fam2.seq) + # Cluster 2
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

f13<- ggseqdplot(mc5.fam3.seq) + # this is actually cluster 3?
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

f14<- ggseqdplot(mc5.fam4.seq) + # Cluster 4
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="none")

f15<- ggseqdplot(mc5.fam5.seq) + # Cluster 5
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year") +
  theme(legend.position="right")

grid.arrange(f11,f12,f13,f14,f15, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5), newpage=TRUE)

grid.arrange(w1,hw6,f11,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w2,hw7,f12,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w3,hw8,f13,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w4,hw9,f14,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w5,hw10,f15,ncol=3, nrow=1, newpage=TRUE) # try to sort by CLUSTER

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Five cluster solution: index plot

# test, following WeightedCluster page 15, can I create silhouette to order sequenceplots?
sil5 <-wcSilhouetteObs(mcdist.simp.om,mc5,measure="ASW")
# oh is it doing it by group instead of individually actually a better way to do this also?

pdf("results/UKHLS_MCSA_simp_5Cluster_Index_sil.pdf")
seqIplot(seq.work.ow, group=mc5, sortv=sil5)
seqIplot(seq.hw.hrs.alt, group=mc5, sortv=sil5)
seqIplot(seq.fam, group=mc5, sortv=sil5)
dev.off()

# Other option
pdf("results/UKHLS_MCSA_simp_5Cluster_Index.pdf",
          width=12,
          height=6)

# Work
w1<- ggseqiplot(mc5.work1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w2<- ggseqiplot(mc5.work2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w3<- ggseqiplot(mc5.work3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w4<- ggseqiplot(mc5.work4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
w5<- ggseqiplot(mc5.work5.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="right")


# HW
hw1<- ggseqiplot(mc5.hw1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
hw2<- ggseqiplot(mc5.hw2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
hw3<- ggseqiplot(mc5.hw3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
hw4<- ggseqiplot(mc5.hw4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
hw5<- ggseqiplot(mc5.hw5.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="right")


# Fam
f1<- ggseqiplot(mc5.fam1.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
f2<- ggseqiplot(mc5.fam2.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
f3<- ggseqiplot(mc5.fam3.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
f4<- ggseqiplot(mc5.fam4.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="none")
f5<- ggseqiplot(mc5.fam5.seq, sortv="from.end") +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Year")  +
  theme(legend.position="right")

# grid.arrange(w1,w2,w3,w4,w5, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5))
# grid.arrange(hw1,hw2,hw3,hw4,hw5, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5), newpage=TRUE)
# grid.arrange(f1,f2,f3,f4,f5, ncol=2, nrow=3, layout_matrix= rbind(c(1,2),c(3,4),5), newpage=TRUE)

grid.arrange(w1,hw1,f1,ncol=3,nrow=1) # try to sort by CLUSTER
grid.arrange(w2,hw2,f2,ncol=3,nrow=1,newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w3,hw3,f3,ncol=3,nrow=1,newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w4,hw4,f4,ncol=3,nrow=1,newpage=TRUE) # try to sort by CLUSTER
grid.arrange(w5,hw5,f5,ncol=3,nrow=1,newpage=TRUE) # try to sort by CLUSTER

dev.off()


## Try to replicate 5-5_Fig 2 - removed code for now as haven't figured out
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sequence index plots start-------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
