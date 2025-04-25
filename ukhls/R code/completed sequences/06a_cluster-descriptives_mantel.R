# ---------------------------------------------------------------------
#    Program: cluster-descriptives_mantel
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: April 24 2025
#    Goal: Compare cross-domain mantel coefficients by cluster
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo", 
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  
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
# Load cluster information created in step 4 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/ukhls/typology-comparison-complete-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First need to get dissimilarity matrix for each cluster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Cluster 1
dist.fam.c1 <- seqdist(mc7.fam1.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c1 <- seqdist(mc7.work.ow1.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c1 <- seqdist(mc7.hw.hrs1.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 2
dist.fam.c2 <- seqdist(mc7.fam2.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c2 <- seqdist(mc7.work.ow2.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c2 <- seqdist(mc7.hw.hrs2.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 3
dist.fam.c3 <- seqdist(mc7.fam3.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c3 <- seqdist(mc7.work.ow3.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c3 <- seqdist(mc7.hw.hrs3.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 4
dist.fam.c4 <- seqdist(mc7.fam4.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c4 <- seqdist(mc7.work.ow4.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c4 <- seqdist(mc7.hw.hrs4.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 5
dist.fam.c5 <- seqdist(mc7.fam5.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c5 <- seqdist(mc7.work.ow5.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c5 <- seqdist(mc7.hw.hrs5.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 6
dist.fam.c6 <- seqdist(mc7.fam6.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c6 <- seqdist(mc7.work.ow6.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c6 <- seqdist(mc7.hw.hrs6.seq, method="OM", indel=1, sm= "CONSTANT")

##### Cluster 7
dist.fam.c7 <- seqdist(mc7.fam7.seq, method="OM", indel=1, sm= "CONSTANT")
dist.work.c7 <- seqdist(mc7.work.ow7.seq, method="OM", indel=1, sm= "CONSTANT")
dist.hw.c7 <- seqdist(mc7.hw.hrs7.seq, method="OM", indel=1, sm= "CONSTANT")

##### Total
dist.fam <- seqdist(seq.fam, method="OM", indel=1, sm= "CONSTANT")
dist.work <- seqdist(seq.work.ow, method="OM", indel=1, sm= "CONSTANT")
dist.hw <- seqdist(seq.hw.hrs.alt, method="OM", indel=1, sm= "CONSTANT")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Then Mantel Coefficients across each domain for each cluster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Cluster 1
mantel_work.hw.c1 = mantel(dist.work.c1, dist.hw.c1)
mantel_work.fam.c1 = mantel(dist.work.c1, dist.fam.c1)
mantel_hw.fam.c1 = mantel(dist.hw.c1, dist.fam.c1)

##### Cluster 2
mantel_work.hw.c2 = mantel(dist.work.c2, dist.hw.c2)
mantel_work.fam.c2 = mantel(dist.work.c2, dist.fam.c2)
mantel_hw.fam.c2 = mantel(dist.hw.c2, dist.fam.c2)

##### Cluster 3
mantel_work.hw.c3 = mantel(dist.work.c3, dist.hw.c3)
mantel_work.fam.c3 = mantel(dist.work.c3, dist.fam.c3)
mantel_hw.fam.c3 = mantel(dist.hw.c3, dist.fam.c3)

##### Cluster 4
mantel_work.hw.c4 = mantel(dist.work.c4, dist.hw.c4)
mantel_work.fam.c4 = mantel(dist.work.c4, dist.fam.c4)
mantel_hw.fam.c4 = mantel(dist.hw.c4, dist.fam.c4)

##### Cluster 5
mantel_work.hw.c5 = mantel(dist.work.c5, dist.hw.c5)
mantel_work.fam.c5 = mantel(dist.work.c5, dist.fam.c5)
mantel_hw.fam.c5 = mantel(dist.hw.c5, dist.fam.c5)

##### Cluster 6
mantel_work.hw.c6 = mantel(dist.work.c6, dist.hw.c6)
mantel_work.fam.c6 = mantel(dist.work.c6, dist.fam.c6)
mantel_hw.fam.c6 = mantel(dist.hw.c6, dist.fam.c6)

##### Cluster 7
mantel_work.hw.c7 = mantel(dist.work.c7, dist.hw.c7)
mantel_work.fam.c7 = mantel(dist.work.c7, dist.fam.c7)
mantel_hw.fam.c7 = mantel(dist.hw.c7, dist.fam.c7)

##### Total
mantel_work.hw = mantel(dist.work, dist.hw)
mantel_work.fam = mantel(dist.work, dist.fam)
mantel_hw.fam = mantel(dist.hw, dist.fam)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cluster <- c('Cluster 1: work/hw', 'Cluster 1: work/fam',  'Cluster 1: hw/fam',
                'Cluster 2: work/hw', 'Cluster 2: work/fam',  'Cluster 2: hw/fam',
                'Cluster 3: work/hw', 'Cluster 3: work/fam',  'Cluster 3: hw/fam',
                'Cluster 4: work/hw', 'Cluster 4: work/fam',  'Cluster 4: hw/fam',
                'Cluster 5: work/hw', 'Cluster 5: work/fam',  'Cluster 5: hw/fam',
                'Cluster 6: work/hw', 'Cluster 6: work/fam',  'Cluster 6: hw/fam',
                'Cluster 7: work/hw', 'Cluster 7: work/fam',  'Cluster 7: hw/fam',
             'Total: work/hw', 'Total: work/fam',  'Total: hw/fam')

stats <- c(mantel_work.hw.c1$statistic, mantel_work.fam.c1$statistic, mantel_hw.fam.c1$statistic,
           mantel_work.hw.c2$statistic, mantel_work.fam.c2$statistic, mantel_hw.fam.c2$statistic,
           mantel_work.hw.c3$statistic, mantel_work.fam.c3$statistic, mantel_hw.fam.c3$statistic,
           mantel_work.hw.c4$statistic, mantel_work.fam.c4$statistic, mantel_hw.fam.c4$statistic,
           mantel_work.hw.c5$statistic, mantel_work.fam.c5$statistic, mantel_hw.fam.c5$statistic,
           mantel_work.hw.c6$statistic, mantel_work.fam.c6$statistic, mantel_hw.fam.c6$statistic,
           mantel_work.hw.c7$statistic, mantel_work.fam.c7$statistic, mantel_hw.fam.c7$statistic,
           mantel_work.hw$statistic, mantel_work.fam$statistic, mantel_hw.fam$statistic)

sig <- c(mantel_work.hw.c1$signif, mantel_work.fam.c1$signif, mantel_hw.fam.c1$signif,
        mantel_work.hw.c2$signif, mantel_work.fam.c2$signif, mantel_hw.fam.c2$signif,
        mantel_work.hw.c3$signif, mantel_work.fam.c3$signif, mantel_hw.fam.c3$signif,
        mantel_work.hw.c4$signif, mantel_work.fam.c4$signif, mantel_hw.fam.c4$signif,
        mantel_work.hw.c5$signif, mantel_work.fam.c5$signif, mantel_hw.fam.c5$signif,
        mantel_work.hw.c6$signif, mantel_work.fam.c6$signif, mantel_hw.fam.c6$signif,
        mantel_work.hw.c7$signif, mantel_work.fam.c7$signif, mantel_hw.fam.c7$signif,
        mantel_work.hw$signif, mantel_work.fam$signif, mantel_hw.fam$signif)
         
mantel_cluster <- data.frame(cluster, stats, sig)

write.xlsx(mantel_cluster, "results/UKHLS/ukhls_mantel_cluster_comparison.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/ukhls/ukhls_mantel_by_cluster.RData")
