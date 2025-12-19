# ---------------------------------------------------------------------
#    Program: mantel_ci_truncated
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: August 28 2025
#    Goal: Compute mantel coefficients for each combination of domains
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo", 
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats","ecodist")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", # "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
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
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load info created in 00
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/setupsequence-truncated.RData")
# we only need distance matrices? so those are created in step 00

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## confidence intervals at overall level
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ALt option while wait for ecodist
#quantile(mantel_work.hw$perm, probs=c(.90, .95, .975, .99))
#quantile(mantel_work.fam$perm, probs=c(.90, .95, .975, .99))
#quantile(mantel_hw.fam$perm, probs=c(.90, .95, .975, .99))

##### 100 permutations - BETTER FOR CONFIDENCE INTERVALS (see Fasang and Aisenbrey also)
mantel.work.hw.ci <- mantel(lower(dist.work.min) ~ lower(dist.hw.min), nperm=100)
mantel.work.fam.ci <- mantel(lower(dist.work.min) ~ lower(dist.fam.min), nperm=100)
mantel.hw.fam.ci <- mantel(lower(dist.hw.min) ~ lower(dist.fam.min), nperm=100)

mantel.df.work.hw.ci <- data.frame(mantel.work.hw.ci)
mantel.df.work.fam.ci <- data.frame(mantel.work.fam.ci)
mantel.df.hw.fam.ci <- data.frame(mantel.hw.fam.ci)

mantel.col <- c('mantelr','pval1','pval2','pval3','llim.2.5%','ulim.97.5%')

mantel.df <- data.frame(mantel.col, mantel.df.work.hw.ci, 
                        mantel.df.work.fam.ci, mantel.df.hw.fam.ci)

write.xlsx(mantel.df, "results/PSID/psid_mantel_ci_truncated_100perm.xlsx")

##### 1000 permutations
#mantel.work.hw.ci <- mantel(lower(dist.work.min) ~ lower(dist.hw.min), nperm=1000)
#mantel.work.fam.ci <- mantel(lower(dist.work.min) ~ lower(dist.fam.min), nperm=1000)
#mantel.hw.fam.ci <- mantel(lower(dist.hw.min) ~ lower(dist.fam.min), nperm=1000)

#mantel.df.work.hw.ci <- data.frame(mantel.work.hw.ci)
#mantel.df.work.fam.ci <- data.frame(mantel.work.fam.ci)
#mantel.df.hw.fam.ci <- data.frame(mantel.hw.fam.ci)

#mantel.col <- c('mantelr','pval1','pval2','pval3','llim.2.5%','ulim.97.5%')

#mantel.df <- data.frame(mantel.col, mantel.df.work.hw.ci, 
#                        mantel.df.work.fam.ci, mantel.df.hw.fam.ci)

#write.xlsx(mantel.df, "results/PSID/psid_mantel_ci_truncated_1000perm.xlsx")

# Save again
# save.image("created data/psid_mantel_by_cluster_ci_trunc.RData")
