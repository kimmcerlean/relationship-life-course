# ---------------------------------------------------------------------
#    Program: mantel_by_cluster
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: November 13 2025
#    Goal: Compare cross-domain mantel coefficients by cluster
#     (want to see if this is useful for sorting clusters)
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

# think one of vegan / ecodist needs to be commented out here to get this to work
# I think one works for CIs and the other works for original Mantel...
# (vegan is commented out in the CI one)
# https://cran.r-project.org/web/packages/ecodist/ecodist.pdf
# https://cran.r-project.org/web/packages/vegan/vegan.pdf - yeah this is the formula I am using here


if (Sys.getenv(c("HOME" )) == "/home/lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo", 
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats") # ,"ecodist"
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats") # ,"ecodist"
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "vegan",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats") # ,"ecodist"
  
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
                         "expss", "vtable", "dplyr", "forcats") # ,"ecodist"
  
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
# Load cluster information created in step 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/psid_typology-comparison-truncated-prep.RData")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First need to get dissimilarity matrix for each cluster (using 8 cluster solution)
# Have to remember to use the missing cost matrices (see step 00)
# AND normalize based on length - which actually VARIES BY CLUSTER
# So need to also get the length by cluster again, can't use what is in 00
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Cluster 1
dist.fam.c1 <- seqdist(mc8.fam1.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c1 <- seqdist(mc8.work.ow1.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c1 <- seqdist(mc8.hw.hrs1.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same, right??)
  # Confirmed this is true using just m1 in step 00. so can just create 1 length matrix here.
  seq.len.fam.c1<-seqlength(mc8.fam1.seq, with.missing = FALSE)

  c1.min.len <- matrix(NA,ncol=length(seq.len.fam.c1),nrow=length(seq.len.fam.c1))
  for (i in 1:length(seq.len.fam.c1)){
    for (j in 1:length(seq.len.fam.c1)){
      c1.min.len[i,j] <- min(c(seq.len.fam.c1[i],seq.len.fam.c1[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c1.min<-dist.fam.c1 / c1.min.len
  dist.work.c1.min<-dist.work.c1 / c1.min.len
  dist.hw.c1.min<-dist.hw.c1 / c1.min.len

##### Cluster 2
dist.fam.c2 <- seqdist(mc8.fam2.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c2 <- seqdist(mc8.work.ow2.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c2 <- seqdist(mc8.hw.hrs2.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c2<-seqlength(mc8.fam2.seq, with.missing = FALSE)
  
  c2.min.len <- matrix(NA,ncol=length(seq.len.fam.c2),nrow=length(seq.len.fam.c2))
  for (i in 1:length(seq.len.fam.c2)){
    for (j in 1:length(seq.len.fam.c2)){
      c2.min.len[i,j] <- min(c(seq.len.fam.c2[i],seq.len.fam.c2[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c2.min<-dist.fam.c2 / c2.min.len
  dist.work.c2.min<-dist.work.c2 / c2.min.len
  dist.hw.c2.min<-dist.hw.c2 / c2.min.len

##### Cluster 3
dist.fam.c3 <- seqdist(mc8.fam3.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c3 <- seqdist(mc8.work.ow3.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c3 <- seqdist(mc8.hw.hrs3.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c3<-seqlength(mc8.fam3.seq, with.missing = FALSE)
  
  c3.min.len <- matrix(NA,ncol=length(seq.len.fam.c3),nrow=length(seq.len.fam.c3))
  for (i in 1:length(seq.len.fam.c3)){
    for (j in 1:length(seq.len.fam.c3)){
      c3.min.len[i,j] <- min(c(seq.len.fam.c3[i],seq.len.fam.c3[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c3.min<-dist.fam.c3 / c3.min.len
  dist.work.c3.min<-dist.work.c3 / c3.min.len
  dist.hw.c3.min<-dist.hw.c3 / c3.min.len

##### Cluster 4
dist.fam.c4 <- seqdist(mc8.fam4.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c4 <- seqdist(mc8.work.ow4.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c4 <- seqdist(mc8.hw.hrs4.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)
  
  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c4<-seqlength(mc8.fam4.seq, with.missing = FALSE)
  
  c4.min.len <- matrix(NA,ncol=length(seq.len.fam.c4),nrow=length(seq.len.fam.c4))
  for (i in 1:length(seq.len.fam.c4)){
    for (j in 1:length(seq.len.fam.c4)){
      c4.min.len[i,j] <- min(c(seq.len.fam.c4[i],seq.len.fam.c4[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c4.min<-dist.fam.c4 / c4.min.len
  dist.work.c4.min<-dist.work.c4 / c4.min.len
  dist.hw.c4.min<-dist.hw.c4 / c4.min.len

##### Cluster 5
dist.fam.c5 <- seqdist(mc8.fam5.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c5 <- seqdist(mc8.work.ow5.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c5 <- seqdist(mc8.hw.hrs5.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c5<-seqlength(mc8.fam5.seq, with.missing = FALSE)
  
  c5.min.len <- matrix(NA,ncol=length(seq.len.fam.c5),nrow=length(seq.len.fam.c5))
  for (i in 1:length(seq.len.fam.c5)){
    for (j in 1:length(seq.len.fam.c5)){
      c5.min.len[i,j] <- min(c(seq.len.fam.c5[i],seq.len.fam.c5[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c5.min<-dist.fam.c5 / c5.min.len
  dist.work.c5.min<-dist.work.c5 / c5.min.len
  dist.hw.c5.min<-dist.hw.c5 / c5.min.len

##### Cluster 6
dist.fam.c6 <- seqdist(mc8.fam6.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c6 <- seqdist(mc8.work.ow6.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c6 <- seqdist(mc8.hw.hrs6.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c6<-seqlength(mc8.fam6.seq, with.missing = FALSE)
  
  c6.min.len <- matrix(NA,ncol=length(seq.len.fam.c6),nrow=length(seq.len.fam.c6))
  for (i in 1:length(seq.len.fam.c6)){
    for (j in 1:length(seq.len.fam.c6)){
      c6.min.len[i,j] <- min(c(seq.len.fam.c6[i],seq.len.fam.c6[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c6.min<-dist.fam.c6 / c6.min.len
  dist.work.c6.min<-dist.work.c6 / c6.min.len
  dist.hw.c6.min<-dist.hw.c6 / c6.min.len

##### Cluster 7
dist.fam.c7 <- seqdist(mc8.fam7.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c7 <- seqdist(mc8.work.ow7.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c7 <- seqdist(mc8.hw.hrs7.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c7<-seqlength(mc8.fam7.seq, with.missing = FALSE)
  
  c7.min.len <- matrix(NA,ncol=length(seq.len.fam.c7),nrow=length(seq.len.fam.c7))
  for (i in 1:length(seq.len.fam.c7)){
    for (j in 1:length(seq.len.fam.c7)){
      c7.min.len[i,j] <- min(c(seq.len.fam.c7[i],seq.len.fam.c7[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c7.min<-dist.fam.c7 / c7.min.len
  dist.work.c7.min<-dist.work.c7 / c7.min.len
  dist.hw.c7.min<-dist.hw.c7 / c7.min.len

##### Cluster 8
dist.fam.c8 <- seqdist(mc8.fam8.seq, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)
dist.work.c8 <- seqdist(mc8.work.ow8.seq, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)
dist.hw.c8 <- seqdist(mc8.hw.hrs8.seq, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

  ## Get cluster length (can just do for one domain bc will be same)
  seq.len.fam.c8<-seqlength(mc8.fam8.seq, with.missing = FALSE)
  
  c8.min.len <- matrix(NA,ncol=length(seq.len.fam.c8),nrow=length(seq.len.fam.c8))
  for (i in 1:length(seq.len.fam.c8)){
    for (j in 1:length(seq.len.fam.c8)){
      c8.min.len[i,j] <- min(c(seq.len.fam.c8[i],seq.len.fam.c8[j]))
    }
  }

  ## Normalize to min length (min lengths created in step 00)
  dist.fam.c8.min<-dist.fam.c8 / c8.min.len
  dist.work.c8.min<-dist.work.c8 / c8.min.len
  dist.hw.c8.min<-dist.hw.c8 / c8.min.len

##### Total (do I need this? shouldn't this be there already?)
#dist.fam <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
#                    sm= fam.miss.cost$sm, with.missing=TRUE)
#dist.work <- seqdist(seq.work.ow, method="OM", indel=work.miss.indel, 
#                     sm= work.miss.cost$sm, with.missing=TRUE)
#dist.hw <- seqdist(seq.hw.hrs, method="OM", indel=hw.miss.indel, 
#                   sm= hw.miss.cost$sm, with.missing=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Then Mantel Coefficients across each domain for each cluster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Cluster 1
mantel_work.hw.c1 = mantel(dist.work.c1.min, dist.hw.c1.min)
mantel_work.fam.c1 = mantel(dist.work.c1.min, dist.fam.c1.min)
mantel_hw.fam.c1 = mantel(dist.hw.c1.min, dist.fam.c1.min)

##### Cluster 2
mantel_work.hw.c2 = mantel(dist.work.c2.min, dist.hw.c2.min)
mantel_work.fam.c2 = mantel(dist.work.c2.min, dist.fam.c2.min)
mantel_hw.fam.c2 = mantel(dist.hw.c2.min, dist.fam.c2.min)

##### Cluster 3
mantel_work.hw.c3 = mantel(dist.work.c3.min, dist.hw.c3.min)
mantel_work.fam.c3 = mantel(dist.work.c3.min, dist.fam.c3.min)
mantel_hw.fam.c3 = mantel(dist.hw.c3.min, dist.fam.c3.min)

##### Cluster 4
mantel_work.hw.c4 = mantel(dist.work.c4.min, dist.hw.c4.min)
mantel_work.fam.c4 = mantel(dist.work.c4.min, dist.fam.c4.min)
mantel_hw.fam.c4 = mantel(dist.hw.c4.min, dist.fam.c4.min)

##### Cluster 5
mantel_work.hw.c5 = mantel(dist.work.c5.min, dist.hw.c5.min)
mantel_work.fam.c5 = mantel(dist.work.c5, dist.fam.c5.min)
mantel_hw.fam.c5 = mantel(dist.hw.c5.min, dist.fam.c5.min)

##### Cluster 6
mantel_work.hw.c6 = mantel(dist.work.c6.min, dist.hw.c6.min)
mantel_work.fam.c6 = mantel(dist.work.c6.min, dist.fam.c6.min)
mantel_hw.fam.c6 = mantel(dist.hw.c6.min, dist.fam.c6.min)

##### Cluster 7
mantel_work.hw.c7 = mantel(dist.work.c7.min, dist.hw.c7.min)
mantel_work.fam.c7 = mantel(dist.work.c7.min, dist.fam.c7.min)
mantel_hw.fam.c7 = mantel(dist.hw.c7.min, dist.fam.c7.min)

##### Cluster 8
mantel_work.hw.c8 = mantel(dist.work.c8.min, dist.hw.c8.min)
mantel_work.fam.c8 = mantel(dist.work.c8.min, dist.fam.c8.min)
mantel_hw.fam.c8 = mantel(dist.hw.c8.min, dist.fam.c8.min)

##### Total (get using OMs from step 00, validate match step 5?)
mantel_work.hw = mantel(dist.work.min, dist.hw.min)
mantel_work.fam = mantel(dist.work.min, dist.fam.min)
mantel_hw.fam = mantel(dist.hw.min, dist.fam.min)

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
                'Cluster 8: work/hw', 'Cluster 8: work/fam',  'Cluster 8: hw/fam',
             'Total: work/hw', 'Total: work/fam',  'Total: hw/fam')

stats <- c(mantel_work.hw.c1$statistic, mantel_work.fam.c1$statistic, mantel_hw.fam.c1$statistic,
           mantel_work.hw.c6$statistic, mantel_work.fam.c6$statistic, mantel_hw.fam.c6$statistic,
           mantel_work.hw.c3$statistic, mantel_work.fam.c3$statistic, mantel_hw.fam.c3$statistic,
           mantel_work.hw.c4$statistic, mantel_work.fam.c4$statistic, mantel_hw.fam.c4$statistic,
           mantel_work.hw.c5$statistic, mantel_work.fam.c5$statistic, mantel_hw.fam.c5$statistic,
           mantel_work.hw.c6$statistic, mantel_work.fam.c6$statistic, mantel_hw.fam.c6$statistic,
           mantel_work.hw.c7$statistic, mantel_work.fam.c7$statistic, mantel_hw.fam.c7$statistic,
           mantel_work.hw.c8$statistic, mantel_work.fam.c8$statistic, mantel_hw.fam.c8$statistic,
           mantel_work.hw$statistic, mantel_work.fam$statistic, mantel_hw.fam$statistic)

sig <- c(mantel_work.hw.c1$signif, mantel_work.fam.c1$signif, mantel_hw.fam.c1$signif,
        mantel_work.hw.c6$signif, mantel_work.fam.c6$signif, mantel_hw.fam.c6$signif,
        mantel_work.hw.c3$signif, mantel_work.fam.c3$signif, mantel_hw.fam.c3$signif,
        mantel_work.hw.c4$signif, mantel_work.fam.c4$signif, mantel_hw.fam.c4$signif,
        mantel_work.hw.c5$signif, mantel_work.fam.c5$signif, mantel_hw.fam.c5$signif,
        mantel_work.hw.c6$signif, mantel_work.fam.c6$signif, mantel_hw.fam.c6$signif,
        mantel_work.hw.c7$signif, mantel_work.fam.c7$signif, mantel_hw.fam.c7$signif,
        mantel_work.hw.c8$signif, mantel_work.fam.c8$signif, mantel_hw.fam.c8$signif,
        mantel_work.hw$signif, mantel_work.fam$signif, mantel_hw.fam$signif)
         
mantel_cluster <- data.frame(cluster, stats, sig)

write.xlsx(mantel_cluster, "results/PSID/psid_mantel_truncated_clusters.xlsx")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## confidence intervals at overall level
## (this is now separate step 5 bc it takes quite a while)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Total
#mantel.work.hw.ci <- mantel(lower(dist.work) ~ lower(dist.hw), nperm=1000)
#mantel.work.fam.ci <- mantel(lower(dist.work) ~ lower(dist.fam), nperm=1000)
#mantel.hw.fam.ci <- mantel(lower(dist.hw) ~ lower(dist.fam), nperm=1000)

#mantel.df.work.hw.ci <- data.frame(mantel.work.hw.ci)
#mantel.df.work.fam.ci <- data.frame(mantel.work.fam.ci)
#mantel.df.hw.fam.ci <- data.frame(mantel.hw.fam.ci)

#mantel.col <- c('mantelr','pval1','pval2','pval3','llim.2.5%','ulim.97.5%')

#mantel.df <- data.frame(mantel.col, mantel.df.work.hw.ci, 
#                        mantel.df.work.fam.ci, mantel.df.hw.fam.ci)

#write.xlsx(mantel.df, "results/PSID/psid_mantel_ci.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("created data/PSID_truncated_mantel_by_cluster.RData")

