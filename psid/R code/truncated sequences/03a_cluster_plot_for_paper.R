# ---------------------------------------------------------------------
#    Program: cluster-plots
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: December 3 2025
#    Goal: Create final RF plots to use in paper for 8 cluster solution
#     Splitting out to test different sort orders more easily
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
# Charts for 8 cluster solution aka one we are using
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 2 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/psid_typology-comparison-truncated-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 2)
# mc8 <- mcdist.om.pam.ward$clustering$cluster8 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc8,
         id2 = row_number())

# Obtain relative frequencies of the eight cluster (using weights)
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

# Need a new column with real cluster values
# See completed cluster plots in UK folder - section (for Lund workshop)
real.cluster<-c(1,2,3,4,5,6,7,8)
mc8.factor<-c(1,6,8,5,2,7,4,3)
cluster.id<-c( "Cluster 1 (8.6%)", "Cluster 2 (12.0%)", "Cluster 3 (6.3%)", 
              "Cluster 4 (6.5%)","Cluster 5 (14.3%)", "Cluster 6 (20.8%)", 
              "Cluster 7 (13.9%)", "Cluster 8 (17.5%)")
cluster_lookup <- data.frame(mc8.factor,cluster.id)

data$real.cluster <- data %>%
  left_join(cluster_lookup, by="mc8.factor")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference (the normalized OM matrix):
# mcdist.det.min <- mcdist.det.om / fam.min.len


#### Relative frequency: 100 K (start, domain: fam)
pdf("results/PSID/PSID_RF100Plot_start_mc8_final.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=3,
          cex.legend=0.7, main="United States")

dev.off()

#### Relative frequency: 100 K (end, domain: fam)
pdf("results/PSID/PSID_RF100Plot_end_mc8_final.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7, main="United States")

dev.off()

#### Relative frequency: 100 K (end, domain: fam)
pdf("results/PSID/PSID_RF100Plot_end_mc8_letterx2.pdf",
    width=16,
    height=22)
    
# paper="letter") # this doesn't work, says margins too small

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=0.7, main="United States")

dev.off()

#### Want to see if doing representative plots (or other options)
### could work for Trinity presentation.
### One risk of this is that the representative sequences might not be 10 years, which might make this wonkier gah
  # yes this is a problem
### still also need to sort out the dimensions above
pdf("results/PSID/PSID_RepSeq_Trunc_mc8.pdf",
    width=15,
    height=28)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="r", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          criterion="density", coverage=.25) # think will need to play around with these  
#I worry with sorting based on frequency is that every single state needs to match at all durations, which is prob not as likely as I think
# coverage of .5 didn't work - all black lol (think it's bc I need SO MANY sequences to hit that)
# so also trying just 10 below

dev.off()

pdf("results/PSID/PSID_RepSeq10_Trunc_mc8.pdf",
    width=15,
    height=28)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="r", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          criterion="density", nrep=10) # think will need to play around with these  
#I worry with sorting based on frequency is that every single state needs to match at all durations, which is prob not as likely as I think

dev.off()

# think this is how you get medoid (per R manual) - the bottom row
pdf("results/PSID/PSID_Medoid_Trunc_mc8.pdf",
    width=15,
    height=28)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="r", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          criterion="dist", nrep=1) 

dev.off()
# Yes-  problem is, most of the medoids are just 3-4 durations long, rest are missing, so this also doesn't work well...

### Trying modal also bc don't know what that will look like
### think this is not nec. any one sequence. it takes modal state at each duration
pdf("results/PSID/PSID_Modal_Trunc_mc8.pdf",
    width=15,
    height=28)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Housework=seq.hw.hrs,Family=seq.fam),
          group = data$real.cluster$cluster.id, type="ms",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

#pdf_convert("C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course/results/for paper/United States Cluster Plot.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course/results/for paper/United States Cluster Plot.png")
