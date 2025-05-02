# ---------------------------------------------------------------------
#    Program: cluster-plots
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: April 24 2025
#    Goal: Create relative frequency and state distribution plots
#         comparing across cluster solutions
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
# Charts for 5 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 4 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/typology-comparison-complete-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 4)
# mc5 <- mcdist.om.pam.det.ward$clustering$cluster5 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc5,
         id2 = row_number())

# Obtain relative frequencies of the five cluster (using weights)
# Convert relative frequencies to percentages (used for labeling the y-axes)

data <- data |>
  count(cluster) |>  # wt = weight40
  mutate(share = n/ sum(n)) |>
  arrange(share) |> 
  mutate(mc.factor = glue("Cluster {row_number()}
                            ({round(share*100,1)}%)"),
         mc.factor = factor(mc.factor)) |> 
  select(cluster, mc.factor, share) |> 
  right_join(data, by = "cluster") |> 
  arrange(id2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference:
# mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),
#                           method="OM", indel=1, sm="CONSTANT") 

#### State distribution
pdf("results/PSID/PSID_MCSA_SDPlot_complete_mc5.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="d",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

#### Frequency
pdf("results/PSID/PSID_MCSA_FreqPlot_complete_mc5.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="f",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()


#### Relative frequency: 100 K, sort 1 (start)
pdf("results/PSID/PSID_MCSA_RF100Plot_start_complete_mc5.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start")

dev.off()

#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/PSID/PSID_MCSA_RF100Plot_startd1_complete_mc5.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=-1)

dev.off()


#### Relative frequency: 100 K, sort 2 (end)
pdf("results/PSID/PSID_MCSA_RF100Plot_end_complete_mc5.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end")

dev.off()

#### Relative frequency: 100 K, sort 3 (mds)
#pdf("results/PSID/PSID_MCSA_RFPlot_100k_mds.pdf",
#    width=15,
#    height=28)

#seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
#          group = data$mc.factor, type="rf", diss=mcdist.det.om,
#          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
#          dom.byrow=FALSE,k=100,sortv="mds")

#dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Charts for 6 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 4 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/typology-comparison-complete-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 4)
# mc6 <- mcdist.om.pam.det.ward$clustering$cluster5 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc6,
         id2 = row_number())

# Obtain relative frequencies of the six cluster (using weights)
# Convert relative frequencies to percentages (used for labeling the y-axes)

data <- data |>
  count(cluster) |>  # wt = weight40
  mutate(share = n/ sum(n)) |>
  arrange(share) |> 
  mutate(mc.factor = glue("Cluster {row_number()}
                            ({round(share*100,1)}%)"),
         mc.factor = factor(mc.factor)) |> 
  select(cluster, mc.factor, share) |> 
  right_join(data, by = "cluster") |> 
  arrange(id2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference:
# mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),
#                           method="OM", indel=1, sm="CONSTANT") 

#### State distribution
pdf("results/PSID/PSID_MCSA_SDPlot_complete_mc6.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="d",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

#### Frequency
pdf("results/PSID/PSID_MCSA_FreqPlot_complete_mc6.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="f",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()


#### Relative frequency: 100 K, sort 1 (start)
pdf("results/PSID/PSID_MCSA_RF100Plot_start_complete_mc6.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start")

dev.off()

#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/PSID/PSID_MCSA_RF100Plot_startd1_complete_mc6.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=-1)

dev.off()


#### Relative frequency: 100 K, sort 2 (end)
pdf("results/PSID/PSID_MCSA_RF100Plot_end_complete_mc6.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end")

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Charts for 7 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 4 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/typology-comparison-complete-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 4)
# mc7 <- mcdist.om.pam.det.ward$clustering$cluster5 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc7,
         id2 = row_number())

# Need a new column with real cluster values

# this isn't working
#data %>%
#  mutate(
#    cluster7 = case_when(
#      mc7.factor == 6 ~ 1,
#      mc7.factor == 7 ~ 2,
#      mc7.factor == 3 ~ 3,
#      mc7.factor == 1 ~ 4,
#      mc7.factor == 5 ~ 5,
#      mc7.factor == 2 ~ 6,
#      mc7.factor == 4 ~ 7
#    )
#  )

real.cluster<-c(1,2,3,4,5,6,7)
mc7.factor<-c(6,7,3,1,5,2,4)
cluster.id<-c("Cluster 1 (18.1%)", "Cluster 2 (13.1%)", "Cluster 3 (17.2%)", "Cluster 4 (10.4%)",
              "Cluster 5 (15.4%)", "Cluster 6 (15.6%)", "Cluster 7 (10.1%)")
cluster_lookup <- data.frame(mc7.factor,cluster.id)

#data |>
#  left_join(cluster_lookup |> select(real.cluster,mc7.factor))

data$real.cluster <- data %>%
  left_join(cluster_lookup, by="mc7.factor")
            
#data$real.cluster <- data %>%
#  left_join(cluster_lookup, by="mc7.factor" %>%
#              select(cluster.id))

#data <- data %>%
#  left_join(cluster_lookup |> select(real.cluster,cluster.id,mc7.factor))

# Obtain relative frequencies of the seven cluster (using weights)
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference:
# mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),
#                           method="OM", indel=1, sm="CONSTANT") 

#### State distribution
pdf("results/PSID/PSID_MCSA_SDPlot_complete_mc7.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor.x, type="d",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

#### Frequency
pdf("results/PSID/PSID_MCSA_FreqPlot_complete_mc7.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor.x, type="f",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()


#### Relative frequency: 100 K, sort 1 (start)
pdf("results/PSID/PSID_MCSA_RF100Plot_start_complete_mc7.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,"Paid Work"=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor.x, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start")

dev.off()

#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/PSID/PSID_MCSA_RF100Plot_startd1_complete_mc7.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,"Paid Work"=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor.x, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=-1)

dev.off()


#### Relative frequency: 100 K, sort 2 (end)
pdf("results/PSID/PSID_MCSA_RF100Plot_end_complete_mc7.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor.x, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.end")

dev.off()


### Current cluster order (For Lund Workshop)

#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/PSID/PSID_MCSA_RF100Plot_042925.pdf",
    width=15,
    height=28)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs.alt),
          group = data$real.cluster$cluster.id, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=2,
          cex.legend=0.7)

dev.off()

pdf_convert("results/PSID/PSID_MCSA_RF100Plot_042925.pdf",
            format = "png", dpi = 300, pages = 1,
            "results/PSID/PSID_MCSA_RF100Plot_042925.png")

# want to play around with size, because this isn't great for powerpoint
#### Relative frequency: 100 K, sort 1a (start, domain1)
pdf("results/PSID/PSID_MCSA_RF100Plot_resize.pdf",
    width=15,
    height=42)

seqplotMD(channels=list('Paid Work'=seq.work.ow,Family=seq.fam,Housework=seq.hw.hrs.alt),
          group = data$real.cluster$cluster.id, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=100,sortv="from.start",dom.crit=2,
          cex.legend=0.7)

dev.off()
