# ---------------------------------------------------------------------
#    Program: cluster-plots
#    Author: Kim McErlean & Lea Pessin 
#    Modified: November 2026
#    Goal: Create final RF plots to use in paper for 6 cluster solution
#     Splitting out to test different sort orders more easily
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
# Charts for 6 cluster solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 2 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/ukhls/typology-comparison-truncated-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (did in last step
# mc6 <- mcdist.om.pam.ward$clustering$cluster6 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc6,
         id2 = row_number())

# Obtain relative frequencies of the six cluster (using weights)
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
real.cluster<-c(1,2,3,4,5,6)
mc6.factor<-c(4,6,3,1,2,5)
cluster.id<-c("Stable neotraditional cohabitors with children (14.0%)", 
              "Stable neotraditional married couples with children (14.3%)", 
              "Egalitarian cohabitation to traditional or second shift marriage (28.2%)",
              "Second shift cohabitation to neotraditional marriage (19.3%)",
              "Traditionalization or intensification of second shift among direct marriers (13.7%)",
              "Cohabitors with children are relatively stably underemployed (10.8%)")
cluster_lookup <- data.frame(mc6.factor,cluster.id)

data$real.cluster <- data %>%
  left_join(cluster_lookup, by="mc6.factor")

# And get SORT order (it used to work because these were names Cluster 1, Cluster 2, etc.)
# ANd R sorts ALPHABETIcally. But I updated names so now I hve to give a sort order
cluster_order<-c("Stable neotraditional cohabitors with children (14.0%)", 
                 "Stable neotraditional married couples with children (14.3%)", 
                 "Egalitarian cohabitation to traditional or second shift marriage (28.2%)",
                 "Second shift cohabitation to neotraditional marriage (19.3%)",
                 "Traditionalization or intensification of second shift among direct marriers (13.7%)",
                 "Cohabitors with children are relatively stably underemployed (10.8%)")

data$cluster.id <- factor(data$real.cluster$cluster.id, levels = cluster_order)
cluster_titles <- levels(data$cluster.id)
levels(data$cluster.id)
table(data$cluster.id, useNA = "ifany")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference (the normalized OM matrix):
# mcdist.det.min <- mcdist.det.om / fam.min.len

#### Relative frequency: 100 K (end, domain: fam)
pdf("results/UKHLS/UKHLS_RF100Plot_end_mc6_final.pdf",
    width=15,
    height=42)

par(oma = c(0, 2, 3, 0))

seqplotMD(channels=list("Paid Work"=seq.work, "Housework"=seq.hw.hrs, "Family"=seq.fam),
          group = data$cluster.id, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=levels(data$cluster.id), 
          col.main="transparent", yaxis="left", yaxt = "n",cex.lab = 1.5,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=1, main="United Kingdom")

mtext(
  text = c("Paid Work", "Housework", "Family"),
  side = 3,
  outer = TRUE,
  line = 0.5,
  at = c(0.17, 0.50, 0.84),
  font = 2,
  cex = 1.2
)

dev.off()

#### Relative frequency: 100 K (end, domain: fam)
pdf("results/UKHLS/UKHLS_RF100Plot_end_mc6_final_size.pdf",
    width=25,
    height=42)

par(oma = c(0, 2, 3, 0))

seqplotMD(channels=list("Paid Work"=seq.work, "Housework"=seq.hw.hrs, "Family"=seq.fam),
          group = data$cluster.id, type="rf", diss=mcdist.det.min,
          xlab="Marital Duration", xtlab = 1:10, ylab=levels(data$cluster.id), 
          col.main="transparent", yaxis="left", yaxt = "n",cex.lab = 1.5,
          dom.byrow=FALSE,k=100,sortv="from.end",dom.crit=3,
          cex.legend=1, main="United Kingdom")

mtext(
  text = c("Paid Work", "Housework", "Family"),
  side = 3,
  outer = TRUE,
  line = 0.5,
  at = c(0.17, 0.50, 0.84),
  font = 2,
  cex = 1.2
)

dev.off()

#### Modal States

pdf("results/UKHLS/UKHLS_MSPlot_mc6_final.pdf",
    width=15,
    height=28)

par(oma = c(0, 2, 3, 0))

seqplotMD(channels=list("Paid Work"=seq.work, "Housework"=seq.hw.hrs, "Family"=seq.fam),
          group = data$cluster.id, type="ms",
          xlab="Marital Duration", xtlab = 1:10, ylab=levels(data$cluster.id),
          col.main="transparent", cex.lab = 1.05, yaxis="all") #, with.missing=FALSE - don't think this did anything

mtext(
  text = c("Paid Work", "Housework", "Family"),
  side = 3,
  outer = TRUE,
  line = 0.5,
  at = c(0.17, 0.50, 0.84),
  font = 2,
  cex = 1.2
)

dev.off()

#pdf_convert("C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course/results/for paper/United Kingdom Cluster Plot_size.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Relationship Life Course/results/for paper/United Kingdom Cluster Plot_size.png")


