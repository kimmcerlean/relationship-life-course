# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 10 2025
#    Goal: Create index plots using sorting from textbook
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 4b
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("created data/typology-comparison-detailed-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 4b)
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
# Attempt seqMDplots ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for reference:
# mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),
#                           method="OM", indel=1, sm="CONSTANT") 

#### State distribution (for reference)
pdf("results/PSID/PSID_MCSA_SDPlot.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="d",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

#### Full index (for reference)
pdf("results/PSID/PSID_MCSA_IndexPlot.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="I",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE) 

dev.off()

#### Relative frequency: default K
pdf("results/PSID/PSID_MCSA_RFPlot.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE)

dev.off()

#### Relative frequency: 500 K
pdf("results/PSID/PSID_MCSA_RFPlot_500k.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="rf", diss=mcdist.det.om,
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE,
          dom.byrow=FALSE,k=500)

dev.off()

#### Frequency
pdf("results/PSID/PSID_MCSA_FreqPlot.pdf",
    width=15,
    height=28)

seqplotMD(channels=list(Family=seq.fam,Work=seq.work.ow,Housework=seq.hw.hrs.alt),
          group = data$mc.factor, type="f",
          xlab="Marital Duration", xtlab = 1:10, ylab=NA, yaxis=FALSE)  

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cluster-specific multidimensional scaling ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# multidimensional scaling results are used to obtain the sequences' 
# sorting order for the index plot

cmd.idx <- map(levels(data$mc.factor),
               ~cmdscale(mcdist.det.om[data$mc.factor == .x,
                                   data$mc.factor == .x],
                         k = 2) |>
                 as_tibble(rownames = "idx", .name_repair = "unique") |> 
                 arrange(across(-idx)) |> 
                 pull(idx) |>
                 as.numeric()) |> 
  unlist()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 <- ggseqiplot(seq.fam[cmd.idx,],
                 group = data$mc.factor[cmd.idx],
                 no.n = T, facet_ncol = 1, strip.position = "left") +
  scale_x_discrete(labels = 1:10, guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Family formation",
       x = "Relationship Duration",
       y = NULL) +
  guides(fill = guide_legend(ncol = 1, title="Family Formation"),
         color = guide_legend(ncol = 1, title="Family Formation")) +
  force_panelsizes(rows = sort(unique(data$share))) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) 


p2 <- ggseqiplot(seq.work.ow[cmd.idx,],
                 group = data$mc.factor[cmd.idx],
                 no.n = T, facet_ncol = 1, strip.position = "left") +
  scale_x_discrete(labels = 1:10, guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Paid Work",
       x = "Relationship Duration",
       y = NULL) +
  guides(fill = guide_legend(ncol = 1, title="Paid Work"),
         color = guide_legend(ncol = 1, title="Paid Work")) +
  force_panelsizes(rows = sort(unique(data$share))) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_blank()) 

p3 <- ggseqiplot(seq.hw.hrs.alt[cmd.idx,],
                 group = data$mc.factor[cmd.idx],
                 no.n = T, facet_ncol = 1, strip.position = "left") +
  scale_x_discrete(labels = 1:10, guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Housework",
       x = "Relationship Duration",
       y = NULL) +
  guides(fill = guide_legend(ncol = 1, title="Housework"),
         color = guide_legend(ncol = 1, title="Housework")) +
  force_panelsizes(rows = sort(unique(data$share))) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_blank()) 


library(patchwork)

ggjoint <- p1 + p2 + p3 + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "right",
        panel.spacing = unit(1, "lines"),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"))


# Plots speichern
ggsave(plot = ggjoint,
       "results/PSID/PSID_MCSA_Index.pdf",
       width = 16, height = 12) # , device = cairo_pdf

save.image("created data/PSID_index_plots.RData")


# pdf_convert("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/results/figures/PSID/PSID_MCSA_IndexPlot.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/results/figures/PSID/PSID_MCSA_IndexPlot.png")

# pdf_convert("results/PSID/PSID_MCSA_Index.pdf",
#            format = "png", dpi = 300, pages = 1,
#            "results/PSID/PSID_MCSA_Index.png")
