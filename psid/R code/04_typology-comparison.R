# ---------------------------------------------------------------------
#    Program: cluster-comparison
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: March 9 2025
#    Goal: compare 3 v. 4 cluster solution of "simple" MCSA
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  
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
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools")
  
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
# Load data created in step 03
# Think actually only need sequences, trying
# this to save space to test the plot outside
# of HPC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load ("created data/cluster-comparison.RData")
load ("created data/setupsequence.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First compare the 3 v 4 cluster solution --
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hierarchical cluster analysis, non-squared dissimilarities
mcdist.simp.om <- seqdistmc(channels=list(seq.work, seq.hw, seq.fam), ## Seq states NOT om matrix
                            method="OM", indel=1, sm="CONSTANT") 

mc.simp.ward1 <- hclust(as.dist(mcdist.simp.om), 
                    method = "ward.D")

# Extract the 3-cluster solution

mc.ward.3cl <-cutree(mc.simp.ward1, k = 3)

# Attach the vector with the 3-cluster solution to the main data.frame

data$mc.ward.3cl<-mc.ward.3cl

# Extract the 4-cluster solution

mc.ward.4cl <-cutree(mc.simp.ward1, k = 4)

# Attach the vector with the 4-cluster solution to the main data.frame

data$mc.ward.4cl<-mc.ward.4cl

# Compare the two
comp.mc.ward<-table(mc.ward.3cl, mc.ward.4cl)

# Print the table
comp.mc.ward

# Export the table as a .csv file

write.csv(comp.mc.ward,("results/PSID_MC-simple-comparison.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempt to plot 4 cluster solution --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply PAM clustering + Ward starting point
# Ward's clustering is explained in Chapter 4

mcdist.om.pam.ward <- wcKMedRange(mcdist.simp.om, kvals = 2:10,
                                  initialclust = mc.simp.ward1)

# Cut tree at cluster==4

mc <- mcdist.om.pam.ward$clustering$cluster4 # these are sub"folders" in ward output

# Label the clusters from 1 to 4
labels<-unique(mc)
sort(labels)

mc.factor <- factor(mc, levels = sort(labels),
                     c("1", "2", "3", "4"))

# Separate objects for each channel and for each cluster (=4*3)

data$mc.factor <- as.numeric(mc.factor)

# Identify position of variables indicating start and end of sequences

work1.seq <- seq.work[data$mc.factor == "1", ]
work2.seq <- seq.work[data$mc.factor == "2", ]
work3.seq <- seq.work[data$mc.factor == "3", ]
work4.seq <- seq.work[data$mc.factor == "4", ]

hw1.seq <- seq.hw[data$mc.factor == "1", ]
hw2.seq <- seq.hw[data$mc.factor == "2", ]
hw3.seq <- seq.hw[data$mc.factor == "3", ]
hw4.seq <- seq.hw[data$mc.factor == "4", ]

fam1.seq <- seq.fam[data$mc.factor == "1", ]
fam2.seq <- seq.fam[data$mc.factor == "2", ]
fam3.seq <- seq.fam[data$mc.factor == "3", ]
fam4.seq <- seq.fam[data$mc.factor == "4", ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Obtain relative frequencies of the four clusters

relfreq <- data %>% 
  count(mc.factor) %>% 
  mutate(share = n/ sum(n)) %>%
  arrange(share)

# Convert relative frequencies to percentages (will be used for labeling the y-axes)
share <- round(as.numeric(relfreq$share)*100, 1)

# display frequencies of each cluster.
# This might also be useful to ensure one cluster is not extremely small?
print(relfreq)

write.csv(relfreq,("results/PSID_MC-4cluster-freq.csv"))

save.image("created data/typology-comparison-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here, we attempt to plot

# Sequences ordered by multidimensional scaling on both channels 

pdf("results/PSID_MCSA_joint.pdf",
          width=12,
          height=12)

def.par <- par(no.readonly = TRUE)

# Each state distribution plot will be displayed according to its relative size
# as we add some additional rows to the cluster some adjustments are required
heights.mc <- c(.05,as.numeric(relfreq$share)*.97,.03)
widths.mc <- c(0.1, 0.325, 0.325, 0.25)

# Specifying the location of the many figures/elements we want to plot
layout.mc <- layout(matrix(c(1,2,3,21,
                             4,5,6,21,
                             7,8,9,22,
                             10,11,12,22,
                             13,14,15,23,
                             16,17,18,23,
                             19,20,20,23), 7, 4, byrow = TRUE), 
                    widths = widths.mc, 
                    heights = heights.mc)
layout.show(layout.mc)

# Labelling of the x-axes
count <- seq(from = 0, to = 22, by = 2)
years <- seq(from = 18, to = 40, by = 2)

# start with the actual graph...

# Row 1 - columns 1-3
par(mar = c(0.2,0,0.2,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Family formation"), 
     cex = 2.5, col = "black", font = 2)

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Labor market"), 
     cex = 2.5, col = "black", font = 2)

# Row 2 - columns 1-3
par(mar = c(2,1,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Cluster 1\n","(",share[1], "%)"), 
     cex = 1.4, col = "black", font = 2)

par(mar = c(2,1,0,1))
seqIplot(mc.fam.year.seq[multidim$mc.factor == relfreq$mc.factor[1],],
         sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[1],
                                    multidim$mc.factor == relfreq$mc.factor[1]], k = 2),
         with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

seqIplot(mc.act.year.seq[multidim$mc.factor == relfreq$mc.factor[1],]
         , sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[1],
                                      multidim$mc.factor == relfreq$mc.factor[1]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

# Row 3 - columns 1-3
par(mar = c(2,1,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Cluster 2\n","(",share[2], "%)"), 
     cex = 1.4, col = "black", font = 2)

par(mar = c(2,1,0,1))
seqIplot(mc.fam.year.seq[multidim$mc.factor == relfreq$mc.factor[2],],
         sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[2],
                                    multidim$mc.factor == relfreq$mc.factor[2]], k = 2),
         with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

seqIplot(mc.act.year.seq[multidim$mc.factor == relfreq$mc.factor[2],]
         , sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[2],
                                      multidim$mc.factor == relfreq$mc.factor[2]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

# Row 4 - columns 1-3
par(mar = c(2,1,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Cluster 3\n","(",share[3], "%)"), 
     cex = 1.4, col = "black", font = 2)

par(mar = c(2,1,0,1))
seqIplot(mc.fam.year.seq[multidim$mc.factor == relfreq$mc.factor[3],], 
         sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[3],
                                    multidim$mc.factor == relfreq$mc.factor[3]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

seqIplot(mc.act.year.seq[multidim$mc.factor == relfreq$mc.factor[3],]
         , sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[3],
                                      multidim$mc.factor == relfreq$mc.factor[3]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

# Row 5 - columns 1-3
par(mar = c(2,1,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Cluster 4\n","(",share[4], "%)"), 
     cex = 1.4, col = "black", font = 2)

par(mar = c(2,1,0,1))
seqIplot(mc.fam.year.seq[multidim$mc.factor == relfreq$mc.factor[4],],
         sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[4],
                                    multidim$mc.factor == relfreq$mc.factor[4]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

seqIplot(mc.act.year.seq[multidim$mc.factor == relfreq$mc.factor[4],]
         , sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[4],
                                      multidim$mc.factor == relfreq$mc.factor[4]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")

# Row 6 - columns 1-3
par(mar = c(2,1,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Cluster 5\n","(",share[5], "%)"), 
     cex = 1.4, col = "black", font = 2)

par(mar = c(2,1,0,1))
seqIplot(mc.fam.year.seq[multidim$mc.factor == relfreq$mc.factor[5],], 
         sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[5],
                                    multidim$mc.factor == relfreq$mc.factor[5]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")
axis(1, at=(seq(0,22, by = 2)), labels = seq(18,40, by = 2), font = 1, cex.axis = 1.2, lwd = 1)

seqIplot(mc.act.year.seq[multidim$mc.factor == relfreq$mc.factor[5],]
         , sortv = cmdscale(mcdist.om[multidim$mc.factor == relfreq$mc.factor[5],
                                      multidim$mc.factor == relfreq$mc.factor[5]], k = 2)
         , with.legend = FALSE, border = NA, 
         axes = FALSE, yaxis = FALSE,  ylab = "")
axis(1, at = count, labels = years, font = 1, cex.axis = 1.2, lwd = 1)

# Row 7 - columns 1-3
par(mar = c(0.2,0,0.2,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("Age"), 
     cex = 1.5, col = "black", font = 2)

# Column 4 
par(mar = c(0.2,0,0.2,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

par(mar = c(4,1,4,1))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend(x = "top",inset = 0, legend = longlab.partner.child, col = colspace.partner.child, 
       lwd = 10, cex = 1.5, ncol = 1, box.lwd = 2)
mtext("Family formation",side = 3, line = 1, cex = 1.2, font = 2)

plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend(x = "top",inset = 0, legend = longlab.activity, col = colspace.activity, 
       lwd = 10, cex = 1.5, ncol = 1, box.lwd = 2)
mtext("Labor market",side = 3, line = 1, cex = 1.2, font = 2)

par(def.par)
dev.off()

