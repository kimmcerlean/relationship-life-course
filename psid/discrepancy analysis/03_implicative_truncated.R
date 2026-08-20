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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load and install packages for whomever is running the script
## the server doesn't let you install packages
## the server doesn't have ggseqplot for now (package incompatibility issue)

if (Sys.getenv(c("HOME" )) == "/home/lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", "writexl",
                         "colorspace","ggplot2","ggpubr", "ggseqplot","dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  
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
# Import data and small things needed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid-setupsequence-truncated.RData")

data$couple_educ_type <- factor(
  data$couple_educ_type,
  levels = c(1,2,3,4),
  labels = c(
    "Neither College",
    "Him College",
    "Her College",
    "Both College"
  )
)

data$parent_info <- factor(
  data$parent_info,
  levels = c(0,1,2),
  labels = c(
    "Always CF",
    "Become Parent",
    "Always Parent"
  )
)

table(data$couple_educ_type) 
table(data$one_college) # which is better? I think 4 groups is ideal but might be a lot to focus on? 
table(data$either_birth_pre_rel)
table(data$parent_info)

subset.cf0 <- data$either_birth_pre_rel %in% c(0)
subset.par1 <- data$either_birth_pre_rel %in% c(1)

subset.cf <- data$parent_info %in% c("Always CF")
subset.trans <- data$parent_info %in% c("Become Parent")
subset.par <- data$parent_info %in% c("Always Parent")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Okay TRY this implicative statistics?
# I think I can actually set missing to TRUE OR FALSE - which might get over my concerns?
# Because this seems to use SEQ object NOT Diss Matrix
# Let's prob explore both
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sequence of typical states
implic.fam.nomiss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                        weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.fam.miss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.miss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                             weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = FALSE,  ## can i ADJUST this with missing to help?
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.miss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = TRUE,  ## can i ADJUST this with missing to help?
                             weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

##Plotting the typical states
x_lab <- c("1","2","3","4","5","6","7","8","9","10")

plot(implic.fam.nomiss, lwd=3, conf.level=c(0.95, 0.99))
plot(implic.fam.miss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab) ## okay, this actually is OKAY and actually probably BETTER HIGHLIGHTS the dissolution
# OR maybe we use that for FAM state because really dissolution is a family state and then remove from other graphs? Let's see...

plot(implic.work.nomiss, lwd=2, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.work.miss, lwd=2, conf.level=c(0.95, 0.99))

plot(implic.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.hw.miss, lwd=3, conf.level=c(0.95, 0.99))

## Test just binary education
implic.one.fam.miss <- seqimplic(seq.fam, group=data$one_college, with.missing = TRUE,  
                             weighted = FALSE, na.rm = TRUE)

implic.one.work.nomiss <- seqimplic(seq.work.ow, group=data$one_college, with.missing = FALSE,
                                weighted = FALSE, na.rm = TRUE)

implic.one.hw.nomiss <- seqimplic(seq.hw.hrs, group=data$one_college, with.missing = FALSE,
                              weighted = FALSE, na.rm = TRUE)

plot(implic.one.fam.miss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.one.work.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.one.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

##Plotting the typical states (Example from Fauser to make pretty maybe) ##
# need to revisit this.
tiff("Z:/your path/graphs/Figure2.tif", width = 4800,
     height = 3600, units = "px", res = 300, compression = "lzw")
plot(implA, lwd=2.5, ylim=c(0,9), conf.level=c(0.95, 0.99, 0.999), 
     sub = "Age of child in months", cex.legend = 2, cex.main=2, 
     cex.axis = 1.5,  cex.lab = 1.5, cex.sub = 1.5)
dev.off()

