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
# Try the Liao and Fasang 2021 approach
# SeqCompare: https://search.r-project.org/CRAN/refmans/TraMineRextras/html/seqCompare.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Small drawback here is technically built to compare two groups at a time
# In some ways, answers DIFF question than DA - like, which groups are most different and are ALL different?

# Let's explore first with the two group college because handled easily, then figure out if I can easily subet by education and compare 2x2
# Will add to export for funsies to compare
one.fam <- seqCompare(seq.fam, seqdata2=NULL, group=data$one_college, stat="all",
           set=NULL,  with.missing = TRUE, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)
          #  s=100

one.work <- seqCompare(seq.work.ow, seqdata2=NULL, group=data$one_college, stat="all",
          set=NULL,  with.missing = TRUE, seed=36963, 
          squared="LRTonly", weighted=FALSE, opt=NULL, 
          BFopt=NULL, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel)
          # s=100

one.hw <- seqCompare(seq.hw.hrs, seqdata2=NULL, group=data$one_college, stat="all",
           set=NULL,  with.missing = TRUE, s=100, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel)
# s=100

# Diss matrices for reference:
# dist.work.om <- seqdist(seq.work.ow, method="OM", indel=work.miss.indel, 
#                        sm= work.miss.cost$sm, with.missing=TRUE)
# dist.hw.om <- seqdist(seq.hw.hrs, method="OM", indel=hw.miss.indel, 
#                      sm= hw.miss.cost$sm, with.missing=TRUE)
# dist.fam.om <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
#                       sm= fam.miss.cost$sm, with.missing=TRUE)

# Are these difference or just one or the other? I think they are just one or the other
seqBIC(seq.fam, seqdata2=NULL, group=data$one_college,
           set=NULL,  with.missing = TRUE, seed=36963, 
           squared="LRTonly", weighted=FALSE, opt=NULL, 
           BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)

seqLRT(seq.fam, seqdata2=NULL, group=data$one_college,
       set=NULL,  with.missing = TRUE, seed=36963, 
       squared="LRTonly", weighted=FALSE, opt=NULL, 
       BFopt=NULL, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel)


### Is this how i create subsequences for each group?
lev <- levels(data$couple_educ_type)
l <- length(lev)
seq.list.fam <- list()
for (i in 1:l){
  seq.list.fam[[i]] <- seq.fam[data$couple_educ_type==lev[i],]
}

sapply(seq.list.fam, nrow) # confirm counts match educ counts

## Then I have to do ALL pairwise? how do I also even know this makes sense / is correct?
seqCompare(list(seq.list.fam[[1]]),list(seq.list.fam[[2]]), stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

# Can also do it this way; okay let's do this
# ~~~~~~~~~~~~~~~
# Family
# ~~~~~~~~~~~~~~~

fam.neither.him <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Him College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.neither.her <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Her College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.neither.both <- seqCompare(seq.fam[data$couple_educ_type=="Neither College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
           s=100, seed=36963, squared="LRTonly")

fam.him.her <- seqCompare(seq.fam[data$couple_educ_type=="Him College",],seq.fam[data$couple_educ_type=="Her College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

fam.him.both <- seqCompare(seq.fam[data$couple_educ_type=="Him College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

fam.her.both <- seqCompare(seq.fam[data$couple_educ_type=="Her College",],seq.fam[data$couple_educ_type=="Both College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=fam.miss.cost$sm, indel=fam.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Paid Work
# ~~~~~~~~~~~~~~~

work.neither.him <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Him College",], stat="all", 
                               with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                               s=100, seed=36963, squared="LRTonly")

work.neither.her <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Her College",], stat="all", 
                               with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                               s=100, seed=36963, squared="LRTonly")

work.neither.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Neither College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                                with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                                s=100, seed=36963, squared="LRTonly")

work.him.her <- seqCompare(seq.work.ow[data$couple_educ_type=="Him College",],seq.work.ow[data$couple_educ_type=="Her College",], stat="all", 
                           with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                           s=100, seed=36963, squared="LRTonly")

work.him.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Him College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                            with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                            s=100, seed=36963, squared="LRTonly")

work.her.both <- seqCompare(seq.work.ow[data$couple_educ_type=="Her College",],seq.work.ow[data$couple_educ_type=="Both College",], stat="all", 
                            with.missing = TRUE, method="OM", sm=work.miss.cost$sm, indel=work.miss.indel,
                            s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Housework
# ~~~~~~~~~~~~~~~
hw.neither.him <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Him College",], stat="all", 
                             with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                             s=100, seed=36963, squared="LRTonly")

hw.neither.her <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Her College",], stat="all", 
                             with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                             s=100, seed=36963, squared="LRTonly")

hw.neither.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Neither College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                              with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                              s=100, seed=36963, squared="LRTonly")

hw.him.her <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Him College",],seq.hw.hrs[data$couple_educ_type=="Her College",], stat="all", 
                         with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                         s=100, seed=36963, squared="LRTonly")

hw.him.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Him College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

hw.her.both <- seqCompare(seq.hw.hrs[data$couple_educ_type=="Her College",],seq.hw.hrs[data$couple_educ_type=="Both College",], stat="all", 
                          with.missing = TRUE, method="OM", sm=hw.miss.cost$sm, indel=hw.miss.indel,
                          s=100, seed=36963, squared="LRTonly")

# ~~~~~~~~~~~~~~~
# Export
# ~~~~~~~~~~~~~~~
one.fam.df <- as.data.frame(one.fam)
one.work.df <- as.data.frame(one.work)
one.hw.df <- as.data.frame(one.hw)
fam.neither.him.df <- as.data.frame(fam.neither.him)
fam.neither.her.df <- as.data.frame(fam.neither.her)
fam.neither.both.df <- as.data.frame(fam.neither.both)
fam.him.her.df <- as.data.frame(fam.him.her)
fam.him.both.df <- as.data.frame(fam.him.both)
fam.her.both.df <- as.data.frame(fam.her.both)
work.neither.him.df <- as.data.frame(work.neither.him)
work.neither.her.df <- as.data.frame(work.neither.her)
work.neither.both.df <- as.data.frame(work.neither.both)
work.him.her.df <- as.data.frame(work.him.her)
work.him.both.df <- as.data.frame(work.him.both)
work.her.both.df <- as.data.frame(work.her.both)
hw.neither.him.df <- as.data.frame(hw.neither.him)
hw.neither.her.df <- as.data.frame(hw.neither.her)
hw.neither.both.df <- as.data.frame(hw.neither.both)
hw.him.her.df <- as.data.frame(hw.him.her)
hw.him.both.df <- as.data.frame(hw.him.both)
hw.her.both.df <- as.data.frame(hw.her.both)

combined_results <- rbind(fam.neither.him.df, fam.neither.her.df, fam.neither.both.df, 
                          fam.him.her.df, fam.him.both.df, fam.her.both.df,
                          work.neither.him.df, work.neither.her.df, work.neither.both.df, 
                          work.him.her.df, work.him.both.df, work.her.both.df,
                          hw.neither.him.df, hw.neither.her.df, hw.neither.both.df, 
                          hw.him.her.df, hw.him.both.df, hw.her.both.df,
                          one.fam.df,one.work.df,one.hw.df)

combined_results <- cbind(comparison = c("Fam: Neither v. Him", "Fam: Neither v. Her",
                                         "Fam: Neither v. Both", "Fam: Him v. Her",
                                         "Fam: Him v. Both", "Fam: Her v. Both",
                                         "Work: Neither v. Him", "Work: Neither v. Her",
                                         "Work: Neither v. Both", "Work: Him v. Her",
                                         "Work: Him v. Both", "Work: Her v. Both",
                                         "HW: Neither v. Him", "HW: Neither v. Her",
                                         "HW: Neither v. Both", "HW: Him v. Her",
                                         "HW: Him v. Both", "HW: Her v. Both",
                                         "One College: Fam", "One College:Work", 
                                         "One College: HW"), combined_results)

write_xlsx(combined_results, "G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/PSID_BIC_LRT_tests.xlsx")
