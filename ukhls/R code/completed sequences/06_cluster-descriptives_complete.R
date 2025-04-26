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
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot","glue","Cairo",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","pdftools","gridExtra","foreign",
                         "reshape2", "Hmisc", "knitr", "kableExtra","OpenMx","grDevices","corrplot",
                         "car", "factoextra","nnet", "descr", "stats", "psych", "effects","ggh4x",
                         "expss", "vtable", "dplyr", "forcats")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load cluster information created in step 4 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/ukhls/typology-comparison-complete-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cut tree (this happened in step 4)
# mc7 <- mcdist.om.pam.det.ward$clustering$cluster5 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc7,
         id2 = row_number())

# Obtain relative frequencies of the seven cluster (using weights)
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Non-cluster specific examination of metrics 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mc.seq<-seqMD(channels=list(seq.work.ow, seq.hw.hrs.alt, seq.fam),what="MDseq")

seqST(mc.seq) # this is for each individual, so we would want to summarize by cluster?
seqlength(mc.seq)
seqtransn(mc.seq)
seqtransn(seq.fam)
seqtransn(seq.hw.hrs.alt)
seqtransn(seq.work.ow)

# These are metrics BY individual
seqindic.fam<-seqindic(seq.fam, indic=c("lgth", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                         "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.work<-seqindic(seq.work.ow, indic=c("lgth", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                              "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.hw<-seqindic(seq.hw.hrs.alt, indic=c("lgth", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                        "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add to data frame
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Family
data$fam_lgth <- seqindic.fam$Lgth
data$fam_spells <- seqindic.fam$Dlgth
data$fam_states <- seqindic.fam$Visited
data$fam_recu <- seqindic.fam$Recu
data$fam_trans <- seqindic.fam$Trans
data$fam_transp <- seqindic.fam$Transp
data$fam_entr <- seqindic.fam$Entr
data$fam_meand <- seqindic.fam$MeanD
data$fam_dustd <- seqindic.fam$Dustd
data$fam_meand2 <- seqindic.fam$MeanD2
data$fam_dustd2 <- seqindic.fam$Dustd2
data$fam_volat <- seqindic.fam$Volat
data$fam_cplx <- seqindic.fam$Cplx
data$fam_turb <- seqindic.fam$Turb
data$fam_turbn <- seqindic.fam$Turbn
data$fam_turb2 <- seqindic.fam$Turb2
data$fam_turb2n <- seqindic.fam$Turb2n

## Work
data$work_lgth <- seqindic.work$Lgth
data$work_spells <- seqindic.work$Dlgth
data$work_states <- seqindic.work$Visited
data$work_recu <- seqindic.work$Recu
data$work_trans <- seqindic.work$Trans
data$work_transp <- seqindic.work$Transp
data$work_entr <- seqindic.work$Entr
data$work_meand <- seqindic.work$MeanD
data$work_dustd <- seqindic.work$Dustd
data$work_meand2 <- seqindic.work$MeanD2
data$work_dustd2 <- seqindic.work$Dustd2
data$work_volat <- seqindic.work$Volat
data$work_cplx <- seqindic.work$Cplx
data$work_turb <- seqindic.work$Turb
data$work_turbn <- seqindic.work$Turbn
data$work_turb2 <- seqindic.work$Turb2
data$work_turb2n <- seqindic.work$Turb2n

## Housework
data$hw_spells <- seqindic.hw$Dlgth
data$hw_states <- seqindic.hw$Visited
data$hw_recu <- seqindic.hw$Recu
data$hw_trans <- seqindic.hw$Trans
data$hw_transp <- seqindic.hw$Transp
data$hw_entr <- seqindic.hw$Entr
data$hw_meand <- seqindic.hw$MeanD
data$hw_dustd <- seqindic.hw$Dustd
data$hw_meand2 <- seqindic.hw$MeanD2
data$hw_dustd2 <- seqindic.hw$Dustd2
data$hw_volat <- seqindic.hw$Volat
data$hw_cplx <- seqindic.hw$Cplx
data$hw_turb <- seqindic.hw$Turb
data$hw_turbn <- seqindic.hw$Turbn
data$hw_turb2 <- seqindic.hw$Turb2
data$hw_turb2n <- seqindic.hw$Turb2n

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cluster specific examination of metrics 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I think I can literally just use this objects created in step 4?
# Cluster 1
# mc7.fam1.seq
# mc7.work.ow1.seq 
# mc7.hw.hrs1.seq


# OR can I do what Lea did and attach them to the data object then use desctable?! that would be ideal?
# so, this just makes a smaller table with these columns
desctable <- subset(data,
                    select = c(mc7.factor, fam_lgth, fam_spells, fam_states, 
                               fam_recu, fam_trans, fam_transp, fam_entr, 
                               fam_meand, fam_dustd, fam_meand2, fam_dustd2,
                               fam_volat, fam_cplx, fam_turb, fam_turbn,
                               fam_turb2, fam_turb2n, work_lgth, work_spells,
                               work_states, work_recu, work_trans, work_transp,
                               work_entr, work_meand, work_dustd, work_meand2,
                               work_dustd2, work_volat, work_cplx, work_turb,
                               work_turbn, work_turb2, work_turb2n, hw_spells,
                               hw_states, hw_recu, hw_trans, hw_transp,
                               hw_entr, hw_meand, hw_dustd, hw_meand2,
                               hw_dustd2, hw_volat, hw_cplx, hw_turb,
                               hw_turbn, hw_turb2, hw_turb2n)
)

# oh, this is just a new column
desctable$clusters <- as.character(desctable$mc7.factor)

sumtable(desctable, digits = 4, fixed.digits = TRUE, numformat = NA, group = 'clusters', group.test = TRUE, 
         out='csv', file='results/UKHLS/cluster_desctable.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean time in a state
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Family
seqistatd(seq.fam) # this is individual level mean time in a state. Do these vary when aggregated?

state.time.fam <- seqmeant(seq.fam) # this is mean time in a state aggregated?
state.time.fam.c1 <- seqmeant(mc7.fam1.seq)
state.time.fam.c2 <- seqmeant(mc7.fam2.seq)
state.time.fam.c3 <- seqmeant(mc7.fam3.seq)
state.time.fam.c4 <- seqmeant(mc7.fam4.seq)
state.time.fam.c5 <- seqmeant(mc7.fam5.seq)
state.time.fam.c6 <- seqmeant(mc7.fam6.seq)
state.time.fam.c7 <- seqmeant(mc7.fam7.seq)

state.fam <- c('MARc0','MARc1','MARc2','MARc3','COHc0','COHc1','COHc2','COHc3','diss','attrit')

state.time.family <- data.frame(state.fam, state.time.fam, state.time.fam.c1, state.time.fam.c2,
                                state.time.fam.c3, state.time.fam.c4,state.time.fam.c5,
                                state.time.fam.c6, state.time.fam.c7)

write.xlsx(state.time.family, "results/UKHLS/ukhls_family_statedurs.xlsx")

# Work
state.time.work.ow <- seqmeant(seq.work.ow)
state.time.work.ow.c1 <- seqmeant(mc7.work.ow1.seq)
state.time.work.ow.c2 <- seqmeant(mc7.work.ow2.seq)
state.time.work.ow.c3 <- seqmeant(mc7.work.ow3.seq)
state.time.work.ow.c4 <- seqmeant(mc7.work.ow4.seq)
state.time.work.ow.c5 <- seqmeant(mc7.work.ow5.seq)
state.time.work.ow.c6 <- seqmeant(mc7.work.ow6.seq)
state.time.work.ow.c7 <- seqmeant(mc7.work.ow7.seq)

state.work.ow <- c('MBW','1.5MBW','dualFT','dualFT-hisOW', 'dualFT-herOW', 'dualOW','FBW','UnderWK')

state.time.work <- data.frame(state.work.ow, state.time.work.ow, state.time.work.ow.c1, state.time.work.ow.c2,
                                state.time.work.ow.c3, state.time.work.ow.c4,state.time.work.ow.c5,
                                state.time.work.ow.c6, state.time.work.ow.c7)

write.xlsx(state.time.work, "results/UKHLS/ukhls_work_statedurs.xlsx")


# Housework
state.time.hw.hrs <- seqmeant(seq.hw.hrs.alt)
state.time.hw.hrs.c1 <- seqmeant(mc7.hw.hrs1.seq)
state.time.hw.hrs.c2 <- seqmeant(mc7.hw.hrs2.seq)
state.time.hw.hrs.c3 <- seqmeant(mc7.hw.hrs3.seq)
state.time.hw.hrs.c4 <- seqmeant(mc7.hw.hrs4.seq)
state.time.hw.hrs.c5 <- seqmeant(mc7.hw.hrs5.seq)
state.time.hw.hrs.c6 <- seqmeant(mc7.hw.hrs6.seq)
state.time.hw.hrs.c7 <- seqmeant(mc7.hw.hrs7.seq)

state.hw.hrs <- c('W-all:high', 'W-all:low', 'W-most:high', 'W-most:med', 'W-most:low',
                  'equal:high', 'equal:low', 'M-most:high', 'M-most:low')

state.time.hw <- data.frame(state.hw.hrs, state.time.hw.hrs, state.time.hw.hrs.c1, state.time.hw.hrs.c2,
                              state.time.hw.hrs.c3, state.time.hw.hrs.c4,state.time.hw.hrs.c5,
                              state.time.hw.hrs.c6, state.time.hw.hrs.c7)

write.xlsx(state.time.hw, "results/UKHLS/ukhls_hw_statedurs.xlsx")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transition matrix
# (Haven't yet output bc not quite sure how to do nicely)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Family
seqtrate(seq.fam) # transition rates

# Work
seqtrate(seq.work.ow) # transition rates

# Housework
seqtrate(seq.hw.hrs.alt) # transition rates

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at relationship transitions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Marriage - gah, this is hard bc will have to do for all #s of kids?
data$time_m0 <- seqfpos(seq.fam, state="MARc0")
data$time_m1 <- seqfpos(seq.fam, state="MARc1")
data$time_m2 <- seqfpos(seq.fam, state="MARc2")
data$time_m3 <- seqfpos(seq.fam, state="MARc3")

data$time_marriage <- with(data, pmin(time_m0, time_m1, time_m2, time_m3, na.rm = TRUE)) 

# Childbearing
data$time_child.mar <- seqfpos(seq.fam, state="MARc1")
data$time_child.coh <- seqfpos(seq.fam, state="COHc1")

data$time_child <- with(data, pmin(time_child.mar, time_child.coh, na.rm = TRUE)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore valence-based metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

######## Considering equality "positive"

## Work
posindic.work<-seqindic(seq.work.ow, indic=c("ppos", "nvolat", "integr"),
                        ipos.args=list(pos.states=c('dualFT','dualFT-hisOW', 'dualFT-herOW', 'dualOW')))

# state.work.ow <- c('MBW','1.5MBW','dualFT','dualFT-hisOW', 'dualFT-herOW', 'dualOW','FBW','UnderWK')


data$work_ppos <- posindic.work$Ppos
data$work_nvolat <- posindic.work$Nvolat
data$work_integr <- posindic.work$Integr

## Housework
posindic.hw<-seqindic(seq.hw.hrs.alt, indic=c("ppos", "nvolat", "integr"),
                      ipos.args=list(pos.states=c('equal:high', 'equal:low')))

#state.hw.hrs <- c('W-all:high', 'W-all:low', 'W-most:high', 'W-most:med', 'W-most:low',
#                  'equal:high', 'equal:low', 'M-most:high', 'M-most:low')

data$hw_ppos <- posindic.hw$Ppos
data$hw_nvolat <- posindic.hw$Nvolat
data$hw_integr <- posindic.hw$Integr

######## Attemption degradation - seems cool because quantifies movement between "good" and "bad" states
######## so again thinking egal / specialized

## Work

# go from most positive to least positive
work.state.order=c('dualFT','dualOW','dualFT-hisOW','1.5MBW','MBW')

data$work_degrad <- seqidegrad(seq.work.ow, 
                         state.order=work.state.order,
                         penalized="NEG")

## Housework

# go from most positive to least positive
hw.state.order=c('equal:low','equal:high','W-most:low','W-most:med','W-most:high','W-all:low','W-all:high')

data$hw_degrad <- seqidegrad(seq.hw.hrs.alt, 
                               state.order=hw.state.order,
                               penalized="NEG")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export these new metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# so, this just makes a smaller table with these columns
domain_table <- subset(data,
                    select = c(mc7.factor, time_marriage, time_child, time_child.mar,
                               time_child.coh, work_ppos, work_nvolat, work_integr, work_degrad,
                               hw_ppos, hw_nvolat, hw_integr, hw_degrad)
)


# oh, this is just a new column
domain_table$clusters <- as.character(domain_table$mc7.factor)

sumtable(domain_table, digits = 4, numformat = NA, group = 'clusters', group.test = TRUE, 
         out='csv', file='results/UKHLS/cluster_domain_info.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other metrics not using for now
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#seqstatf(seq.fam) # frequency by state (will do in stata)
#seqstatd(seq.fam) # frequency by duration


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.image("created data/ukhls/UKHLS_cluster_descriptives.RData")

