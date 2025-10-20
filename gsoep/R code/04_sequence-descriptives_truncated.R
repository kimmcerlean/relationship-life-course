# ---------------------------------------------------------------------
#    Program: sequence-descriptives_truncated
#    Author: Kim McErlean & Lea Pessin 
#    Date: January 2025
#    Modified: August 28 2025
#    Goal: Create various metrics to describe sequences, at a total 
#         level and across cluster solutions (select metrics)
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
# Load cluster information created in step 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("created data/gsoep/typology-comparison-truncated-prep.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster information to source data ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We are using 9 clusters for now

# Cut tree (this happened in step 2)
# mc9 <- mcdist.om.pam.ward$clustering$cluster9 # these are sub"folders" in ward output

# add cluster membership indicator 
data <- data |>
  mutate(cluster = mc9,
         id2 = row_number())

# Obtain relative frequencies and convert to percentages

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
# this was created in step 1 already

# mcsa<-seqMD(channels=list(seq.work.ow, seq.hw.hrs.weekly, seq.fam),
#            with.missing=TRUE,
#            what="MDseq") ##, right=NA)

# turbulence
# seqST(mcsa) # this is for each individual, so we would want to summarize by cluster?
# seqlength(mcsa, with.missing = FALSE) # Is it problematic that these all have length 10?
# I guess I am not actually describing the MCSA?
# I think for all of these, we want missing to be IGNORED, false is usually the default anyway

# seqtransn(mcsa)
# seqtransn(seq.fam)
# seqtransn(seq.fam, with.missing=TRUE) # yes so we DEF don't want this, because then everyone has at least one transition - to missing
# seqtransn(seq.hw.hrs.weekly)
# seqtransn(seq.work.ow)

# These are metrics BY individual
seqindic.fam<-seqindic(seq.fam, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                         "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.work<-seqindic(seq.work.ow, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                              "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.hw<-seqindic(seq.hw.hrs.weekly, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                        "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add to data frame
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Family
data$fam_lgth <- seqindic.fam$Lgth
data$fam_lgth_nomiss <- seqindic.fam$NonM
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
data$work_lgth_nomiss <- seqindic.work$NonM
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
data$hw_lgth <- seqindic.hw$Lgth
data$hw_lgth_nomiss <- seqindic.hw$NonM
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
# mc9.fam1.seq
# mc9.work.ow1.seq 
# mc9.hw.hrs1.seq


# so, this just makes a smaller table with these columns
desctable <- subset(data,
                    select = c(mc9.factor, fam_lgth, fam_lgth_nomiss, fam_spells,
                               fam_states, fam_recu, fam_trans, fam_transp, fam_entr, 
                               fam_meand, fam_dustd, fam_meand2, fam_dustd2,
                               fam_volat, fam_cplx, fam_turb, fam_turbn,
                               fam_turb2, fam_turb2n, work_lgth, work_lgth_nomiss, 
                               work_spells, work_states, work_recu, work_trans, 
                               work_transp, work_entr, work_meand, work_dustd, 
                               work_meand2, work_dustd2, work_volat, work_cplx, 
                               work_turb, work_turbn, work_turb2, work_turb2n, 
                               hw_lgth, hw_lgth_nomiss, hw_spells,
                               hw_states, hw_recu, hw_trans, hw_transp,
                               hw_entr, hw_meand, hw_dustd, hw_meand2,
                               hw_dustd2, hw_volat, hw_cplx, hw_turb,
                               hw_turbn, hw_turb2, hw_turb2n)
)

# oh, this is just a new column
desctable$clusters <- as.character(desctable$mc9.factor)

sumtable(desctable, digits = 4, fixed.digits = TRUE, numformat = NA, group = 'clusters', group.test = TRUE, 
         out='csv', file='results/GSOEP/tables/cluster_desctable_truncated.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean time in a state
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Family
seqistatd(seq.fam) # this is individual level mean time in a state. Do these vary when aggregated?

state.time.fam <- seqmeant(seq.fam) # this is mean time in a state aggregated?
state.time.fam.c1 <- seqmeant(mc9.fam1.seq)
state.time.fam.c2 <- seqmeant(mc9.fam2.seq)
state.time.fam.c3 <- seqmeant(mc9.fam3.seq)
state.time.fam.c4 <- seqmeant(mc9.fam4.seq)
state.time.fam.c5 <- seqmeant(mc9.fam5.seq)
state.time.fam.c6 <- seqmeant(mc9.fam6.seq)
state.time.fam.c7 <- seqmeant(mc9.fam7.seq)
state.time.fam.c8 <- seqmeant(mc9.fam8.seq)
state.time.fam.c9 <- seqmeant(mc9.fam9.seq)

state.fam <- c('MARc0','MARc1','MARc2','MARc3','COHc0','COHc1','COHc2','COHc3')

state.time.family <- data.frame(state.fam, state.time.fam, state.time.fam.c1, state.time.fam.c2,
                                state.time.fam.c3, state.time.fam.c4,state.time.fam.c5,
                                state.time.fam.c6, state.time.fam.c7, state.time.fam.c8,
                                state.time.fam.c9)

write.xlsx(state.time.family, "results/GSOEP/tables/gsoep_family_statedurs.xlsx")

# Work
state.time.work.ow <- seqmeant(seq.work.ow)
state.time.work.ow.c1 <- seqmeant(mc9.work.ow1.seq)
state.time.work.ow.c2 <- seqmeant(mc9.work.ow2.seq)
state.time.work.ow.c3 <- seqmeant(mc9.work.ow3.seq)
state.time.work.ow.c4 <- seqmeant(mc9.work.ow4.seq)
state.time.work.ow.c5 <- seqmeant(mc9.work.ow5.seq)
state.time.work.ow.c6 <- seqmeant(mc9.work.ow6.seq)
state.time.work.ow.c7 <- seqmeant(mc9.work.ow7.seq)
state.time.work.ow.c8 <- seqmeant(mc9.work.ow8.seq)
state.time.work.ow.c9 <- seqmeant(mc9.work.ow9.seq)

state.work.ow <- c('MBW','1.5MBW','dualFT','dualFT-anyOW', 'FBW','UnderWK')

state.time.work <- data.frame(state.work.ow, state.time.work.ow, state.time.work.ow.c1, state.time.work.ow.c2,
                                state.time.work.ow.c3, state.time.work.ow.c4,state.time.work.ow.c5,
                                state.time.work.ow.c6, state.time.work.ow.c7, state.time.work.ow.c8,
                              state.time.work.ow.c9)

write.xlsx(state.time.work, "results/GSOEP/tables/gsoep_work_statedurs.xlsx")


# Housework
state.time.hw.hrs <- seqmeant(seq.hw.hrs.weekly)
state.time.hw.hrs.c1 <- seqmeant(mc9.hw.hrs1.seq)
state.time.hw.hrs.c2 <- seqmeant(mc9.hw.hrs2.seq)
state.time.hw.hrs.c3 <- seqmeant(mc9.hw.hrs3.seq)
state.time.hw.hrs.c4 <- seqmeant(mc9.hw.hrs4.seq)
state.time.hw.hrs.c5 <- seqmeant(mc9.hw.hrs5.seq)
state.time.hw.hrs.c6 <- seqmeant(mc9.hw.hrs6.seq)
state.time.hw.hrs.c7 <- seqmeant(mc9.hw.hrs7.seq)
state.time.hw.hrs.c8 <- seqmeant(mc9.hw.hrs8.seq)
state.time.hw.hrs.c9 <- seqmeant(mc9.hw.hrs9.seq)

state.hw.hrs <- c('W-most:high', 'W-most:low',
                  'equal:high', 'equal:low', 'M-most:all')

state.time.hw <- data.frame(state.hw.hrs, state.time.hw.hrs, state.time.hw.hrs.c1, state.time.hw.hrs.c2,
                              state.time.hw.hrs.c3, state.time.hw.hrs.c4,state.time.hw.hrs.c5,
                              state.time.hw.hrs.c6, state.time.hw.hrs.c7,state.time.hw.hrs.c8,
                            state.time.hw.hrs.c9)

write.xlsx(state.time.hw, "results/GSOEP/tables/gsoep_hw_statedurs.xlsx")


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

# want to do integral potential of each category
# Marriage
data$fam_integr_marr <- seqipos(seq.fam, dss=NULL, pos.states=c('MARc0','MARc1','MARc2','MARc3'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Cohabitation
data$fam_integr_coh <- seqipos(seq.fam, dss=NULL, pos.states=c('COHc0','COHc1','COHc2','COHc3'), 
                               neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore valence-based metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

######## Considering equality "positive"

## Work
posindic.work<-seqindic(seq.work.ow, indic=c("ppos", "nvolat", "integr"),
                        ipos.args=list(pos.states=c('dualFT','dualFT-anyOW')))

data$work_ppos <- posindic.work$Ppos
data$work_nvolat <- posindic.work$Nvolat
data$work_integr <- posindic.work$Integr

# want to do integral potential of each category
# Dual FT
data$work_integr_dual <- seqipos(seq.work.ow, dss=NULL, pos.states=c('dualFT','dualFT-anyOW'), 
                                 neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Male BW
data$work_integr_mbw <- seqipos(seq.work.ow, dss=NULL, pos.states=c('MBW','1.5MBW'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Female BW
data$work_integr_fbw <- seqipos(seq.work.ow, dss=NULL, pos.states=c('FBW'), 
                                neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Underwork
data$work_integr_under <- seqipos(seq.work.ow, dss=NULL, pos.states=c('underWK'), 
                                  neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)



## Housework
posindic.hw<-seqindic(seq.hw.hrs.weekly, indic=c("ppos", "nvolat", "integr"),
                      ipos.args=list(pos.states=c('equal:high', 'equal:low')))

#state.hw.hrs <- c('W-most:high', 'W-most:low',
#                  'equal:high', 'equal:low', 'M-most:all')

data$hw_ppos <- posindic.hw$Ppos
data$hw_nvolat <- posindic.hw$Nvolat
data$hw_integr <- posindic.hw$Integr

# want to do integral potential of each category
# Equal
data$hw_integr_eq <- seqipos(seq.hw.hrs.weekly, dss=NULL, pos.states=c('equal:high', 'equal:low'), 
                             neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Her Most or All
data$hw_integr_her <- seqipos(seq.hw.hrs.weekly, dss=NULL, pos.states=c('W-most:high','W-most:low'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Him Most or All
data$hw_integr_him <- seqipos(seq.hw.hrs.weekly, dss=NULL, pos.states=c('M-most:all'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

######## Attemption degradation - seems cool because quantifies movement between "good" and "bad" states
######## so again thinking egal / specialized

## Work

# go from most positive to least positive
work.state.order=c('dualFT','dualFT-anyOW','1.5MBW','MBW')

# I did neg before - but thinking - do I want to either distinguish
# between neg and pos (to show neg happens more?)
# or - what does both do? (does it exacerbate negative?)
# I actually think all are useful for different reasons, so let's keep all
data$work_degrad_neg <- seqidegrad(seq.work.ow, 
                         state.order=work.state.order,
                         penalized="NEG")

data$work_degrad_pos <- seqidegrad(seq.work.ow, 
                               state.order=work.state.order,
                               penalized="POS")

data$work_degrad_both <- seqidegrad(seq.work.ow, 
                                   state.order=work.state.order,
                                   penalized="BOTH")

## Housework

# go from most positive to least positive
hw.state.order=c('equal:low','equal:high','W-most:low','W-most:high')

data$hw_degrad_neg <- seqidegrad(seq.hw.hrs.weekly, 
                               state.order=hw.state.order,
                               penalized="NEG")

data$hw_degrad_pos <- seqidegrad(seq.hw.hrs.weekly, 
                                 state.order=hw.state.order,
                                 penalized="POS")

data$hw_degrad_both <- seqidegrad(seq.hw.hrs.weekly, 
                                 state.order=hw.state.order,
                                 penalized="BOTH")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export these new metrics by cluster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# so, this just makes a smaller table with these columns
domain_table <- subset(data,
                    select = c(mc9.factor, time_marriage, time_child, time_child.mar,
                               time_child.coh, fam_integr_marr, fam_integr_coh,
                               work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
                               work_integr_fbw, work_integr_under, work_degrad_neg,
                               work_degrad_pos, work_degrad_both, hw_ppos,
                               hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
                               hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
)


# oh, this is just a new column
domain_table$clusters <- as.character(domain_table$mc9.factor)

sumtable(domain_table, digits = 4, numformat = NA, group = 'clusters', group.test = TRUE, 
         out='csv', file='results/GSOEP/tables/cluster_domain_info.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export metrics at overall level
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

overall_metrics <- subset(data,
                          select = c(fam_lgth, fam_lgth_nomiss, fam_spells, fam_states, 
                                     fam_recu, fam_trans, fam_transp, fam_entr, 
                                     fam_meand, fam_volat, fam_cplx, fam_turb2n, 
                                     work_lgth, work_lgth_nomiss, work_spells, work_states, 
                                     work_recu, work_trans, work_transp, work_entr, 
                                     work_meand, work_volat, work_cplx, work_turb2n, 
                                     hw_lgth, hw_lgth_nomiss, hw_spells,
                                     hw_states, hw_recu, hw_trans, hw_transp,
                                     hw_entr, hw_meand, hw_volat, hw_cplx, hw_turb2n,
                                     time_marriage, time_child, time_child.mar,
                                     time_child.coh, fam_integr_marr, fam_integr_coh,
                                     work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
                                     work_integr_fbw, work_integr_under, work_degrad_neg,
                                     work_degrad_pos, work_degrad_both, hw_ppos,
                                     hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
                                     hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
)


sumtable(overall_metrics, digits = 4, fixed.digits = TRUE, numformat = NA, 
         out='csv', file='results/GSOEP/tables/GSOEP_Overall_Sequence_Metrics.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.image("created data/gsoep/gsoep_trunc_cluster_descriptives.RData")

