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
# Think it'd be cool to also look at sequence METRICS by group
# (e.g. volatility, those integrative potential, etc.)
# Think this would add value to above
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Metrics by individual channel (these don't have the valence ones yet)
seqindic.fam<-seqindic(seq.fam, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                        "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.work<-seqindic(seq.work.ow, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                             "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

seqindic.hw<-seqindic(seq.hw.hrs, indic=c("lgth", "nonm", "dlgth", "visited", "recu", "trans", "transp", "meand", "meand2",
                                          "dustd", "dustd2","entr", "volat", "cplx", "turb", "turbn", "turb2", "turb2n"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add to data frame and create object to export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(seqindic.fam)  <- paste0("fam_",  tolower(names(seqindic.fam)))
names(seqindic.work) <- paste0("work_", tolower(names(seqindic.work)))
names(seqindic.hw)   <- paste0("hw_",   tolower(names(seqindic.hw)))

data <- bind_cols(data,
                   seqindic.fam,
                   seqindic.work,
                   seqindic.hw)

# so, this just makes a smaller table with these columns
desctable <- data %>%
select(couple_educ_type,
         starts_with("fam_"),
         starts_with("work_"),
         starts_with("hw_"))

# oh, this is just a new column
# desctable$educ <- as.character(desctable$couple_educ_type)

sumtable(desctable, digits = 4, fixed.digits = TRUE, numformat = NA, group = 'couple_educ_type', group.test = TRUE, 
         out='csv', file="G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid_educ_desctable_truncated.csv")

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



    ## something else I want to explore, you can reweight the integrative potential by recency
    ## I wonder if something like *that* can account better for people consistently in a state v. transition into a state and stay
    data$w_integr_dual_wgt <- seqipos(seq.work.ow, dss=NULL, pos.states=c('dualFT','dualFT-anyOW'), 
                                     neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

    data$w_integr_mbw_wgt <- seqipos(seq.work.ow, dss=NULL, pos.states=c('MBW','1.5MBW'), 
                                      neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

## Housework
posindic.hw<-seqindic(seq.hw.hrs, indic=c("ppos", "nvolat", "integr"),
                      ipos.args=list(pos.states=c('equal:high', 'equal:low')))

#state.hw.hrs <- c('W-most:high', 'W-most:low',
#                  'equal:high', 'equal:low', 'M-most:all')

data$hw_ppos <- posindic.hw$Ppos
data$hw_nvolat <- posindic.hw$Nvolat
data$hw_integr <- posindic.hw$Integr

# want to do integral potential of each category
# Equal
data$hw_integr_eq <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('equal:high', 'equal:low'), 
                             neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Her Most or All
data$hw_integr_her <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('W-most:high','W-most:low'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

# Him Most or All
data$hw_integr_him <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('M-most:all'), 
                              neg.states=NULL, index="integr", pow=1, w=.5, with.missing=FALSE)

    ## Try reweighting here
    data$hw_integr_eq_wgt <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('equal:high', 'equal:low'), 
                             neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)

    data$hw_integr_her_wgt <- seqipos(seq.hw.hrs, dss=NULL, pos.states=c('W-most:high','W-most:low'), 
                                  neg.states=NULL, index="integr", pow=3, w=.5, with.missing=FALSE)
  
    educ_integr_test <- subset(data,
                               select = c(couple_educ_type,
                                          work_integr_dual, work_integr_mbw,
                                          w_integr_dual_wgt, w_integr_mbw_wgt,
                                          hw_integr_eq, hw_integr_her,
                                          hw_integr_eq_wgt, hw_integr_her_wgt)
    )
    
    sumtable(educ_integr_test, group = 'couple_educ_type') ## Okay so this is VERY interesting, because it actually narrows the education differences between neither / both
    # bc more specialized at end, it actually makes it HIGHER for both college (but not true of dual FT because less educated have OTHER arrangements like underwork)

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

data$hw_degrad_neg <- seqidegrad(seq.hw.hrs, 
                                 state.order=hw.state.order,
                                 penalized="NEG")

data$hw_degrad_pos <- seqidegrad(seq.hw.hrs, 
                                 state.order=hw.state.order,
                                 penalized="POS")

data$hw_degrad_both <- seqidegrad(seq.hw.hrs, 
                                  state.order=hw.state.order,
                                  penalized="BOTH")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export metrics at overall level jic
# (should match original paper?)
# This is actually better list and gets all together?
# Going to just use this for by education also
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

educ_metric_table <- subset(data,
                       select = c(couple_educ_type,
                                  fam_lgth, fam_nonm, fam_dlgth, fam_visited, 
                                  fam_recu, fam_trans, fam_transp, fam_entr, 
                                  fam_meand, fam_volat, fam_cplx, fam_turb2n, 
                                  work_lgth, work_nonm, work_dlgth, work_visited, 
                                  work_recu, work_trans, work_transp, work_entr, 
                                  work_meand, work_volat, work_cplx, work_turb2n, 
                                  hw_lgth, hw_nonm, hw_dlgth,
                                  hw_visited, hw_recu, hw_trans, hw_transp,
                                  hw_entr, hw_meand, hw_volat, hw_cplx, hw_turb2n,
                                  time_marriage, time_child, time_child.mar,
                                  time_child.coh, fam_integr_marr, fam_integr_coh,
                                  work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
                                  work_integr_fbw, work_integr_under, work_degrad_neg,
                                  work_degrad_pos, work_degrad_both, hw_ppos,
                                  hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
                                  hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
)

sumtable(educ_metric_table, digits = 4, numformat = NA, group = 'couple_educ_type', group.test = TRUE, 
         out='csv', file="G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid_educ_metrics_truncated.csv")


#overall_metrics <- subset(data,
#                          select = c(fam_lgth, fam_lgth_nomiss, fam_spells, fam_states, 
#                                     fam_recu, fam_trans, fam_transp, fam_entr, 
#                                     fam_meand, fam_volat, fam_cplx, fam_turb2n, 
#                                     work_lgth, work_lgth_nomiss, work_spells, work_states, 
#                                     work_recu, work_trans, work_transp, work_entr, 
#                                     work_meand, work_volat, work_cplx, work_turb2n, 
#                                     hw_lgth, hw_lgth_nomiss, hw_spells,
#                                     hw_states, hw_recu, hw_trans, hw_transp,
#                                     hw_entr, hw_meand, hw_volat, hw_cplx, hw_turb2n,
#                                     time_marriage, time_child, time_child.mar,
#                                     time_child.coh, fam_integr_marr, fam_integr_coh,
#                                     work_ppos, work_nvolat, work_integr_dual, work_integr_mbw,
#                                     work_integr_fbw, work_integr_under, work_degrad_neg,
#                                     work_degrad_pos, work_degrad_both, hw_ppos,
#                                     hw_nvolat, hw_integr_eq, hw_integr_her, hw_integr_him,
#                                     hw_degrad_neg, hw_degrad_pos, hw_degrad_both)
#)


#sumtable(overall_metrics, digits = 4, fixed.digits = TRUE, numformat = NA, 
#         out='csv', file='results/PSID/tables/PSID_Overall_Sequence_Metrics.csv')

