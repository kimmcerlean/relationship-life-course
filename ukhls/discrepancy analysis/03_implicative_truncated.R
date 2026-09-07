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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load and install packages for whomever is running the script
## the server doesn't let you install packages
## the server doesn't have ggseqplot for now (package incompatibility issue)

if (Sys.getenv(c("HOME" )) == "/home/lpessin") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}

if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
                         "colorspace","ggplot2","ggpubr", "ggseqplot", "dplyr", "vtable",
                         "patchwork", "cluster", "WeightedCluster","dendextend","seqHMM","haven",
                         "labelled", "readxl", "openxlsx","tidyverse","gridExtra","foreign","pdftools")
  lapply(required_packages, require, character.only = TRUE)
}


if (Sys.getenv(c("USERNAME")) == "mcerl") {
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
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
  required_packages <- c("TraMineR", "TraMineRextras","RColorBrewer", "paletteer", 
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
# Import data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("educational differences/R data/ukhls-setupsequence-truncated.RData")

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
implic.fam.nomiss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = FALSE, 
                               weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.fam.miss <- seqimplic(seq.fam, group=data$couple_educ_type, with.missing = TRUE,  
                             weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = FALSE, 
                                weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.miss <- seqimplic(seq.work.ow, group=data$couple_educ_type, with.missing = TRUE,  
                              weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = FALSE,  
                              weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.miss <- seqimplic(seq.hw.hrs, group=data$couple_educ_type, with.missing = TRUE,  
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

# Want to combine all plots - but this puts each on separate page

pdf("educational differences/results/ukhls/UKHLS_implicative_statistic_paginated.pdf")

layout.fig1 <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE),
                      heights = c(1,1,1))
layout.show(layout.fig1)

# par(mar = c(5, 5, 3, 3))
par(mar = c(4, 4, 3, 1))

# Family channel
plot(implic.fam.miss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

# Paid Work Channel: With Overwork
plot(implic.work.nomiss, lwd=2, conf.level=c(0.95, 0.99), xtlab = x_lab)

# Housework Channel: Hours with Group-specific thresholds
plot(implic.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)


dev.off()

# Here is how you get combined to 1 page
objs <- list(
  Family = implic.fam.miss,
  `Paid work` = implic.work.nomiss,
  Housework = implic.hw.nomiss
)

levs <- implic.fam.miss$levels

pdf("educational differences/results/ukhls/UKHLS_implicative_statistic.pdf",
    width = 20, height = 10)

par(mfrow = c(3, 4),
    mar = c(3, 3, 3, 1),
    oma = c(1, 1, 1, 1))

for (cat in names(objs)) {
  obj <- objs[[cat]]
  
  for (g in seq_along(levs)) {
    
    y <- -obj$indices[g, , ]
    y[y < 0] <- NA
    
    matplot(t(y),
            type = "l",
            lty = 1,
            lwd = 2,
            col = obj$cpal,
            ylim = c(0, max(-obj$indices, na.rm = TRUE)),
            xaxt = "n",
            xlab = "",
            ylab = "Implication",
            main = paste(cat, "\n", levs[g]))
    
    axis(1, at = seq_along(x_lab), labels = x_lab, cex.axis = 0.7)
    
    h <- qnorm(0.95)
    abline(h = h, lty = 3, col = "grey12")
    
    text(x = length(x_lab) - 0.5,
         y = h + 0.4,
         labels = "Conf. 0.95",
         cex = 0.7,
         col = "grey30")
  }
}

dev.off()

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do I want to examine by parental status?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reminder:
#subset.cf <- data$parent_info %in% c("Always CF")
#subset.trans <- data$parent_info %in% c("Become Parent")
#subset.par <- data$parent_info %in% c("Always Parent")

## Create implicative objects by group:
implic.fam.nomiss.cf <- seqimplic(seq.fam[subset.cf, ], group=data$couple_educ_type[subset.cf], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                  weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss.cf <- seqimplic(seq.work.ow[subset.cf, ], group=data$couple_educ_type[subset.cf], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                   weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss.cf <- seqimplic(seq.hw.hrs[subset.cf, ], group=data$couple_educ_type[subset.cf], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                 weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.fam.nomiss.trans <- seqimplic(seq.fam[subset.trans, ], group=data$couple_educ_type[subset.trans], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                     weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss.trans <- seqimplic(seq.work.ow[subset.trans, ], group=data$couple_educ_type[subset.trans], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                      weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss.trans <- seqimplic(seq.hw.hrs[subset.trans, ], group=data$couple_educ_type[subset.trans], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                    weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.fam.nomiss.par <- seqimplic(seq.fam[subset.par, ], group=data$couple_educ_type[subset.par], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                   weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.work.nomiss.par <- seqimplic(seq.work.ow[subset.par, ], group=data$couple_educ_type[subset.par], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                    weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

implic.hw.nomiss.par <- seqimplic(seq.hw.hrs[subset.par, ], group=data$couple_educ_type[subset.par], with.missing = FALSE,  ## can i ADJUST this with missing to help?
                                  weighted = FALSE, na.rm = TRUE) ## na.rm is about missing on GROUP variables

## Test compare plots
plot(implic.fam.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.fam.nomiss.cf, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.fam.nomiss.trans, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.fam.nomiss.par, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.work.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.work.nomiss.cf, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.work.nomiss.trans, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.work.nomiss.par, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

plot(implic.hw.nomiss, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.hw.nomiss.cf, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.hw.nomiss.trans, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)
plot(implic.hw.nomiss.par, lwd=3, conf.level=c(0.95, 0.99), xtlab = x_lab)

# ~~~~~~~~~~~~~~~~~~~~~~~~
## Export plots for each
# ~~~~~~~~~~~~~~~~~~~~~~~~

## Childfree
objs.cf <- list(
  Family = implic.fam.nomiss.cf,
  `Paid work` = implic.work.nomiss.cf,
  Housework = implic.hw.nomiss.cf
)

levs.cf <- implic.fam.nomiss.cf$levels

pdf("educational differences/results/ukhls/UKHLS_implicative_statistic_Childfree.pdf",
    width = 20, height = 10)

par(mfrow = c(3, 4),
    mar = c(3, 3, 3, 1),
    oma = c(1, 1, 1, 1))

for (cat in names(objs.cf)) {
  obj.cf <- objs.cf[[cat]]
  
  for (g in seq_along(levs.cf)) {
    
    y <- -obj.cf$indices[g, , ]
    y[y < 0] <- NA
    
    matplot(t(y),
            type = "l",
            lty = 1,
            lwd = 2,
            col = obj.cf$cpal,
            ylim = c(0, max(-obj.cf$indices, na.rm = TRUE)),
            xaxt = "n",
            xlab = "",
            ylab = "Implication",
            main = paste(cat, "\n", levs.cf[g]))
    
    axis(1, at = seq_along(x_lab), labels = x_lab, cex.axis = 0.7)
    
    h <- qnorm(0.95)
    abline(h = h, lty = 3, col = "grey12")
    
    text(x = length(x_lab) - 0.5,
         y = h + 0.4,
         labels = "Conf. 0.95",
         cex = 0.7,
         col = "grey30")
  }
}

dev.off()


## Become Parents
objs.trans <- list(
  Family = implic.fam.nomiss.trans,
  `Paid work` = implic.work.nomiss.trans,
  Housework = implic.hw.nomiss.trans
)

levs.trans <- implic.fam.nomiss.trans$levels

pdf("educational differences/results/ukhls/UKHLS_implicative_statistic_BecomeParents.pdf",
    width = 20, height = 10)

par(mfrow = c(3, 4),
    mar = c(3, 3, 3, 1),
    oma = c(1, 1, 1, 1))

for (cat in names(objs.trans)) {
  obj.trans <- objs.trans[[cat]]
  
  for (g in seq_along(levs.trans)) {
    
    y <- -obj.trans$indices[g, , ]
    y[y < 0] <- NA
    
    matplot(t(y),
            type = "l",
            lty = 1,
            lwd = 2,
            col = obj.trans$cpal,
            ylim = c(0, max(-obj.trans$indices, na.rm = TRUE)),
            xaxt = "n",
            xlab = "",
            ylab = "Implication",
            main = paste(cat, "\n", levs.trans[g]))
    
    axis(1, at = seq_along(x_lab), labels = x_lab, cex.axis = 0.7)
    
    h <- qnorm(0.95)
    abline(h = h, lty = 3, col = "grey12")
    
    text(x = length(x_lab) - 0.5,
         y = h + 0.4,
         labels = "Conf. 0.95",
         cex = 0.7,
         col = "grey30")
  }
}

dev.off()


## Always Parents
objs.par <- list(
  Family = implic.fam.nomiss.par,
  `Paid work` = implic.work.nomiss.par,
  Housework = implic.hw.nomiss.par
)

levs.par <- implic.fam.nomiss.par$levels

pdf("educational differences/results/ukhls/UKHLS_implicative_statistic_AlwaysParents.pdf",
    width = 20, height = 10)

par(mfrow = c(3, 4),
    mar = c(3, 3, 3, 1),
    oma = c(1, 1, 1, 1))

for (cat in names(objs.par)) {
  obj.par <- objs.par[[cat]]
  
  for (g in seq_along(levs.par)) {
    
    y <- -obj.par$indices[g, , ]
    y[y < 0] <- NA
    
    matplot(t(y),
            type = "l",
            lty = 1,
            lwd = 2,
            col = obj.par$cpal,
            ylim = c(0, max(-obj.par$indices, na.rm = TRUE)),
            xaxt = "n",
            xlab = "",
            ylab = "Implication",
            main = paste(cat, "\n", levs.par[g]))
    
    axis(1, at = seq_along(x_lab), labels = x_lab, cex.axis = 0.7)
    
    h <- qnorm(0.95)
    abline(h = h, lty = 3, col = "grey12")
    
    text(x = length(x_lab) - 0.5,
         y = h + 0.4,
         labels = "Conf. 0.95",
         cex = 0.7,
         col = "grey30")
  }
}

dev.off()

