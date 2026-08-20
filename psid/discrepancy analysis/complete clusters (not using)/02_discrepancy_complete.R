# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attempt discrepancy analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid-setupsequence-complete.RData")

table(data$couple_educ_type) 

dissassoc(mcdist.det.om, group = data$couple_educ_type)

# higher discrepancy = more internally diverse trajectories
# lower discrepancy = more homogeneous trajectories
# so makes sense college = more homogenous (but quite small)
# Barlett and Levene more formally test if groups differ on their internal heterogeneity (so they do) - but none of these are pairwise
# concern is that all of this - R2 is quite low.
# I wonder if HERE, doing NON-MCSA is better because with like what 6x8x5 states, are they going to be different always (bc could be like male BW, HWA, male BW HW B?)
# (versus within one channel - easier to see similariies or differs
# splitting might ALSO make it easier to say like -- okay FAMILY patterns same, but gendered DoL is NOT (or vice versa). or even like paid work
# Wait this is prob MORE interesting, and I already have the diss matrices anyway?
# R-squared is literally exp / total

dissassoc(dist.fam.om, group = data$couple_educ_type)
dissassoc(dist.work.ow.om, group = data$couple_educ_type)
dissassoc(dist.hw.hrs.om, group = data$couple_educ_type)

# Index plots by group
seqIplot(seq.fam, group = data$couple_educ_type)
seqIplot(seq.work.ow, group = data$couple_educ_type)
seqIplot(seq.hw.hrs, group = data$couple_educ_type)

# adding sort end brings missing back in? okay no all have missing just easier / harder to see if some views, I am dumb
seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.end",  with.missing = FALSE)

seqIplot(seq.fam, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.work.ow, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)
seqIplot(seq.hw.hrs, group = data$couple_educ_type, sortv = "from.start",  with.missing = FALSE)

# State distro by group
seqdplot(seq.fam, group = data$couple_educ_type)
seqdplot(seq.work.ow, group = data$couple_educ_type)
seqdplot(seq.hw.hrs, group = data$couple_educ_type)

# representative seq by group
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.om, criterion = "dist")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.om, criterion = "density")
seqrplot(seq.fam, group = data$couple_educ_type, diss=dist.fam.om, criterion = "freq")

seqrplot(seq.work.ow, group = data$couple_educ_type, diss=dist.work.ow.om)
seqrplot(seq.hw.hrs, group = data$couple_educ_type, diss=dist.hw.hrs.om)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Oh yeah do I want to try the moving window thing?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get code from Studer pape. this might make most sense with the complete sequences?
# esp bc the input is distance calculation and such and I worry this is harder to do when some sequences no longer compared to each other?
# I mean I guess I can still get the cost arguments from before?
# although I think the inputs have changed since the article came out (see example):
# https://traminer.unige.ch/doc/seqdiff.html

educ.diff.fam <- seqdiff(seq.fam, data$couple_educ_type)

plot(educ.diff.fam, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.fam, stat="discrepancy")

educ.diff.work <- seqdiff(seq.work.ow, data$couple_educ_type)
plot(educ.diff.work, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.work, stat="discrepancy")

educ.diff.hw <- seqdiff(seq.hw.hrs, data$couple_educ_type)
plot(educ.diff.hw, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.hw, stat="discrepancy")

# these are basically the same, just discrepancy value is diff
educ.diff.fam2 <- seqdiff(seq.fam, data$couple_educ_type,
                        seqdist.args = list(method = "OM", indel=1, sm= "CONSTANT"))
plot(educ.diff.fam2, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.fam2, stat="discrepancy")

# what about window? think above maybe does window of length 1, here, it reduces what can be shown
educ.diff.fam.w <- seqdiff(seq.fam, data$couple_educ_type,cmprange=c(-2,2))
plot(educ.diff.fam.w, stat=c("Pseudo R2", "Levene"))
plot(educ.diff.fam.w, stat="discrepancy")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Think it'd be cool to also look at sequence METRICS by group
# (e.g. volatility, those integrative potential, etc.)
# Think this would add value to above
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~