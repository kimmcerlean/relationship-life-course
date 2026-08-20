# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dissimilarity matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist.work.ow.om <- seqdist(seq.work.ow, method="OM", indel=1, sm= "CONSTANT")
dist.hw.hrs.om <- seqdist(seq.hw.hrs, method="OM", indel=1, sm= "CONSTANT")
dist.fam.om <- seqdist(seq.fam, method="OM", indel=1, sm= "CONSTANT")

# Need MC distance object
mcsa<-seqMD(channels=list(seq.work.ow, seq.hw.hrs, seq.fam),
            what="MDseq") ##, right=NA)


mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                           method="OM", indel=1, sm="CONSTANT") 

# Save locally to make easier to use later
save.image("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/psid-setupsequence-complete.RData")
