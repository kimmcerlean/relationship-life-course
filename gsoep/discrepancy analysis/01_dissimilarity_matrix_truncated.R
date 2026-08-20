# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dissimilarity matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First set costs of sm to 0 for missing
fam.miss.cost <- seqcost(seq.fam, method="CONSTANT", 
                         miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

work.miss.cost <- seqcost(seq.work.ow, method="CONSTANT", 
                          miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

hw.miss.cost <- seqcost(seq.hw.hrs, method="CONSTANT", 
                        miss.cost=0, with.missing=TRUE, miss.cost.fixed=TRUE)

# Then make indel costs very high
fam.miss.indel<- rep(1,ncol(fam.miss.cost$sm))
fam.miss.indel[length(fam.miss.indel)] <- 99999
fam.miss.indel

work.miss.indel<- rep(1,ncol(work.miss.cost$sm))
work.miss.indel[length(work.miss.indel)] <- 99999
work.miss.indel

hw.miss.indel<- rep(1,ncol(hw.miss.cost$sm))
hw.miss.indel[length(hw.miss.indel)] <- 99999
hw.miss.indel

# Now use these costs to create NON-normalized matrices
dist.work.om <- seqdist(seq.work.ow, method="OM", indel=work.miss.indel, 
                        sm= work.miss.cost$sm, with.missing=TRUE)

dist.hw.om <- seqdist(seq.hw.hrs, method="OM", indel=hw.miss.indel, 
                      sm= hw.miss.cost$sm, with.missing=TRUE)

dist.fam.om <- seqdist(seq.fam, method="OM", indel=fam.miss.indel, 
                       sm= fam.miss.cost$sm, with.missing=TRUE)

# Then create matrices of shortest length 
fam.min.len <- matrix(NA,ncol=length(seq.len.fam),nrow=length(seq.len.fam))
for (i in 1:length(seq.len.fam)){
  for (j in 1:length(seq.len.fam)){
    fam.min.len[i,j] <- min(c(seq.len.fam[i],seq.len.fam[j]))
  }
}

work.min.len <- matrix(NA,ncol=length(seq.len.work),nrow=length(seq.len.work))
for (i in 1:length(seq.len.work)){
  for (j in 1:length(seq.len.work)){
    work.min.len[i,j] <- min(c(seq.len.work[i],seq.len.work[j]))
  }
}

hw.min.len <- matrix(NA,ncol=length(seq.len.hw),nrow=length(seq.len.hw))
for (i in 1:length(seq.len.hw)){
  for (j in 1:length(seq.len.hw)){
    hw.min.len[i,j] <- min(c(seq.len.hw[i],seq.len.hw[j]))
  }
}

# Then normalize based on that length
dist.fam.min<-dist.fam.om / fam.min.len
dist.work.min<-dist.work.om / work.min.len
dist.hw.min<-dist.hw.om / hw.min.len

# Need MC distance object
mcsa<-seqMD(channels=list(seq.work.ow, seq.hw.hrs, seq.fam),
            with.missing=TRUE,
            what="MDseq") ##, right=NA)


mcdist.det.om <- seqdistmc(channels=list(seq.work.ow, seq.hw.hrs, seq.fam), ## Seq states NOT om matrix
                           method="OM", 
                           indel=list(work.miss.indel,hw.miss.indel, fam.miss.indel),
                           sm=list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                           with.missing=TRUE) 

## Divide by length matrix
mcdist.det.min <- mcdist.det.om / fam.min.len

# Save locally to make easier to use later
save.image("G:/Other computers/My Laptop/Documents/Research Projects/Relationship Life Course (with LP)/discrepancy analysis exploration/gsoep-setupsequence-truncated.RData")
