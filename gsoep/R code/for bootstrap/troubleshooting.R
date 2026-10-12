print(WeightedCluster:::seqnull)
print(WeightedCluster:::seqnullspell)

# Two problems:
# States with no transitions 
  #Error in sample.int(length(x), size, replace, prob) : 
  #  NA in probability vector

# Length - not working as intended

################################################################################
# This troubleshoots the states with no transitions BUT that was just
# an artifact of Kim's sampling. Once I went from 500 sequences to the whole
# MI of 1. this got solved. Retaining for reference...

K <- length(comb.alphabet)
c(n_states = K, cells = K^2,
  obs_transitions = sum(ok_obs),
  distinct_obs = length(obs_trans),
  density = length(obs_trans) / K^2)

tr  <- seqtrate(comb.seq, with.missing = FALSE)
rs  <- rowSums(tr, na.rm = TRUE)
bad <- rownames(tr)[rs == 0 | !is.finite(rs)]
length(bad); head(bad, 20)

# are they purely terminal-position artefacts?
last_states <- comb[cbind(seq_len(nrow(comb)), len.m)]
mean(last_states %in% bad)          # share of sequence-endings sitting in a dead-end state
setdiff(bad, unique(last_states))   # dead-ends that aren't terminal -> something else is wrong

Tn <- ncol(comb)
from <- comb[, -Tn][ok_obs]; to <- comb[, -1][ok_obs]
c(self_share = round(mean(from == to), 3),
  zero_successor = sum(!(comb.alphabet %in% unique(from))))


# This surfaces the states with no transitions out of them.
for (m in null_models) {
  r <- try(seqnull(comb.seq, model = m, imp.trans.limit = 0), silent = TRUE)
  cat(m, if (inherits(r, "try-error")) "FAIL" else "ok", "\n")
}

set.seed(seed)
try(seqnull(comb.seq, model = "combined", imp.trans.limit = 0))
traceback()

off <- from != to
bad <- comb.alphabet[!(comb.alphabet %in% unique(from[off]))]
length(bad); head(bad, 20)

sum(comb == "FBW|M-most:all|MARc2", na.rm = TRUE)

seqtrate(comb.seq)

K <- length(comb.alphabet)
n_spell_trans <- sum(off)                  # from your earlier block
c(K = K,
  cells = K^2,
  distinct_spell_trans = length(unique(paste(from[off], to[off]))),
  density = length(unique(paste(from[off], to[off]))) / K^2)


tab <- table(paste(from[off], to[off]))
c(total_changes   = sum(off),
  distinct        = length(tab),
  singleton_share = round(mean(tab == 1), 3),
  mean_count      = round(mean(tab), 2))

set.seed(seed)
for (m in null_models) {
  r <- try(seqnull(comb.seq, model = m, imp.trans.limit = imp_limit), silent = TRUE)
  cat(m, ":", if (inherits(r, "try-error")) "FAIL" else "ok", "\n")
}

#combined : FAIL 
#duration : ok 
#sequencing : ok

################################################################################
# This examines the null missing problem

n1 <- seqnull(comb.seq, model = "combined", imp.trans.limit = -1)
table(seqlength(n1)); summary(len.m)

## Here we can see that the nulls for the observed are longer
n1 <- seqnull(comb.seq, model = "combined", imp.trans.limit = -1)

short <- which(len.m <= 4)[1:3]          # a few heavily censored people

for (i in short) {
  cat("\n--- row", i, " observed length:", len.m[i], "---\n")
  cat("OBS : ", paste(as.matrix(comb.seq)[i, ], collapse = "  "), "\n")
  cat("NULL: ", paste(as.matrix(n1)[i, ],        collapse = "  "), "\n")
}

ch <- split_channels(seqnull(comb.seq, model = "combined", imp.trans.limit = -1))
table(seqlength(seqdef(as.data.frame(ch$work), states = alph.work, right = NA),
                with.missing = FALSE))

# Checks 1 and 2 were on n1, a test object I made outside your code.
# That's not quite enough — the thing that matters is the object seqdistmc() actually receives,
# after split_channels() and seqdef(). So you ran that chain and checked the length again. Still 10.

#
lapply(list(work = work.miss.cost$sm, hw = hw.miss.cost$sm, fam = fam.miss.cost$sm),
       function(s) {
         i <- which(rownames(s) == "*")
         if (!length(i)) return("no missing row")
         c(row_max = max(s[i, ]), col_max = max(s[, i]))
       })

c(work.miss.indel, hw.miss.indel, fam.miss.indel)

attr(seq.work, "void"); attr(seq.work, "nr")
table(as.matrix(seq.work)[, 10])

mask <- col(ch$work) > len.m
ch$work[mask] <- NA; ch$hw[mask] <- NA; ch$fam[mask] <- NA

identical(table(seqlength(s.work, with.missing = FALSE)), table(len.m))

################################################################################
# Here is Kim attempting to create an impossible transition matrix
# THis is a decision point

# ---- put this right after: alph.work <- ...; alph.hw <- ...; alph.fam <- ... ----
print(alph.fam)   # LOOK at this first, then fix the two patterns below

fam.comb <- vapply(strsplit(comb.alphabet, "|", fixed = TRUE), `[`, "", 3)

is_mar <- grepl("^MAR", fam.comb)
is_coh <- grepl("^COH", fam.comb)
stopifnot(any(is_mar), any(is_coh))

g   <- expand.grid(i = seq_along(comb.alphabet), j = seq_along(comb.alphabet))
bad <- is_mar[g$i] & is_coh[g$j]

my_impossible <- setNames(comb.alphabet[g$j[bad]], comb.alphabet[g$i[bad]])

cat("blocked pairs:", length(my_impossible),
    "| origins:", length(unique(names(my_impossible))), "\n")

head(my_impossible)
names(my_impossible)
unname(my_impossible)
length(unique(names(my_impossible)))
head(data.frame(from = names(my_impossible),
                to   = unname(my_impossible)), 65)

# dead-end guard: no origin may have ALL 100 destinations blocked
blocked <- split(unname(my_impossible), names(my_impossible))
stopifnot(all(vapply(blocked, function(d) length(setdiff(comb.alphabet, d)) > 0, TRUE)))

#################################################################################
# Now further issues on creating seqdist -  this appears to be accidentally related to 
# the length issue

length(alphabet(seq.work)); alphabet(seq.work)
dim(work.miss.cost$sm);     rownames(work.miss.cost$sm)
length(alphabet(s.work))

# so, this is showing that some sequences have 5 and some have 6 - BECAUSE the nulls have no missing...


# SO need to fix the missing
# Checks:
length(len.m); nrow(ch$work); class(ch$work)


#And yes — once one_rep() returns, run table(as.vector(seqlength(s.work, with.missing = FALSE))) against table(len.v) and they should be identical, plus print two or three censored rows side by side exactly as you did with n1. Fix the outer() line first, then we do that comparison as its own step.
null <- seqnull(comb.seq, model = null_models[1], imp.trans.limit = imp_limit)
ch   <- split_channels(null)
len.v <- as.vector(len.m)
mask <- col(ch$work) > len.v
ch$work[mask] <- NA; ch$hw[mask] <- NA; ch$fam[mask] <- NA
s.work <- seqdef(as.data.frame(ch$work), states = alph.work, right = NA)

table(as.vector(seqlength(s.work, with.missing = FALSE)))
table(len.v)
# these should match and they do. they also match ORIGINAL data sequence length

#3    4    5    6    7    8    9   10 
#669  542  518  418  360  320  295 2303 

table(data$sequence_length)
#3    4    5    6    7    8    9   10 
#669  542  518  418  360  320  295 2303 

## Now compare actual examples
i <- head(which(len.v < 10), 3)
print(seq.work[i, ])      # observed
print(s.work[i, ])        # null, post-mask
len.v[i]

# 
#seqnull() rescales durations to fill 10 positions — durprop <- 1 + (ncol - nspell) * durprop/rowSums(...). A woman observed 4 years has her spells stretched to sum to 10, roughly 2.5 × their real length. Then you cut at position 4.
#So her null row is the first 4 years of a stretched trajectory — systematically fewer transitions than her real 4-year window had. Short null rows come out too stable.

tr_obs  <- rowSums(m.work[, -1] != m.work[, -10], na.rm = TRUE)
tr_null <- rowSums(ch$work[, -1] != ch$work[, -10], na.rm = TRUE)
tapply(tr_obs, len.v, mean)
tapply(tr_null, len.v, mean)

tr_obs  <- rowSums(m.fam[, -1] != m.fam[, -10], na.rm = TRUE)
tr_null <- rowSums(ch$fam[, -1] != ch$fam[, -10], na.rm = TRUE)
tapply(tr_obs, len.v, mean)
tapply(tr_null, len.v, mean)

#################################################################################
# Update the first code to fix the lenght thing?
# It really doesn't change much. I am now very confusd; should I integrate this?


set.seed(seed)
null_desc <- list(observed = trans_shares(obs_ch, obs_trans))
for (model in null_models) {
  pilot <- t(sapply(seq_len(R_pilot), function(i) {
    ch <- split_channels(seqnull(comb.seq, model = model, imp.trans.limit = imp_limit))
    mask <- col(ch$work) > as.vector(len.m)
    ch$work[mask] <- NA; ch$hw[mask] <- NA; ch$fam[mask] <- NA
    trans_shares(ch, obs_trans)
  }))
  null_desc[[paste0(model, "_mean")]] <- colMeans(pilot)
  null_desc[[paste0(model, "_min")]]  <- apply(pilot, 2, min)
  null_desc[[paste0(model, "_max")]]  <- apply(pilot, 2, max)
}

null_desc <- do.call(rbind, null_desc)
print(round(null_desc, 4))
write.csv(null_desc, file.path(out_dir, paste0("null_models_lengthadj_", country, ".csv")))

# This looks weird, so additional troubleshoot
set.seed(seed)
n  <- seqnull(comb.seq, model = "duration", imp.trans.limit = imp_limit)
ch <- split_channels(n)
cat("null NA cells:", sum(is.na(ch$work)),
    "| observed NA cells:", sum(is.na(m.work)), "\n")

#null NA cells: 0 | observed NA cells: 14212 