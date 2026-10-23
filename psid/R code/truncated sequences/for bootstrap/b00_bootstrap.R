# ---------------------------------------------------------------------
#    Program: b00_bootstrap.R
#    Author: Kim McErlean & Lea Pessin
#    Date: October 2026
#    Goal: validate the MCSA typology with Studer's (2021) parametric bootstrap,
#          adapted to (a) multichannel CAT distances, (b) right-censored
#          sequences normalised by shared length, (c) stacked imputations.
#
#    Builds on:
#      00_setupsequence_truncated.R     (sequence definition and costs, copied below so this
#                                        script reads the .dta directly)
#      typology-comparison-MCSA_truncated.R (procedure: hclust ward.D -> wcKMedRange(initialclust))
#
#    Logic (Studer 2021, Hennig & Lin 2015):
#      1. observed CQIs across k, here once per imputation block (5 lines)
#      2. generate "similar but nonclustered" data with a null model, run the SAME
#         distance + clustering pipeline, record CQIs across k; repeat R times
#         2.1 We can use the seqnull function for the combined null and the spell null but not the duration one
#         2.2. We adapt the seqnull function for the duration null so it rescales to observed sequence length
#      3. compare: raw null interval per k, and standardised "Max T"
#
#    Everything runs at single-imputation scale (n ~ 5,400), never on the stacked
#    matrix: the observed CQIs use one D* per imputation (= the diagonal blocks of the
#    stacked matrix), and the null replicates rotate the generating imputation
#    (replicate r uses imputation imps[(r-1) %% length(imps) + 1]), giving one band
#    that integrates over imputations.
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'G:/.shortcut-targets-by-id/1xwBJnyjpp046iCE2k6qzOz2M3gRNGpeK/WeEqualize Papers/Relationship Life Course' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'
kim.server <- '/home/kmcerlea/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim); .libPaths("G:/Other computers/My Laptop/Documents/R/R library") }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea); .libPaths(c("G:/My Drive/Software/R Library", .libPaths())) }  # prepend, keep default library
if (Sys.getenv(c("HOME" )) == "/home/lpessin") { setwd(lea.server) }
if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") { setwd(kim.server) }
getwd() # check it worked

# ~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~

required_packages <- c("TraMineR", "WeightedCluster", "cluster", "parallel",
                       "haven", "tidyverse")
lapply(required_packages, require, character.only = TRUE)

# ~~~~~~~~~~~~~~~~~~
# Settings ----
# ~~~~~~~~~~~~~~~~~~

country     <- "PSID"
kvals       <- 2:10            # same range as typology-comparison-MCSA_truncated.R (extend to 2:15 if you re-run that one)
k_chosen    <- 8               # reported solution for this country: SET to the PSID solution under the new alphabets (only marks the dotted line on the figures)
R_boot      <- 200             # replicates per null model
null_models <- c("combined", "duration", "sequencing", "combined_unrestricted")
# combined:   Studer's generic null 
# duration:   observed ordering kept exactly (all observed transitions at their observed
#             frequency) -possibly problematic for our data, TBD
# sequencing: durations kept attached to states, only the order randomised
# combined_unrestricted: SENSITIVITY check = combined null with imp.trans.limit = -1 

imp_limit   <- 0               # seqnull(imp.trans.limit): only transitions never observed between combined states are
imp_limit_unrestricted <- -1   # used by the "combined_unrestricted" sensitivity null only

R_pilot     <- 20              # pilot nulls per model for the descriptive table on what each null preserves
imps        <- 1:5             # imputations used (same five as the main analysis); nulls rotate over them
n_cores     <- 5               # Lea's laptop (10 cores, 16 GB)
seed        <- 20261001
cqi_keep    <- c("PBC", "HG", "ASW", "R2", "HC")   # HC: lower is better, handled below
out_dir     <- file.path("results", country, "bootstrap")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data: sequence objects per imputation, costs (as in 00_setupsequence_truncated.R)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- read_dta("created data/psid_couples_wide_truncated.dta")
data <- data %>% filter(`_mi_m` %in% imps, sequence_length >= 3)

t <- 1:10                      # 10 years; the .dta also has a year 11, not used (as in 00)
col_work <- which(names(data) %in% paste0("couple_work_trunc",        t))
col_hw   <- which(names(data) %in% paste0("couple_hw_hrs_4cat_trunc", t))
col_fam  <- which(names(data) %in% paste0("family_type_5cat_trunc",   t))
stopifnot(length(col_work) == 10, length(col_hw) == 10, length(col_fam) == 10)

shortlab.work   <- c("MBW", "1.5MBW", "dualFT", "FBW", "underWK")
longlab.work    <- c("male breadwinner", "1.5 male breadwinner", "dual full-time", "female breadwinner", "under work")
shortlab.hw.hrs <- c("W-most:high", "W-most:low", "equal:all", "M-most:all")
longlab.hw.hrs  <- c("woman does most/all: high", "woman does most/all: low", "equal:all", "man does most: all")
shortlab.fam    <- c("MARc0", "MARc1", "MARc2", "COHc0", "COHc1")
longlab.fam     <- c("married, 0 Ch", "married, 1 Ch", "married, 2 or more Ch", "cohab, 0 Ch", "cohab, 1 or more Ch")

# one list per imputation: sequence objects, character matrices (NA beyond the observed
# length), combined-state object for the null generator, observed lengths
to_chr <- function(s) { m <- as.matrix(s); m[m %in% c("*", "%")] <- NA; m }
build_imp <- function(m) {
  dm <- data[data$`_mi_m` == m, ]
  s.work <- seqdef(dm[, col_work], labels = longlab.work,   states = shortlab.work,   right = NA)
  s.hw   <- seqdef(dm[, col_hw],   labels = longlab.hw.hrs, states = shortlab.hw.hrs, right = NA)
  s.fam  <- seqdef(dm[, col_fam],  labels = longlab.fam,    states = shortlab.fam,    right = NA)
  ch   <- list(work = to_chr(s.work), hw = to_chr(s.hw), fam = to_chr(s.fam))
  comb <- matrix(paste(ch$work, ch$hw, ch$fam, sep = "|"), nrow = nrow(ch$work))
  comb[is.na(ch$work) | is.na(ch$hw) | is.na(ch$fam)] <- NA
  comb.seq <- seqdef(as.data.frame(comb), states = sort(unique(as.vector(comb[!is.na(comb)]))), right = "DEL")
  len <- as.vector(seqlength(s.work, with.missing = FALSE))
  stopifnot(all(seqlength(comb.seq) == len), all(len == dm$sequence_length))
  list(m = m, seq = list(work = s.work, hw = s.hw, fam = s.fam), ch = ch, comb = comb, comb.seq = comb.seq, len = len)
}
IMP <- lapply(imps, build_imp); names(IMP) <- paste0("imp", imps)
blocks <- names(IMP)

alph.work <- alphabet(IMP[[1]]$seq$work); alph.hw <- alphabet(IMP[[1]]$seq$hw); alph.fam <- alphabet(IMP[[1]]$seq$fam)
for (im in IMP) stopifnot(identical(alphabet(im$seq$work), alph.work),
                          identical(alphabet(im$seq$hw),   alph.hw),
                          identical(alphabet(im$seq$fam),  alph.fam))

# Costs (verbatim from 00): theory-based raw matrices, rescaled to max 2, censored state "*" at 0
sw <- c("MBW", "1.5MBW", "dualFT", "FBW", "underWK")
work.raw <- matrix(c(0, 2, 3, 5, 3,
                     2, 0, 1, 3, 3,
                     3, 1, 0, 2, 4,
                     5, 3, 2, 0, 2,
                     3, 3, 4, 2, 0), 5, byrow = TRUE, dimnames = list(sw, sw))
sh <- c("W-most:high", "W-most:low", "equal:all", "M-most:all")
hw.raw <- matrix(c(0, 1, 2, 4,
                   1, 0, 2, 4,
                   2, 2, 0, 2,
                   4, 4, 2, 0), 4, byrow = TRUE, dimnames = list(sh, sh))
sf <- c("MARc0", "MARc1", "MARc2", "COHc0", "COHc1")
fam.raw <- matrix(c(0, 4, 6, 3, 8,
                    4, 0, 2, 7, 4,
                    6, 2, 0, 9, 4,
                    3, 7, 9, 0, 5,
                    8, 4, 4, 5, 0), 5, byrow = TRUE, dimnames = list(sf, sf))
build_sm <- function(raw, seqobj, cval = 2) {
  alph <- alphabet(seqobj)
  stopifnot(setequal(alph, rownames(raw)), isSymmetric(raw), all(diag(raw) == 0))
  m  <- raw[alph, alph] * cval / max(raw)
  sm <- rbind(cbind(m, 0), 0)
  dimnames(sm) <- list(c(alph, "*"), c(alph, "*"))
  sm
}
build_indel <- function(seqobj) c(rep(1, length(alphabet(seqobj))), 99999)
work.miss.cost <- list(sm = build_sm(work.raw, IMP[[1]]$seq$work))
hw.miss.cost   <- list(sm = build_sm(hw.raw,   IMP[[1]]$seq$hw))
fam.miss.cost  <- list(sm = build_sm(fam.raw,  IMP[[1]]$seq$fam))
work.miss.indel <- build_indel(IMP[[1]]$seq$work)
hw.miss.indel   <- build_indel(IMP[[1]]$seq$hw)
fam.miss.indel  <- build_indel(IMP[[1]]$seq$fam)

# D* for one imputation (or one null draw): channels as character matrices, NA beyond len.
# Padded OM with "*" at zero cost + 99999 indel, divided by the common observed length.
dist_channels <- function(ch, len) {
  mask <- col(ch$work) > len
  ch$work[mask] <- NA; ch$hw[mask] <- NA; ch$fam[mask] <- NA
  s.work <- seqdef(as.data.frame(ch$work), alphabet = alph.work, right = NA)
  s.hw   <- seqdef(as.data.frame(ch$hw),   alphabet = alph.hw,   right = NA)
  s.fam  <- seqdef(as.data.frame(ch$fam),  alphabet = alph.fam,  right = NA)
  l <- as.vector(seqlength(s.work, with.missing = FALSE))
  D <- seqdistmc(channels = list(s.work, s.hw, s.fam), method = "OM",
                 indel = list(work.miss.indel, hw.miss.indel, fam.miss.indel),
                 sm    = list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                 with.missing = TRUE)
  D / outer(l, l, pmin)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The clustering procedure, exactly as in typology-comparison-MCSA_truncated.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cluster_cqi <- function(D, kvals) {
  ward <- hclust(as.dist(D), method = "ward.D")
  pam  <- wcKMedRange(D, kvals = kvals, initialclust = ward)
  st   <- as.matrix(pam$stats)[, cqi_keep, drop = FALSE]
  rownames(st) <- paste0("k", kvals)
  st
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Observed CQIs, one set per imputation (each imputation's own D*, ~12 s each)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs_stats <- list(); pooled_q <- list()
for (b in blocks) {
  D <- dist_channels(IMP[[b]]$ch, IMP[[b]]$len)
  obs_stats[[b]] <- cluster_cqi(D, kvals)
  # Optional: quality of the POOLED partition in this imputation. Requires the pooled
  # assignment (from Stata) merged on data as data$cluster_pooled.
  if ("cluster_pooled" %in% names(data))
    pooled_q[[b]] <- wcClusterQuality(D, data$cluster_pooled[data$`_mi_m` == IMP[[b]]$m])$stats[cqi_keep]
  rm(D); invisible(gc())
}
obs_arr <- aperm(simplify2array(obs_stats), c(3, 1, 2))     # block x k x index
saveRDS(obs_arr, file.path(out_dir, paste0("observed_cqi_by_imputation_", country, ".rds")))
if (length(pooled_q))
  write.csv(do.call(rbind, pooled_q), file.path(out_dir, paste0("pooled_partition_quality_by_imputation_", country, ".csv")))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Combined-state sequence object for the null generator (built in build_imp):
#    one label per position = work|hw|fam, censored positions void (right = "DEL").
#    Diagnostics below are run on imputation 1.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

m.work <- IMP[[1]]$ch$work; m.hw <- IMP[[1]]$ch$hw; m.fam <- IMP[[1]]$ch$fam
comb     <- IMP[[1]]$comb
comb.seq <- IMP[[1]]$comb.seq
len.m    <- IMP[[1]]$len
comb.alphabet <- alphabet(comb.seq)

# How much room the duration null has: free years per couple = observed length
# minus number of combined-state spells. 0 means the null reproduces the couple exactly.
dss.m    <- as.matrix(seqdss(comb.seq))
nspell.m <- rowSums(!is.na(dss.m) & dss.m != attr(comb.seq, "void"))
free.yrs <- as.vector(len.m) - nspell.m
print(table(free.yrs))
print(table(nspell.m))
cat("single-spell couples:", round(mean(nspell.m == 1), 3), "\n")
cat("couples with 0 free years:", round(mean(free.yrs == 0), 3), "\n")

# Dead-end check, precondition for imp_limit = 0: with imp.trans.limit = 0 seqnull() zeroes every
# spell-to-spell transition never observed, so a combined state with no observed exit would give an
# all-zero row and "NA in probability vector". Must be 0 states; otherwise use imp_limit = -1 for
# this country and say so.
for (b in blocks) {
  tr.dss   <- seqtrate(seqdss(IMP[[b]]$comb.seq))
  dead_end <- rownames(tr.dss)[rowSums(tr.dss > 0 & col(tr.dss) != row(tr.dss)) == 0]
  cat(b, "- combined states with no observed exit:", length(dead_end), "\n")
  if (length(dead_end) > 0) print(dead_end)
  stopifnot(imp_limit == -1 || length(dead_end) == 0)
}
#here results suggest that the duration null doesn't make much sense for our data, I need to think about this more

  # ## Testing adding impossible matrix
  # fam.comb <- vapply(strsplit(comb.alphabet, "|", fixed = TRUE), `[`, "", 3)
  # 
  # is_mar <- grepl("^MAR", fam.comb)
  # is_coh <- grepl("^COH", fam.comb)
  # stopifnot(any(is_mar), any(is_coh))
  # 
  # g   <- expand.grid(i = seq_along(comb.alphabet), j = seq_along(comb.alphabet))
  # bad <- is_mar[g$i] & is_coh[g$j]
  # 
  # my_impossible <- setNames(comb.alphabet[g$j[bad]], comb.alphabet[g$i[bad]])
  # 
  # cat("blocked pairs:", length(my_impossible),
  #     "| origins:", length(unique(names(my_impossible))), "\n")
  # 
  # head(my_impossible)
  # names(my_impossible)
  # unname(my_impossible)
  # length(unique(names(my_impossible)))
  # head(data.frame(from = names(my_impossible),
  #                 to   = unname(my_impossible)), 65)
  # 
  # # dead-end guard: no origin may have ALL 100 destinations blocked
  # blocked <- split(unname(my_impossible), names(my_impossible))
  # stopifnot(all(vapply(blocked, function(d) length(setdiff(comb.alphabet, d)) > 0, TRUE)))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. One replicate: generate -> split into channels -> CAT distance -> normalise
#    -> ward.D + PAM -> CQIs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# seqnull() is the generator behind seqnullcqi(); if it is not exported in your
# WeightedCluster version, replace with WeightedCluster:::seqnull

# generated combined-label object -> three character matrices (NA after censoring)
split_channels <- function(null) {
  lab  <- as.matrix(null); lab[lab %in% c("*", "%")] <- NA
  parts <- strsplit(as.vector(lab), "|", fixed = TRUE)
  getp  <- function(i) { v <- vapply(parts, function(p) if (length(p) == 3) p[i] else NA_character_, ""); matrix(v, nrow = nrow(lab)) }
  list(work = getp(1), hw = getp(2), fam = getp(3))
}

# NEW: This replaces the seqnull() for the duration model only
# Duration null with per-couple length: same as WeightedCluster:::seqnullspell
# (randomize = "duration") except that redrawn durations sum to the observed
# length len[i] instead of ncol(seqdata). Returns a character matrix with NA
# after the observed length, in the shape split_channels() expects.
seqnull_duration_len <- function(seqdata, len) {
  len  <- as.integer(as.vector(len))
  Tn   <- ncol(seqdata)
  dss2 <- as.matrix(seqdss(seqdata))
  dss2[dss2 == attr(seqdata, "void")] <- NA_character_
  if (ncol(dss2) < Tn)                       # seqdss() drops all-void columns
    dss2 <- cbind(dss2, matrix(NA_character_, nrow(dss2), Tn - ncol(dss2)))
  nspell <- rowSums(!is.na(dss2))
  stopifnot(length(len) == nrow(dss2), all(nspell <= len))
  
  # one uniform per spell; rescale so durations sum to len[i], each spell >= 1
  durprop <- matrix(runif(length(dss2)), nrow(dss2), ncol(dss2))
  durprop[is.na(dss2)] <- NA
  durprop <- 1 + (len - nspell) * durprop / rowSums(durprop, na.rm = TRUE)
  
  dur2    <- floor(durprop)
  durrest <- durprop - dur2
  durtot  <- rowSums(dur2, na.rm = TRUE)
  # Studer's rounding (seqnullspell line 94), kept verbatim for comparability.
  # Note: order() not rank(), so the shortfall years go to a permuted set of
  # spells rather than those with the largest remainders. Row totals are exact.
  cond    <- (t(apply(durrest, 1, order, decreasing = TRUE)) <= (len - durtot))
  dur2[cond] <- dur2[cond] + 1
  stopifnot(all(rowSums(dur2, na.rm = TRUE) == len))
  
  # unroll; [1:Tn] pads with NA beyond len[i]
  out <- t(sapply(seq_len(nrow(dss2)), function(i) {
    ok <- !is.na(dur2[i, ])
    rep(dss2[i, ok], times = as.integer(dur2[i, ok]))[1:Tn]
  }))
  dimnames(out) <- dimnames(as.matrix(seqdata))
  out
}

# One place that maps a null-model name to its generator, for imputation b (name in blocks).
# Used by null_len_check(), the pilot loop in 2b and one_rep(), so the three stay consistent.
#   duration              -> seqnull_duration_len() (per-couple length)
#   combined              -> seqnull(model = "combined", imp.trans.limit = imp_limit)
#   combined_unrestricted -> seqnull(model = "combined", imp.trans.limit = imp_limit_unrestricted)
#   sequencing            -> seqnull(model = "sequencing")  (imp.trans.limit has no effect there)
draw_null <- function(model, b = blocks[1]) {
  cs <- IMP[[b]]$comb.seq
  switch(model,
    duration              = seqnull_duration_len(cs, IMP[[b]]$len),
    combined              = seqnull(cs, model = "combined",   imp.trans.limit = imp_limit),
    combined_unrestricted = seqnull(cs, model = "combined",   imp.trans.limit = imp_limit_unrestricted),
    sequencing            = seqnull(cs, model = "sequencing", imp.trans.limit = imp_limit),
    stop("unknown null model: ", model))
}

# NEW: demonstrates missing value problem
# Modified to adjust for unequal length
null_len_check <- function(m) {
  rowSums(!is.na(split_channels(draw_null(m))$work))
}

#null_len_check <- function(m) {
#  ch <- split_channels(seqnull(comb.seq, model = m, imp.trans.limit = imp_limit,
#                               imp.trans = my_impossible))
#  rowSums(!is.na(ch$work))
#}

set.seed(seed)
for (m in null_models) {
  nl <- null_len_check(m)
  cat(m, "— null lengths:", paste(range(nl), collapse = "–"),
      "| observed:", paste(range(len.m), collapse = "–"), "\n")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2b. What each null preserves: descriptive table for the appendix, not a decision rule.
#     For the observed block and R_pilot draws from each null model: share of year-to-year
#     transitions that lower / raise parity, change marital status, change any state;
#     share of single-spell sequences (fully reproduced by the duration null, Studer's caveat);
#     share of combined transitions never observed in the data (must be 0 at imp_limit = 0).
#     Expected pattern: duration null matches the observed transition shares exactly
#     (ordering is kept); sequencing and combined nulls show more transitions of every
#     kind, parity decreases included, because ordering is what they randomise.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trans_shares <- function(ch, observed_trans = NULL) {
  f <- ch$fam; w <- ch$work; h <- ch$hw; Tn <- ncol(f)
  nch <- as.integer(sub(".*c", "", f)); dim(nch) <- dim(f)
  mar <- substr(f, 1, 3) == "MAR";      dim(mar) <- dim(f)
  ok  <- !is.na(f[, -1]) & !is.na(f[, -Tn])                    # both positions observed
  d_nch <- (nch[, -1] - nch[, -Tn])[ok]
  d_mar <- (mar[, -1] != mar[, -Tn])[ok]
  chg   <- (f[, -1] != f[, -Tn]) | (w[, -1] != w[, -Tn]) | (h[, -1] != h[, -Tn])
  any_ch <- chg[ok]
  chg[!ok] <- FALSE
  single_spell <- mean(rowSums(chg) == 0)                      # no change in any channel over the observed window
  out <- c(n_pairs = sum(ok),
           parity_down = mean(d_nch < 0), parity_up = mean(d_nch > 0),
           marital_change = mean(d_mar), any_state_change = mean(any_ch),
           single_spell_seq = single_spell)
  if (!is.null(observed_trans)) {                              # share of transitions never seen in the data
    lab <- matrix(paste(w, h, f, sep = "|"), nrow = nrow(f))
    key <- paste(lab[, -Tn], lab[, -1])[ok]
    out <- c(out, unobserved_transition = mean(!(key %in% observed_trans)))
  }
  out
}

obs_ch    <- list(work = m.work, hw = m.hw, fam = m.fam)
ok_obs    <- !is.na(comb[, -1]) & !is.na(comb[, -ncol(comb)])
# should this have 9 or 10? is this transitions INTO non-null states? or just non-null states? The former makes more sense looking at the results
obs_trans <- unique(paste(comb[, -ncol(comb)], comb[, -1])[ok_obs])
#So here is this what transitions are observed that are between non-null states?

# Is marriage -> cohabitation absent from the data? (PSID: 0 year-pairs, so blocked for the US)
fam_from <- sub(".*\\|", "", comb[, -ncol(comb)])[ok_obs]   # family part of the 'from' state
fam_to   <- sub(".*\\|", "", comb[, -1])[ok_obs]            # family part of the 'to' state
cat("marriage -> cohabitation year-pairs observed:", sum(grepl("^MAR", fam_from) & grepl("^COH", fam_to)), "\n")
cat("parity decreases observed:", sum(as.integer(sub(".*c", "", fam_to)) < as.integer(sub(".*c", "", fam_from))), "\n")

set.seed(seed)
null_desc <- list(observed = trans_shares(obs_ch, obs_trans))
for (model in null_models) {
  pilot <- t(sapply(seq_len(R_pilot), function(i) {
    ch   <- split_channels(draw_null(model))
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
write.csv(null_desc, file.path(out_dir, paste0("null_models_description_", country, ".csv")))
# equal the observed ones; 'single_spell_seq' (observed row) is the share of sequences the
# duration null cannot randomise and should be small.


#here the function works differently for the duration model vs the other two (see draw_null)
# replicate r is generated from imputation blocks[(r-1) %% length(blocks) + 1], so the R_boot
# replicates of each null model are spread evenly over the imputations
one_rep <- function(r, model) {
  b  <- blocks[(r - 1) %% length(blocks) + 1]
  ch <- split_channels(draw_null(model, b))
  cluster_cqi(dist_channels(ch, IMP[[b]]$len), kvals)
}


# we time one replication on the combined null
# OH i misread this. we are TIMING one replicate before doing this. we are not doing something at "time one"
set.seed(seed)
t1 <- system.time(test_rep <- one_rep(1, null_models[1]))
print(t1); print(test_rep)
# Is this for the p-value or should this match our cluster metrics without the null?
cat("Estimated core-hours for", R_boot, "replicates x", length(null_models), "models:",
    round(t1["elapsed"] * R_boot * length(null_models) / 3600, 1), "\n")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Run the bootstrap. Linux/server: forked mclapply (as before). Windows: mclapply cannot
#    fork, so with n_cores > 1 a PSOCK cluster + parLapply is used instead; each worker loads
#    the packages, receives a copy of the workspace (one_rep and everything it uses) and gets
#    its own reproducible RNG stream (clusterSetRNGStream = parallel counterpart of mc.set.seed).
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RNGkind("L'Ecuyer-CMRG"); set.seed(seed)
null_res <- list()
use_psock <- .Platform$OS.type == "windows" && n_cores > 1

if (use_psock) {
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterEvalQ(cl, { library(TraMineR); library(WeightedCluster) })
  parallel::clusterExport(cl, setdiff(ls(envir = .GlobalEnv), "cl"))
  parallel::clusterSetRNGStream(cl, seed)
}

for (model in null_models) {
  res <- if (use_psock) {
    # model is passed as an ARGUMENT: a closure's free variable `model` would be looked up in the
    # worker's global env, i.e. the stale value exported before the loop (bug found 21 Oct 2026:
    # all four nulls had silently run as the same model)
    parallel::parLapply(cl, seq_len(R_boot), function(r, m) one_rep(r, m), m = model)
  } else {
    parallel::mclapply(seq_len(R_boot), function(r) one_rep(r, model),
                       mc.cores = n_cores, mc.set.seed = TRUE)
  }
  null_res[[model]] <- aperm(simplify2array(res), c(3, 1, 2))   # replicate x k x index
  saveRDS(null_res, file.path(out_dir, paste0("null_cqi_", country, ".rds")))
  cat("done:", model, "\n")
}

if (use_psock) parallel::stopCluster(cl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Compare observed with null: raw intervals per k, standardised values, Max T
#    (Studer 2021, "Statistical Test"; Hennig & Lin 2015)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sign_cqi <- ifelse(cqi_keep == "HC", -1, 1)      # flip HC so that higher = better everywhere
names(sign_cqi) <- cqi_keep

summarise_boot <- function(null_arr, obs_arr, model) {
  R <- dim(null_arr)[1]
  out <- list()
  for (j in seq_along(cqi_keep)) {
    cq <- cqi_keep[j]; sg <- sign_cqi[cq]
    nu <- null_arr[, , j] * sg                                 # R x k
    ob <- matrix(obs_arr[, , j], nrow = dim(obs_arr)[1]) * sg  # blocks x k (matrix() keeps the dims when there is 1 block)
    mu <- colMeans(nu); sdv <- apply(nu, 2, sd)
    nu_std <- sweep(sweep(nu, 2, mu), 2, sdv, "/")
    ob_std <- sweep(sweep(ob, 2, mu), 2, sdv, "/")
    maxT   <- apply(nu_std, 1, max)                            # null distribution of the max over k
    crit95 <- quantile(maxT, 0.95)
    q_lo   <- apply(nu, 2, quantile, 0.025) * sg; q_hi <- apply(nu, 2, quantile, 0.975) * sg
    for (b in seq_len(nrow(ob))) for (kk in seq_along(kvals)) {
      out[[length(out) + 1]] <- data.frame(
        country = country, model = model, index = cq, block = blocks[b], k = kvals[kk],
        observed   = obs_arr[b, kk, j],
        null_lo    = min(q_lo[kk], q_hi[kk]), null_hi = max(q_lo[kk], q_hi[kk]),
        null_mean  = mu[kk] * sg,
        std        = ob_std[b, kk],                             # standardised (higher = better)
        maxT_crit95 = crit95,
        exceeds_maxT = ob_std[b, kk] > crit95,
        p_maxT     = (sum(maxT >= ob_std[b, kk]) + 1) / (R + 1),
        p_k        = (sum(nu_std[, kk] >= ob_std[b, kk]) + 1) / (R + 1))
    }
  }
  bind_rows(out)
}

boot_tab <- bind_rows(lapply(null_models, function(m) summarise_boot(null_res[[m]], obs_arr, m)))
write.csv(boot_tab, file.path(out_dir, paste0("bootstrap_summary_", country, ".csv")), row.names = FALSE)

# which k are validated (observed above the null interval in every block), per model and index
validated <- boot_tab %>%
  group_by(model, index, k) %>%
  summarise(all_blocks_exceed_maxT = all(exceeds_maxT),
            all_blocks_above_raw   = all(if (index[1] == "HC") observed < null_lo else observed > null_hi),
            min_std = min(std), .groups = "drop")
print(as.data.frame(validated), digits = 3)
write.csv(validated, file.path(out_dir, paste0("bootstrap_validated_k_", country, ".csv")), row.names = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Figures: raw CQI with null band and five observed lines; standardised with Max T line
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (model in null_models) {
  d <- boot_tab %>% filter(model == !!model) %>%
    mutate(index = factor(index, levels = cqi_keep,
                          labels = c("PBC", "Hubert's gamma", "ASW", "pseudo R2", "HC (lower = better)")))
  band <- d %>% distinct(index, k, null_lo, null_hi, null_mean)
  
  p_raw <- ggplot() +
    geom_ribbon(data = band, aes(k, ymin = null_lo, ymax = null_hi), fill = "grey80", alpha = 0.8) +
    geom_line(data = band, aes(k, null_mean), colour = "grey50", linetype = 2) +
    geom_line(data = d, aes(k, observed, group = block), colour = "#0072B2", linewidth = 0.5, alpha = 0.8) +
    geom_vline(xintercept = k_chosen, linetype = 3) +
    facet_wrap(~ index, scales = "free_y") +
    scale_x_continuous(breaks = kvals) +
    labs(x = "Number of clusters", y = NULL,
         title = paste0(country, ": observed CQIs (one line per imputation) vs. null model '", model, "'"),
         subtitle = "Grey band: 2.5 to 97.5 percentile of bootstrapped null CQIs; dotted line: reported solution") +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  ggsave(file.path(out_dir, paste0("cqi_raw_vs_null_", model, "_", country, ".pdf")), p_raw, width = 10, height = 6)
  
  p_std <- ggplot(d, aes(k, std, group = block)) +
    geom_hline(aes(yintercept = maxT_crit95), colour = "grey40", linetype = 2) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_line(colour = "#0072B2", linewidth = 0.5, alpha = 0.8) +
    geom_vline(xintercept = k_chosen, linetype = 3) +
    facet_wrap(~ index, scales = "free_y") +
    scale_x_continuous(breaks = kvals) +
    labs(x = "Number of clusters", y = "Standardised CQI (higher = more structure than null)",
         title = paste0(country, ": standardised CQIs vs. null model '", model, "'"),
         subtitle = "Dashed line: 95th percentile of the Max-T null distribution") +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  ggsave(file.path(out_dir, paste0("cqi_std_maxT_", model, "_", country, ".pdf")), p_std, width = 10, height = 6)
}

save(obs_arr, null_res, boot_tab, validated, kvals, k_chosen, null_models, R_boot,
     file = file.path(out_dir, paste0("bootstrap_", country, ".RData")))
