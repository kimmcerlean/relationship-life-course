# ---------------------------------------------------------------------
#    Program: bootstrap-validation_truncated.R
#    Author: Kim McErlean & Lea Pessin
#    Date: October 2026
#    Goal: validate the MCSA typology with Studer's (2021) parametric bootstrap,
#          adapted to (a) multichannel CAT distances, (b) right-censored
#          sequences normalised by shared length, (c) stacked imputations.
#
#    Builds on:
#      00_setupsequence_truncated.R     (seq.*, *.miss.cost, *.miss.indel)
#      cluster-comparison_truncated.R   (mcdist.det.min, saved in the .RData loaded below)
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
#    Everything runs at single-imputation scale (n ~ 5,600), never on the
#    stacked 28k matrix, except extracting the five observed blocks from it.
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# clear the environment
rm(list = ls())

options(repos=c(CRAN="https://cran.r-project.org"))


# set WD for whomever is running the script
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course' #leas folder
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

country     <- "GSOEP"
kvals       <- 2:10            # same range as typology-comparison-MCSA_truncated.R (extend to 2:15 if you re-run that one)
k_chosen    <- 9               # reported solution for this country
R_boot      <- 200             # replicates per null model
null_models <- c("combined", "duration", "sequencing")
# combined:   Studer's generic null (ordering AND durations randomised), headline baseline
# duration:   observed ordering kept exactly (all observed transitions at their observed
#             frequency), only time in each state randomised -> tests duration/timing structure
# sequencing: durations kept attached to states, only the order randomised -> tests ordering structure

####### Revisit this, trying with impossible transition matrix for now
imp_limit   <- 0               # seqnull(imp.trans.limit): only transitions never observed between combined states are
# excluded (the default -1 would let the generator invent transitions). No other restriction:
# every observed transition, parity decreases included, is possible in the null.
#imp_limit   <- -1

R_pilot     <- 20              # pilot nulls per model for the descriptive table on what each null preserves
imp_for_null <- 1              # imputation block used to generate the null data
n_cores     <- max(1, parallel::detectCores() - 2)
seed        <- 20261001
cqi_keep    <- c("PBC", "HG", "ASW", "R2", "HC")   # HC: lower is better, handled below
out_dir     <- file.path("results", country, "bootstrap")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load sequences and the stacked distance matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load("created data/gsoep/gsoep_cluster-comparison-truncated.RData")
load("temp data/gsoep/gsoep_cluster-comparison-test-data.RData")

# objects used: data, seq.work.ow, seq.hw.hrs.weekly, seq.fam,
#               work.miss.cost, hw.miss.cost, fam.miss.cost,
#               work.miss.indel, hw.miss.indel, fam.miss.indel, mcdist.det.min

imp_id  <- data$`_mi_m`
#imp_id  <- sampled_data$`_mi_m`
n_imp   <- length(unique(imp_id))
blocks  <- lapply(sort(unique(imp_id)), function(m) which(imp_id == m))
names(blocks) <- paste0("imp", sort(unique(imp_id)))

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
# 1. Observed CQIs, one set per imputation block (sub-blocks of the stacked matrix)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs_stats <- lapply(blocks, function(idx) cluster_cqi(mcdist.det.min[idx, idx], kvals))
# array: block x k x index
obs_arr <- simplify2array(obs_stats)              # k x index x block
obs_arr <- aperm(obs_arr, c(3, 1, 2))             # block x k x index
saveRDS(obs_arr, file.path(out_dir, "observed_cqi_by_imputation.rds"))

# Optional: quality of the POOLED partition evaluated on each block.
# Requires the pooled assignment (from Stata) merged on data as data$cluster_pooled.
# Skip this for now (10/11) because don't have this
if ("cluster_pooled" %in% names(data)) {
  pooled_q <- t(sapply(blocks, function(idx)
    wcClusterQuality(mcdist.det.min[idx, idx], data$cluster_pooled[idx])$stats[cqi_keep]))
  write.csv(pooled_q, file.path(out_dir, "pooled_partition_quality_by_imputation.csv"))
}

# Keep only the imputation block used for the null, then drop the stacked matrices
idx_null <- blocks[[imp_for_null]]
seq.work.m <- seq.work[idx_null, ]
seq.hw.m   <- seq.hw.hrs[idx_null, ]
seq.fam.m  <- seq.fam[idx_null, ]
len.m      <- seqlength(seq.work.m, with.missing = FALSE)
rm(list = intersect(ls(), c("mcdist.det.min", "mcdist.det.om", "fam.min.len", "work.min.len", "hw.min.len",
                            "dist.work.om", "dist.hw.om", "dist.fam.om",
                            "dist.work.min", "dist.hw.min", "dist.fam.min")))
invisible(gc())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Combined-state sequence object for the null generator
#    One label per position = work|hw|fam. Censored positions are void (right = "DEL"),
#    so the generator cuts each null sequence at the observed length.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

to_chr <- function(s) { m <- as.matrix(s); m[m %in% c("*", "%")] <- NA; m }
m.work <- to_chr(seq.work.m); m.hw <- to_chr(seq.hw.m); m.fam <- to_chr(seq.fam.m)
comb   <- matrix(paste(m.work, m.hw, m.fam, sep = "|"), nrow = nrow(m.work))
comb[is.na(m.work) | is.na(m.hw) | is.na(m.fam)] <- NA
comb.alphabet <- sort(unique(as.vector(comb[!is.na(comb)])))
comb.seq <- seqdef(as.data.frame(comb), states = comb.alphabet, right = "DEL")
stopifnot(all(seqlength(comb.seq) == len.m))

# How much room the duration null has: free years per couple = observed length
# minus number of combined-state spells. 0 means the null reproduces the couple exactly.
dss.m    <- as.matrix(seqdss(comb.seq))
nspell.m <- rowSums(!is.na(dss.m) & dss.m != attr(comb.seq, "void"))
free.yrs <- as.vector(len.m) - nspell.m
print(table(free.yrs))
print(table(nspell.m))
cat("single-spell couples:", round(mean(nspell.m == 1), 3), "\n")
cat("couples with 0 free years:", round(mean(free.yrs == 0), 3), "\n")

alph.work <- alphabet(seq.work); alph.hw <- alphabet(seq.hw.hrs); alph.fam <- alphabet(seq.fam)

  ## Testing adding impossible matrix
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

# NEW: demonstrates missing value problem
# Modified to adjust for unequal length
null_len_check <- function(m) {
  null <- if (m == "duration") seqnull_duration_len(comb.seq, len.m)
  else seqnull(comb.seq, model = m, imp.trans.limit = imp_limit)
  rowSums(!is.na(split_channels(null)$work))
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

set.seed(seed)
null_desc <- list(observed = trans_shares(obs_ch, obs_trans))
for (model in null_models) {
  pilot <- t(sapply(seq_len(R_pilot), function(i) {
    null <- if (model == "duration") seqnull_duration_len(comb.seq, len.m)
    else seqnull(comb.seq, model = model, imp.trans.limit = imp_limit)
    ch   <- split_channels(null)
    mask <- col(ch$work) > as.vector(len.m)
    ch$work[mask] <- NA; ch$hw[mask] <- NA; ch$fam[mask] <- NA
    trans_shares(ch, obs_trans)
  }))
  null_desc[[paste0(model, "_mean")]] <- colMeans(pilot)
  null_desc[[paste0(model, "_min")]]  <- apply(pilot, 2, min)
  null_desc[[paste0(model, "_max")]]  <- apply(pilot, 2, max)
}
#### This is where I got error that stopped it all
#### that went away once I updated to full imputation NOT sample. TOO SPARSE when just 500 sequences

null_desc <- do.call(rbind, null_desc)
print(round(null_desc, 4))
write.csv(null_desc, file.path(out_dir, paste0("null_models_description_", country, ".csv")))
# Checks: 'unobserved_transition' is 0 for every model; the duration null's transition shares
# equal the observed ones; 'single_spell_seq' (observed row) is the share of sequences the
# duration null cannot randomise and should be small.


#here the function works differently for the duration model vs the other two
one_rep <- function(r, model) {
  null <- if (model == "duration") {
    seqnull_duration_len(comb.seq, len.m)
  } else {
    seqnull(comb.seq, model = model, imp.trans.limit = imp_limit) # ,imp.trans = my_impossible)
  }
  ch   <- split_channels(null)
  len.v <- as.vector(len.m)
  mask <- col(ch$work) > len.v
  ch$work[mask] <- NA
  ch$hw[mask]   <- NA
  ch$fam[mask]  <- NA
  s.work <- seqdef(as.data.frame(ch$work), alphabet = alph.work, right = NA)
  s.hw   <- seqdef(as.data.frame(ch$hw),   alphabet = alph.hw,   right = NA)
  s.fam  <- seqdef(as.data.frame(ch$fam),  alphabet = alph.fam,  right = NA)
  len    <- as.vector(seqlength(s.work, with.missing = FALSE))
  D <- seqdistmc(channels = list(s.work, s.hw, s.fam), method = "OM",
                 indel = list(work.miss.indel, hw.miss.indel, fam.miss.indel),
                 sm    = list(work.miss.cost$sm, hw.miss.cost$sm, fam.miss.cost$sm),
                 with.missing = TRUE)
  D <- D / outer(len, len, pmin)          # same normalisation as mcdist.det.min
  cluster_cqi(D, kvals)
}

##Error here: Error:  [!] size of substitution cost matrix must be 5x5
##Also, alphabets and labels are mismatched...

# time one replicate before launching
# OH i misread this. we are TIMING one replicate before doing this. we are not doing something at "time one"
set.seed(seed)
t1 <- system.time(test_rep <- one_rep(1, null_models[1]))
print(t1); print(test_rep)
# Is this for the p-value or should this match our cluster metrics without the null?
cat("Estimated core-hours for", R_boot, "replicates x", length(null_models), "models:",
    round(t1["elapsed"] * R_boot * length(null_models) / 3600, 1), "\n")

## The code runs through here. I am not convinced it is correct.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Run the bootstrap (forked parallelism; on Windows use parLapply instead)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RNGkind("L'Ecuyer-CMRG"); set.seed(seed)
null_res <- list()
for (model in null_models) {
  res <- parallel::mclapply(seq_len(R_boot), function(r) one_rep(r, model),
                            mc.cores = n_cores, mc.set.seed = TRUE)
  null_res[[model]] <- aperm(simplify2array(res), c(3, 1, 2))   # replicate x k x index
  saveRDS(null_res, file.path(out_dir, "null_cqi.rds"))
  cat("done:", model, "\n")
}

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
    nu <- null_arr[, , j] * sg; ob <- obs_arr[, , j] * sg      # R x k ; blocks x k
    mu <- colMeans(nu); sdv <- apply(nu, 2, sd)
    nu_std <- sweep(sweep(nu, 2, mu), 2, sdv, "/")
    ob_std <- sweep(sweep(ob, 2, mu), 2, sdv, "/")
    maxT   <- apply(nu_std, 1, max)                            # null distribution of the max over k
    crit95 <- quantile(maxT, 0.95)
    q_lo   <- apply(nu, 2, quantile, 0.025) * sg; q_hi <- apply(nu, 2, quantile, 0.975) * sg
    for (b in seq_len(nrow(ob))) for (kk in seq_along(kvals)) {
      out[[length(out) + 1]] <- data.frame(
        country = country, model = model, index = cq, block = names(blocks)[b], k = kvals[kk],
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
