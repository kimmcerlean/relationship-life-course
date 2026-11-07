# ---------------------------------------------------------------------
#    Program: b01_null_by_imputation.R
#    Author: Kim McErlean & Lea Pessin
#    Date: October 2026
#    Goal: appendix check for the parametric bootstrap (b00_bootstrap.R). The null
#          replicates in b02 rotate over the five imputations and are pooled into one
#          band. This script asks whether that is innocuous: it generates R_check
#          replicates of the primary null (combined, imp_limit = 0) from EACH imputation
#          separately and compares the null means across imputations, in units of the
#          within-imputation sd. Also recomputes the observed CQIs per imputation.
#
#    Outputs (results/<country>/bootstrap/):
#      null_by_imputation_full_<country>.csv     long: index, k, imputation, null mean/sd, observed
#      null_by_imputation_summary_<country>.csv  one row per k: max between-imputation shift of the
#                                               null mean / pooled sd, and observed gap / sd, per index
#
#    Setup (data, costs, D*, clustering) copied from b00_bootstrap.R so this runs on its own.
#    Compute: length(imps) x R_check replicates, ~2 min each on one core. Run AFTER b02.
# --------------------------------------------------------------------

rm(list = ls())
options(repos=c(CRAN="https://cran.r-project.org"))

# set WD for whomever is running the script
lea <- 'G:/.shortcut-targets-by-id/1xwBJnyjpp046iCE2k6qzOz2M3gRNGpeK/WeEqualize Papers/Relationship Life Course' #leas folder
kim <- 'G:/My Drive/WeEqualize Papers/Relationship Life Course' # Kim
lea.server <- '/home/lpessin/stage/Life Course'
kim.server <- '/home/kmcerlea/stage/Life Course'

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim); .libPaths("G:/Other computers/My Laptop/Documents/R/R library") }
if (Sys.getenv(c("USERNAME")) == "lpessin") { setwd(lea); .libPaths(c("G:/My Drive/Software/R Library", .libPaths())) }
if (Sys.getenv(c("HOME" )) == "/home/lpessin") { setwd(lea.server) }
if (Sys.getenv(c("HOME" )) == "/home/kmcerlea") { setwd(kim.server) }
getwd()

required_packages <- c("TraMineR", "WeightedCluster", "cluster", "parallel", "haven", "tidyverse")
lapply(required_packages, require, character.only = TRUE)

# ~~~~~~~~~~~~~~~~~~
# Settings ----
# ~~~~~~~~~~~~~~~~~~

country   <- "GSOEP"
imps      <- 1:5               # same imputations as b02
R_check   <- 20                # null replicates per imputation
kvals     <- 2:10
imp_limit <- 0                 # primary specification, as in b02
n_cores   <- 5
seed      <- 20261021
cqi_keep  <- c("PBC", "HG", "ASW", "R2", "HC")
out_dir   <- file.path("results", country, "bootstrap")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data, costs, D*, clustering: as in b00_bootstrap.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- read_dta("created data/gsoep/gsoep_couples_wide_truncated.dta")
data <- data %>% filter(`_mi_m` %in% imps, sequence_length >= 3)

t <- 1:10
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
  list(m = m, seq = list(work = s.work, hw = s.hw, fam = s.fam), ch = ch, comb.seq = comb.seq, len = len)
}
IMP <- lapply(imps, build_imp); names(IMP) <- paste0("imp", imps)
blocks <- names(IMP)

alph.work <- alphabet(IMP[[1]]$seq$work); alph.hw <- alphabet(IMP[[1]]$seq$hw); alph.fam <- alphabet(IMP[[1]]$seq$fam)
for (im in IMP) stopifnot(identical(alphabet(im$seq$work), alph.work),
                          identical(alphabet(im$seq$hw),   alph.hw),
                          identical(alphabet(im$seq$fam),  alph.fam))

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
cluster_cqi <- function(D, kvals) {
  ward <- hclust(as.dist(D), method = "ward.D")
  pam  <- wcKMedRange(D, kvals = kvals, initialclust = ward)
  st   <- as.matrix(pam$stats)[, cqi_keep, drop = FALSE]
  rownames(st) <- paste0("k", kvals)
  st
}
split_channels <- function(null) {
  lab  <- as.matrix(null); lab[lab %in% c("*", "%")] <- NA
  parts <- strsplit(as.vector(lab), "|", fixed = TRUE)
  getp  <- function(i) { v <- vapply(parts, function(p) if (length(p) == 3) p[i] else NA_character_, ""); matrix(v, nrow = nrow(lab)) }
  list(work = getp(1), hw = getp(2), fam = getp(3))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Observed CQIs per imputation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs <- lapply(IMP, function(im) cluster_cqi(dist_channels(im$ch, im$len), kvals))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. R_check combined-null replicates from each imputation (no rotation)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

one_rep_imp <- function(r, b) {
  null <- seqnull(IMP[[b]]$comb.seq, model = "combined", imp.trans.limit = imp_limit)
  cluster_cqi(dist_channels(split_channels(null), IMP[[b]]$len), kvals)
}
jobs <- expand.grid(r = seq_len(R_check), b = blocks, stringsAsFactors = FALSE)

RNGkind("L'Ecuyer-CMRG"); set.seed(seed)
use_psock <- .Platform$OS.type == "windows" && n_cores > 1
if (use_psock) {
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterEvalQ(cl, { library(TraMineR); library(WeightedCluster) })
  parallel::clusterExport(cl, setdiff(ls(envir = .GlobalEnv), "cl"))
  parallel::clusterSetRNGStream(cl, seed)
  res <- parallel::parLapply(cl, seq_len(nrow(jobs)), function(j, jobs) one_rep_imp(jobs$r[j], jobs$b[j]), jobs = jobs)
  parallel::stopCluster(cl)
} else {
  res <- parallel::mclapply(seq_len(nrow(jobs)), function(j) one_rep_imp(jobs$r[j], jobs$b[j]),
                            mc.cores = n_cores, mc.set.seed = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Tables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

full <- list()
for (cq in cqi_keep) for (b in blocks) {
  nu <- t(sapply(res[jobs$b == b], function(x) x[, cq]))        # R_check x k
  full[[length(full) + 1]] <- data.frame(country = country, index = cq, imputation = b, k = kvals,
                                         null_mean = colMeans(nu), null_sd = apply(nu, 2, sd),
                                         observed = obs[[b]][, cq])
}
full <- bind_rows(full)
write.csv(full, file.path(out_dir, paste0("null_by_imputation_full_", country, ".csv")), row.names = FALSE)

# summary: per index and k, (max - min of null means across imputations) / pooled within sd,
# and (mean observed - mean null) / pooled within sd for scale
summ <- full %>%
  group_by(index, k) %>%
  summarise(shift_in_sd = (max(null_mean) - min(null_mean)) / sqrt(mean(null_sd^2)),
            obs_gap_in_sd = (mean(observed) - mean(null_mean)) / sqrt(mean(null_sd^2)),
            .groups = "drop")
summ_wide <- summ %>%
  pivot_wider(names_from = index, values_from = c(shift_in_sd, obs_gap_in_sd)) %>%
  arrange(k)
print(as.data.frame(summ_wide), digits = 3)
write.csv(summ_wide, file.path(out_dir, paste0("null_by_imputation_summary_", country, ".csv")), row.names = FALSE)
# shift_in_sd: largest difference between imputation-specific null means, in units of the
# within-imputation sd (noise level with R_check = 20 is roughly 0.3 to 0.5 for 5 imputations).
# obs_gap_in_sd: how far the observed sits from the null, same units.
