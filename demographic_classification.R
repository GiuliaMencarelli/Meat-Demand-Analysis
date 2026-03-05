# Household Demographic Classification
# Author: Giulia Mencarelli
#
# Constructs two demographic grouping variables added to dt:
#   - hh_type             : household structure (family, adult-only, elderly)
#   - dominant_adult_final: dominant life-cycle group of the household
#
# These variables are used in the subgroup analysis script to cut
# elasticity and tax simulation results by household type and age group.
#
# Requires: dt from tax_simulation.R

library(data.table)

### 1. Identify and prepare member-level age and relationship columns

age_cols <- grep("^c_c_etacalc_", names(dt), value = TRUE)
rel_cols <- grep("^c_relaz_",     names(dt), value = TRUE)

stopifnot(length(age_cols) > 0, length(rel_cols) > 0)
stopifnot(length(age_cols) == length(rel_cols))

# Coerce to numeric (replaces any "." or non-numeric entries with NA)
dt[, (age_cols) := lapply(.SD, as.numeric), .SDcols = age_cols]
dt[, (rel_cols) := lapply(.SD, as.numeric), .SDcols = rel_cols]

# Build matrices and mask columns that exceed actual household size
age_mat <- as.matrix(dt[, ..age_cols])
rel_mat <- as.matrix(dt[, ..rel_cols])

member_mask        <- col(age_mat) <= dt$hh_size
age_mat[!member_mask] <- NA
rel_mat[!member_mask] <- NA

age_mat <- matrix(age_mat, nrow = nrow(dt), ncol = length(age_cols))
rel_mat <- matrix(rel_mat, nrow = nrow(dt), ncol = length(rel_cols))

### 2. Count household members by age group

# Age codes: 1 = child, 2 = young adult (18-34), 3 = prime-age (35-64), 4 = elderly (65+)
dt[, n_children      := apply(age_mat, 1, function(x) sum(x == 1,        na.rm = TRUE))]
dt[, n_adults        := apply(age_mat, 1, function(x) sum(x %in% c(2,3), na.rm = TRUE))]
dt[, n_elderly       := apply(age_mat, 1, function(x) sum(x == 4,        na.rm = TRUE))]
dt[, n_young_adults  := rowSums(age_mat == 2, na.rm = TRUE)]
dt[, n_prime_adults  := rowSums(age_mat == 3, na.rm = TRUE)]
dt[, n_elderly_adults := rowSums(age_mat == 4, na.rm = TRUE)]

### 3. Household structure typology

dt[, hh_type := fifelse(
  n_children > 0 & n_adults >= 1,  "Family with children",
  fifelse(
    n_adults >= 1 & n_children == 0, "Adult-only household",
    fifelse(
      n_adults == 0 & n_elderly >= 1,  "Elderly household",
      "Other"
    )
  )
)]

### 4. Dominant life-cycle group

# Find which adult age group has the most members in the household
dt[, max_adults := pmax(n_young_adults, n_prime_adults, n_elderly_adults)]

# Flag ties (two or more groups equally represented)
dt[, n_groups_at_max := (n_young_adults  == max_adults) +
     (n_prime_adults  == max_adults) +
     (n_elderly_adults == max_adults)]
dt[, has_tie := n_groups_at_max > 1]

# For tied households, fall back to reference person's age group
is_ref  <- rel_mat == 1
ref_age <- vapply(seq_len(nrow(age_mat)), function(i) {
  idx <- which(is_ref[i, ])
  if (length(idx) == 1) age_mat[i, idx] else NA_real_
}, numeric(1))

dt[, ref_age := ref_age]

dt[, ref_lifecycle := fifelse(
  ref_age == 4, "Elderly (65+)",
  fifelse(
    ref_age == 3, "Prime-age adults (35-64)",
    fifelse(
      ref_age == 2, "Young adults (18-34)",
      NA_character_
    )
  )
)]

# Final dominant group: use plurality winner, or reference person to break ties
dt[, dominant_adult_final := fifelse(
  has_tie,
  ref_lifecycle,
  fifelse(
    n_elderly_adults == max_adults, "Elderly (65+)",
    fifelse(
      n_prime_adults == max_adults,  "Prime-age adults (35-64)",
      "Young adults (18-34)"
    )
  )
)]

### 5. Distribution checks

cat("Household type distribution:\n")
print(dt[, .N, by = hh_type][order(-N)])

cat("\nDominant life-cycle group distribution:\n")
print(dt[, .N, by = dominant_adult_final][order(-N)])