# Welfare Analysis: Compensating Variation, Consumer Surplus, GHG Reduction
# Author: Giulia Mencarelli
#
# Computes household-level welfare effects of the carbon tax:
#   - Tax burden (T_h) and per-capita rebate (R_h)
#   - Compensating variation (CV_h) via Hicksian elasticities
#   - Consumer surplus change (CS_h) via trapezoid approximation
#   - GHG emissions before and after the tax
#   - All measures summarised by expenditure quintile
#
# Requires: dt from demographic_classification.R
#           results_meat from meat_demand_LA_AIDS.R
#           ghg, Pop from tax_simulation.R

library(data.table)
library(Hmisc)

### 1. Household identifier

dt[, hh_id := .I]

### 2. Tax burden and per-capita rebate

# Tax paid per good = price increase * post-tax quantity (AIDS method)
dt[, `:=`(
  T_beef      = (p_beef_new      - p_beef)      * q_beef_aids,
  T_pork      = (p_pork_new      - p_pork)      * q_pork_aids,
  T_poultry   = (p_poultry_new   - p_poultry)   * q_poultry_aids,
  T_processed = (p_processed_new - p_processed) * q_processed_aids
)]

dt[, T_h := T_beef + T_pork + T_poultry + T_processed]

# Aggregate tax revenue (population-weighted)
R   <- sum(dt$w_anno * dt$T_h)

# Per-capita rebate (revenue recycled uniformly per person)
r   <- R / Pop
dt[, R_h := r * hh_size]

### 3. Percentage price changes and baseline expenditures

dt[, `:=`(
  dp_beef      = (p_beef_new      - p_beef)      / p_beef,
  dp_pork      = (p_pork_new      - p_pork)      / p_pork,
  dp_poultry   = (p_poultry_new   - p_poultry)   / p_poultry,
  dp_processed = (p_processed_new - p_processed) / p_processed
)]

# Baseline expenditures (kept explicit for clarity)
dt[, `:=`(
  exp0_beef      = exp_beef,
  exp0_pork      = exp_pork,
  exp0_poultry   = exp_poultry,
  exp0_processed = exp_processed
)]

### 4. Compensating variation via Hicksian elasticities

# Recover Marshallian elasticities and mean shares from estimation
marshall <- results_meat$price_elasticities
eta      <- results_meat$expenditure_elasticities
w_bar    <- results_meat$mean_shares

# Slutsky equation: H_ij = e_ij + w_j * eta_i
H <- matrix(0, 4, 4)
for (i in 1:4) {
  for (j in 1:4) {
    H[i, j] <- marshall[i, j] + w_bar[j] * eta[i]
  }
}
rownames(H) <- colnames(H) <- c("beef", "pork", "poultry", "processed")

# Hicksian quantity changes (proportional) for each good
dt[, `:=`(
  dqH_beef =
    H["beef", "beef"]        * dp_beef +
    H["beef", "pork"]        * dp_pork +
    H["beef", "poultry"]     * dp_poultry +
    H["beef", "processed"]   * dp_processed,
  
  dqH_pork =
    H["pork", "beef"]        * dp_beef +
    H["pork", "pork"]        * dp_pork +
    H["pork", "poultry"]     * dp_poultry +
    H["pork", "processed"]   * dp_processed,
  
  dqH_poultry =
    H["poultry", "beef"]     * dp_beef +
    H["poultry", "pork"]     * dp_pork +
    H["poultry", "poultry"]  * dp_poultry +
    H["poultry", "processed"] * dp_processed,
  
  dqH_processed =
    H["processed", "beef"]      * dp_beef +
    H["processed", "pork"]      * dp_pork +
    H["processed", "poultry"]   * dp_poultry +
    H["processed", "processed"] * dp_processed
)]

# CV welfare term per good: dp + dqH + dp*dqH (second-order approximation)
dt[, `:=`(
  welfare_beef      = dp_beef      + dqH_beef      + dp_beef      * dqH_beef,
  welfare_pork      = dp_pork      + dqH_pork      + dp_pork      * dqH_pork,
  welfare_poultry   = dp_poultry   + dqH_poultry   + dp_poultry   * dqH_poultry,
  welfare_processed = dp_processed + dqH_processed + dp_processed * dqH_processed
)]

# CV per good = baseline expenditure * welfare term
dt[, `:=`(
  CV_beef      = exp0_beef      * welfare_beef,
  CV_pork      = exp0_pork      * welfare_pork,
  CV_poultry   = exp0_poultry   * welfare_poultry,
  CV_processed = exp0_processed * welfare_processed
)]

dt[, CV_h := CV_beef + CV_pork + CV_poultry + CV_processed]

total_CV       <- sum(dt$CV_h * dt$w_anno)
CV_per_capita  <- total_CV / Pop

### 5. Consumer surplus (trapezoid approximation)

# Absolute price changes
dt[, `:=`(
  Dp_beef      = p_beef_new      - p_beef,
  Dp_pork      = p_pork_new      - p_pork,
  Dp_poultry   = p_poultry_new   - p_poultry,
  Dp_processed = p_processed_new - p_processed
)]

# Sum of baseline and post-tax quantities (for trapezoid area)
dt[, `:=`(
  q_sum_beef      = q_beef_aids      + q_beef_0,
  q_sum_pork      = q_pork_aids      + q_pork_0,
  q_sum_poultry   = q_poultry_aids   + q_poultry_0,
  q_sum_processed = q_processed_aids + q_processed_0
)]

# CS change = -0.5 * price change * quantity sum
dt[, `:=`(
  CS_beef      = 0.5 * Dp_beef      * q_sum_beef,
  CS_pork      = 0.5 * Dp_pork      * q_sum_pork,
  CS_poultry   = 0.5 * Dp_poultry   * q_sum_poultry,
  CS_processed = 0.5 * Dp_processed * q_sum_processed
)]

dt[, CS_h := CS_beef + CS_pork + CS_poultry + CS_processed]

total_CS           <- sum(dt$CS_h * dt$w_anno)
CS_loss_per_capita <- total_CS / Pop
net_CS_per_capita  <- CS_loss_per_capita - r

# Net CS after rebate
dt[, net_CS_h := CS_h - R_h]

### 6. Net welfare effect

dt[, net_welfare       := CV_h - R_h]
dt[, net_welfare_ratio := net_welfare / sp_tot_str]
dt[, CV_h_ratio        := CV_h / sp_tot_str]

### 7. GHG emissions before and after tax

dt[, ghg_0 :=
     q_beef_0      * ghg["beef"]      +
     q_pork_0      * ghg["pork"]      +
     q_poultry_0   * ghg["poultry"]   +
     q_processed_0 * ghg["processed"]]

dt[, ghg_1 :=
     q_beef_aids      * ghg["beef"]      +
     q_pork_aids      * ghg["pork"]      +
     q_poultry_aids   * ghg["poultry"]   +
     q_processed_aids * ghg["processed"]]

total_ghg_0            <- sum(dt$w_anno * dt$ghg_0)
total_ghg_1            <- sum(dt$w_anno * dt$ghg_1)
total_ghg_reduction    <- total_ghg_1 - total_ghg_0
ghg_reduction_per_capita <- total_ghg_reduction / Pop

### 8. Summary by expenditure quintile

dt[, quintile := cut2(sp_tot_str, g = 5, weights = w_anno)]
dt[, quintile := factor(quintile, levels = levels(quintile),
                        labels = paste0("Q", 1:5))]

results_quintile <- dt[, .(
  mean_CV_h        = weighted.mean(CV_h,            w_anno),
  mean_net_welfare = weighted.mean(net_welfare,      w_anno),
  mean_CV_h_ratio  = weighted.mean(CV_h_ratio,       w_anno),
  mean_net_ratio   = weighted.mean(net_welfare_ratio, w_anno),
  share_losers     = weighted.mean(net_welfare > 0,  w_anno)
), by = quintile][order(quintile)]

cat("Welfare results by expenditure quintile:\n")
print(results_quintile)

cat(sprintf("\nAggregate tax revenue:         EUR %.2f\n", R))
cat(sprintf("Per-capita rebate:             EUR %.4f\n", r))
cat(sprintf("CV per capita:                 EUR %.4f\n", CV_per_capita))
cat(sprintf("CS loss per capita:            EUR %.4f\n", CS_loss_per_capita))
cat(sprintf("GHG reduction per capita:      %.4f kg CO2e\n", ghg_reduction_per_capita))