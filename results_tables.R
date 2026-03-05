
# Results Tables and Export
# Author: Giulia Mencarelli
#
# Assembles all policy-relevant results into labelled tables and exports
# them to Excel. Tables are organised as follows:
#   A. Demand responses (elasticities)
#   B. Quantity responses to the carbon tax
#   C. Environmental impacts
#   D. Demographic heterogeneity
#   E. Policy efficiency
#   F. Welfare effects
#
# Requires: dt from welfare_analysis.R (includes ghg_0, ghg_1, CV_h,
#           net_welfare, quintile)
#           results_meat from meat_demand_LA_AIDS.R
#           results_quintile, R, Pop, ghg from welfare_analysis.R

library(data.table)
library(openxlsx)

setDT(dt)

goods       <- c("beef", "pork", "poultry", "processed")
good_labels <- c(beef = "Beef", pork = "Pork",
                 poultry = "Poultry", processed = "Processed meat")

# Convenience wrapper for population-weighted means
wmean <- function(x) weighted.mean(x, dt$w_anno, na.rm = TRUE)

# A. Demand responses

# Table A1: Expenditure and own-price elasticities at sample means
table_A1_elasticities <- data.table(
  good                   = good_labels,
  expenditure_elasticity = results_meat$expenditure_elasticities,
  own_price_elasticity   = diag(results_meat$price_elasticities)
)

# B. Quantity responses to the carbon tax

# Table B1: Household-level total quantities before and after tax
table_B1_quantities <- rbindlist(lapply(goods, function(g) {
  q0 <- wmean(dt[[paste0("q_", g, "_0")]])
  q1 <- wmean(dt[[paste0("q_", g, "_aids")]])
  data.table(
    meat       = good_labels[g],
    q_baseline = q0,
    q_post_tax = q1,
    delta_q    = q1 - q0,
    pct_change = 100 * (q1 - q0) / q0
  )
}))

# Table B2: Per-capita quantities
table_B2_percapita <- rbindlist(lapply(goods, function(g) {
  q0 <- wmean(dt[[paste0("q_", g, "_0")]]    / dt$hh_size)
  q1 <- wmean(dt[[paste0("q_", g, "_aids")]] / dt$hh_size)
  data.table(
    meat          = good_labels[g],
    q_pc_baseline = q0,
    q_pc_post_tax = q1,
    delta_pc      = q1 - q0
  )
}))

# C. Environmental impacts

# ghg_0 and ghg_1 (household-level emissions) were created in welfare_analysis.R
total_ghg_0 <- sum(dt$w_anno * dt$ghg_0)
total_ghg_1 <- sum(dt$w_anno * dt$ghg_1)

# Table C1: Aggregate and per-capita emissions
table_C1_emissions <- data.table(
  total_baseline     = total_ghg_0,
  total_post_tax     = total_ghg_1,
  total_reduction    = total_ghg_0 - total_ghg_1,
  pct_reduction      = 100 * (total_ghg_0 - total_ghg_1) / total_ghg_0,
  percapita_baseline = total_ghg_0 / Pop,
  percapita_post_tax = total_ghg_1 / Pop
)

# Table C2: Emissions by meat type and their share of total reduction
table_C2_emissions_by_meat <- rbindlist(lapply(goods, function(g) {
  e0 <- sum(dt$w_anno * dt[[paste0("q_", g, "_0")]]    * ghg[g])
  e1 <- sum(dt$w_anno * dt[[paste0("q_", g, "_aids")]] * ghg[g])
  data.table(
    meat             = good_labels[g],
    baseline         = e0,
    reduction        = e0 - e1,
    contribution_pct = 100 * (e0 - e1) / (total_ghg_0 - total_ghg_1)
  )
}))

# D. Demographic heterogeneity

# Table D1: Quantity and emission changes by household structure
table_D1_hh_type <- dt[, .(
  delta_q   = weighted.mean(
    (q_beef_aids + q_pork_aids + q_poultry_aids + q_processed_aids) -
      (q_beef_0    + q_pork_0    + q_poultry_0    + q_processed_0),
    w = w_anno, na.rm = TRUE),
  delta_ghg = weighted.mean(ghg_1 - ghg_0, w = w_anno, na.rm = TRUE)
), by = hh_type]

# Table D2: Quantity and emission changes by dominant life-cycle group
table_D2_lifecycle <- dt[, .(
  delta_q      = weighted.mean(
    (q_beef_aids + q_pork_aids + q_poultry_aids + q_processed_aids) -
      (q_beef_0    + q_pork_0    + q_poultry_0    + q_processed_0),
    w = w_anno, na.rm = TRUE),
  delta_ghg    = weighted.mean(ghg_1 - ghg_0,
                               w = w_anno, na.rm = TRUE),
  baseline_q   = weighted.mean(
    q_beef_0 + q_pork_0 + q_poultry_0 + q_processed_0,
    w = w_anno, na.rm = TRUE),
  baseline_ghg = weighted.mean(ghg_0, w = w_anno, na.rm = TRUE)
), by = dominant_adult_final][
  , `:=`(
    pct_delta_q   = 100 * delta_q   / baseline_q,
    pct_delta_ghg = 100 * delta_ghg / baseline_ghg
  )][
    , c("baseline_q", "baseline_ghg") := NULL
  ]

# E. Policy efficiency

# Table E1: Cost per tonne of CO2 abated
table_E1_efficiency <- data.table(
  total_revenue      = R,
  emission_reduction = total_ghg_0 - total_ghg_1,
  cost_per_ton_CO2   = R / (total_ghg_0 - total_ghg_1)
)

# F. Welfare effects

# Table F1: Aggregate welfare metrics
table_F1_welfare <- data.table(
  mean_CV          = wmean(dt$CV_h),
  mean_net_welfare = wmean(dt$net_welfare),
  mean_CV_ratio    = wmean(dt$CV_h_ratio),
  mean_net_ratio   = wmean(dt$net_welfare_ratio)
)

# Table F2: Welfare by income quintile (from welfare_analysis.R)
table_F2_quintiles <- results_quintile

# Export to Excel and RData

policy_tables <- list(
  A1_elasticities      = table_A1_elasticities,
  B1_quantities        = table_B1_quantities,
  B2_percapita         = table_B2_percapita,
  C1_emissions         = table_C1_emissions,
  C2_emissions_by_meat = table_C2_emissions_by_meat,
  D1_hh_type           = table_D1_hh_type,
  D2_lifecycle         = table_D2_lifecycle,
  E1_efficiency        = table_E1_efficiency,
  F1_welfare           = table_F1_welfare,
  F2_quintiles         = table_F2_quintiles
)

save(policy_tables, file = "policy_results_tables.RData")

wb <- createWorkbook()
for (nm in names(policy_tables)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, policy_tables[[nm]])
}
saveWorkbook(wb, file = "policy_results_tables.xlsx", overwrite = TRUE)

cat("Results exported to policy_results_tables.xlsx\n")