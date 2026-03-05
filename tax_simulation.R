# Carbon Tax Simulation on Meat Demand
# Author: Giulia Mencarelli
#
# Simulates the effect of a GHG-based carbon tax on household meat consumption
# using two approaches:
#   A) Elasticity-based approximation
#   B) Full LA-AIDS structural recomputation
#
# Requires: results_meat object from meat_demand_LA_AIDS.R


library(data.table)

dt <- as.data.table(results_meat$data)

# 1. Environmental impact coefficients and tax rates

# GHG emissions per kg of meat (kg CO2e / kg), source: [add your source]
ghg <- c(beef = 56.3, pork = 8.39, poultry = 10.9, processed = 8.39)

# Carbon price (EUR per kg CO2e)
co2_price <- 0.08

# Specific tax per kg
tax_perkg <- ghg * co2_price

### 2. Compute new post-tax prices

# Ad-valorem tax rates (tax as share of baseline price)
dt[, `:=`(
  tau_beef      = tax_perkg["beef"]      / p_beef,
  tau_pork      = tax_perkg["pork"]      / p_pork,
  tau_poultry   = tax_perkg["poultry"]   / p_poultry,
  tau_processed = tax_perkg["processed"] / p_processed
)]

# Post-tax prices
dt[, `:=`(
  p_beef_new      = p_beef      * (1 + tau_beef),
  p_pork_new      = p_pork      * (1 + tau_pork),
  p_poultry_new   = p_poultry   * (1 + tau_poultry),
  p_processed_new = p_processed * (1 + tau_processed)
)]

### 3. Baseline quantities

dt[, `:=`(
  q_beef_0      = exp_beef      / p_beef,
  q_pork_0      = exp_pork      / p_pork,
  q_poultry_0   = exp_poultry   / p_poultry,
  q_processed_0 = exp_processed / p_processed
)]

goods <- c("beef", "pork", "poultry", "processed")

### 4. Method A: Elasticity-based quantity changes

PE <- results_meat$price_elasticities
EE <- results_meat$expenditure_elasticities

# Log price changes
dt[, `:=`(
  dlnp_beef      = log(p_beef_new      / p_beef),
  dlnp_pork      = log(p_pork_new      / p_pork),
  dlnp_poultry   = log(p_poultry_new   / p_poultry),
  dlnp_processed = log(p_processed_new / p_processed)
)]

# Predicted log quantity change = sum of (price elasticity * log price change)
for (i in goods) {
  e_i <- PE[paste0("w_", i), ]
  
  dt[, paste0("dlnq_", i) :=
       e_i["p_beef"]      * dlnp_beef  +
       e_i["p_pork"]      * dlnp_pork  +
       e_i["p_poultry"]   * dlnp_poultry +
       e_i["p_processed"] * dlnp_processed]
  
  dt[, paste0("q_", i, "_elas") :=
       get(paste0("q_", i, "_0")) * exp(get(paste0("dlnq_", i)))]
}

### 5. Method B: Full LA-AIDS structural recomputation

alpha <- results_meat$coefficients$alpha
beta  <- results_meat$coefficients$beta
gamma <- results_meat$coefficients$gamma

# Stone price index under new prices
dt[, P_stone_new :=
     w_beef      * log(p_beef_new)      +
     w_pork      * log(p_pork_new)      +
     w_poultry   * log(p_poultry_new)   +
     w_processed * log(p_processed_new)]

# New budget shares from AIDS share equations
for (i in seq_along(goods)) {
  g <- goods[i]
  dt[, paste0("w_", g, "_new") :=
       alpha[i] +
       gamma[i, 1] * log(p_beef_new)      +
       gamma[i, 2] * log(p_pork_new)      +
       gamma[i, 3] * log(p_poultry_new)   +
       gamma[i, 4] * log(p_processed_new) +
       beta[i] * log(tot_meat / exp(P_stone_new))]
}

# Renormalize shares to sum to 1
dt[, share_sum := w_beef_new + w_pork_new + w_poultry_new + w_processed_new]
for (g in goods) {
  dt[, paste0("w_", g, "_new") := get(paste0("w_", g, "_new")) / share_sum]
}

# New expenditures and quantities
for (g in goods) {
  dt[, paste0("exp_", g, "_new") := get(paste0("w_",   g, "_new")) * tot_meat]
  dt[, paste0("q_",   g, "_aids") := get(paste0("exp_", g, "_new")) /
       get(paste0("p_",   g, "_new"))]
}

### 6. Summary of quantity changes

cat("\nCarbon tax simulation results (mean quantity changes):\n\n")
cat(sprintf("  %-12s  %10s  %10s  %10s\n", "Meat", "Baseline", "Elas.", "AIDS"))
cat(strrep("-", 48), "\n")

for (g in goods) {
  q0   <- mean(dt[[paste0("q_", g, "_0")]])
  qe   <- mean(dt[[paste0("q_", g, "_elas")]])
  qa   <- mean(dt[[paste0("q_", g, "_aids")]])
  cat(sprintf("  %-12s  %10.4f  %10.4f  %10.4f\n", g, q0, qe, qa))
}