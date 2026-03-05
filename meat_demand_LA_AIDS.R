# Meat Demand Analysis - Linear Approximation AIDS (LA-AIDS)
# Author: Giulia Mencarelli
# Data: Italian Household Budget Survey (HBS) and Regional Price Data


# Packages
if (!requireNamespace("data.table",   quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("micEconAids",  quietly = TRUE)) install.packages("micEconAids")
if (!requireNamespace("readxl",       quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("MASS",         quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("Hmisc",        quietly = TRUE)) install.packages("Hmisc")
if (!requireNamespace("writexl",      quietly = TRUE)) install.packages("writexl")

library(data.table)
library(micEconAids)
library(readxl)
library(MASS)
library(Hmisc)
library(writexl)

# File paths
hbs_path <- "/Users/giuliamencarelli/Documents/R/micro-ready final.xlsx"
price_path <- "/Users/giuliamencarelli/Documents/VS Code/prices_prep_final/filtered_prices.xlsx"

### 1. Load data

hbs    <- as.data.table(read_excel(hbs_path))
prices <- as.data.table(read_excel(price_path))

### 2. Prepare regional price data

# Converts Italian month abbreviations to numbers
month_map <- c("gen" = 1, "feb" = 2, "mar" = 3, "apr" = 4,
               "mag" = 5, "giu" = 6, "lug" = 7, "ago" = 8,
               "set" = 9, "ott" = 10, "nov" = 11, "dic" = 12)

prices[, month_num := month_map[Month]]

# Reshapes from long to wide
prices_wide <- dcast(prices, Province + month_num ~ Category, value.var = "Price")

setnames(prices_wide,
         old = c("Carne Fresca Bovino Adulto, Primo Taglio (1000 Gr)",
                 "Carne Fresca Suina Con Osso (1000 Gr)",
                 "Petto Di Pollo (1000 Gr)",
                 "Prosciutto Cotto (1000 Gr)",
                 "Prosciutto Crudo (1000 Gr)",
                 "Pancetta In Confezione (1000 Gr)"),
         new = c("p_beef_raw", "p_pork_raw", "p_poultry_raw",
                 "p_prosc_cotto", "p_prosc_crudo", "p_pancetta"))

# Processed meat price is equal to average of available processed items
# (na.rm = TRUE handles Firenze where Pancetta is missing)
prices_wide[, `:=`(
  p_beef      = p_beef_raw,
  p_pork      = p_pork_raw,
  p_poultry   = p_poultry_raw,
  p_processed = rowMeans(.SD, na.rm = TRUE)
), .SDcols = c("p_prosc_cotto", "p_prosc_crudo", "p_pancetta")]

setnames(prices_wide, c("Province", "month_num"), c("region", "month"))

prices_clean <- prices_wide[, .(month, region, p_beef, p_pork, p_poultry, p_processed)]
prices_clean <- prices_clean[complete.cases(prices_clean)]

cat("Price data: ", nrow(prices_clean), "observations,",
    uniqueN(prices_clean$region), "regions,",
    uniqueN(prices_clean$month), "months\n")

### 3. Prepares HBS meat expenditure and budget shares

# Aggregates HBS items into 4 meat categories
hbs[, `:=`(
  exp_beef      = d061_mensile,
  exp_pork      = d062_mensile,
  exp_poultry   = d064_mensile,
  exp_processed = d058_mensile + d059_mensile + d066_mensile + d068_mensile
)]

hbs[, tot_meat := exp_beef + exp_pork + exp_poultry + exp_processed]

# Keeps only meat-consuming households
hbs_meat <- hbs[tot_meat > 0]

# Budget shares
hbs_meat[, `:=`(
  w_beef      = exp_beef      / tot_meat,
  w_pork      = exp_pork      / tot_meat,
  w_poultry   = exp_poultry   / tot_meat,
  w_processed = exp_processed / tot_meat
)]

# Replaces zero shares with small epsilon and renormalizes
# (avoids log(0) issues in price index computation)
eps        <- 1e-6
share_cols <- c("w_beef", "w_pork", "w_poultry", "w_processed")

hbs_meat[, (share_cols) := lapply(.SD, function(x) ifelse(x <= 0, eps, x)),
         .SDcols = share_cols]

share_sums <- rowSums(hbs_meat[, ..share_cols])
hbs_meat[, (share_cols) := lapply(.SD, function(x) x / share_sums),
         .SDcols = share_cols]

### 4. Merges HBS with prices

hbs_meat[, `:=`(month = as.integer(Month), region = Province)]

meat_data <- merge(hbs_meat, prices_clean, by = c("month", "region"), all.x = TRUE)

# Keeps only observations with complete price data
meat_data_final <- meat_data[complete.cases(meat_data[, .(p_beef, p_pork, p_poultry, p_processed)])]

cat("Final dataset:", nrow(meat_data_final), "observations,",
    uniqueN(meat_data_final$region), "regions\n")

### 5. Estimate LA-AIDS model (homogeneity + symmetry imposed)

share_names      <- c("w_beef", "w_pork", "w_poultry", "w_processed")
price_names      <- c("p_beef", "p_pork", "p_poultry", "p_processed")
expenditure_name <- "tot_meat"
n_goods          <- length(share_names)

aids_meat <- aidsEst(
  priceNames  = price_names,
  shareNames  = share_names,
  totExpName  = expenditure_name,
  data        = as.data.frame(meat_data_final),
  method      = "LA",
  priceIndex  = "S",
  hom         = TRUE,
  sym         = TRUE
)

print(summary(aids_meat))

# Extracts coefficients as a flat named vector
coefs     <- coef(aids_meat)
all_coefs <- unlist(coefs)
V         <- vcov(aids_meat)

### 6. Elasticities at sample means (point estimates + bootstrap SE)

mean_shares     <- colMeans(meat_data_final[, ..share_names])
mean_log_prices <- colMeans(log(meat_data_final[, ..price_names]))
P_stone         <- sum(mean_shares * mean_log_prices)

# Function: computes expenditure and Marshallian price elasticities
calc_elasticities <- function(alpha, beta, gamma, shares, logprices, P_stone) {
  n         <- length(shares)
  exp_elas  <- 1 + beta / shares
  price_elas <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      kron_ij <- as.integer(i == j)
      price_elas[i, j] <- -kron_ij +
        gamma[i, j] / shares[i] -
        beta[i] * (logprices[j] - P_stone) / shares[i]
    }
  }
  list(exp = exp_elas, price = price_elas)
}

# Point estimates
alpha_hat <- as.numeric(all_coefs[paste0("alpha.", share_names)])
beta_hat  <- as.numeric(all_coefs[paste0("beta.",  share_names)])
gamma_hat <- matrix(as.numeric(all_coefs[paste0("gamma", 1:n_goods^2)]), n_goods, n_goods)

elas_point     <- calc_elasticities(alpha_hat, beta_hat, gamma_hat,
                                    mean_shares, mean_log_prices, P_stone)
expenditure_elas <- elas_point$exp
price_elas       <- elas_point$price

# Bootstrap standard errors (parametric, 1000 draws)
set.seed(123)
B               <- 1000
exp_elas_draws  <- matrix(NA, B, n_goods)
price_elas_draws <- array(NA, c(B, n_goods, n_goods))

theta_draws <- MASS::mvrnorm(B, mu = unlist(coefs), Sigma = V)

for (b in 1:B) {
  a_b <- theta_draws[b, paste0("alpha.", share_names)]
  b_b <- theta_draws[b, paste0("beta.",  share_names)]
  g_b <- matrix(theta_draws[b, paste0("gamma", 1:n_goods^2)], n_goods, n_goods)
  
  e_b <- calc_elasticities(a_b, b_b, g_b, mean_shares, mean_log_prices, P_stone)
  exp_elas_draws[b, ]     <- e_b$exp
  price_elas_draws[b, , ] <- e_b$price
}

exp_elas_se   <- apply(exp_elas_draws,    2,      sd)
price_elas_se <- apply(price_elas_draws,  c(2, 3), sd)

cat("\nExpenditure elasticities:\n")
print(round(data.frame(Point = expenditure_elas, SE = exp_elas_se,
                       row.names = share_names), 4))

cat("\nOwn-price elasticities:\n")
print(round(data.frame(Point = diag(price_elas), SE = diag(price_elas_se),
                       row.names = share_names), 4))

cat("\nFull price elasticity matrix:\n")
print(round(price_elas, 4))

### 7. Elasticities by expenditure quintile

# Assigns households to quintiles based on total household expenditure (weighted)
meat_data_final[, quintile := cut2(sp_tot_str, g = 5, weights = w_anno)]
meat_data_final[, quintile := factor(quintile, levels = levels(quintile),
                                     labels = paste0("Q", 1:5))]

# Quintile-specific mean shares and log prices
quintile_stats <- meat_data_final[, .(
  mean_w_beef        = mean(w_beef),
  mean_w_pork        = mean(w_pork),
  mean_w_poultry     = mean(w_poultry),
  mean_w_processed   = mean(w_processed),
  mean_logp_beef     = mean(log(p_beef)),
  mean_logp_pork     = mean(log(p_pork)),
  mean_logp_poultry  = mean(log(p_poultry)),
  mean_logp_processed = mean(log(p_processed)),
  n_obs = .N
), by = quintile][order(quintile)]

# Computes elasticities per quintile using pooled model coefficients
quintile_elasticities <- list()

for (q in paste0("Q", 1:5)) {
  q_data    <- quintile_stats[quintile == q]
  shares_q  <- c(q_data$mean_w_beef, q_data$mean_w_pork,
                 q_data$mean_w_poultry, q_data$mean_w_processed)
  lprices_q <- c(q_data$mean_logp_beef, q_data$mean_logp_pork,
                 q_data$mean_logp_poultry, q_data$mean_logp_processed)
  
  quintile_elasticities[[q]] <- calc_elasticities(
    alpha_hat, beta_hat, gamma_hat, shares_q, lprices_q,
    P_stone = sum(shares_q * lprices_q)
  )
}

# Collects into summary matrices
exp_elas_by_quintile   <- t(sapply(quintile_elasticities, `[[`, "exp"))
own_price_by_quintile  <- t(sapply(quintile_elasticities, function(x) diag(x$price)))
colnames(exp_elas_by_quintile) <- colnames(own_price_by_quintile) <-
  c("Beef", "Pork", "Poultry", "Processed")

cat("\nExpenditure elasticities by quintile:\n")
print(round(exp_elas_by_quintile, 4))

cat("\nOwn-price elasticities by quintile:\n")
print(round(own_price_by_quintile, 4))

### 8. Export results to Excel

# Overall elasticities
exp_results <- data.table(
  Meat  = share_names,
  Point = round(expenditure_elas, 4),
  SE    = round(exp_elas_se, 4),
  Lower = round(expenditure_elas - 1.96 * exp_elas_se, 4),
  Upper = round(expenditure_elas + 1.96 * exp_elas_se, 4)
)

own_price_results <- data.table(
  Meat  = share_names,
  Point = round(diag(price_elas), 4),
  SE    = round(diag(price_elas_se), 4),
  Lower = round(diag(price_elas) - 1.96 * diag(price_elas_se), 4),
  Upper = round(diag(price_elas) + 1.96 * diag(price_elas_se), 4)
)

cross_price_results <- data.table(
  expand.grid(From = share_names, To = share_names),
  Point = as.vector(round(price_elas, 4)),
  SE    = as.vector(round(price_elas_se, 4))
)
cross_price_results[, Lower := round(Point - 1.96 * SE, 4)]
cross_price_results[, Upper := round(Point + 1.96 * SE, 4)]

# Quintile elasticities
exp_quintile_table      <- as.data.table(exp_elas_by_quintile,  keep.rownames = "Quintile")
own_price_quintile_table <- as.data.table(own_price_by_quintile, keep.rownames = "Quintile")

cross_price_quintile_table <- rbindlist(lapply(paste0("Q", 1:5), function(q) {
  m  <- round(quintile_elasticities[[q]]$price, 4)
  colnames(m) <- rownames(m) <- c("Beef", "Pork", "Poultry", "Processed")
  dt <- as.data.table(m, keep.rownames = "From")
  dt[, Quintile := q]
  setcolorder(dt, c("Quintile", "From"))
  dt
}))

write_xlsx(
  list(
    ExpElast_Overall     = exp_results,
    OwnPrice_Overall     = own_price_results,
    CrossPrice_Overall   = cross_price_results,
    ExpElast_Quintile    = exp_quintile_table,
    OwnPrice_Quintile    = own_price_quintile_table,
    CrossPrice_Quintile  = cross_price_quintile_table,
    QuintileStats        = quintile_stats
  ),
  path = "LA_AIDS_Elasticities.xlsx"
)

cat("\nResults exported to LA_AIDS_Elasticities.xlsx\n")