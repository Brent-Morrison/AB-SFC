# --------------------------------------------------------------------------------------------------------------------------
#
# Agent based model
# https://arxiv.org/pdf/1709.05117
#
# 
# --------------------------------------------------------------------------------------------------------------------------

# Initialization
set.seed(123) # Ensuring reproducibility

# Parameters
num_firms <- 1000         # Number of firms
base_consume_prop <- 0.5   # Baseline propensity to consume
choice_intensity <- 2      # Intensity of choice parameter
price_adj_rate <- 0.1      # Baseline price adjustment parameter
hire_fire_ratio <- 1.3     # The ratio of hiring/firing propensities
fire_prop <- 0.2           # Baseline firing propensity 
hire_prop <- 0.2           # Baseline hiring propensity
div_frac <- 0.02           # Fraction of dividends
max_leverage <- 3          # Bankruptcy threshold / maximum leverage in the economy
firm_revive_rate <- 0.1    # Frequency of firm revival
bankr_support_share <- 0.5 # Share of bankruptcies supported by the firms
consume_infl_react <- 4    # Reaction of consumption to inflation
policy_intensity <- 2.5    # Taylor-like rule parameter - quantifies the intensity of the policy
firm_rate_react <- 50      # Reaction of firms to interest rates
fragility_sens <- 0        # Baseline financial fragility sensitivity
target_infl <- 0.04        # Taylor-like rule parameter - target inflation level
base_interest <- 0.03      # Taylor-like rule parameter - baseline interest rate
ema_param <- 0.2           # Exponentially moving average (EMA) parameter
wage_index <- 1            # Indexation of wages to inflation
tau_R <- 0.5               # Importance of past realised inflation on expected inflation
tau_T <- 0.5               # Importance of target inflation on expected inflation

total_time <- 10000        # Total simulation time
equil_time <- 5000         # Equilibrium period


# Firms' initial production
initial_output <- 0.1 + 0.9 * runif(1)  

# Firm attributes (empty vectors)
prices <- rep(NA, num_firms)      # Prices - attempted sale of output
output <- rep(NA, num_firms)      # Quantity of perishable goods produced
demand <- rep(NA, num_firms)      # Demand for goods
wages  <- rep(1 , num_firms)      # Firm wages paid to employees
cash_bal  <- rep(NA, num_firms)   # Firm cash balance, if negative then debt
profit    <- rep(NA, num_firms)   # Firm profit
is_active <- rep(1, num_firms)    # Active (1) / inactive (0) firm

# Firm attributes (seed data) for each firm
for (i in 1:num_firms) {
  prices[i]   <- 1 + 0.1 * (2 * runif(1) - 1)
  output[i]   <- initial_output + 0.1 * (2 * runif(1) - 1)
  demand[i]   <- initial_output
  wages[i]    <- 1
  cash_bal[i] <- 2 * wages[i] * output[i] * runif(1)
  profit[i]   <- prices[i] * min(demand[i], output[i]) - wages[i] * output[i]
}

# Total household savings (number of firms less total cash?)
total_savings <- num_firms - sum(cash_bal)

if (policy_intensity == 0) {
  target_infl <- 0
  tau_T <- 0
}

# Main Loop
for (t in 1:total_time) {
  avg_output_per_firm  <- sum(output) / num_firms
  unemployment_rate    <- 1 - avg_output_per_firm
  avg_price            <- sum(prices * output) / sum(output)
  avg_wage             <- sum(wages * output) / sum(output)
  unemployment_target  <- exp(wages / avg_wage) / sum(is_active * exp(wages / avg_wage)) / (num_firms * unemployment_rate)
  
  # Central Bank Policy
  inflation_based_on_tax       <- tau_T * target_infl
  inflation_policy_sensitivity <- policy_intensity * (target_infl - inflation_based_on_tax)
  inflation_adjustment         <- max(firm_rate_react * (inflation_policy_sensitivity - inflation_based_on_tax), fragility_sens)
  
  # Firms update prices, production, and wages
  for (i in 1:num_firms) {
    if (is_active[i] == 1) {
      if (cash_bal[i] > -max_leverage * wages[i] * output[i]) {
        wage_adjustment_ratio   <- -cash_bal[i] / (wages[i] * output[i])
        growth_adjustment_plus  <- hire_prop * (1 - inflation_adjustment * wage_adjustment_ratio)
        growth_adjustment_minus <- fire_prop * (1 + inflation_adjustment * wage_adjustment_ratio)
        
        if (output[i] < demand[i]) {
          if (profit[i] > 0) {
            wages[i] <- wages[i] * (1 + price_adj_rate * (1 - inflation_adjustment * wage_adjustment_ratio) * runif(1))
            wages[i] <- min(wages[i], (profit[i] * min(demand[i], output[i])) / output[i])
          }
          output[i] <- output[i] + min(growth_adjustment_plus * (demand[i] - output[i]), unemployment_target[i])
          if (prices[i] < avg_price) prices[i] <- prices[i] * (1 + price_adj_rate * runif(1))
        } else {
          if (profit[i] < 0) {
            wages[i] <- wages[i] * (1 - price_adj_rate * (1 + inflation_adjustment * wage_adjustment_ratio) * runif(1))
          }
          output[i] <- max(0, output[i] - growth_adjustment_minus * (demand[i] - output[i]))
          if (prices[i] > avg_price) prices[i] <- prices[i] * (1 - price_adj_rate * runif(1))
        }
        prices[i] <- prices[i] * (1 + inflation_based_on_tax)
        wages[i]  <- wages[i] * (1 + wage_index * inflation_based_on_tax)
        wages[i]  <- max(wages[i], 0)
      } else {
        is_active[i] <- 0
        total_savings <- total_savings - cash_bal[i]
      }
    }
  }
  
  # Update Unemployment
  unemployment_rate <- 1 - sum(output) / num_firms
  avg_price <- sum(prices * output) / sum(output)
  
  # Revivals
  for (i in 1:num_firms) {
    if (is_active[i] == 0 && runif(1) < firm_revive_rate) {
      output[i] <- unemployment_rate * runif(1)
      is_active[i] <- 1
      profit[i] <- avg_price
      wages[i] <- avg_wage
      cash_bal[i] <- wages[i] * output[i]
    }
  }
}

print("Simulation Complete")




# Dummy SFC set-up
# https://www.levyinstitute.org/pubs/wp_891.pdf
act <- c("Co","Wa","Ca","Mo","NW")
sct <- c("HH", "FI", "BA", "GV", "CB", "RW")
tim <- 1:3

bal <- array(
  data = rep( 0, length(act) * length(sct) * length(tim))
  ,dim = c(length(act), length(sct), length(tim))
  ,dimnames = list(act, sct, tim)
)
bal[,,1] <- c(c(0,0,100,-80,-20,0,0,140,-50,-90,0,0,-240,130,110), rep(0, 15))
bal


# Posting template (Consumption)
txnCo <- bal[,,1]
txnCo[] <- 0
txnCo["Co", c("HH","FI")] <- c(1,-1)
txnCo["Ca", c("HH","FI")] <- c(-1,1)
txnCo


# Posting template (Wages)
txnWa <- bal[,,1]
txnWa[] <- 0 #c(0,-1,1,0,0,0,1,-1,0,0,0,0,0,0,0)
txnWa["Wa", c("HH","FI")] <- c(-1,1)
txnWa["Ca", c("HH","FI")] <- c(1,-1)
txnWa

# Post transactions
bal[,,2] <- bal[,,1] + txnCo * 5 + txnWa * 6
bal[,,2]

sum(abs(colSums(bal[,,1])))
sum(abs(rowSums(bal[,,1])))

bal[,,1][grepl("C", rownames(bal[,,1])),]


# --------------------------------------------------------------------------------------------------------------------------
#
# Agent based model
# https://uwol.github.io/docs/2015-06_CEF2015-322.pdf
# 
# --------------------------------------------------------------------------------------------------------------------------

calculate_new_price <- function(offered_amount_in_last_period, offered_amount_in_penultimate_period,
                                sold_amount_in_last_period, sold_amount_in_penultimate_period,
                                price_in_last_period, price_in_penultimate_period) {
  
  # Nothing sold?
  if (offered_amount_in_last_period > 0 & sold_amount_in_last_period <= 0) {
    return(calculate_decreased_price_explicit(price_in_last_period, price_in_penultimate_period))
  }
  
  # Everything sold?
  if (offered_amount_in_last_period > 0 & sold_amount_in_last_period == offered_amount_in_last_period) {
    return(calculate_raised_price_explicit(price_in_last_period, price_in_penultimate_period))
  }
  
  # Sold less?
  if (offered_amount_in_last_period > 0 & sold_amount_in_penultimate_period > 0 &
      sold_amount_in_last_period < sold_amount_in_penultimate_period &
      offered_amount_in_last_period >= sold_amount_in_penultimate_period) {
    return(calculate_decreased_price_explicit(price_in_last_period, price_in_penultimate_period))
  }
  
  # Sold more?
  if (offered_amount_in_last_period > 0 & sold_amount_in_penultimate_period > 0 &
      sold_amount_in_last_period > sold_amount_in_penultimate_period &
      offered_amount_in_penultimate_period >= sold_amount_in_last_period) {
    return(calculate_raised_price_explicit(price_in_last_period, price_in_penultimate_period))
  }
  
  # Implicit pricing pressure
  return(calculate_raised_price_implicit(price_in_last_period))
}

calculate_decreased_price_explicit <- function(price_in_last_period, price_in_penultimate_period) {
  price_factor <- calculate_price_decreasing_factor(price_in_last_period, price_in_penultimate_period)
  return(price_in_last_period / (1 + price_factor))
}

calculate_raised_price_explicit <- function(price_in_last_period, price_in_penultimate_period) {
  price_factor <- calculate_price_raising_factor(price_in_last_period, price_in_penultimate_period)
  return(price_in_last_period * (1 + price_factor))
}

calculate_price_decreasing_factor <- function(price_in_last_period, price_in_penultimate_period) {
  if (price_in_last_period < price_in_penultimate_period) {
    return(min(max_price_factor, price_factor * price_factor_change))
  }
  if (price_in_last_period > price_in_penultimate_period) {
    return(price_factor / price_factor_change)
  }
}

calculate_price_raising_factor <- function(price_in_last_period, price_in_penultimate_period) {
  if (price_in_last_period > price_in_penultimate_period) {
    return(min(max_price_factor, price_factor * price_factor_change))
  }
  if (price_in_last_period < price_in_penultimate_period) {
    return(price_factor / price_factor_change)
  }
}

calculate_output_maximizing_inputs_iterative <- function(input_types, price_functions_of_input_types, 
                                                         budget, number_of_iterations,
                                                         initialization_value, prices_are_nan,
                                                         needs_all_input_factors_non_zero_for_partial_derivate) {
  if (prices_are_nan & needs_all_input_factors_non_zero_for_partial_derivate) {
    return(rep(0, length(input_types)))
  }
  
  if (budget <= 0) {
    return(rep(0, length(input_types)))
  }
  
  bundle_of_inputs <- setNames(rep(initialization_value, length(input_types)), input_types)
  budget_spent <- 0
  budget_per_iteration <- budget / number_of_iterations
  
  while (TRUE) {
    if (budget_spent + budget_per_iteration > budget) {
      break
    }
    
    optimal_input_type <- find_highest_partial_derivate_per_price(bundle_of_inputs, price_functions_of_input_types)
    
    if (is.null(optimal_input_type)) {
      break
    }
    
    price_function_of_optimal_input_type <- price_functions_of_input_types[[optimal_input_type]]
    old_amount_of_optimal_input_type <- bundle_of_inputs[[optimal_input_type]]
    marginal_price_of_optimal_input_type <- price_function_of_optimal_input_type$get_marginal_price(old_amount_of_optimal_input_type)
    additional_amount_of_input_type <- min(
      budget_per_iteration / marginal_price_of_optimal_input_type,
      max(old_amount_of_optimal_input_type, initialization_value)
    )
    
    bundle_of_inputs[[optimal_input_type]] <- bundle_of_inputs[[optimal_input_type]] + additional_amount_of_input_type
    budget_spent <- budget_spent + marginal_price_of_optimal_input_type * additional_amount_of_input_type
  }
  
  for (input_type in input_types) {
    bundle_of_inputs[[input_type]] <- bundle_of_inputs[[input_type]] - initialization_value
  }
  
  return(bundle_of_inputs)
}


# --------------------------------------------------------------------------------------------------------------------------
#
# SFC model
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2664125
# 
# --------------------------------------------------------------------------------------------------------------------------

# Create the data frame
df <- data.frame(
  row.names = c("Deposits", "Loans", "Cons. Goods", "Cap. Goods", "Bonds", "Reserves", "Advances", "Net Worth"),
  Households = c(80704.1, 0, 0, 0, 0, 0, 0, 80704.1),
  Cons_Firms = c(25000, -52194.4, 2997.4, 53863.6, 0, 0, 0, 29666.6),
  Cap_Firms = c(5000, -1298, 0, 500, 0, 0, 0, 4202),
  Banks = c(-110704, 53492.5, 0, 0, 38273.5, 28564.6, 0, 9626.4),
  Govt = c(0, 0, 0, 0, -66838.1, 0, 0, -66838.1),
  Central_Bank = c(0, 0, 0, 0, 28564.6, -28564.6, 0, 0),
  Total = c(0, 0, 2997.4, 54363.6, 0, 0, 0, 57361)
)

# Convert the data frame to a matrix
mat <- as.matrix(df)

# Print the matrix
print(mat)


# Create the data frame
df1 <- data.frame(
  row.names = c("consumption", "wages", "dole", "cg_inventory", "investments", "depreciation", 
                "taxes", "dep_interest", "bond_nterest", "loan_nterest", "adv_interest", "profits", 
                "cb_profit", "deposits", "advances", "reserves", "gov_bonds", "loans"),
  households = c(-32971.4, 36800, 1280, 0, 0, 0, -7084.7, 200.3, 0, 0, 0, 2367.6, 0, -600.8, 0, 0, 0, 0),
  households_KA = c(32971.4, -25000, 0, 22.3, 0, -4974, -484.8, 62, 0, -388.5, 0, -2208.4, 0, 0, 0, 0, 0, 0),
  cons_firms_CA = c(0, 0, 0, -22.3, -5375, 4974, 0, 0, 0, 0, 0, 220.8, 0, -186.1, 0, 0, 0, 388.5),
  cons_firms_KA = c(0, -5000, 0, 3.7, 5375, 0, -68.7, 12.4, 0, -9.7, 0, -312.8, 0, 0, 0, 0, 0, 0),
  cap_firms_CA = c(0, 0, 0, -3.7, 0, 0, 0, 0, 0, 0, 0, 31.3, 0, -37.2, 0, 0, 0, 9.7),
  cap_firms_KA = c(0, 0, 0, 0, 0, 0, -39.3, -274.7, 0, 398.2, 0, -179.1, 0, 0, 0, 0, 0, 0),
  banks_CA = c(0, 0, 0, 0, 0, 0, 0, 0, 95, 0, 0, 71.7, 0, 824.1, 0, -212.6, -284.9, -398.2),
  banks_KA = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 497.6, 0),
  government = c(0, -6800, -1280, 0, 0, 0, 7677.4, 0, -165.9, 0, 0, 0, 70.9, 0, 0, 0, 0, 0),
  central_bank = c(0, 0, 0, 0, 0, 0, 0, 0, 70.9, 0, 0, 0, -70.9, 0, 0, 212.6, -212.6, 0)
)

# Convert the data frame to a matrix
mat1 <- as.matrix(df1)

# Print the matrix
print(mat1)


# --------------------------------------------------------------------------------------------------------------------------
#
# Market function
# 
# --------------------------------------------------------------------------------------------------------------------------

nc <- 12  # Number of consumers
np <- 4   # Number of producers
r <- 5:15 # Range of individual consumer demand
su <- 0.5 # Stock uplift, producer stock uplift over total consumer demand

# Matrix of consumers
cons <- matrix(
  c(1:nc,                            # consumer id
    sample(r, nc, replace = TRUE),   # quantity demanded (q)
    rep(0, nc),                      # producer transacted with (prod) & price transacted at (p, initially nil)
    sample(1:np, nc, replace = TRUE) # random initialisation of preferred producer
    ), 
  ncol = 4, 
  dimnames = list(NULL, c("cons_id", "q", "p", "prod"))
  )
sum(cons[, "q"])

# Expected demand per producer with stock uplift
qpt <- mean(r)*nc/np*(1+su) 

sample((qpt-5):(qpt+5), np, replace = TRUE)

# Matrix of producers
# - producer id
prod <- matrix(
  c(
    1:np, 
    sample((qpt-5):(qpt+5), np, replace = TRUE), 
    rep(0, np),
    runif(np, min = 0.85, max = 1.15)
    ), 
  ncol = 4, 
  dimnames = list(NULL, c("prod_id", "q", "qpost", "p"))
  )
prod[,"qpost"] <- prod[, "q"]

# Random selection of consumer for first sale
s <- sample(1:nc, size = nc, replace = FALSE)

prod <- prod[order(prod[, "p"]), ]
cons
prod

# For each consumer, determine which producer is matched to transact with 
for (i in 1:nrow(cons)) {
  
  # Get the quantity required by consumer i (s is random order of consumer id) 
  q_rqrd <- cons[cons[, "cons_id"] == s[i], "q"]
  
  # If the remaining stock of the producer is equal to or greater than that required by the consumer
  # use that producer, else use that producer and the next cheapest producer
  # TO DO - functionality to model consumer loyalty to prior producer
  q_trans <- rep(NA, nrow(prod))
  for (j in 1:nrow(prod)) {
    if(j == 1) {
      q_trans[j] <- min(q_rqrd, prod[j, "qpost"])
    } else {
      q_trans[j] <- min(q_rqrd - sum(q_trans[1:(j-1)]), prod[j, "qpost"])
    }
  }
  
  # Update the "qpost" balance (the quantity of stock held by each producer)
  prod[, "qpost"] <- prod[, "qpost"] - q_trans
  
  # Remove the original entry from the consumer matrix
  cons <- cons[-which(cons[, "cons_id"] == s[i]),]
  
  # Append the price transacted AT & producer transacted WITH into "cons" matrix
  cons <- rbind(
    cons, 
    matrix( c(rep(s[i], length(q_trans) ), q_trans, prod[, "p"], prod[, "prod_id"] ), ncol = 4)
    )
  
  # Remove the nil balances
  cons <- cons[-which(cons[, "q"] == 0),]

}
cons <- cons[order(cons[, "cons_id"]), ]
prod <- prod[order(prod[, "prod_id"]), ]
cons
prod

# Check balances
sum(cons[, "q"]) == (sum(prod[, "q"]) - sum(prod[, "qpost"]))




# --------------------------------------------------------------------------------------------------------------------------
#
# Get Australian data
# https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-finance-and-wealth/
# https://www.abs.gov.au/statistics/classifications/standard-economic-sector-classifications-australia-sesca/latest-release
# https://www.abs.gov.au/ausstats/abs@.nsf/0/7b54bcb1c8f409ebca25768400828278/$FILE/P5204_2009_Time_Series_Workbook_Listing.xls
# 
# --------------------------------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)

ana_tables <- read.csv("./data/abs_ana_tables.csv")
files <- ana_tables[grep("Financial Assets and Liabilities|Balance Sheet", ana_tables$table), "file_no"]


# Index of required tables
base_url <- "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-finance-and-wealth/"
qtr <- "dec-2024/"


# Gather ABS data
dat_list <- list()  # data
ind_list <- list()  # index
for (f in files) {
  print(f)
  url <- paste0(base_url, qtr, f)
  temp <- tempfile() 
  download.file(url, temp, mode = "wb")
  sheets <- excel_sheets(path = temp)
  
  # Account for files with multiple tabs
  d_list <- list()
  for (j in sheets[grep("Data", sheets)]) {
    print(j)
    # Data
    d <- read_xlsx(path = temp, sheet = j, range = cell_limits(ul = c(10, 1), lr = c(NA, NA)))
    d <- d %>% 
      rename(date = `Series ID`) %>% 
      pivot_longer(!date, names_to = "series_id", values_to = "amount") %>% 
      select(series_id, date, amount) %>% 
      arrange(series_id, date)
    d_list[[j]] <- d
  }
  dat1 <- bind_rows(d_list)
  dat_list[[f]] <- dat1
  
  # Index
  ind1 <- read_xlsx(path = temp, sheet = "Index", col_names = FALSE, range = cell_limits(c(12, 1), c(NA, NA)))
  names(ind1) <- c("data_item", "na1", "na2", "series_type", "series_id", "start", "end", "n_obs", "unit", "data_type", "frequency", "collection_month")
  ind1 <- subset(ind1, select = -c(na1, na2))
  ind1 <- ind1 %>% 
    filter(!is.na(series_type)) %>% 
    separate(data_item, sep = ";", into = c("stock_flow","balance_type","instrument","counterparty"))
  ind1$file_no <- f
  ind_list[[f]] <- ind1
  
}
dat <- bind_rows(dat_list)
ind <- bind_rows(ind_list)

rem <- " borrowed by:| held by:| accepted by:| issued by:| drawn by:"
inst <- ind %>% 
  filter(balance_type %in% c("Assets", "Liabilities")) %>% 
  group_by(balance_type, instrument) %>% 
  summarise(n = n()) %>% 
  mutate(instrument_clean = gsub(rem, "", instrument))
  

write.csv(dat, "./data/ana_data.csv")
write.csv(dat[dat$date == "2024-12-01",], "./data/ana_data24.csv")
write.csv(ind, "./data/ana_index.csv")
write.csv(inst, "./data/ana_instruments.csv")


# --------------------------------------------------------------------------------------------------------------------------
#
# Read data 
# 
# --------------------------------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)

dat <- read.csv("./data/ana_data.csv")
ind <- read.csv("./data/ana_index.csv")
ana_tables <- read.csv("./data/abs_ana_tables.csv")
ref <- read.csv("./data/ana_ref.csv")
rem <- " borrowed by:| held by:| accepted by:| issued by:| drawn by:"


d <- dat %>% 
  left_join(ind, by = join_by(series_id)) %>% 
  left_join(ana_tables[, c("type","sisca","sub_sector","file_no")], by = join_by(file_no)) %>% 
  left_join(ref[ref$lookup_ref == "cp_ss", c("ref1", "ref2")], by = join_by(counterparty == ref2)) %>% 
  select(series_id:counterparty, data_type, file_no:sub_sector, cp_sub_sector = ref1) %>% 
  mutate(instrument_clean1 = gsub(rem, "", instrument)) %>% 
  left_join(ref[ref$lookup_ref == "instrument", c("ref1", "ref2")], by = join_by(instrument_clean1 == ref1)) %>% 
  rename(instrument_clean = ref2) %>% 
  filter(sub_sector != "na") %>% 
  select(-c("stock_flow","instrument", "instrument_clean1")) %>% 
  mutate(amount = if_else(balance_type == "Assets", amount, -amount))


# Unique instruments / standardise name across counterparty
inst <- sort(unique(d$instrument_clean))
dates <- sort(unique(d$date))


# Checks
d_temp <- d[(d$date == '2024-12-01' & d$data_type == 'STOCK_CLOSE' & 
               d$counterparty != "Total (Counterparty sectors)" & d$sub_sector != "na" & 
               d$amount != 0 & d$balance_type != "Change in/Net financial position") &
              !is.na(d$series_id), ]
d_temp %>% group_by(instrument_clean) %>% summarise(amount = sum(amount))  

d1 <- d %>%
  filter(
    balance_type %in% c("Assets", "Liabilities"),
    sub_sector == "household",
    data_type == "STOCK_CLOSE",
    counterparty != "Total (Counterparty sectors)",
    date == "2024-06-01"
  ) %>% 
  select(balance_type, instrument_clean, counterparty, amount) %>% 
  pivot_wider(names_from = counterparty, values_from = amount, values_fn = sum) %>% 
  arrange(balance_type)

d2 <- d %>%
  filter(
    balance_type %in% c("Assets", "Liabilities"),
    sub_sector == "household",
    data_type == "STOCK_CLOSE",
    counterparty != "Total (Counterparty sectors)",
    date == "2023-06-01"
  ) %>% 
  select(balance_type, instrument_clean, counterparty, amount) %>% 
  pivot_wider(names_from = counterparty, values_from = amount, values_fn = sum) %>% 
  arrange(balance_type)

# Change in net assets - calculated
sum(d1[, 3:19], na.rm = TRUE) - sum(d2[, 3:19], na.rm = TRUE) 

# Change in net assets - total assets & total liabs line items
sum( d[(d$date == '2024-06-01' & d$series_id %in% c("A3425684T", "A3431543A")), "amount"] ) - sum( d[(d$date == '2023-06-01' & d$series_id %in% c("A3425684T", "A3431543A")), "amount"] )

# Change in/Net financial position
sum(d[(d$date %in% c('2023-09-01','2023-12-01','2024-03-01','2024-06-01') & d$series_id == "A3427368A"), "amount"])



# Sectoral balance sheet matrix
mat <- d_temp %>% 
  filter(!is.na(series_id)) %>% 
  select(sub_sector, instrument_clean, amount) %>% 
  arrange(sub_sector, instrument_clean) %>% 
  pivot_wider(names_from = sub_sector, values_from = amount, values_fn = sum, values_fill = 0)

rowSums(mat[,2:ncol(mat)])
colSums(mat[,2:ncol(mat)])






# Mis-matches
z <- d_temp %>% #filter(instrument_clean == 'Bonds') %>% 
  mutate(
    abs_amount = abs(amount), 
    cp0 = paste0(sub_sector, cp_sub_sector, ""),
    cp = sapply(cp0, function(x) paste(sort(unlist(strsplit(x, ""))), collapse = ""))
    ) %>% 
  group_by(cp, instrument_clean) %>% 
  mutate(amount1 = sum(amount)) %>% 
  filter(amount1 != 0)




write.csv(z, "temp.csv")
z1 <- "broad_money_instsecuritiser"

