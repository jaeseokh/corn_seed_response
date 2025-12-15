# Code/src/functions_simulation_basis.R

library(data.table)
library(mgcv)
library(stats)

# --- Helper 1: Detrending History ---
# Projects historical neighbor yields to the current technology level (Target Year)
get_trend_adjusted_yields <- function(history_dt, target_year) {
  
  # history_dt must have columns: year (numeric), yield (numeric)
  # Returns data.table with added column 'trend_adj_yield'
  
  # 1. Fit linear trend (Genetic Gain)
  model_trend <- lm(yield ~ year, data = history_dt)
  
  # 2. Extract the trend coefficient (Slope ~ +2 bu/ac)
  beta_tech <- coef(model_trend)["year"]
  
  # 3. Calculate Project Yield
  # Formula: Detrended_Y_k = Y_k + beta * (TargetYear - Year_k)
  history_dt[, trend_adj_yield := yield + beta_tech * (target_year - year)]
  
  return(history_dt)
}

# --- Helper 2: Predict The "Shape" ---
# Generates the yield response vector for a specific field holding other inputs constant
get_field_shape <- function(gam_model, field_data, s_range) {
  
  # gam_model: The fitted GAM object for this field
  # field_data: The original data used to fit the model (to calculate medians)
  # s_range: Vector of seeding rates to predict over
  
  # 1. Create a "Representative" observation (Median of all other variables)
  # We use median to represent the "Typical" soil/N condition of the field
  median_data <- field_data[, lapply(.SD, median, na.rm = TRUE)]
  
  # 2. Replicate this observation for every step in s_range
  pred_dt <- median_data[rep(1, length(s_range))]
  pred_dt[, s_rate := s_range] # Overwrite s_rate with the range we want to test
  
  # 3. Predict Yield
  # type = "response" gives us yield in bu/ac
  predicted_yields <- predict(gam_model, newdata = pred_dt, type = "response")
  
  return(as.numeric(predicted_yields))
}

# --- Main Function: Generate Simulation Basis ---
# Runs the loop over 30 years of history to find the distribution of optimal rates
generate_field_basis <- function(
    gam_model,             
    field_data,            # Needed to define the "Shape"
    scym_history,          # Data.table of neighbor yields
    trial_year,            
    seed_price = 3.50,     # Cost per 1k seeds 
    corn_price = 4.50,     # Price per bu
    s_range = seq(18, 42, by = 0.5) 
) {
  
  # 1. Get the "Shape" (Trial DNA)
  trial_curve_y <- get_field_shape(gam_model, field_data, s_range)
  
  # 2. Prepare History (Climate DNA)
  basis_dt <- copy(scym_history)
  basis_dt <- get_trend_adjusted_yields(basis_dt, target_year = trial_year)
  
  # 3. Define the Anchor (Trial Year Performance)
  # We use the raw SCYM yield of the trial year to represent "Realized Reality"
  anchor_yield <- basis_dt[year == trial_year, yield]
  
  # Safety: If trial year data is missing in SCYM, return NULL
  if(length(anchor_yield) == 0) return(NULL)
  
  # 4. Simulation Loop
  results_list <- list()
  
  for(k in 1:nrow(basis_dt)) {
    
    hist_year <- basis_dt$year[k]
    hist_yield_adj <- basis_dt$trend_adj_yield[k]
    
    # A. Calculate Scalar (theta)
    # How good was this historical year compared to the trial year?
    theta_k <- hist_yield_adj / anchor_yield
    
    # Clip extreme scalars to prevent unrealistic physics (e.g., 20% or 200%)
    theta_k <- max(min(theta_k, 2.0), 0.3) 
    
    # B. Create Synthetic Curve
    synthetic_curve <- trial_curve_y * theta_k
    
    # C. Economic Optimization
    revenue <- synthetic_curve * corn_price
    cost    <- s_range * seed_price
    profit  <- revenue - cost
    
    # Find Optimal Rate
    opt_idx <- which.max(profit)
    
    # D. Store Result
    results_list[[k]] <- data.table(
      sim_year = hist_year,
      theta_scalar = theta_k,
      sim_optimal_s = s_range[opt_idx],  # The Rational Target
      sim_profit_max = max(profit),
      sim_yield_max = max(synthetic_curve)
    )
  }
  
  return(rbindlist(results_list))
}