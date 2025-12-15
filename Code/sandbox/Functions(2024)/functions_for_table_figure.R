
# Define a function to create plots
create_combined_plot <- function(data, item, y_label, color) {
  ggplot(data %>% filter(Item == item)) +
    geom_line(aes(x = Year, y = Value, linetype = "Unadjusted"), size = 1, color = color) +
    geom_line(aes(x = Year, y = Value_adjusted, linetype = "PPI Adjusted"), size = 1, color = color) +
    scale_linetype_manual(values = c("Unadjusted" = "dashed", "PPI Adjusted" = "solid")) +
    labs(y = y_label, linetype = "") +
    scale_x_continuous(breaks = seq(1996, 2024, by = 4), limits = c(1996, 2024)) +
    theme_minimal() +
     theme(
      legend.position= "none",
         axis.title.x = element_blank(),
      axis.title = element_text(size = 18),       # Increase axis title font size
      axis.text = element_text(size = 16),        # Increase axis text font size
      legend.text = element_text(size = 16)       # Increase legend font size
    )
}


process_info_table <- function(input_tb_binded) {
  # Step 2: Calculate the quadratic fit and yield maximizing seeding rate for each field
  for(i in 1:length(input_tb_binded$ffy_id)){
    
    eval_s <- readRDS(here("Data", "processed", "Analysis_results", paste0(input_tb_binded$ffy_id[i], "_eval_tb.rds")))
    
    # Apply quadratic fit to each element in eval_s
    quad_fit <- lm(yield_hat ~ s_rate + I(s_rate^2), data = eval_s)
    
    # Check the coefficients of the quadratic fit
    coef_2nd <- quad_fit$coef[3]
    
    # Update input_tb_binded with the quadratic fit coefficient
    input_tb_binded[i, `:=`(quad_fit = coef_2nd)]
  }
  
  # Step 3: Generate response type variable based on the conditions
  input_tb_binded <- input_tb_binded[, resp_type := fcase(
    dif_s < 0 & quad_fit < 0 & ymsr == eosr, "A1",  # under_seed & corner sol
    dif_s < 0 & quad_fit < 0 & ymsr != eosr, "A2",
    dif_s > 0 & quad_fit < 0 & ymsr == eosr, "B1",  # over_seed & corner sol
    dif_s > 0 & quad_fit < 0 & ymsr != eosr, "B2",  
    quad_fit > 0, "C",
    default = NA_character_  # Default value if no conditions match
  )]

  # Filter out rows with NA response type
  input_tb_resp <- input_tb_binded[!is.na(resp_type)]
  
  # Convert resp_type to a factor with specified levels
  input_tb_resp <- input_tb_resp[, resp_type := factor(resp_type, levels = c("A1", "A2", "B1", "B2", "C"))]

  return(input_tb_resp)
}
