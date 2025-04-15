cic <- function(data, outcome, primary, alist, blist, criterion = 5, complete) {
  # Initial model
  if(complete == TRUE){data = data[complete.cases(data), c(outcome, primary, alist, blist)]}
  
  base_model <- lm(paste(outcome, "~", primary, "+", paste(alist, collapse = "+"), "+", paste(blist, collapse = "+")), data = data)
  
  base_coeff <- coef(base_model)[primary]
  base_N <- dim(base_model$model)[1]

  cat("Initial full model includes:\n")
  cat("  Primary predictor:", primary, "\n")
  cat("  A-list:", alist, "\n")
  cat("  B-list:", blist, "\n\n")
  
  min_cic <- 0
  
  while (TRUE) {
    min_cic_last <- min_cic
    remove_var <- ""
    
    cic <- rep(NA, length(blist))
    c <- 1
    for (var in blist) {
      new_blist <- setdiff(blist, var)
      
      trimmed_model <- lm(paste(outcome, "~", primary, "+", paste(alist, collapse = "+"), "+", paste(new_blist, collapse = "+")), data = data)
      new_coeff <- coef(trimmed_model)[primary]
      
      cic[c] <- 100 * abs((new_coeff - base_coeff) / base_coeff)
      c <- c + 1
    }
    min_cic <- cic[which.min(cic)]
    if(min_cic < criterion){remove_var = blist[which.min(cic)]}
    
    if (remove_var == "") break
    
    cat("Removing", remove_var, "CIC =", round(min_cic, 2), "%\n")
    
    blist <- setdiff(blist, remove_var)
    #base_model <- lm(paste(outcome, "~", primary, "+", paste(alist, collapse = "+"), "+", paste(blist, collapse = "+")), data = data)
    #base_coeff <- coef(base_model)[primary]
   
  }
  
  final_model <- lm(paste(outcome, "~", primary, "+", paste(alist, collapse = "+"), "+", paste(blist, collapse = "+")), data = data)
  final_coeff <- coef(final_model)[primary]
  final_N <- dim(final_model$model)[1]
  
  cat("\nFinal model includes:\n")
  cat("  Primary predictor:", primary, "\n")
  cat("  A-list:", alist, "\n")
  cat("  B-list:", paste(blist, collapse = ", "), "\n\n")
  
  cat("Coefficients for", primary, ":\n")
  cat("  Initial model (N =", base_N, "):", base_coeff, "\n")
  cat("  Final model (N =", final_N, "):", coef(final_model)[primary], "\n")
  cat("  Absolute percent change in coefficient:", round(min_cic_last, 2), "%\n")
  
  return(final_model)
}


