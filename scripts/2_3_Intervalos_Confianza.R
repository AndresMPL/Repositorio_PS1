

reg_intervalo_conf <- function(x, y) {
                  n <- length(y) # Find length of y to use as sample size
                  lm_model <- lm(y ~ x + I(x^2)) # Fit linear model 
  
  # Extract fitted coefficients from model object
  b0 <- lm_model$coefficients[1]
  b1 <- lm_model$coefficients[2]
  b2 <- lm_model$coefficients[3]
  
  # Find SSE and MSE
  sse <- sum((y - lm_model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t_val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y_fit <- b0 + b1*x_new + b2*x_new^2
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y_fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 1000)
  y_fit2 <- b0 + b1*x_new2 + b2*x_new2^2
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y_fit2 + t_val * se)
  slope.lower <- suppressWarnings(y_fit2 - t_val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower , slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, cex = 1, pch = 21, bg = 'gray')
  lines(y_fit2, col = 'black', lwd = 2)
  lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
  
  }

intervalo_conf <- reg_intervalo_conf(tps1_female$age, tps1_female$salario)