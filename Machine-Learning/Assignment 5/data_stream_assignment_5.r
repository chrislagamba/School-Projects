
# install.packages("pacman")
pacman::p_load(stream, zoo, ggplot2, gridExtra)
library(rmarkdown)
library(knitr)

set.seed(500)

# Generate data stream
n_before <- 1000
n_after <- 1000
total <- n_before + n_after

stream_before <- rnorm(n_before, mean = 0, sd = sqrt(2))
stream_after <- rnorm(n_after, mean = 2, sd = sqrt(2))
stream <- c(stream_before, stream_after)

# Create labels and predictions
true_labels <- ifelse(stream < 2, 1, 0)
predictions <- rep(1, total)
loss <- ifelse(true_labels == predictions, 0, 1)

# Define window models and error calculations
cummean <- function(x) cumsum(x) / seq_along(x)
cumvar <- function(x) (cumsum(x^2) - cumsum(x)^2 / seq_along(x)) / (seq_along(x) - 1)

window_models <- list(
  cumulative = list(
    error = cummean(loss),
    stdev = sqrt(cumvar(loss))
  ),
  w100 = list(
    error = rollapply(loss, 100, mean, align = "right", fill = NA),
    stdev = rollapply(loss, 100, sd, align = "right", fill = NA)
  ),
  w200 = list(
    error = rollapply(loss, 200, mean, align = "right", fill = NA),
    stdev = rollapply(loss, 200, sd, align = "right", fill = NA)
  ),
  w500 = list(
    error = rollapply(loss, 500, mean, align = "right", fill = NA),
    stdev = rollapply(loss, 500, sd, align = "right", fill = NA)
  )
)

# Detection implementation
alarms <- list()

for (model in names(window_models)) {
  error <- window_models[[model]]$error
  stdev <- window_models[[model]]$stdev
  n <- length(error)
  
  min_error <- rep(NA, n)
  min_stdev <- rep(NA, n)
  warning_alarm <- logical(n)
  drift_alarm <- logical(n)
  
  # Find first valid index with both error and stdev available
  first_valid <- which(!is.na(error) & !is.na(stdev))[1]
  
  if (!is.na(first_valid)) {
    current_min_error <- error[first_valid]
    current_min_stdev <- stdev[first_valid]
    
    for (i in first_valid:n) {
      # Update minimums only with valid values
      if (!is.na(error[i])) {
        current_min_error <- min(current_min_error, error[i], na.rm = TRUE)
      }
      if (!is.na(stdev[i])) {
        current_min_stdev <- min(current_min_stdev, stdev[i], na.rm = TRUE)
      }
      
      min_error[i] <- current_min_error
      min_stdev[i] <- current_min_stdev
      
      # Only evaluate if all components are valid
      if (!is.na(error[i]) && !is.na(stdev[i]) &&
         !is.na(current_min_error) && !is.na(current_min_stdev)) {
        
        threshold_warning <- current_min_error + 2 * current_min_stdev
        threshold_drift <- current_min_error + 3 * current_min_stdev
        current_value <- error[i] + stdev[i]
        
        # Check for valid comparisons
        if (!is.na(current_value)) {
          if (current_value > threshold_drift) {
            drift_alarm[i] <- TRUE
          } else if (current_value > threshold_warning) {
            warning_alarm[i] <- TRUE
          }
        }
      }
    }
  }
  
  alarms[[model]] <- list(
    warning = which(warning_alarm),
    drift = which(drift_alarm)
  )
}

# Visualization
plots <- list()
for (model in names(window_models)) {
  df <- data.frame(
    time = 1:total,
    error = window_models[[model]]$error,
    stdev = window_models[[model]]$stdev
  )
  
  p <- ggplot(df) +
    geom_line(aes(x = time, y = error), color = "blue") +
    geom_vline(xintercept = 1000, color = "red", linetype = "dashed") +
    geom_point(data = df[alarms[[model]]$warning,],
               aes(x = time, y = error), color = "orange", size = 1) +
    geom_point(data = df[alarms[[model]]$drift,],
               aes(x = time, y = error), color = "red", size = 1) +
    labs(title = paste("Window Model:", model),
         x = "Position in Time", y = "Prequential Error") +
    scale_y_continuous(limits = c(0, 0.8)) +
    theme_minimal()
  plots[[model]] <- p
}

grid.arrange(grobs = plots, ncol = 2)

