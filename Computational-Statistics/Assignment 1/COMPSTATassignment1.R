library(stream)
library(readr)
library(visdat)
library(dplyr)
library(animation)


## COMPSTAT Assignment 1 group project ##
## Diana Rios and Chris LaGamba ##


# load the dataset
bidmc14 <- read_csv("bidmc_14_Numerics.csv")
View(bidmc14)


# take a look at the data
head(bidmc14) 
str(bidmc14) # all columns col_double
summary(bidmc14) # HR min 93, HR max 97, RESP min 12, RESP max 34
vis_miss(bidmc14) # visualize missing values; no missing values


# separate the HR and RESP columns for use with stream
hrresp_bidmc14 <- bidmc14 %>%
  select(HR, RESP)
# convert to a stream object
stream_hrresp <- DSD_Memory(hrresp_bidmc14)

# defining the grid parameters
num_bins <- 10 # Number of bins for each dimension
hr_range <- range(hrresp_bidmc14$HR)
resp_range <- range(hrresp_bidmc14$RESP)

# create the grids
hr_bins <- seq(hr_range[1], hr_range[2], length.out = num_bins + 1)
resp_bins <- seq(resp_range[1], resp_range[2], length.out = num_bins + 1)

# initializing the counters for grid cells
grid_counters <- matrix(0, nrow = num_bins, ncol = num_bins)

# LANDMARK WINDOW (fixed start time, updates as new data arrives)
# Initialize landmark window
landmark_data <- data.frame()

# Create animation
saveGIF({
  for (i in 1:1000) {
    new_point <- get_points(stream_hrresp, n = 1)
    
    # If the stream is exhausted, reset it
    if (nrow(new_point) == 0) {
      reset_stream(stream_hrresp)
      new_point <- get_points(stream_hrresp, n = 1)
    }
    
    # Add new point to the window
    landmark_data <- rbind(landmark_data, new_point)
    
    # Plot the landmark window
    plot(landmark_data$HR, landmark_data$RESP, 
         xlab = "HR", ylab = "RESP", 
         main = "Landmark Window", 
         xlim = range(hrresp_bidmc14$HR), 
         ylim = range(hrresp_bidmc14$RESP))
  }
}, interval = 0.1, movie.name = "landmark_window.gif")

# update_landmark <- function(new_point, grid_counters) {
#   # Update counters for the new point
#   hr_bin <- findInterval(new_point$HR, hr_bins)
#   resp_bin <- findInterval(new_point$RESP, resp_bins)
#   grid_counters[hr_bin, resp_bin] <- grid_counters[hr_bin, resp_bin] + 1
#   return(grid_counters)
# }


# SLIDING WINDOW (fixed size, moves forward as new data arrives)



# WEIGHTED WINDOW (older data points are given less weight)



# FADING WINDOW (older data points are exponentially decayed)



# Process the stream
for (i in 1:nrow(hrresp_bidmc14)) {
  new_point <- hrresp_bidmc14[i, ]
  
  # Find bin indices
  hr_bin <- findInterval(new_point$HR, hr_bins)
  resp_bin <- findInterval(new_point$RESP, resp_bins)
  
  # Ensure bin indices are within valid range
  if (hr_bin > 0 && hr_bin <= num_bins && resp_bin > 0 && resp_bin <= num_bins) {
    grid_counters[hr_bin, resp_bin] <- grid_counters[hr_bin, resp_bin] + 1
  }
}


## LANDMARK WINDOW ##



## SLIDING ANIMATION ##
# Initialize sliding window
window_size <- 100
sliding_data <- data.frame()

# Create animation
saveGIF({
  for (i in 1:1000) {
    new_point <- get_points(stream_hrresp, n = 1)
    
    # If the stream is exhausted, reset it
    if (nrow(new_point) == 0) {
      reset_stream(stream_hrresp)
      new_point <- get_points(stream_hrresp, n = 1)
    }
    
    # Add new point to the window
    sliding_data <- rbind(sliding_data, new_point)
    
    # Remove oldest point if window size is exceeded
    if (nrow(sliding_data) > window_size) {
      sliding_data <- sliding_data[-1, ]
    }
    
    # Plot the sliding window
    plot(sliding_data$HR, sliding_data$RESP, 
         xlab = "HR", ylab = "RESP", 
         main = "Sliding Window", 
         xlim = range(hrresp_bidmc14$HR), 
         ylim = range(hrresp_bidmc14$RESP))
  }
}, interval = 0.1, movie.name = "sliding_window.gif")


## WEIGHTED WINDOW ##
# Initialize weighted window
window_size <- 100
weighted_data <- data.frame()
weights <- exp(-0.1 * (window_size:1))  # Exponential weights

# Create animation
saveGIF({
  for (i in 1:1000) {
    new_point <- get_points(stream_hrresp, n = 1)
    
    # If the stream is exhausted, reset it
    if (nrow(new_point) == 0) {
      reset_stream(stream_hrresp)
      new_point <- get_points(stream_hrresp, n = 1)
    }
    
    # Add new point to the window
    weighted_data <- rbind(weighted_data, new_point)
    
    # Remove oldest point if window size is exceeded
    if (nrow(weighted_data) > window_size) {
      weighted_data <- weighted_data[-1, ]
    }
    
    # Apply weights to the current window
    current_window_size <- nrow(weighted_data)
    current_weights <- weights[(window_size - current_window_size + 1):window_size]
    weighted_values <- weighted_data * current_weights
    
    # Plot the weighted window
    plot(weighted_values$HR, weighted_values$RESP, 
         xlab = "HR", ylab = "RESP", 
         main = "Weighted Window", 
         xlim = range(hrresp_bidmc14$HR), 
         ylim = range(hrresp_bidmc14$RESP))
  }
}, interval = 0.1, movie.name = "weighted_window.gif")


## FADING WINDOW ##
# Create animation
saveGIF({
  for (i in 1:1000) {
    new_point <- get_points(stream_hrresp, n = 1)
    
    # If the stream is exhausted, reset it
    if (nrow(new_point) == 0) {
      reset_stream(stream_hrresp)
      new_point <- get_points(stream_hrresp, n = 1)
    }
    
    # Add new point to the window
    fading_data <- rbind(fading_data, new_point)
    
    # Apply exponential decay to all points in the window
    fading_data <- fading_data * exp(-decay_rate * (nrow(fading_data):1))
    
    # Plot the fading window
    plot(fading_data$HR, fading_data$RESP, 
         xlab = "HR", ylab = "RESP", 
         main = "Fading Window", 
         xlim = range(hrresp_bidmc14$HR), 
         ylim = range(hrresp_bidmc14$RESP))
  }
}, interval = 0.1, movie.name = "fading_window.gif")

