# a simple logic for an expected points calculation for the team in possession
## reset step: the kick after ANY score

###############
### HELPERS ###
###############

# function to calculate expected points from a given position and time left
xp_open_play = function(meter_line, time_left) {
  # meter_line: distance from try line in meters (0-100)
  # time_left: time left in the half (seconds)
  
  # calculates the average next score from a position with some amount of time left from data
  ## ideally, selection bias control for team strength here
}

# function to calculate the probability that a kick is made
p_make = function(meter_line, angle = 45) {
  # meter_line: distance from try line in meters (0-100)
  # angle: angle to the posts in degrees (default 45* for penalties)
  
  # use logistic regression model
}

##################
### MAIN LOGIC ###
##################

# formal expected points calculation at a penalty decision point
expected_points <- function(meter_line, time_left) {
  # meter_line: distance from try line in meters (0-100)
  # time_left: time left in the half (seconds)
  
  # probability penalty is scored
  p_make = p_make(meter_line, angle = 45) # could be specialized to teams' kicker quality w/ helper
  # expected yard line of kick if missed
  miss_meter_line = "some value here" # could be specialized to teams' kicker quality w/ helper
  # how long does a kick take to land from a point
  kick_time = "average time a penalty kick takes"
  
  # expected points from choosing to kick
  xp_kick = 3 * p_make + xp_open_play(miss_meter_line, time_left - kick_time) * (1 - p_make)
  
  # how much distance is gained from kick to touch
  kick_to_touch = "average distance gained" # could be specialized to teams' quality w/ helper
  # how long does a kick to touch take
  kick_to_touch_time = "average time a kick to touch takes"
  
  # expected points from going for try
  xp_run = xp_open_play(meter_line - kick_to_touch, time_left - kick_to_touch_time)
  
  # print optimal decision, expected points from each option
  cat("Expected points from kicking: ", round(xp_kick, 2), "\n")
  cat("Expected points from going for try: ", round(xp_run, 2), "\n")
  if (xp_kick > xp_run) {
    cat("Optimal decision: Kick for goal\n")
  } else {
    cat("Optimal decision: Go for try\n")
    }
}