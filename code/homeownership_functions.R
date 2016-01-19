##Homeownership functions
#average Black homeownership rate by year
calc_black_rate <- function() {
  average_black_rate = c()
  for (i in 1994:2014) {
    black_rate_total = 0
    count = 0
    curr_year_black = home_rates$Black[home_rates$Year == i]
    for (i in 1:length(curr_year_black)) {
      black_rate_total = black_rate_total + as.numeric(as.character(curr_year_black[i]))
      count = count + 1
    }
    average_black_rate = c(average_black_rate, black_rate_total/count)
  }
  return (average_black_rate)
}
calc_white_rate <- function(){
  average_white_rate = c()
  for (i in 1994:2014) {
    white_rate_total = 0
    count = 0
    curr_year_white = home_rates$White[home_rates$Year == i]
    for (i in 1:length(curr_year_white)) {
      white_rate_total = white_rate_total + as.numeric(as.character(curr_year_white[i]))
      count = count + 1
    }
    average_white_rate = c(average_white_rate, white_rate_total/count)
  }
  return (average_white_rate)
}

calc_hispanic_rate <- function(){
  average_hispanic_rate = c()
  for (i in 1994:2014) {
    hispanic_rate_total = 0
    count = 0
    curr_year_hispanic = home_rates$Hispanic[home_rates$Year == i]
    for (i in 1:length(curr_year_hispanic)) {
      hispanic_rate_total = hispanic_rate_total + as.numeric(as.character(curr_year_hispanic[i]))
      count = count + 1
    }
    average_hispanic_rate = c(average_hispanic_rate, hispanic_rate_total/count)
  }
  return (average_hispanic_rate)
}

calc_other_rate <- function(){
  average_other_rate = c()
  for (i in 1994:2014) {
    other_rate_total = 0
    count = 0
    curr_year_other = home_rates$Other[home_rates$Year == i]
    for (i in 1:length(curr_year_other)) {
      other_rate_total = other_rate_total + as.numeric(as.character(curr_year_other[i]))
      count = count + 1
    }
    average_other_rate = c(average_other_rate, other_rate_total/count)
  }
  return (average_other_rate)
}

calc_US_rate <- function(){
  average_US_rate = c()
  for (i in 1994:2014) {
    US_rate_total = 0
    count = 0
    curr_year_US = home_rates$`US Total`[home_rates$Year == i]
    for (i in 1:length(curr_year_US)) {
      US_rate_total = US_rate_total + as.numeric(as.character(curr_year_US[i]))
      count = count + 1
    }
    average_US_rate = c(average_US_rate, US_rate_total/count)
  }
  return (average_US_rate)
}


#function that calculates the difference in homeownership rates between the US total
#and a minority group 
calc_diff <- function(us_total, subgroup) {
  difference = us_total - subgroup
  return(difference)
}

