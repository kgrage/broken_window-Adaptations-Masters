## need to adapt fake data function to include amplitude and frequency

fakedata_sinusoidal <- function(startyear, Nyears, startPop, noise, startK, startR, breaks, changeK, changeR, amplitude = 100, frequency = 0.1) {
  if (missing(startyear)) {
    startyear <- 1900
  }
  if (missing(Nyears)) {
    Nyears <- 20
  }
  year <- seq(startyear, (startyear + Nyears - 1))
  
  if (missing(startPop)) {
    startPop <- 1000
  }
  if (missing(noise)) {
    noise <- 0
  }
  if (missing(startK)) {
    startK <- 1500
  }
  if (missing(startR)) {
    startR <- 1.5
  }
  if (missing(breaks)) {
    breaks <- list()
  }
  if (missing(changeK)) {
    changeK <- 0
  }
  if (missing(changeR)) {
    changeR <- 0
  }
  
  noisevector <- rnorm(length(year), mean = 0, sd = noise)
  change <- c(FALSE)
  for (i in 1:(length(year) - 1)) {
    switch <- breaks %in% year[i]
    change <- c(change, switch)
  }
  
  k <- c(startK)
  for (i in 1:(length(year) - 1)) {
    if (change[i + 1]) {
      changesetK <- c(changeK, -changeK)
      nextk <- k[i] * (100 + sample(changesetK, 1)) / 100
    } else {
      nextk <- k[i]
    }
    k <- c(k, nextk)
  }
  
  r <- c(startR)
  for (i in 1:(length(year) - 1)) {
    if (change[i + 1]) {
      changesetR <- c(changeR, -changeR)
      nextr <- r[i] * (100 + sample(changesetR, 1)) / 100
    } else {
      nextr <- r[i]
    }
    r <- c(r, nextr)
  }
  
  Nt <- c(startPop)
  for (i in 1:length(year)) {
    Nt1 <- Nt[i] + amplitude * sin(2 * pi * frequency * (year[i] - startyear)) + startPop * exp(r[i]) * noisevector[i]
    Nt <- c(Nt, Nt1)
  }
  
  addyear <- max(year) + 1
  year <- c(year, addyear)
  simdata <- as.data.frame(cbind(year, Nt))
  
  return(simdata)
}
## lets create an array of high and low amplitudes and high and low noise values
  ## this creates some negative values but it will get resolved in the standardization so it doesnt REALLY matter
sin_low_slow <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=1, Nyears=20, breaks=list("1905", "1910"), amplitude=100, frequency=.05)
plot(sin_low_slow)
sin_high_slow <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=5, Nyears=20, breaks=list("1905", "1910"), amplitude=100, frequency=.05)
plot(sin_high_slow)
sin_low_quick <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=1, Nyears=20, breaks=list("1905", "1910"), amplitude=100, frequency=.1)
plot(sin_low_quick)
sin_high_quick <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=5, Nyears=20, breaks=list("1905", "1910"), amplitude=100, frequency=.1)
plot(sin_high_quick)

sinusoidal_data_h <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=.03, Nyears=20,  breaks=list("1905", "1910"), amplitude=100, frequency=.1)
plot(sinusoidal_data_h)
sinusoidal_data_l <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=.02, Nyears=20,  breaks=list("1905", "1910"), amplitude=100, frequency=.1)
plot(sinusoidal_data_l)
sinusoidal_data_non <- fakedata_sinusoidal(startPop=1000, startR=1.5, noise=0, Nyears=20,  breaks=list("1905", "1910"), amplitude=100, frequency=.1)
plot(sinusoidal_data_non)

## make sure user has ggplot2, animation, and minpack.lm
if(require("ggplot2")){
  print("done")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  if(require(ggplot2)){
    print("ggplot2 installed and loaded")
  } else {
    stop("could not install package ggplot2")
  }
}
if(require("animation")){
  print("done")
} else {
  print("trying to install animation")
  install.packages("animation")
  if(require(animation)){
    print("animation installed and loaded")
  } else {
    stop("could not install package animation")
  }
}
if(require("minpack.lm")){
  print("done")
} else {
  print("trying to install minpack.lm")
  install.packages("minpack.lm")
  if(require(ggplot2)){
    print("minpack.lm installed and loaded")
  } else {
    stop("could not install package minpack.lm")
  }
}


#######################

# set it so we get our decimal places rather than sci notation in our outputs
options(scipen=10)

# technical things that need to be done:

# data needs to be normalized in some way because we will be operating across domains 
# and likely with dramatically different values. Let's use a standardization approach so
# we don't give undue influence to outliers but also gets responses in the same magnitude
# something like x(standard)=(x-mean)/Stdev, the z score

standardize_sinusoidal<-function(data){
  #operate on the second column in data, where our response variable is
  data$stand.response<-(data[,2]-mean(data[,2]))/sd(data[,2])
  #name the columns consistently so we don't get headaches later
  names(data)<-c("year", "response", "stand.response")
  # Shift the response variable by adding the absolute value of the minimum value plus a small constant
  # we need non negatives!
  shift_value <- abs(min(data$stand.response)) + .1
  data$stand.response <- data$stand.response + shift_value
  #spit back our new data frame
  return(data)
}
stand_sinu_h <- standardize_sinusoidal(sinusoidal_data_h)
stand_sinu_l <- standardize_sinusoidal(sinusoidal_data_l)
stand_sinu_non <- standardize_sinusoidal(sinusoidal_data_non)
plot(stand_sinu_h$year, stand_sinu_h$stand.response)
plot(stand_sinu_l$year, stand_sinu_l$stand.response)

sin_high_slow1 <- standardize_sinusoidal(sin_high_slow)
plot(sin_high_slow1$year, sin_high_slow1$stand.response)
sin_high_quick1 <- standardize_sinusoidal(sin_high_quick)
plot(sin_high_quick1$year, sin_high_quick1$stand.response)
sin_low_slow1 <- standardize_sinusoidal(sin_low_slow)
plot(sin_low_slow1$year, sin_low_slow1$stand.response)
sin_low_quick1 <- standardize_sinusoidal(sin_low_quick)
plot(sin_low_quick1$year, sin_low_quick1$stand.response)

#seems to be functioning

# next we need a function that runs a simple linear model of x=year, y=response variable

### need ask for a and b. maybe change so it auto calculates and finds b given the data entered.

## K NOTE find a way to ask user for a and b, default to my parameters

plot(sin_low_slow1$year, sin_low_slow1$stand.response)
plot(sin_low_quick1$year, sin_low_quick1$stand.response)
plot(sin_high_slow1$year, sin_high_slow1$stand.response)
plot(sin_high_quick1$year, sin_high_quick1$stand.response)

## sidetrack. i think this is going to work much better
sinusoidalfit <- function(data, a = (max(data$stand.response) - min(data$stand.response)), 
                          b = .10, c = mean(data$stand.response), d = 10) {
  data$year_scaled <- data$year - 1900
  start_values <- list(a = a, b = b, c = c, d = d)
  control_params <- nls.control(maxiter = 1000, minFactor = 1e-8)
  # Fit the model using nls
  model <- nlsLM(stand.response ~ a * sin(2*pi*b * year_scaled + d) + c, 
                 data = data, start = start_values, control = control_params) 
  #   control = nls.control(minFactor = 0.001), maxiter = 1000)
  
  # Extract residuals for R-squared calculation
  residuals <- resid(model)
  TSS <- sum((data$stand.response - mean(data$stand.response))^2)
  RSS <- sum(residuals^2)
  R_squared <- 1 - (RSS / TSS)
  
  # Pull out the parameter estimates and their standard errors
  a_estimate <- coef(model)["a"]
  b_estimate <- coef(model)["b"]
  c_estimate <- coef(model)["c"]
  d_estimate <- coef(model)["d"]
  
  a_se <- sqrt(diag(vcov(model))["a"])
  b_se <- sqrt(diag(vcov(model))["b"])
  c_se <- sqrt(diag(vcov(model))["c"])
  
  # P-value for the main sinusoidal coefficient (a)
  p_value <- summary(model)$coefficients["a", 4]
  
  output <- c(
    min(data$year),  # Year the analysis started on
    nrow(data),      # Number of data points the analysis includes
    length(unique(data$year)),  # Number of years the analysis includes
    a_estimate,      # a (amplitude)
    a_se,            # Standard error of a
    b_estimate,      # b (vertical shift)
    b_se,            # Standard error of b
    c_estimate,      # c (frequency)
    c_se,            # Standard error of c
    d_estimate, 
    R_squared,
    p_value
  )
  
  return(output)
  #return(summary(model))
}

sinusoidalfit(stand_sinu_l, a=2, b=.1, d=10)
sinusoidalfit(stand_sinu_h, b=.1, d=10)

sinusoidalfit(sin_low_slow1, b=.05)
sinusoidalfit(sin_low_quick1, c=.1)
sinusoidalfit(sin_high_slow1, c=.05)
sinusoidalfit(sin_high_quick1, c=.1)

sinusoidal_breakup <- function(data, window) {
  remaining <- data
  output <- data.frame(
    start_year= integer(0),
    N_data = integer(0),
    N_years = integer(0),
    a_estimate = numeric(0),
    a_se = numeric(0),
    b_estimate = numeric(0),      # b (vertical shift)
    b_se = numeric(0),            # Standard error of b
    c_estimate = numeric(0),      # c (frequency)
    c_se = numeric(0),   
    d_estimate = numeric(0),# Standard error of c
    R_squared = numeric(0),
    p_value = numeric(0)
  )
  
  numyears <- length(unique(data$year))
  
  while (numyears > (window - 1)) {
    chunk <- subset(remaining, year < (min(year) + window))
    out <- sinusoidalfit(chunk)
    
    if (window == length(unique(chunk$year))) {
      output <- rbind(output, out)
    } else {
      output <- output
    }
    
    remaining <- subset(remaining, year > min(year))
    numyears <- length(unique(remaining$year))
  }
  names(output) <- c(
    "start_year", "N_data", "N_years",
    "a_estimate", "a_se", "b_estimate", "b_se", "c_estimate", "c_se", "d_estimate", "r_square", "p_value"
  )
  return(output)
  #return(summary(model))
}
sinusoidal_breakup(stand_sinu_l, 21)
sinusoidal_breakup(stand_sinu_h, 21)

#and now try this on the test data
###breakup(test1, 3)

# now time to write the function that will iterate through our targetted windows
# let's make a decision rule that our test data set must be greater than 10y in length
# let's make this idiot-proof and build the standardize function right into this function
# so we can literally run each properly prepared raw data set with a single line

multiple_breakups_sinu<-function(data){
  data1<-standardize_sinusoidal(data) #standardize data
  count<-length(data1$year)
  output <- data.frame(
    year= integer(0),
    length = integer(0),
    years = integer(0),
    a_estimate = numeric(0),
    a_se = numeric(0),
    b_estimate = numeric(0),
    b_se = numeric(0),
    c_estimate = numeric (0),
    c_se = numeric(0),
    d_estimate = numeric(0),
    R_squared = numeric(0),
    p_value = numeric(0)
  )
  for(i in 5:(count)){
    outeach<-sinusoidal_breakup(data1, i) #fit at each window length
    output<-rbind(output, outeach)#bind it to the frame
  }
  return(output)
}
multiple_breakups_sinu(sinusoidal_data_h)
multiple_breakups_sinu(sinusoidal_data_l)
multiple_breakups_sinu(sin_low_slow)
###test2<-multiple_breakups(test)
#fan-flipping-tastic! it looks like that works. Kathryn re-tweets!


#let's create a plotting function

pyramid_plot_sinu_a<- function(data, title="", significance=0.05, plot_insig=TRUE, rsq_points=FALSE){
  out<-multiple_breakups_sinu(data)
  years<-length(unique(out$start_year))
  maxyears<-max(out$N_years)
  count<-nrow(out)
  #compute mean and sd of longest series for vertical lines
  true_a<-out[count,4] #find the a value of the longest series
  true_se<-(out[count,5])*(sqrt(out[count, 2])) #find the se value of the longest series
  #remember to convert standard error to standard deviation
  
  max_true<-true_a+true_se #compute max and min values for slopes we are calling true
  min_true<-true_a-true_se
  out$significance<-ifelse(out$p_value<significance, "YES", "NO")
  if(rsq_points==TRUE){
    point_scale<-10*out$r_square
    yespt<-1
  }else{
    point_scale<-2
    yespt<-16
  }
  if(plot_insig==FALSE){
    out<-out[which(out$p_value<significance),]
  }
  plot<- ggplot(out) +
    theme_classic() +
    geom_hline(yintercept = true_a, linetype = 2) +
    geom_hline(yintercept = max_true, linetype = 3, color="grey38") +
    geom_hline(yintercept = min_true, linetype = 3, color="grey38") +
    aes(y = a_estimate, x = N_years,  ymin = (a_estimate-a_se), 
        ymax = (a_estimate+a_se)
        , shape=significance, color=significance
    ) +
    geom_linerange(show.legend = F)+ 
    geom_point(size=point_scale)+ 
    ggtitle(title)+
    scale_shape_manual(values=c("NO"=4,"YES"=yespt))+
    scale_color_manual(values=c("NO"="red","YES"="black"))+
    xlab("Number of years in window")+
    scale_x_continuous(lim=c(3, maxyears))+
    coord_flip()
  return(plot)
}

pyramid_plot_sinu_b<- function(data, title="", significance=0.05, plot_insig=TRUE, rsq_points=FALSE){
  out<-multiple_breakups_sinu(data)
  years<-length(unique(out$start_year))
  maxyears<-max(out$N_years)
  count<-nrow(out)
  #compute mean and sd of longest series for vertical lines
  true_b<-out[count,6] #find the b value of the longest series
  true_bse<-(out[count,7])*(sqrt(out[count, 2])) #find the se value of the longest series
  #remember to convert standard error to standard deviation
  
  max_true<-true_b+true_bse #compute max and min values for slopes we are calling true
  min_true<-true_b-true_bse
  out$significance<-ifelse(out$p_value<significance, "YES", "NO")
  if(rsq_points==TRUE){
    point_scale<-10*out$r_square
    yespt<-1
  }else{
    point_scale<-2
    yespt<-16
  }
  if(plot_insig==FALSE){
    out<-out[which(out$p_value<significance),]
  }
  plot<- ggplot(out) +
    theme_classic() +
    geom_hline(yintercept = true_b, linetype = 2) +
    geom_hline(yintercept = max_true, linetype = 3, color="grey38") +
    geom_hline(yintercept = min_true, linetype = 3, color="grey38") +
    aes(y = b_estimate, x = N_years,  ymin = (b_estimate-b_se), 
        ymax = (b_estimate+b_se)
        , shape=significance, color=significance
    ) +
    geom_linerange(show.legend = F)+ 
    geom_point(size=point_scale)+ 
    ggtitle(title)+
    scale_shape_manual(values=c("NO"=4,"YES"=yespt))+
    scale_color_manual(values=c("NO"="red","YES"="black"))+
    xlab("Number of years in window")+
    scale_x_continuous(lim=c(3, maxyears))+
    coord_flip()
  return(plot)
}
pyramid_plot_sinu_a(sinusoidal_data_h, plot_insig=FALSE)
pyramid_plot_sinu_a(sinusoidal_data_l, plot_insig=FALSE)
pyramid_plot_sinu_b(sinusoidal_data_h, plot_insig=FALSE)
pyramid_plot_sinu_b(sinusoidal_data_l, plot_insig=FALSE)

pyramid_plot_sinu_a(sin_low_quick)
pyramid_plot_sinu_b(sin_low_quick)


###pyramid_plot(test, title="test plot", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)



#now that we have visualization, we need a way to pull relevant metrics out of the computation
#so let's say our longest series is our 'truth', and we want to know how many years it takes 
#to reach 'stability'-so let's define stability as >(some percentage of slopes) occuring within 
#the standard deviation of the slope of the longest series, for a given window length, allow user to change # of SEs


stability_time_sinu_a <- function(data, min_percent = 95, error_multiplyer = 1) {
  test <- multiple_breakups_sinu(data)
  count <- nrow(test)
  true_a <- test[count, 4]  # Find the growth rate of the longest series
  true_se<-(test[count,5])*(sqrt(test[count, 2]))*error_multiplyer # Find the error of the longest series
  max_true <- true_a + true_se  # Compute max and min values for growth rate
  min_true <- true_a - true_se
  windows <- unique(test$N_years)  # Get a list of unique window lengths
  stability <- max(windows)  # Start with the assumption that the longest window is the only stable one
  for (i in 1:length(windows)) {  # For each window length, compute proportion 'correct'
    window_length <- windows[i]
    test_subset <- test[which(test$N_years == window_length), ]
    number_of_windows <- nrow(test_subset)  # How many windows
    correct_subset <- test_subset[which((test_subset$a_estimate < max_true) & (test_subset$a_estimate > min_true)), ]
    number_of_correct <- nrow(correct_subset)  # How many windows give the right answer
    percentage_correct <- 100 * number_of_correct / number_of_windows
    if (percentage_correct > min_percent) {
      if (window_length < stability) {
        stability <- window_length
      }
    }
  }
  return(stability)
}

stability_time_sinu_b <- function(data, min_percent = 95, error_multiplyer = 1) {
  test <- multiple_breakups_sinu(data)
  count <- nrow(test)
  true_b <- test[count, 6]  # Find the growth rate of the longest series
  true_se<-(test[count,7])*(sqrt(test[count, 2]))*error_multiplyer # Find the error of the longest series
  max_true <- true_b + true_se  # Compute max and min values for growth rate
  min_true <- true_b - true_se
  windows <- unique(test$N_years)  # Get a list of unique window lengths
  stability <- max(windows)  # Start with the assumption that the longest window is the only stable one
  for (i in 1:length(windows)) {  # For each window length, compute proportion 'correct'
    window_length <- windows[i]
    test_subset <- test[which(test$N_years == window_length), ]
    number_of_windows <- nrow(test_subset)  # How many windows
    correct_subset <- test_subset[which((test_subset$b_estimate < max_true) & (test_subset$b_estimate > min_true)), ]
    number_of_correct <- nrow(correct_subset)  # How many windows give the right answer
    percentage_correct <- 100 * number_of_correct / number_of_windows
    if (percentage_correct > min_percent) {
      if (window_length < stability) {
        stability <- window_length
      }
    }
  }
  return(stability)
}
#TESTING TESTING DOES THIS MAKE SENSE? LOOK AGAINST HOW THE LINEAR MODEL DOES
stability_time_sinu_a(sin_low_slow)
stability_time_sinu_b(sin_low_slow)

stability_time_sinu_both <- function(data, min_percent = 95, error_multiplier = 1) {
  stability_a <- stability_time_sinu_a(data, min_percent, error_multiplier)
  stability_b <- stability_time_sinu_b(data, min_percent, error_multiplier)
  
  return(max(stability_a, stability_b))
}
stability_time_sinu_both(sin_low_slow)


#now a function that finds the absoloute range of findings, and the absolute 
#range of significant findings

sinu_range<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_sinu(data)
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_a<-max(test1$a_estimate)
  min_a<-min(test1$a_estimate)
  a_range<-c(min_a, max_a)
  max_b <- max(test1$b_estimate)
  min_b <- min(test1$b_estimate)
  b_range <- c(min_b, max_b)
  return(list("a range" = a_range, "b range" = b_range))
  
}

#and try it out
sinu_range(sin_low_slow)

#now we want to find the absolute over and under estimate compared to the slope of the 
#longest series

relative_range_sinu<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_sinu(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_b <- test[count, 6]
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_a<-max(test1$a_estimate)-true_a
  min_a<-min(test1$a_estimate)-true_a
  arange<-c(min_a, max_a)
  max_b<-max(test1$b_estimate)-true_b
  min_b<-min(test1$b_estimate)-true_b
  brange<-c(min_b, max_b)
  return(list("a range" = arange, "b range" = brange))
  
}

relative_range_sinu(sin_low_slow, only_significant = FALSE, significance = 0.05)

relative_range_after_stability_sinu<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_sinu(data)
  stime<-stability_time_sinu_both(data)
  stest<-test[which(test$N_years>=stime),]
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_b <- test[count, 6]
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-stest[which(test$p_value<significance),]
  }else{
    test1<-stest
  }
  max_a<-max(test1$a_estimate)-true_a
  min_a<-min(test1$a_estimate)-true_a
  arange <- c(min_a, max_a)
  max_b <- max(test1$b_estimate) - true_b
  min_b <- min(test1$b_estimate) - true_b
  brange<-c(min_b, max_b)
  return(list("a range" = arange, "b range" = brange))
  
}

relative_range_after_stability_sinu(sin_low_slow, only_significant = FALSE, significance = 0.05)

#proportion significant- finds the proportion of total windows with statistically significant values

proportion_significant_sinu<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups_sinu(data)
  count<-nrow(test)
  significant_regressions<-test[which(test$p_value<significance),]
  count_sig<-nrow(significant_regressions)
  proportion<-count_sig/count
  return(proportion)
  
}

proportion_significant_sinu(sin_low_slow, significance=0.05)

#proportion significantly wrong- we're going to define this as 'directionally wrong'
#where there is a significant relationship that does not match the direction of the true slope


proportion_wrong_sinu <- function(data, significance = 0.05) {
  test <- multiple_breakups_sinu(data)
  count <- nrow(test)
  true_a <- test[count, 4]  # Find the 'a' value of the longest series
  true_b <- test[count, 6]  # Find the 'b' value of the longest series
  true_p <- test[count, 9]
  
  # Case 1: true 'a' is not significant
  if (true_p > significance) {
    wrong_windows <- test[which(test$p_value < significance), ]
  } else {  # true 'a' is significant
    if (true_a > 0) {  # true 'a' is positive
      # Condition 1: 'a' is positive, 'b' is positive
      wrong_windows <- test[which(test$a_estimate < 0 | test$b_estimate < 0 | test$p_value > significance), ]
    } else {  # true 'a' is negative
      if (true_b > 0) {  # true 'b' is positive
        # Condition 2: 'a' is negative, 'b' is positive
        wrong_windows <- test[which(test$a_estimate > 0 | test$b_estimate < 0 | test$p_value > significance), ]
      } else {  # true 'b' is negative
        # Condition 3: 'a' is negative, 'b' is negative
        wrong_windows <- test[which(test$a_estimate > 0 | test$b_estimate > 0 | test$p_value > significance), ]
      }
    }
  }
  
  count_wrong <- nrow(wrong_windows)
  proportion <- count_wrong / count
  return(proportion)
}

proportion_wrong_sinu(sin_low_slow, significance=0.05)

## check in on this. i think this is working well but im not sure i really understand whats happening so 
## i want to make sure my adaptation makes sense

#proportion wrong by series length- basically the same thing as proportion wrong but looped 
#over all the unique window lengths. Will output a data frame with a window length and proportion
#of outputs are significantly misleading, plus average r square for that window length

## come back. not working properly. proportions are proportions

proportion_wrong_series_sinu<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups_sinu(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_p<-test[count,9]
  true_b <- test[count,6]
  windows<-unique(test$N_years)#get a list of unique window lengths
  prop.vec <- numeric()  # Initialize an empty numeric vector to store proportions
  r.vec <- numeric()  # Initialize an empty numeric vector to store average rsquare values
  for(i in 1:length(windows)){#for each window length, compute proportion 'wrong'
    window_length<-windows[i]
    test_subset<-test[which(test$N_years==window_length),]
    number_of_windows<-nrow(test_subset)#how many windows
    #case 1: true slope is not significant
    if (true_p > significance) {
      wrong_windows <- test[which(test$p_value < significance), ]
    } else {  # true 'a' is significant
      if (true_a > 0) {  # true 'a' is positive
        # Condition 1: 'a' is positive, 'b' is positive
        wrong_windows <- test[which(test$a_estimate < 0 | test$b_estimate < 0 | test$p_value > significance), ]
      } else {  # true 'a' is negative
        if (true_b > 0) {  # true 'b' is positive
          # Condition 2: 'a' is negative, 'b' is positive
          wrong_windows <- test[which(test$a_estimate > 0 | test$b_estimate < 0 | test$p_value > significance), ]
        } else {  # true 'b' is negative
          # Condition 3: 'a' is negative, 'b' is negative
          wrong_windows <- test[which(test$a_estimate > 0 | test$b_estimate > 0 | test$p_value > significance), ]
        }
      }
    }
    count_wrong<-nrow(wrong_windows)
    proportion<-count_wrong
    #/number_of_windows
    prop.vec<-c(prop.vec, proportion)
    avg.confidence<-mean(test_subset$r_square)
    r.vec<-c(r.vec, avg.confidence)
  }
  
  x_name <- "window_length"
  y_name <- "proportion_wrong"
  z_name <- "avg_r_square"
  
  df <- data.frame(windows, prop.vec, r.vec)
  names(df) <- c(x_name, y_name, z_name)
  return(df)
  
}


#test it
proportion_wrong_series_sinu(sin_low_slow, significance = 0.1)
proportion_wrong_series_exp(test3, significance = 0.1)
#proportion significantly wrong under stability time- we're going to define this as 'directionally wrong'
#where there is a significant relationship that does not match the direction of the true slope

proportion_wrong_before_stability_expon<- function(data, significance=0.05, min_percent=95, error_multiplyer=1){#returns a single value between 0 and 1
  
  test<-multiple_breakups_expon(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_p<-test[count,7]
  
  #cut out data below threshold
  threshold<-stability_time_expon(data, min_percent, error_multiplyer)#find stability threshold
  test1<-test[which(test$N_years<threshold),]
  count1<-nrow(test1)
  #case 1: true slope is not significant
  if (true_p>significance){
    wrong_windows<-test1[which(test1$p_value<significance),]
  }else{ #true slope is significant
    if(true_a>0){#true slope is positive
      wrong_windows<-test1[which(test1$a_estimate<0|test1$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }else{#true slope is negative
      wrong_windows<-test1[which(test1$a_estimate>0|test1$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }
  }
  count_wrong<-nrow(wrong_windows)
  proportion<-count_wrong/count1
  return(proportion)
  
}

proportion_wrong_before_stability_expon(test3, significance=0.05)

#implement another charting function that gives the proportion wrong by window length

wrongness_plot_expon<-function(data, significance=0.05, min_percent=95, error_multiplyer=1, title =""){
  threshold<-stability_time_expon(data, min_percent, error_multiplyer)#find stability threshold
  wrongness<-proportion_wrong_series_expon(data, significance)
  maxyears<-max(wrongness$window_length)
  plot<- ggplot(wrongness) +
    theme_classic() +
    geom_vline(xintercept = (threshold-0.1), linetype = 3, color="grey38") +
    geom_smooth(aes(y = proportion_wrong, x = window_length, 
                    linetype="Propwrong", color="Propwrong"), se=FALSE)+
    geom_point(aes(y = proportion_wrong, x = window_length, 
                   shape="Propwrong", fill="Propwrong"), size=3)+
    geom_smooth(aes(y = avg_r_square, x = window_length, 
                    linetype="rsq", color="rsq"), se=FALSE)+
    geom_point(aes(y = avg_r_square, x = window_length, 
                   shape="rsq", fill="rsq"), size=3)+
    scale_fill_manual(name="", values=c(Propwrong="black",rsq="orange"),
                      labels=c("Proportion\n wrong", expression("Average R"^2)))+
    scale_shape_manual(name="", values=c(Propwrong=21, rsq=24), 
                       labels=c("Proportion\n wrong", expression("Average R"^2)))+
    scale_linetype_manual(name="", values=c(Propwrong=1, rsq=2), 
                          labels=c("Proportion\n wrong", expression("Average R"^2)))+
    scale_color_manual(name="", values=c(Propwrong="blue", rsq="red"), 
                       labels=c("Proportion\n wrong", expression("Average R"^2)))+
    ggtitle(title)+
    xlab("Number of years in window")+
    ylab("Average value")+
    ylim(0,1)
  return(plot)
}

#test
wrongness_plot_expon(test3)

#now for a function that plots all the lines by window length



##########################################################
## MANUSCRIPT FIGURES
##########################################################

RMSE_plot_sinu <- function(data, title = "", significance = 0.05, min_percent = 95, window_length = 5, error_multiplier = 1.0, plot_lim = 3) {
  data1 <- standardize_sinusoidal(data)  # Assume this function standardizes your data for sinusoidal fitting
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_sinu(data)  # Assume this function provides sinusoidal model parameters
  count <- nrow(out)
  
  # Define the true sinusoidal model based on the largest window
  true_function <- function(x) {
    out[count,4]*sin(2*pi*out[count,6]*x + out[count,10]) +out[count, 8]
  }
  
  true_function_points <- data.frame(x = data1$year_scaled, y = true_function(data1$year_scaled))
  true_RMSE <- sqrt(mean((data1$stand.response - true_function_points$y)^2)) * error_multiplier
  upper_bound <- true_function_points$y + true_RMSE
  lower_bound <- true_function_points$y - true_RMSE
  
  y_min_limit <- min(lower_bound) - plot_lim
  y_max_limit <- max(upper_bound) + plot_lim
  
  out <- out[which(out$N_years == window_length),]  # Only work with one window length per plot
  
  plot <- ggplot(data1, aes(x = year_scaled, y = stand.response)) +
    theme_classic() + geom_point(data = data1, aes(x = year_scaled, y = stand.response))
  
  total_points_within_bounds <- 0
  total_points <- 0
  
  for (i in 1:nrow(out)) {
    model_fit <- function(x) {
      a <- out$a_estimate[i]
      b <- out$b_estimate[i]
      c <- out$c_estimate[i]
      d <- out$d_estimate[i]
      a * sin(2 * pi * b * x + d) + c
    }
    
    model_fit_points <- data.frame(x = data1$year_scaled, y = model_fit(data1$year_scaled))
    points_within_bounds <- sum(model_fit_points$y >= lower_bound & model_fit_points$y <= upper_bound)
    total_points_within_bounds <- total_points_within_bounds + points_within_bounds
    total_points <- total_points + nrow(model_fit_points)
    
    line_color <- ifelse(out$p_value[i] < significance, "black", "red")
    plot <- plot + geom_line(data = model_fit_points, aes(x = x, y = y), color = line_color, size = 0.25)
  }
  
  percentage_within_bounds <- 100 * total_points_within_bounds / total_points
  
  if (percentage_within_bounds >= min_percent) {
    message("Most points are entirely within the RMSE bounds.")
  } else {
    message("Most points are not entirely within the RMSE bounds.")
  }
  
  plot <- plot + ggtitle(title) +
    geom_line(data = true_function_points, aes(x = x, y = y), color = "black", size = 0.5) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3, fill = "darkblue") +
    xlab("Year") + ylab("Z-scaled response") +
    coord_cartesian(ylim = c(y_min_limit, y_max_limit))
  
  return(plot)
}

RMSE_plot_sinu(sinusoidal_data_h, window_length = 10,  plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_h, window_length = 11,  plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_h, window_length = 12,  plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_h, window_length = 13, plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_h, window_length = 14, plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_h, window_length = 17, plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_l, window_length = 10, plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_l, window_length = 11,  plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_l, window_length = 12,  plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_l, window_length = 13, plot_lim = 2)
RMSE_plot_sinu(sinusoidal_data_l, window_length = 14, plot_lim = 2)

stability_time_RMSE_sinu <- function(data, min_percent=95, error_multiplier=1) {
  data1 <- standardize_sinusoidal(data) # Assuming this function standardizes your data
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_sinu(data) # Assuming this function gives breakups for all window lengths
  count <- nrow(out)
  true_function <- function(x) { out[count,4]*sin(2*pi*out[count,6]*x + out[count,10]) +out[count, 8] }
  true_function_points <- data.frame(x = data1$year_scaled, y = true_function(data1$year_scaled))
  true_RMSE <- sqrt(mean((data1$stand.response - true_function_points$y)^2)) * error_multiplier # Error multiplier applied here
  upper_bound <- true_function_points$y + true_RMSE
  lower_bound <- true_function_points$y - true_RMSE
  
  windows <- unique(out$N_years) # Get a list of unique window lengths
  stability <- max(windows) # Start with the assumption that the longest window is the only stable one
  
  for (window_length in windows) { # For each window length, compute proportion 'correct'
    test_subset <- out[which(out$N_years == window_length),]
    within_bounds_count <- 0 # Initialize counter for models within bounds
    for (i in 1:nrow(test_subset)) {
      model_fit <- function(x) {
        a <- test_subset$a_estimate[i]
        b <- test_subset$b_estimate[i]
        c <- test_subset$c_estimate[i]
        d <- test_subset$d_estimate[i]
        return(a * sin(2*pi* b * x +d) + c)
      }
      model_fit_points <- data.frame(x = data1$year_scaled, y = model_fit(data1$year_scaled))
      # Check if the model fit is mostly within bounds
      fit_within_bounds_percentage <- mean(model_fit_points$y >= lower_bound & model_fit_points$y <= upper_bound) * 100
      
      if (fit_within_bounds_percentage >= min_percent) {
        within_bounds_count <- within_bounds_count + 1
      }
    }
    
    percentage_within_bounds <- 100 * within_bounds_count / nrow(test_subset)
    if (percentage_within_bounds >= min_percent) {
      stability <- min(stability, window_length) # Update stability
    }
  }
  
  return(stability)
}
stability_time_RMSE_sinu(sinusoidal_data_h)

RMSE_plot_sinu_11_h <- RMSE_plot_sinu(sinusoidal_data_h, title="11 year window", window = 11, plot_lim=1)
RMSE_plot_sinu_12_h <- RMSE_plot_sinu(sinusoidal_data_h, title="12 year window", window = 12, plot_lim=1)
RMSE_plot_sinu_13_h <- RMSE_plot_sinu(sinusoidal_data_h, title="13 year window", window = 13, plot_lim=1, error_multiplier =2)
RMSE_plot_sinu_14_h <- RMSE_plot_sinu(sinusoidal_data_h, title="14 year window", window = 14, plot_lim=1, error_multiplier =2)
RMSE_plot_sinu_15_h <- RMSE_plot_sinu(sinusoidal_data_h, title="15 year window", window = 15, plot_lim=1, error_multiplier =2)
RMSE_plot_sinu_16_h <- RMSE_plot_sinu(sinusoidal_data_h, title="16 year window", window = 16, plot_lim=1, error_multiplier =2)
RMSE_plot_sinu_17_h <- RMSE_plot_sinu(sinusoidal_data_h, title="17 year window", window = 17, plot_lim=1)
RMSE_plot_sinu_18_h <- RMSE_plot_sinu(sinusoidal_data_h, title="18 year window", window = 18, plot_lim=1)
RMSE_plot_sinu_19_h <- RMSE_plot_sinu(sinusoidal_data_h, title="19 year window", window = 19, plot_lim=1)

RMSE
grid.arrange(RMSE_plot_sinu_11_h, RMSE_plot_sinu_12_h, RMSE_plot_sinu_13_h, RMSE_plot_sinu_14_h, RMSE_plot_sinu_15_h, RMSE_plot_sinu_16_h
             , RMSE_plot_sinu_17_h, RMSE_plot_sinu_18_h, RMSE_plot_sinu_19_h, nrow=3, top="Highly Variable Sinusoidal Data")
RMSE_plot_sinu_5_h <- RMSE_plot_sinu(sinusoidal_data_h, title="5 year window", window = 5, plot_lim=1, error_multiplier =2)
RMSE_plot_sinu_10_h <- RMSE_plot_sinu(sinusoidal_data_h, title="10 year window", window = 10, plot_lim=1, error_multiplier =2)
grid.arrange(RMSE_plot_sinu_5_h, RMSE_plot_sinu_10_h, RMSE_plot_sinu_15_h, top="Sinusoidal Data", nrow=1)
multiple_breakups_sinu(sinusoidal_data_h)
RMSE_plot_sinu_12_l <- RMSE_plot_sinu(sinusoidal_data_l, title="12 year window", window = 12, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_13_l <- RMSE_plot_sinu(sinusoidal_data_l, title="13 year window", window = 13, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_14_l <- RMSE_plot_sinu(sinusoidal_data_l, title="14 year window", window = 14, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_15_l <- RMSE_plot_sinu(sinusoidal_data_l, title="15 year window", window = 15, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_16_l <- RMSE_plot_sinu(sinusoidal_data_l, title="16 year window", window = 16, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_17_l <- RMSE_plot_sinu(sinusoidal_data_l, title="17 year window", window = 17, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_18_l <- RMSE_plot_sinu(sinusoidal_data_l, title="18 year window", window = 18, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_19_l <- RMSE_plot_sinu(sinusoidal_data_l, title="19 year window", window = 19, plot_lim=1, RMSE_level = 2)
RMSE_plot_sinu_20_l <- RMSE_plot_sinu(sinusoidal_data_l, title="20 year window", window = 20, plot_lim=1, RMSE_level = 2)

grid.arrange(RMSE_plot_sinu_12_l, RMSE_plot_sinu_13_l, RMSE_plot_sinu_14_l, RMSE_plot_sinu_15_l, RMSE_plot_sinu_16_l, RMSE_plot_sinu_17_l, RMSE_plot_sinu_18_l, 
             RMSE_plot_sinu_19_l, RMSE_plot_sinu_20_l, nrow=3, top="Minimally Variable Sinusoidal Data")

stability_time_sinu(sinusoidal_data_l)
stability_time_sinu(sinusoidal_data_h)

percent_plot_sinu <- function(data, title = "", significance = 0.05, plot_only_significant = FALSE, error_multiplier = 1) {
  data1 <- standardize_sinusoidal(data)
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_sinu(data)
  out$start_year_scaled <- out$start_year - 1900
  count <- nrow(out)
  
  # Calculate the true model fit based on the longest window
  true_function <- function(x) {
    out[count,4]*sin(2*pi*out[count,6]*x + out[count,10]) +out[count, 8]
  }
  
  true_predictions <- true_function(data1$year_scaled)
  true_RMSE <- sqrt(mean((data1$stand.response - true_predictions)^2)) * error_multiplier
  upper_bound <- true_predictions + true_RMSE
  lower_bound <- true_predictions - true_RMSE
  
  results_df <- data.frame(start_year_scaled=numeric(), N_years=numeric(), PercentWithinBounds=numeric(), p_value=numeric())
  
  for(i in 1:nrow(out)) {
    a_estimate <- out$a_estimate[i]
    b_estimate <- out$b_estimate[i]
    c_estimate <- out$c_estimate[i]
    d_estimate <- out$d_estimate[i]
    model_predictions <- a_estimate * sin(2*pi*b_estimate*(data1$year_scaled) + d_estimate) + c_estimate
    
    within_bounds <- (model_predictions >= lower_bound) & (model_predictions <= upper_bound)
    percent_within_bounds <- mean(within_bounds) * 100
    
    results_df <- rbind(results_df, data.frame(start_year_scaled=out$start_year_scaled[i], N_years=out$N_years[i], PercentWithinBounds=percent_within_bounds, p_value=out$p_value[i]))
  }
  
  if(plot_only_significant) {
    results_df <- results_df %>% filter(p_value < significance)
  }
  
  results_df <- results_df %>%
    group_by(N_years, PercentWithinBounds) %>%
    mutate(overlap_count = n()) %>%
    ungroup()
  
  plot <- ggplot(results_df, aes(x = N_years, y = PercentWithinBounds)) +
    geom_point(aes(color = overlap_count), alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle(title) +
    labs(x = "Window Length", y = "Percent Within RMSE Bounds") +
    theme_minimal() +
    coord_flip()
  
  return(plot)
  #return(true_predictions)
}
library(dplyr)
ss <- percent_plot_sinu(sinusoidal_data_h, title="Sinusoidal Significant", plot_only_significant = TRUE)
sa <- percent_plot_sinu(sinusoidal_data_h, title="Sinusoidal All", plot_only_significant = FALSE)
RMSE_plot_sinu(sinusoidal_data_h, window_length=10)
library(gridExtra)
grid.arrange(ss, sa, qs, qa, es, ea, nrow = 3)





