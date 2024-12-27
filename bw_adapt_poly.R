# How does when you look, and how long, affect the conclusions you reach about your data?
# Are short term studies more likely to yield significant results?
# Are short term studies more likely to find *erroneous* significant trends?
# This script will perform a simple moving window analysis to answer these questions
# for long term data across a variety of domains- essentially, data gets subsetted, 
# we run a simple linear regression on each subset and record summary stats, trends

#assume data is coming in in the form year, response variable
#use this test data set to build stuff
test<-read.csv(file="https://raw.githubusercontent.com/BahlaiLab/bad_breakup_2/master/R_model/test.csv", header=TRUE)

## dummy data polynomial

## call fake data function
source("/Users/kathryngrage/Documents/algo_adapt/simulationsK.R")

fakedata_quadratic <- function(start_year = 1900, end_year = 1920, a = 0.01, b = -0.4, c = 2000, noise_sd = 0) {
  year <- seq(start_year, end_year)
  noise <- rnorm(length(year), mean = 0, sd = noise_sd)
  response <- a * (year - start_year)^2 + b * (year - start_year) + c + noise
  
  data.frame(year = year, response = response)
}
quadratic_data_non <- fakedata_quadratic(a=.5, b=.05, noise_sd=0)
plot(quadratic_data_non)

quadratic_data_l <- fakedata_quadratic(a=.5, b=.05, noise_sd=5)
plot(quadratic_data_l)

quadratic_data_h <- fakedata_quadratic(a=.5, b=.05, noise_sd=12)
plot(quadratic_data_h)
### whatever. it doesnt look that bad. standardize to get rid of the negative and get to work.
#######################
#make sure required packages are installed

#we need ggplot2, animation

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

standardize_quad<-function(data){
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
quadratic_data_non_stand <- standardize_quad(quadratic_data_non)
quadratic_data_l_stand <- standardize_quad(quadratic_data_l)
quadratic_data_h_stand <- standardize_quad(quadratic_data_h)

plot(quadratic_data_non_stand$year, quadratic_data_non_stand$stand.response)
plot(quadratic_data_l_stand$year, quadratic_data_l_stand$stand.response)
plot(quadratic_data_h_stand$year, quadratic_data_h_stand$stand.response)
#try it on test data
###test1<-standardize(test)
#seems to be functioning

# next we need a function that runs a simple linear model of x=year, y=response variable

### need ask for a and b. maybe change so it auto calculates and finds b given the data entered.


## K NOTE find a way to ask user for a and b, default to my parameters
quadfit <- function(data, a=0, b=0, c=0) {
  data$year_scaled <- data$year - 1900
  
  start_values <- list(a=a, b=b, c=c)
  
  # Fit the model using nlsLM with the scaled year
  model <- nlsLM(stand.response ~  a*(year_scaled^2) + b*(year_scaled) + c, 
                 data = data, start = start_values, control = nls.control(maxiter = 100))
  #extract residuals so we can make an R^squared
  residuals <- resid(model)
  TSS <- sum((data$stand.response - mean(data$stand.response))^2)
  RSS <- sum(residuals^2)
  R_squared <- 1 - (RSS / TSS)
  
  # Pull out the parameter estimates and their standard errors
  a_estimate <- coef(model)["a"]
  b_estimate <- coef(model)["b"]
  c_estimate <- coef(model)["c"]
  
  a_se <- sqrt(diag(vcov(model))["a"])
  b_se <- sqrt(diag(vcov(model))["b"])
  c_se <- sqrt(diag(vcov(model))["c"])
  
  # P-value for the main sinusoidal coefficient (a)
  p_value <- summary(model)$coefficients["a", 4]
  
  output <- c(
    min(data$year),  # Year the analysis started on
    nrow(data),      # Number of data points the analysis includes
    length(unique(data$year)),  # Number of years the analysis includes
    a_estimate,      # a 
    a_se,            # Standard error of a
    b_estimate,      # b 
    b_se,            # Standard error of b
    c_estimate,
    c_se,
    R_squared,
    p_value
  )
  #return(summary(model))
  return(output)
}
quadfit(quadratic_data_non_stand, a=.007, b=.0007, c=.1)
quadfit(quadratic_data_l_stand, a=.007, b=.0007, c=.1)
quadfit(quadratic_data_h_stand, a=.007, b=.0007, c=.1)

quadratic_breakup <- function(data, window) {
  data$year_scaled <- data$year - 1900
  remaining <- data
  output <- data.frame(
    year= integer(0),
    length = integer(0),
    years = integer(0),
    a_estimate = numeric(0),
    a_se = numeric(0),
    b_estimate = numeric(0),
    b_se = numeric(0),
    c_estimate = numeric(0),
    c_se = numeric(0),
    R_squared = numeric(0),
    p_value=numeric(0)
  )
  
  numyears <- length(unique(data$year))
  
  while (numyears > (window - 1)) {
    chunk <- subset(remaining, year_scaled < (min(year_scaled) + window))
    out <- quadfit(chunk)
    
    if (window == length(unique(chunk$year_scaled))) {
      output <- rbind(output, out)
    } else {
      output <- output
    }
    
    remaining <- subset(remaining, year_scaled > min(year_scaled))
    numyears <- length(unique(remaining$year_scaled))
  }
  
  names(output) <- c(
    "start_year", "N_data", "N_years",
    "a_estimate", "a_se", "b_estimate", "b_se", "c_estimate", "c_se", "r_square", "p_value"
  )
  return(output)
}
quadratic_breakup(quadratic_data_non_stand, 6)
quadratic_breakup(quadratic_data_l_stand, 21)
quadratic_breakup(quadratic_data_h_stand, 15)
quadratic_breakup(quadratic_data_h_stand, 11)
quadratic_breakup(quadratic_data_h_stand, 6)

#and now try this on the test data
###breakup(test1, 3)

# now time to write the function that will iterate through our targetted windows
# let's make a decision rule that our test data set must be greater than 10y in length
# let's make this idiot-proof and build the standardize function right into this function
# so we can literally run each properly prepared raw data set with a single line

multiple_breakups_quad<-function(data){
  data1<-standardize_quad(data) #standardize data
  count<-length(data1$year)
  output <- data.frame(
    year= integer(0),
    length = integer(0),
    years = integer(0),
    a_estimate = numeric(0),
    a_se = numeric(0),
    b_estimate = numeric(0),
    b_se = numeric(0),
    c_estimate = numeric(0),
    c_se = numeric(0),
    R_squared = numeric(0),
    p_value = numeric(0)
  )
  for(i in 4:(count)){
    outeach<-quadratic_breakup(data1, i) #fit at each window length
    output<-rbind(output, outeach)#bind it to the frame
  }
  out<-output
  return(out)
}
multiple_breakups_quad(quadratic_data_h)
multiple_breakups_quad(quadratic_data_l)
multiple_breakups_quad(quadratic_data_non)
###test2<-multiple_breakups(test)
#fan-flipping-tastic! it looks like that works. Kathryn re-tweets!


#let's create a plotting function

pyramid_plot_quad_a<- function(data, title="", significance=0.05, plot_insig=TRUE, rsq_points=FALSE){
  out<-multiple_breakups_quad(data)
  years<-length(unique(out$start_year))
  maxyears<-max(out$N_years)
  count<-nrow(out)
  #compute mean and sd of longest series for vertical lines
  true_a<-out[count,4] #find the a value of the longest series
  true_se<- (out[count,5])*(sqrt(out[count, 2])) #find the se value of the longest series
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

pyramid_plot_quad_b<- function(data, title="", significance=0.05, plot_insig=TRUE, rsq_points=FALSE){
  out<-multiple_breakups_quad(data)
  years<-length(unique(out$start_year))
  maxyears<-max(out$N_years)
  count<-nrow(out)
  #compute mean and sd of longest series for vertical lines
  true_b<-out[count,6] #find the a value of the longest series
  true_se<-(out[count,7])*(sqrt(out[count, 2])) #find the sd of the longest series
  #remember to convert standard error to standard deviation
  
  max_true<-true_b+true_se #compute max and min values for slopes we are calling true
  min_true<-true_b-true_se
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
pyramid_plot_quad_a(quadratic_data_h)
pyramid_plot_quad_b(quadratic_data_h)
pyramid_plot_quad_a(quadratic_data_l)
pyramid_plot_quad_b(quadratic_data_l)
pyramid_plot_quad_a(quadratic_data_non)
###pyramid_plot(test, title="test plot", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)



#now that we have visualization, we need a way to pull relevant metrics out of the computation
#so let's say our longest series is our 'truth', and we want to know how many years it takes 
#to reach 'stability'-so let's define stability as >(some percentage of slopes) occuring within 
#the standard deviation of the slope of the longest series, for a given window length, allow user to change # of SEs


stability_time_quad_a <- function(data, min_percent = 95, error_multiplyer = 1) {
  test <- multiple_breakups_quad(data)
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

stability_time_quad_b <- function(data, min_percent = 95, error_multiplyer = 1) {
  test <- multiple_breakups_quad(data)
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
stability_time_quad_a(quadratic_data_h)
stability_time_quad_b(quadratic_data_h)
stability_time_quad_a(quadratic_data_l)
stability_time_quad_b(quadratic_data_l)

stability_time_quad_both <- function(data, min_percent = 95, error_multiplier = 1) {
  stability_a <- stability_time_quad_a(data, min_percent, error_multiplier)
  stability_b <- stability_time_quad_b(data, min_percent, error_multiplier)
  
  return(max(stability_a, stability_b))
}
stability_time_quad_both(quadratic_data_h)
stability_time_quad_both(quadratic_data_l)

#now a function that finds the absoloute range of findings, and the absolute 
#range of significant findings

expon_range<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_expon(data)
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_a<-max(test1$a_estimate)
  min_a<-min(test1$a_estimate)
  a_range<-c(min_a, max_a)
  return(a_range)
  
}

#and try it out
expon_range(test3)

#now we want to find the absolute over and under estimate compared to the slope of the 
#longest series

relative_range_expon<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_expon(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_a<-max(test1$a_estimate)-true_a
  min_a<-min(test1$a_estimate)-true_a
  arange<-c(min_a, max_a)
  return(arange)
  
}

relative_range_expon(test3, only_significant = FALSE, significance = 0.05)

relative_range_after_stability_expon<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups_expon(data)
  stime<-stability_time_expon(data)
  stest<-test[which(test$N_years>=stime),]
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-stest[which(test$p_value<significance),]
  }else{
    test1<-stest
  }
  max_a<-max(test1$a_estimate)-true_a
  min_a<-min(test1$a_estimate)-true_a
  arange<-c(min_a, max_a)
  return(arange)
  
}

relative_range_after_stability_expon(test3, only_significant = FALSE, significance = 0.05)

#proportion significant- finds the proportion of total windows with statistically significant values

proportion_significant_expon<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups_expon(data)
  count<-nrow(test)
  significant_regressions<-test[which(test$p_value<significance),]
  count_sig<-nrow(significant_regressions)
  proportion<-count_sig/count
  return(proportion)
  
}

proportion_significant_expon(test3, significance=0.05)

#proportion significantly wrong- we're going to define this as 'directionally wrong'
#where there is a significant relationship that does not match the direction of the true slope


proportion_wrong_expon<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups_expon(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_p<-test[count,7]
  #case 1: true slope is not significant
  if (true_p>significance){
    wrong_windows<-test[which(test$p_value<significance),]
  }else{ #true slope is significant
    if(true_a>0){#true slope is positive
      wrong_windows<-test[which(test$a_estimate<0|test$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }else{#true slope is negative
      wrong_windows<-test[which(test$a_estimate>0|test$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }
  }
  count_wrong<-nrow(wrong_windows)
  proportion<-count_wrong/count
  return(proportion)
  
}

proportion_wrong_expon(test3, significance=0.05)



#proportion wrong by series length- basically the same thing as proportion wrong but looped 
#over all the unique window lengths. Will output a data frame with a window length and proportion
#of outputs are significantly misleading, plus average r square for that window length

proportion_wrong_series_expon<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups_expon(data)
  count<-nrow(test)
  true_a<-test[count,4] #find the slope of the longest series
  true_p<-test[count,7]
  windows<-unique(test$N_years)#get a list of unique window lengths
  prop.vec<-c()#create a blank vector to store proportions in
  r.vec<-c()#create vector for storing average rsquare values in
  for(i in 1:length(windows)){#for each window length, compute proportion 'wrong'
    window_length<-windows[i]
    test_subset<-test[which(test$N_years==window_length),]
    number_of_windows<-nrow(test_subset)#how many windows
    #case 1: true slope is not significant
    if (true_p>significance){
      wrong_windows<-test_subset[which(test_subset$p_value<significance),]
    }else{ #true slope is significant
      if(true_a>0){#true slope is positive
        wrong_windows<-test_subset[which(test_subset$a_estimate<0|test_subset$p_value>significance),]#wrong means the slope is the wrong sign or 0
      }else{#true slope is negative
        wrong_windows<-test_subset[which(test_subset$a_estimate>0|test_subset$p_value>significance),]#wrong means the slope is the wrong sign or 0
      }
    }
    count_wrong<-nrow(wrong_windows)
    proportion<-count_wrong/number_of_windows
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
proportion_wrong_series_expon(test3, significance = 0.1)

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
library(grid)

RMSE_plot_quad <- function(data, title = "", significance = 0.05, min_percent = 95, window_length = 5, error_multiplier = 1.0, plot_lim = 3) {
  data1 <- standardize_quad(data) # Standardize data for data frame
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_quad(data)
  count <- nrow(out)
  
  true_function <- function(x) {
    out[count, 4] * (x^2) + out[count, 6] * x + out[count, 8]
  }
  
  true_function_points <- data.frame(x = data1$year_scaled, y = true_function(data1$year_scaled))
  true_RMSE <- sqrt(mean((data1$stand.response - true_function_points$y)^2)) * error_multiplier
  upper_bound <- true_function_points$y + true_RMSE
  lower_bound <- true_function_points$y - true_RMSE
  
  y_min_limit <- min(lower_bound) - plot_lim
  y_max_limit <- max(upper_bound) + plot_lim
  
  out <- out[which(out$N_years == window_length),] # Only work with one window length per plot
  
  plot <- ggplot(data1, aes(x = year_scaled, y = stand.response)) +
    theme_classic() + geom_point(data = data1, aes(x = year_scaled, y = stand.response))
  
  total_points_within_bounds <- 0
  total_points <- 0
  
  for (i in 1:nrow(out)) {
    model_fit <- function(x) {
      a <- out$a_estimate[i]
      b <- out$b_estimate[i]
      c <- out$c_estimate[i]
      a * (x^2) + b * x + c
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
stability_time_RMSE_quad <- function(data, min_percent=95, error_multiplier=1) {
  data1 <- standardize_quad(data) # Standardizing the data
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_quad(data) # Getting breakups for all window lengths
  count <- nrow(out)
  
  # Define the true model based on the largest window
  true_function <- function(x) { out[count,4]*(x^2) + out[count,6]*x + out[count,8] }
  true_function_points <- data.frame(x = data1$year_scaled, y = true_function(data1$year_scaled))
  
  # Calculate the true RMSE and bounds
  true_RMSE <- sqrt(mean((data1$stand.response - true_function_points$y)^2)) * error_multiplier
  upper_bound <- true_function_points$y + true_RMSE
  lower_bound <- true_function_points$y - true_RMSE
  
  # Initialize variables to track the shortest window where most fits are within bounds
  shortest_window <- Inf # Start with infinity; will reduce this to the shortest qualifying window
  windows <- sort(unique(out$N_years)) # Ensure windows are sorted to start from shortest to longest
  
  for (window_length in windows) {
    test_subset <- out[which(out$N_years == window_length),]
    models_within_bounds_count <- 0 # Counter for entire models within bounds
    
    for (i in 1:nrow(test_subset)) {
      model_fit <- function(x) {
        a <- test_subset$a_estimate[i]
        b <- test_subset$b_estimate[i]
        c <- test_subset$c_estimate[i]
        return(a * (x^2) + b * x + c)
      }
      model_fit_points <- data.frame(x = data1$year_scaled, y = model_fit(data1$year_scaled))
      
      # Check if the entire model fit is within bounds
      fit_within_bounds <- all(model_fit_points$y >= min(lower_bound) & model_fit_points$y <= max(upper_bound))
      if (fit_within_bounds) {
        models_within_bounds_count <- models_within_bounds_count + 1
      }
    }
    
    percentage_within_bounds <- 100 * models_within_bounds_count / nrow(test_subset)
    if (percentage_within_bounds >= min_percent) {
      shortest_window <- window_length
      break # Found the shortest window where most models fit within bounds; no need to check longer windows
    }
  }
  
  if (shortest_window == Inf) {
    return(NA) # Indicates no window length met the criterion
  } else {
    return(shortest_window)
  }
}

stability_time_RMSE_quad(quadratic_data_h)
RMSE_plot_quad_5_h <- RMSE_plot_quad(quadratic_data_h, title="5 year window", window = 5, plot_lim=1, error_multiplier =2 )
RMSE_plot_quad_10_h <- RMSE_plot_quad(quadratic_data_h, title="10 year window", window = 10, plot_lim=1, error_multiplier =2 )

RMSE_plot_quad_11_h <- RMSE_plot_quad(quadratic_data_h, title="11 year window", window = 11, plot_lim=1, min_percent = 90)
RMSE_plot_quad_12_h <- RMSE_plot_quad(quadratic_data_h, title="12 year window", window = 12, plot_lim=1, error_multiplier =2)
RMSE_plot_quad_13_h <- RMSE_plot_quad(quadratic_data_h, title="13 year window", window = 13, plot_lim=1, error_multiplier =2 )
RMSE_plot_quad_14_h <- RMSE_plot_quad(quadratic_data_h, title="14 year window", window = 14, plot_lim=1, error_multiplier =2 )
RMSE_plot_quad_15_h <- RMSE_plot_quad(quadratic_data_h, title="15 year window", window = 15, plot_lim=1, error_multiplier =2 )
RMSE_plot_quad_16_h <- RMSE_plot_quad(quadratic_data_h, title="16 year window", window = 16, plot_lim=1)
RMSE_plot_quad_17_h <- RMSE_plot_quad(quadratic_data_h, title="17 year window", window = 17, plot_lim=1)
RMSE_plot_quad_18_h <- RMSE_plot_quad(quadratic_data_h, title="18 year window", window = 18, plot_lim=1)
RMSE_plot_quad_18_h
RMSE_plot_quad_14_h
RMSE_plot_quad_13_h
RMSE_plot_quad_15_h
RMSE_plot_quad(quadratic_data_h, title="19 year window", window = 20)
grid.arrange(RMSE_plot_quad_11_h, RMSE_plot_quad_12_h, RMSE_plot_quad_13_h, RMSE_plot_quad_14_h, RMSE_plot_quad_15_h, RMSE_plot_quad_16_h
             , RMSE_plot_quad_17_h, RMSE_plot_quad_18_h, RMSE_plot_quad_19_h, nrow=3, top="Highly Variable Quadratic Data")
RMSE_plot_quad_13_l <- RMSE_plot_quad(quadratic_data_l, title="13 year window", window = 13, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_14_l <- RMSE_plot_quad(quadratic_data_l, title="14 year window", window = 14, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_15_l <- RMSE_plot_quad(quadratic_data_l, title="15 year window", window = 15, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_16_l <- RMSE_plot_quad(quadratic_data_l, title="16 year window", window = 16, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_17_l <- RMSE_plot_quad(quadratic_data_l, title="17 year window", window = 17, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_18_l <- RMSE_plot_quad(quadratic_data_l, title="18 year window", window = 18, plot_lim=1, error_multiplier = 2)
RMSE_plot_quad_19_l <- RMSE_plot_quad(quadratic_data_l, title="19 year window", window = 19, plot_lim=1, RMSE_level = 2)
RMSE_plot_quad_20_l <- RMSE_plot_quad(quadratic_data_l, title="20 year window", window = 20, plot_lim=1, RMSE_level = 2)
RMSE_plot_quad_21_l <- RMSE_plot_quad(quadratic_data_l, title="21 year window", window = 21, plot_lim=1, RMSE_level = 2)
grid.arrange(RMSE_plot_quad_13_l, RMSE_plot_quad_14_l, RMSE_plot_quad_15_l, RMSE_plot_quad_16_l, RMSE_plot_quad_17_l, RMSE_plot_quad_18_l, 
             RMSE_plot_quad_19_l, RMSE_plot_quad_20_l, RMSE_plot_quad_21_l, nrow=3, top="Minimally Variable Quadratic Data")
grid.arrange(RMSE_plot_quad_5_h, RMSE_plot_quad_10_h, RMSE_plot_quad_15_h, nrow=1, top="Quadratic Data")
stability_time_RMSE(quadratic_data_l)
stability_time_RMSE(quadratic_data_h)

mm <- multiple_breakups_quad(quadratic_data_h)

library(purrr)
library(dplyr)

qs <- percent_plot_quad(quadratic_data_h, title="Quadraric Significant", plot_only_significant = TRUE)
qa <- percent_plot_quad(quadratic_data_h, title="Quadraric All", plot_only_significant = FALSE)

percent_plot_quad <- function(data, title = "", significance = 0.05, plot_only_significant = FALSE, error_multiplier = 1) {
  data1 <- standardize_quad(data)
  data1$year_scaled <- data$year - 1900
  out <- multiple_breakups_quad(data)
  out$start_year_scaled <- out$start_year - 1900
  count <- nrow(out)
  
  # Calculate the true model fit based on the longest window
  true_function <- function(x) {
    out[count, 4] * (x^2) + out[count, 6] * x + out[count, 8]
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
    model_predictions <- a_estimate * (data1$year_scaled^2) + b_estimate * data1$year_scaled + c_estimate
    
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
}
RMSE_plot_quad(quadratic_data_h, window_length = 14)
#if (plot_insig) {
#plot <- plot + geom_point(data = results_df %>% filter(PercentWithinBounds < 
#(significance * 100)), aes(x = as.factor(StartYear), y = PercentWithinBounds, 
#color = as.factor(WindowLength)), size = ifelse(rsq_points, out$r_square * 10, 2))
#}













