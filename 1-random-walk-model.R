# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sdrw is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)
#Positive criterion =correct, Negative criterion=incorrect

add.evidence <- function(drift, sdrw){
  internal.evidence <- 0
  number.of.samples <-0
  criterion<-3
  drift<-0
  sdrw<-0.3
  while (internal.evidence <= criterion && internal.evidence>= (-criterion)) {
    number.of.samples <- number.of.samples+1
    newval <-rnorm(1, drift, sdrw)
    internal.evidence <- internal.evidence+newval
  }  
  #If above criterion =3, it would print correct.

  return(c(internal.evidence > criterion, number.of.samples))
}
  
random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  data <- replicate(samples, add.evidence(drift, sdrw))
  accuracy.array <- data[1, 1:samples] #Grab the 1st thing out of Return (internal.evidence
  #and number.of.samples) and do that for 1 to out of all samples.
  rt.array <-data[2, 1:samples] #Grab the 2nd thing out of Return (internal.evidence
  #and number.of.samples) and do that for 1 to out of all samples.
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}


# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- random.walk.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
