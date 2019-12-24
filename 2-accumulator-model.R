# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 40)
# rate.1 is the evidence accumulation rate for the incorrect response (default value is 40)
# criterion is the threshold for a response (default value is 3)

#Accumulator 1 and 2 are going up at a different rate. Whichever that gets to the threshold first is the correct.

# one oddity: note that higher values for rate.1 and rate.2 will actually produce slower RTs.
# this is because the rate parameter is controlling the rate of decay of the exponential distribution,
# so faster rates mean that less evidence is likely to accumulate on each step. we could make
# these parameters more intuitive by taking 1/rate.1 and 1/rate.2 as the values to rexp().

add.evidence <- function(rate.1=40, rate.2=40){
  evidence.accu.1 <- 0
  evidence.accu.2 <-0
  number.of.samples <-0
  criterion<-3
  rate.1 <-40
  rate.2 <-40
  while ((evidence.accu.1 <= criterion) &&(evidence.accu.2 <= criterion)) {
    number.of.samples <- number.of.samples+1
    newval1 <-rexp(1,rate.1)
    newval2 <-rexp(1, rate.2)
    evidence.accu.1<-evidence.accu.1+newval1
    evidence.accu.2<-evidence.accu.2+newval2
  }  
  if(evidence.accu.1>evidence.accu.2){ #Accumulator 1 =correct
    return(c(TRUE, number.of.samples))
  }
  if(evidence.accu.2>evidence.accu.1){ #Accumulator 2 =incorrect
    return(c(FALSE, number.of.samples))
  }
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

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  data <- replicate(samples,add.evidence(rate.1, rate.2))
  accuracy.array <- accuracy.array <- data[1, 1:samples] #Grab the 1st thing out of Return (internal.evidence
  #and number.of.samples) and do that for 1 to out of all samples.
  rt.array <-data[2, 1:samples] #Grab the 2nd thing out of Return (internal.evidence
  #and number.of.samples) and do that for 1 to out of all samples.

  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}


#Josh's code
accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  
  accuracy.array <- numeric()
  rt.array <- numeric()
  
  for(i in 1:samples){
    evidence.1 <- 0
    evidence.2 <- 0
    rt <- 0
    while(evidence.1 < criterion && evidence.2 < criterion){
      evidence.1 <- evidence.1 + rexp(1, rate.1)
      evidence.2 <- evidence.2 + rexp(1, rate.2)
      rt <- rt + 1
    }
    rt.array[i] <- rt
    accuracy.array[i] <- (evidence.1 > evidence.2)
  }
  
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

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
