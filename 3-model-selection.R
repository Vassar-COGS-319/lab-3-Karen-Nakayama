# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

Test.1<-random.walk.model(1000, drift= 0.5, sdrw = 0.3, criterion=4)
sum(Test.1$correct) / length(Test.1$correct) 
mean(Test.1$rt) 

Test.2<-accumulator.model(1000, rate.1=50, rate.2 = 55, criterion = 3)
sum(Test.2$correct) / length(Test.2$correct)
mean(Test.2$rt)

#Josh's code
rw.model.result <- random.walk.model(1000, drift=0.012, sdrw=0.3, criterion = 4.8)
rw.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(rw.model.result$correct) #random.walk.model produced a slightly worse result than Josh's earlier, which
#explains why the results are very different from Josh's with the same drift and criterion.

rw.model.result <- random.walk.model(1000, drift=5, sdrw=0.3, criterion = 1)
rw.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(rw.model.result$correct) #whatever the change in drift or criterion, the rt and accuracy can't
#get high enough. With my random.walk.model, accumulator.model is a much better model.

acc.model.result <- accumulator.model(1000, rate.1 = 85, rate.2 = 91, criterion=3)
acc.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(acc.model.result$correct)

dev.off()
layout(matrix(1:4, nrow=2, byrow=T))
hist((rw.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,100), main="RW Model, correct", xlab="RT")
hist((rw.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,100), main="RW Model, incorrect", xlab="RT")
hist((acc.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,10), main="ACC Model, correct", xlab="RT")
hist((acc.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,10), main="ACC Model, incorrect", xlab="RT")

#With my version of random.walk.model, both the correct and incorrect are at a similar value on the
#plot with similar variability and symmetry. It is the same for accumulator model.

rw.model.result %>% group_by(correct) %>% summarize(sd.rt = sd(rt))
acc.model.result %>% group_by(correct) %>% summarize(sd.rt = sd(rt))

#With my random.walk.model, the SD is around 9 while accumulator model is around 13. But, the RT 
#and accuracy is nowhere close to the data.
