# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sdrw is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)

random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}
evidence <- 0
num_loops = 0
i <- rnorm(1, mean = 0, sd=0.3)
evidence = evidence + i
while(-3<evidence && evidence<3){
  i <- rnorm(1, mean = 0, sd=0.3)
  evidence = evidence + i
  num_loops = num_loops + 1
}
print(num_loops)
print(evidence)


if (evidence <= -3) {print(FALSE)}
if (evidence >= 3) {print(TRUE)}

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

#######SOLUTION CODE
random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  accuracy.array <- numeric()
  rt.array <- numeric()
  
  for(i in 1:samples){
    evidence <- 0
    rt <- 0
    while(abs(evidence) < criterion){
      evidence <- evidence + rnorm(1, drift, sdrw)
      rt <- rt + 1
    }
    rt.array[i] <- rt
    accuracy.array[i] <- (evidence > criterion)
  }
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
  
}
####### I give myselef a 3 in thi lab because it was completed in class meaning that I 
######## colaborated with classmates and also the internet in order to figure out how 
######## to code the model and also asked professor de leeuw questions to figure out the 
######## the kinks in the syntax and get the model to function which it did in the end even
######## though it is slightly different that the solution (naming of variables etc)



