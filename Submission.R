# read in the 72-driver dataset
library(randomForest)
library(ROCR)
library(microbenchmark)
load("sampleDriversChitta-2")


drivernum = vector(length=length(sampdrivers))
for(i in 1:length(sampdrivers)){
  drivernum[i] = strsplit(sampdrivernums, split="/")[[i]][2]
}


calParams <- function(trip){
  noStops=0
  size=4
  speed  = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2)
  dist = sum(sqrt(diff(trip[,1])^2 + diff(trip[,2])^2)) #meters
  time = length(trip[,1]) # seconds
  minDist = min(dist)
  maxDist = max(dist)
  minTime = min(time)
  maxTime = max(time)
  acceleration = diff(speed)
  speedProb = sum(acceleration == 0)/length(acceleration)  
  noStops = sum(speed == 0)
  if (noStops!=0)
  {
    speedPoints = which(speed == 0)
    accelerationAfter = rep(0, noStops)
    for (i in 1:noStops) {
      if (speedPoints[i]<length(speed)-size){
        accelerationAfter[i] = max(diff(speed[speedPoints[i]:(speedPoints[i] + size)]))}
    }
    accelerationBefore = rep(0, noStops)
    for (i in 1:noStops) {
      if (speedPoints[i]>size){
        accelerationBefore[i] = min(diff(speed[(speedPoints[i] - size):speedPoints[i]]))
      }
    }
    aa = quantile(accelerationAfter,names=FALSE)
    ab = quantile(accelerationBefore,names=FALSE)
    return(c(speedProb,list(noStops),aa[3],ab[3],minDist,maxDist,minTime,maxTime))
  }
  else return (c(speedProb,list(0),0,0,minDist,maxDist,minTime,maxTime))
}

stopList=list()
for(i in 1:length(sampdrivers)){
  print (i)
  stopList[[i]] = t(sapply(sampdrivers[[i]], calParams))
}

getFeatures = data.frame()
for (i in 1:length(sampdrivers)){
  #getFeatures = unname(getFeatures)
  getFeatures = rbind(getFeatures, stopList[[i]])
  print (i)
}

# method 1: use RF to fit a 72-class problem
response = rep(drivernum, each=200)
rfout = randomForest(getFeatures, as.factor(response), ntree=1000)


# look at the ith class versus a random sample of size nbigclass 
# from the other 11 classes:
rfouti = list(72)
nbigclass = 2000

for(i in 1:72){
  # include all observations from class i:
  imain = (1:nrow(getFeatures))[response==drivernum[i]]
  # take a random sample of the remaining observations
  irest = (1:nrow(getFeatures))[response!=drivernum[i]]
  irestsamp = sample(irest, nbigclass, replace=FALSE)
  respi = c(rep(1, length(imain)), rep(2, length(irestsamp)))
  rfouti[[i]] = randomForest(getFeatures[c(imain, irestsamp),], 
                             as.factor(respi), ntree=500,
                             classwt=c(.95, .05), cutoff=c(.05, .95))
}

# given a matrix of votes, this function estimates the probability of
# the observation coming from class 1
getprob = function(votes){  
  votes[,1] = votes[,1]*95
  votes[,2] = votes[,2]*5
  totals = apply(votes, 1, sum)
  votes = votes/totals
  prob = votes[,1]
}

probs = NULL
template = read.csv("sampleSubmission.csv")
for(i in 1:72){
  # select the relevant votes for this driver
  votes = rfouti[[i]]$votes[1:200,]
  # use function "getprob" to estimate the probabilities 
  probi = getprob(votes)
  drivertrip = paste(drivernum[i], 1:200, sep="_")
  template[template[,1]==drivertrip, 2] = probi
  # combine with previous probabilities
  probs = c(probs, probi)
}

plot(probs)

write.csv(template, "newSubmission.csv", row.names=FALSE)

