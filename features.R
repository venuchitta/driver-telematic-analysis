load("C:/Users/RobotoCop/Documents/School/USU/Spring15/STAT5820/Assignment2/sampleDrivers-5.RData")

numdrivers = length(sampdrivers)
drivernum = matrix(0,1,numdrivers)
drivers = matrix(nrow=numdrivers, ncol = 15, byrow=TRUE)

for(i in 1:numdrivers) {
  drivernum[i] = strsplit(sampdrivernums[i], split="/")[[1]][2]
  driver = sampdrivers[[i]]
  numtrips = length(driver)
  dist = matrix(0,1,numtrips)
  time = matrix(0,1,numtrips)
  north = matrix(0,1,numtrips)
  south = matrix(0,1,numtrips)
  east = matrix(0,1,numtrips)
  west = matrix(0,1,numtrips)
  
  cruiseSpeed = matrix(0,1,numtrips)
  stopTime = matrix(0,1,numtrips)
  deccToStop = matrix(0,1,numtrips)
  accelFromStop = matrix(0,1,numtrips)
  
  
  for(j in 1:numtrips) {
    trip = driver[[j]]
    time[j] = length(trip$x) # seconds
    dist[j] = sum(sqrt(diff(trip$x)^2 + diff(trip$y)^2)) #meters
    north[j] = max(trip$y)
    south[j] = min(trip$y)
    east[j] = max(trip$x)
    west[j] = min(trip$x)
    
    speed = sqrt(diff(trip$x)^2 + diff(trip$y)^2)
    stopTime[j] = sum(speed==0)
    acceleration = diff(speed)
    stopPoints = which(speed==0)

    size = 4
    accel = rep(0,length(stopPoints))
    decel = rep(0,length(stopPoints))
    if(length(stopPoints>0)) {
      for(k in 1:length(stopPoints)) {
        if (stopPoints[k]<length(speed)-size) {
          accel[k] = max(diff(speed[stopPoints[k]:(stopPoints[k] + size)]))
        }
        if (stopPoints[k]>size) {
          decel[k] = min(diff(speed[(stopPoints[k] - size):stopPoints[k]]))
        }
      }
      accelFromStop[j] = sum(accel)/length(accel)
      deccToStop[j] = sum(decel)/length(decel)
    }
    
    cruisePoints = which(acceleration<1) #no acceleration (cruising speed)
    if(length(cruisePoints)>0) {
      total = 0
      for(k in 1:length(cruisePoints)) {
        total = total + speed[k]
      }
      
      cruiseSpeed[j] = total/length(cruisePoints)
    }
  }
  
  mindist = min(dist)
  maxdist = max(dist)
  mintime = min(time)
  maxtime = max(time)
  
  final.north = max(north)
  final.south = min(south)*-1
  final.east = max(east)
  final.west = min(west)*-1
  
  meandist = sum(dist)/length(driver)
  meantime = sum(time)/length(driver)
  meanspeed = meandist/meantime
  meanCruise = sum(cruiseSpeed)/length(driver)
  meanAccel = sum(accelFromStop)/length(driver)
  meanDecel  = sum(deccToStop)/length(driver)
  meanStopTime= sum(stopTime)/length(driver)
  
  drivers[i,1] = maxdist
  drivers[i,2] = final.north
  drivers[i,3] = final.west
  drivers[i,4] = final.south
  drivers[i,5] = final.east
  drivers[i,6] = maxtime
  drivers[i,7] = mindist
  drivers[i,8] = mintime
  drivers[i,9] = meandist
  drivers[i,10] = meantime
  drivers[i,11] = meanspeed
  drivers[i,12] = meanCruise
  drivers[i,13] = meanAccel
  drivers[i,14] = meanDecel
  drivers[i,15] = meanStopTime
}

colnames(drivers) = c("Max Distance", "Max North Distance", "Max West Distance",
                      "Max South Distance", "Max East Distance", "Max Time", 
                      "Min Distance", "Min Time", "Mean Distance", "Mean Time",
                      "Mean Speed", "Mean Cruise Speed", "Mean Acceleration From Stop",
                      "Mean Decceleration to Stop", "Mean Stop Time")
rownames(drivers) = c(drivernum)
rownames(drivers) = paste("Driver", rownames(drivers), sep = " ")
print(drivers)