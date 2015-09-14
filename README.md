# driver-telematic-analysis

In this project, the trips that are not from the driver of interest need to be recognized. 
The features that are used for this assignment are: 
* Maximum Distance 
* Maximum Time 
* Minimum Distance 
* Minimum Time 
* Mean Cruise Speed 
* Mean Acceleration from Stop 
* Mean Deceleration to Stop 
* No of Stops 

Each time ith class is compared against the random sample from the other 11 classes using Random Forest that is a classifier that uses a set of binary rules applied to calculate a target value. 
A different subset of the training data are selected, with replacement, to train each tree Remaining training data (OOB) are used to estimate error and variable importance Class assignment is made by the number of votes from all of the trees and for regression the average of the results is used. A function called getProb is used to estimate the probabilities of trips if they belong to same driver or not. High probabilities indicate that the trips belong to the driver and Low probabilities indicate that particular driver does not drive the trips. 
The submitted file contains 14400 trips with the probabilities.
