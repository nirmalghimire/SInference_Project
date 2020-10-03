#Part 1:Simulation
#setting seed for generating data
set.seed(-123)

#setting up 40 exponential
n<-40

#setting lambda to 0.2 for all simulations
lambda<- 0.2

#Need 1000 simulations
S<-1000

#setting z value for 95% confidence interval (CI)
z<-1.96

#A.	Simulating data in R and calculating means
#Creating dataframe with 40 columns and 1000 exponential simulations
mydata<-matrix(rexp(n*S,lambda),nrow=S)

#Checking if the data frame is created
summary(mydata)

#B.	Calculating Sample Mean and Theoretical Mean Using Simulated Data
#mean of simulated data per row 
Row_Mean<-rowMeans(mydata)

#Checking if tha worked
summary(Row_Mean)

#Single mean of all row means
MeanOfMean<-mean(SMean)

#Printing the Simulated Mean
MeanOfMean

#Calculating Theoretical Mean 
theoreticalMean<-1/lambda

# Printing the Theoretical Mean
theoreticalMean

#C.	Calculating Sample Variance and Theoretical Variance Using Simulated Data
sampleVar<-var(Row_Mean)
#printing the sample variance
sampleVar

#Calculating the theoretical variance
theoreticalVar<-(1/lambda)^2/(n)
#Printing the theoretical variance
theoreticalVar

#D.	Checking Data Distribution and Assessing if they are Approximately Normal
#Histogram of the sample mean
hist(SMean, 
     main="Histogram of Sample Data Distribution",
     xlab="Mean",
     xlim=c(2,8),
     col="darkmagenta",
     freq=FALSE
     )