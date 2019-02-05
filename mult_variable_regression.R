"
You've been contacted by a group of people. 
They want you to fit a regression model that can be interpreted. 
The group has recorded some top secret data, which needs Presidential Security
Clearance to access. Due to obvious reasons, they cannot tell you what each feature
of the data pertains to. You must fit a regression model on the response variables 
(final three columns) 
Their Cyber-Security team has only approved the use of numpy library in python a
nd base R as your tools. They don't have time to review another library and you need to act now!
  They need to be predicting accurately by Monday! 
  You told them that you've only attended two weeks of training in Data Science, 
and can build Regression models from scratch with L1/L2/Elastic Net Regularization. 
They have no one else they can turn to. You're the only one they can trust. 

You've already agreed to it. You want the experience don't you? you ask yourself.
On the bright side, at least you get to use your One-Hot Encoding function if you 
find predictor variables that are categorical. 
"

onehotencoding <- function(x, name){
  items <- unique(x)
  out <- matrix(0, nrow = length(x), ncol = length(items))
  
  for (i in 1:length(items)){
    out[x == items[i],i] <- 1
  }
  
  out <- data.frame(out)
  colnames(out) <-paste(name, items, sep = '_')
  
  return (out)
}




mydata <- read.csv("c:/Users/chris/Desktop/DATA SCIENCE JOB/Regression_project_3_2_1_2019.csv")


mydata <- cbind(mydata, onehotencoding(mydata$predictor.3, 'pred.3'))
mydata <- cbind(mydata, onehotencoding(mydata$predictor.95, 'pred.95'))

# get the featrue and response
feature_col <- !(colnames(mydata) %in% c('predictor.3','predictor.95' ,'response', 'response.1', 'response.2'))
X <- mydata[,feature_col]
y <- mydata[,'response']

# normalization:

normalize <- function(x){
  mymean <- mean(x)
  mystd <- sd(x)
  return ((x-mymean)/mystd)
}

X <- apply(X, 2, normalize)


# split data into two groups of train and test
set.seed(45)
indx <- sample(nrow(mydata), 0.05*nrow(mydata))

X_train <- X[indx,]
X_test <- X[-indx,]
y_train <- y[indx]
y_test <- y[-indx]


# function to estimate the multilinear regression coefficients
regression <- function(X,y, epochs=500, lr=1e-5, tol=1e-5, l2=0){
  
  # add interception column to feature matrix
  X <- cbind(rep(1, nrow(X)), X)
  
  # number of coefficients to estiamte
  nvar <- ncol(X)
  
  # initialize the coefficients
  w <- runif(nvar)
  
  # iterate for epochs
  for (i in 1:epochs){
    w_mod <- w
    w_mod[1] <- 0
    
    # gradient descent update 
    update <- lr*(t(X) %*% (X %*% w - y) + l2*w_mod)
    w <- w - update
    
    error <- t(X %*% w - y) %*% (X %*% w - y)
    
    if (sqrt(sum(update^2))  < tol){
      break
    }
    
  }
  return (w)
}

# function to predict the target values using the feature and estimated coefficients
prediction <- function(w, X){
  X <- cbind(rep(1, nrow(X)), X)
  return (X %*% w)
}

# function to calculcate r2
r2 <- function(y_true, y_pred){
  ssres <- sum((y_true - y_pred)^2)
  sstot <- sum((y_true - mean(y_true))^2)
  return (1- ssres/sstot)
}

# estimate multilinear regression coefficients
coeff <- regression(X_train, y_train)


# predict the target values for the test set
pred <- prediction(coeff, X_test)
# calculate r2 for the test set
r2(y_test, pred)

plot(y_test, pred)
abline(a=1, b=0, col='red')

#summary(lm(y_train ~ X_train))
