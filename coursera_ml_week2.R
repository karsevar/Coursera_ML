housing_url <- "/Users/masonkarsevar/desktop/machine-learning-ex1/ex1/ex1data2.txt"
housing <- read.table(housing_url, sep = ",")
colnames(housing) <- c("size","bedrooms","price")

inverse()

##normal equation exercise:
normal_equation <- function(data, subset1, subset2){
	x <- data[,subset1]
	y <- data[,subset2]
	X <- data.frame(X1 = c(rep(1, nrow(data))), x)
	X <- as.matrix(X)
	theta <- c(rep(NA, ncol(data)))
	theta <- solve(t(X)%*%X)%*%t(X)%*%y 
	return(theta)
}
normal_equation(housing, c("size","bedrooms"), c("price"))# I guess this function worked. will need to check if the base R lm() function will have the same answer.

housing_fit <- lm(price ~ bedrooms + size, data = housing)
summary(housing_fit)#No way!!! the base regression model within R has the same values for the theta 0 through 2 values. This is very exciting.

X <- data.frame(x1 = c(rep(1, nrow(housing))), housing[,c("size","bedrooms")])
y <- housing[,"price"]

X <- as.matrix(X)
((solve(t(X)%*%X)%*%t(X))%*%y)

##normalization function:
normalize_fun <- function(data, subset){
	X <- data[,subset]
	X_norm <- matrix(rep(NA, times = (nrow(X) * ncol(X))), nrow = nrow(X), ncol = ncol(X))
	for(i in 1:ncol(X)){
		for(j in 1:nrow(X)){
			X_norm[j,i] <- ((X[j,i] - mean(X[,i]))/(max(X[,i])-min(X[,i])))
		}
	}
	return(X_norm)
}

X_normalized <- normalize_fun(housing, c("size","bedrooms"))# Not bad for the first try but this function could be less convoluted. will need to look for into this.

#Experimentation to get the function above:
X <- housing[,c("size","bedrooms")]
y <- housing[,c("price")]
X_norm <- matrix(rep(NA, times = (nrow(X) * ncol(X))), nrow = nrow(X), ncol = ncol(X))
for(i in 1:ncol(X)){
	for(j in 1:nrow(X)){
		X_norm[j,i] <- ((X[j,i] - mean(X[,i]))/(max(X[,i]) - min(X[,i])))
	}
}

##Gradient Descent exercise:
X <- normalize_fun(housing, c("size","bedrooms"))
iterations <- 15
theta_mat <- matrix(rep(NA, times = iterations*ncol(X)), ncol = ncol(X), nrow = iterations)
theta <- c(500000, 160000)
theta <- as.matrix(theta)
learning_rate <- 0.1
for(i in 1:iterations){
	for(j in 1:nrow(X)){
	theta <- (1/(2*ncol(X)))*sum((t(theta)%*%X) - y)^2
	theta[,i] <- theta 
}

#Single variable experimentation:
X <- normalize_fun(housing, c("size","bedrooms"))
X <- data.frame(index0 = c(rep(1, times = nrow(X))), X)
X <- as.matrix(X)
y

iteration <- 1000
theta <- c(10000, 1000, 1000)
theta_0 <- 100000
theta_1 <- 100000
theta_2 <- 100000
learning_rate <- 0.005
theta_0_new <- NA
theta_1_new <- NA
theta_2_new <- NA 
theta <- matrix(rep(NA, times = 3000), nrow = 1000, ncol = 3)
for(iter in 1:iteration){
	theta_0_new <- theta_0 - (learning_rate*1/nrow(X))*sum(((theta_0 + theta_1*X[,1] + theta_2*X[,1])-y))
	theta_1_new <- theta_1 - (learning_rate*1/nrow(X))*sum(((theta_0 + theta_1*X[,2] + theta_2*X[,3]) - y)*X[,2])
	theta_2_new <- theta_2 - (learning_rate*1/nrow(X))*sum(((theta_0 + theta_1*X[,2] + theta_2*X[,3])-y)*X[,3])
	theta_0 <- theta_0_new
	theta_1 <- theta_1_new 
	theta_2 <- theta_2_new
	theta[iter,] <- c(theta_0_new, theta_1_new, theta_2_new)
}

##Attempt 2:
theta <- c(12, 15, 25)
theta <- as.matrix(theta)
X_normalized <- normalize_fun(housing, c("size","bedrooms"))
X_normalized <- data.frame(x1 = rep(1, nrow(housing)), X_normalized)
X_normalized <- as.matrix(X_normalized)
t(theta) %*% t(X_normalized)

iteration <- 100
theta <- c(100000, 10000, 100000)
theta_new <- rep(NA, times = 3)
learning_rate <- 0.01
theta_matrix <- matrix(rep(NA, times = 300), ncol = 3, nrow = 100)
for(iter in 1:iteration){
	for(j in 1:ncol(X)){
		for(i in 1:nrow(X)){
			theta_sum <- (t(theta)%*%t(X_normalized) - t(y))*X_normalized[i,j]
		}
		theta_new[j] <- theta[j] - ((learning_rate * 1/nrow(X))*theta_sum)
	}
	theta_matrix[iter,] <- theta
	theta <- theta_new	  
}

#New idea:
iteration <- 100
theta <- c(1, 100, 20)
for(iter in 1:iteration){
	theta_sum <- t(theta)%*%t(X_normalized) - t(y)
	theta_new <- (theta - (learning_rate*1/nrow(X_normalized))) %*% (theta_sum %*% X_normalized)
	theta <- theta_new 
}

#Ridge regression normal equation:
A <- diag(3)
A[1,1] <- 0 #this is the for the identity matrix that is used with the regularization parameter sigma.
sigma <- 0# This hyperparameter setting will create a linear regression model.
theta_hat <- (solve((t(X)%*%X) + (sigma*A))%*%(t(X)%*%y))#There we go. To get the same answer as the normal equation for the linear regression model not scale the x variables. this might be a problem later on but at least I know that the ridge regression equation works.
summary(lm(y ~ X_normalized))#Interesting when you use the lm() function with the scaled X variables you get different theta values will need to look into why this is. Perhaps I might be correct after all with my gradient descent implementations.
