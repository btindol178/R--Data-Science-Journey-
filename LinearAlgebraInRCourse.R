# Linear algebra course
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
rm(list=ls())
setwd("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT")
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
# Vectors
x <- rep(1, 4);x

# 2 to 8 by 2 
y <- seq(2,8, by = 2);y

# another way to make vector
z <- c(1,5,-2,4);z

# you can change elements of a vector by index
z[3] <- 7;z

# Matrices (Cases rows and features as columns)
matrix(2,3,2) # 2x3 matrix of all 2

# element by element creation
matrix(c(1,-1,2,3,2,-2), nrow = 2, ncol =3, byrow=TRUE)

# Change byrow
A <- matrix(c(1,-1,2,3,2,-2), nrow = 2, ncol =3, byrow=FALSE)

# Change assignment by index as well
A[2,1] <- 100 ;A

###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
# Matrix vector operations

# Create the matrix
A <- matrix(c(1,-1,2,1,4,-2), nrow= 3, ncol = 2, byrow = TRUE)
A

# Create the Vector
b <- c(1,2);b

# Matrix to vector multiplication
A%*%b

###################################################################################################################################
# Create a new matrix and vector
A <- matrix(c(1,3,2,1,1,4),nrow = 2, ncol = 3, byrow = TRUE);A

# the matrix will look like this
# 1,3
# 2,1
# 1,4
#Create a new vector
b <- c(0,1,2)
#0
#1
#2


# How matrix vector Multiplication works
# Each element of a multiplied by b is a product of a particular row of A and b
# This is the dot product
#
A[1,]%*%b # 1*0 + 3*1 + 2*2 = 7

A[2,]%*%b # 1*0 + 1*1 + 4*2 = 9

A%*%b

###################################################################################################################################
# Matrix - Matrix calculations

# New matrix
A <- matrix(c(1,2,2,1),nrow=2,ncol=2,byrow = TRUE);A
#1 2
#2 1
b <- matrix(c(0,1,1,2),nrow=2,ncol=2,byrow = TRUE);b
#0 1
#1 2

# The final matrix output after matrix multiplication  will be
#2 5
#1 4
#Multiply them rows by columns first matrix first row by second matrix first row
A%*%b  # 1*0 + 2*2 then do second column of second matrix by first row of first matrix 1*1 + 2*2 


# Complete matrix multiplication the matrix will be 2x2
# matrix 1 row 1 column 1 times matrix 2 row 1 column 1 then matrix 1 column 2 times matrix 2 row 2 column 1
# final matrix column 1 row 1
first <-  A[1,1]*b[1,1] + A[1,2]*b[2,1] # index 1 of matrix add these 2 numbers
# The final matrix row 1 column 2
second <- A[1,1]*b[1,2]  + A[1,2]*b[2,2] 
# The final matrix column 1 row 2
third <- A[2,1]*b[1,1]  + A[2,2]*b[2,1] 
# The final matrix column 2 row 2
fourth <- A[2,1]*b[1,2]  + A[2,2]*b[2,2] 

finalmatrix <- matrix(c(first,second,third,fourth),nrow = 2, ncol = 2, byrow = TRUE)
finalmatrix

# B * A is not A * B matrix multiplication is not commutative
A%*%b

b%*%A

# see they are not the same matrix multiplication
A%*%b == b%*%A

# and A*b is not the same
A*b

# Identity matrix this matrix
# when multiplied by similar dimenstion matrix or vector will return similar matrix back
print(A)

I <- diag(2)
print(I)

I%*%A # Origional matrix

A%*%I # Original matrix

## Take the inverse of the 2 by 2 identity matrix
I <- diag(2);I # Identity matrix

solve(diag(2)) # inverse of identity

print(A) # A matrix

Ainv <- solve(A);Ainv # Inverse of A

Ainv%*%A # multiply Ainverse by A

A%*%Ainv # multiply A by its inverse

###################################################################################################################################
#Solving Matrix Vector Equations
#Massey Matrix
M <- read.csv("WMBAmasseymatrix.csv");colnames(M)[1] <- 'Atlanta';M
M <- as.matrix(M)
# Differentials
f <-read.csv("WMBAdifferential.csv");rownames(f) <- f[,1]; f <- f[-c(1)];colnames(f)[1]<-"Differential";f
f <- as.vector(f)
f <- rbind(f,0)

# Find the sum of the first column of M
sum(M[, 1])

# Find the sum of the vector f
sum(f)
# No solution is parallel planes or lines
# if there is a solution there is a cross in planes

#For our WNBA Massey Matrix model, some adjustments need to be made for a solution to our rating problem to exist and be unique.
# cannot invert matrix 
z <- read.csv("matrixtoinvert.csv");z
solve(z)

# Add a row of 1's
z <- rbind(z, rep(1, 12));M_2
z <- as.matrix(z)
# Add a column of -1's 
M_3 <- cbind(M_2, rep(-1, 12))
M_3 <- rbind(M_3,rep(1, 12))

# Change the element in the lower-right corner of the matrix
M_3[13, 13] <- 1

# Print M_3
print(M_3)

# Inverse of M
solve(M_3)

# Solving matrix vector equation
r <- solve(z)%*%f

# Moore - Penrose Generalized Inverse
library(MASS)
print(A)

# generalized inverse
ginv(A)

# A times gen a
ginv(A)%*%A
A%*%ginv(A)

ginv(z)%*%f

####################################################################################################
#Eigen vectors and values
#they take a collection of objects that can be highly dimensional and take a subset that accuratly depicts 

# Multiply A by the given vector
print(A%*%c(1, 1))

print(A%*%c(1, 0))

print(A%*%c(4, 0))

# Show that double an eigenvector is still an eigenvector
A%*%((2)*c(0.2425356, 0.9701425, 0)) - 7*(2)*c(0.2425356, 0.9701425, 0)

# Show half of an eigenvector is still an eigenvector
A%*%((0.5)*c(0.2425356, 0.9701425, 0)) - 7*(0.5)*c(0.2425356, 0.9701425, 0)

# Eigen values
eigen(A)

E <- eigen(A)# get eigen
E$values[1] # get values
E$vectors[,1] # get vectors

# if multiplied together we get real number
eigen(A)$values[1]*eigen(A)$values[2]


# Compute the eigenvalues of A and store in Lambda
Lambda <- eigen(A)

# Print eigenvalues
print(Lambda$values[1])
print(Lambda$values[2])

# Verify that these numbers satisfy the conditions of being an eigenvalue
det(Lambda$values[1]*diag(2) - A)
det(Lambda$values[2]*diag(2) - A)


# Find the eigenvectors of A and store them in Lambda
Lambda <- eigen(A)

# Print eigenvectors
print(Lambda$vectors[, 1])
print(Lambda$vectors[, 2])

# Verify that these eigenvectors & their associated eigenvalues satisfy lambda*v - A*v = 0
Lambda$values[1]*Lambda$vectors[, 1] - A%*%Lambda$vectors[, 1]
Lambda$values[2]*Lambda$vectors[, 2] - A%*%Lambda$vectors[, 2]

# stationary ldistribution of markov model
Lambda <- eigen(A)
V1 <- Lambda$vectors[,1]/sum(Lambda$vectors[,1])
V1 # Hardy-weinberg principle in genetics

# Markov models for allele frequencies
# This code iterates mutation 1000 times
x <- c(1, 0, 0, 0)
for (j in 1:1000) {x <- M%*%x}

# Print x
print(x)

# Print and scale first eigenvector of M
Lambda <- eigen(M)
v1 <- Lambda$vectors[, 1]/sum(Lambda$vectors[, 1])

# Print v1
print(v1)

##########################################################################################################
##########################################################################################################
##########################################################################################################
#Principle component analysis
combine <- read.csv("NFLPlayerdataset.csv");colnames(combine)[1] <- "player";combine

# Find the correlation between variables forty and three_cone
cor(combine$forty, combine$three_cone)

# Find the correlation between variables vertical and broad_jump
cor(combine$vertical, combine$broad_jump)


# PCA theory
A <- matrix(c(1,2,2,4,3,6,4,8,5,10),nrow = 5,ncol = 2, byrow=TRUE);A

#Subtract mean from each column
A[,1]<- A[,1]-mean(A[,1])
A[,2]<- A[,2]-mean(A[,2])
A

#Then do a transpose matrixmultiplied b nrow in a 
t(A)%*%A/(nrow(A)-1) # Get 2x2 matrix (covariance between first and second variable shoudl be same as second and first hence 5 in diagonal )
var(A[,1]) ;var(A[,2]) # diagonal terms are the variances of each of the columns

# egien data for matrix a
eigen(t(A)%*%A/(nrow(A)-1))
# One eigen value makes up all total variance because second column is just twice as big as first

# Apply to the combine dataset now
# Extract columns 5-12 of combine
A <- combine[, 5:12]

# Make A into a matrix
A <- as.matrix(A)

# Subtract the mean of each column
A[, 1] <- A[, 1] - mean(A[, 1])
A[, 2] <- A[, 2] - mean(A[, 2])
A[, 3] <- A[, 3] - mean(A[, 3])
A[, 4] <- A[, 4] - mean(A[, 4])
A[, 5] <- A[, 5] - mean(A[, 5])
A[, 6] <- A[, 6] - mean(A[, 6])
A[, 7] <- A[, 7] - mean(A[, 7])
A[, 8] <- A[, 8] - mean(A[, 8])

# Create matrix B from equation in instructions
B <- t(A)%*%A/(nrow(A) - 1)

# Compare 1st element of the 1st column of B to the variance of the first column of A
B[1,1]
var(A[, 1])

# Compare 1st element of 2nd column of B to the 1st element of the 2nd row of B to the covariance between the first two columns of A
B[1, 2]
B[2, 1]
cov(A[, 1], A[, 2])

# Find eigenvalues of B
options(scipen = 999)
V <- eigen(B)

# Print eigenvalues
V$values 
# We can see the first principle component accoutns for 96 % of the variance
V$values[1]/sum(V$values)

##################################################################################################################################
##################################################################################################################################
# Now do PCA in R without linear algebra
head(combine[c(5:12)])

# do principle component analysis
prcomp(A) # notice the variability is posed in terms of standard deviaitions which is just square roots of the variances 
# weight is given the largest principle component

#compare how much they contribute 
summary(prcomp(A))

# apply to data
head(prcomp(A)$x[,1:2])

# Adding principle component analysis to data!
head(cbind(combine[,1:4],prcomp(A)$x[,1:2]))

#########################################################################################################
#Lets do pca again
# Scale columns 5-12 of combine
B <- scale(combine[, 5:12])

# Print the first 6 rows of the data
head(B)

# Summarize the principal component analysis
summary(prcomp(B))

# Subset combine only to "WR"
combine_WR <- subset(combine, position == "WR")

# Scale columns 5-12 of combine_WR
B <- scale(combine_WR[, 5:12])

# Print the first 6 rows of the data
head(B)

# Summarize the principal component analysis
summary(prcomp(B))