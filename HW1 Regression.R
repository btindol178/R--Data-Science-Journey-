# Random data simulation variables
n=100;sd=3;sd2 =5

#Actual random simulation
x =rnorm(n, 10, sd2)
e =rnorm(n, 0, sd)


# Visualizing xi
hist(x)
#mean of xi
mean(x)
# sd of xi
sd(x) 


# visualizing ei
hist(e)
# mean of ei
mean(e)
#sd of ei
sd(e) 

plot(x)

df <- data.frame(x,e)

# estimation of yi for each value of x
df$yi <- 2+.1*x + e

plot(df$x,df$yi,ylab = "Mother Height",xlab = "Daughter Height",main = "Simulated Data Blake Tindol")
abline(lm(df$y ~ df$x, data = df), col = "blue")
