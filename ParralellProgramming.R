# BIG DATA WRITING EFFICIENT R CODE
df <-read.csv("trendweather1.csv")
df <- df[-c(1)]

mat <- as.matrix(df)
# WHICH VERSION OF R DO WE HAVE
# Print the R version details using version
version

# Assign the variable major to the major component
major <- version$major

# Assign the variable minor to the minor component
minor <- version$minor

# How long does it take to read movies from CSV?
# system.time(read.csv("trendweather1.csv))

# COMPARE THE TWO DIFFERENT FILE TIMES UPLOADING........
# Load the package
library(microbenchmark)

# Compare the two functions
compare <- microbenchmark(read.csv("movies.csv"), 
                          readRDS("movies.rds"), 
                          times = 10)

# Print compare
compare
#################################################################################

# NEEEEEEEEEEEEEEEEEEED TO DOWNLOAD R TOOLS......................
Sys.which("make")

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

install.packages("jsonlite", type = "source")

install.packages("devtools")
library(devtools)
find_rtools() ##########################
########################################################################3
# HOW GOOD IS MY MACHINE?(HARDWARE!!!!!!!!!!!!!!!!!!!!!!!!!!!)
# Load the package
install.packages("benchmarkme")
library(benchmarkme)

# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()

# Assign the variable cpu to the cpu specs
cpu <- get_cpu()

# Load the package
library("benchmarkme")

# Run the benchmark
res <- benchmark_io(runs = 1, size = 5)

# Plot the results
plot(res)
#########################################################################
# FINE TUNING EFFICIENT R CODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# MEMORY ALLOCATION!!!!!!!!!!!!!

# Use <- with system.time() to store the result as res_grow
system.time(res_grow <- growing(n))

# Use <- with system.time() to store the result as res_allocate
n <- 30000
system.time(res_allocate <- pre_allocate(n))

# vectorizing code..............................
x <- rnorm(10)
x2 <- numeric(length(x))

# USING FORE LOOP TO TIME EACH VALUE OF X TO EACHOTHER AND STORE IN X2
for(i in 1:10){
  x2[i] <- x[i] * x[i]
}

# VETORIZED VERSION OF THAT
x2_imp <- x * x # VECTORIZED VERSION OF THE FOR LOOP ABOVE

# NON VECTORIZED CODE PART 2
# x is a vector of probabilities
total <- 0
for(i in seq_along(x)){
  total <- total + log(x[i])
}


# MATRIX MANIPULATION IS FASTER THAN DATAFRAME
# Which is faster, mat[, 1] or df[, 1]? 
microbenchmark(mat[, 1], df[, 1])

# Which is faster, mat[1, ] or df[1, ]? 
microbenchmark(mat[1, ], df[1, ])


################################################################
# PLOTTING PERFORMANCE OF CODE
# Load the profvis package
install.packages("profvis")
library(profvis)

# Profile the following code
profvis({
  # Load and select data
  comedies <- movies[movies$Comedy == 1, ]
  
  # Plot data of interest
  plot(comedies$year, comedies$rating)
  
  # Loess regression line
  model <- loess(rating ~ year, data = comedies)
  j <- order(comedies$year)
  
  # Add fitted line to the plot
  lines(comedies$year[j], model$fitted[j], col = "red")
})
################################################################
# Load the microbenchmark package
library(microbenchmark)

# The previous data frame solution is defined
# d() Simulates 6 dices rolls
d <- function() {
  data.frame(
    d1 = sample(1:6, 3, replace = TRUE),
    d2 = sample(1:6, 3, replace = TRUE)
  )
}

# Complete the matrix solution
m <- function() {
  matrix(sample(1:6, 6, replace = TRUE), ncol = 2)
}

# Use microbenchmark to time m() and d()
microbenchmark(
  data.frame_solution = d(),
  matrix_solution     = m()
)
###################################################
# Example data
rolls

# Define the previous solution 
app <- function(x) {
  apply(x, 1, sum)
}

# Define the new solution
r_sum <- function(x) {
  rowSums(x)
}

# Compare the methods
microbenchmark(
  app_sol = app(rolls),
  r_sum_sol = r_sum(rolls)
)
###########################################################
# Example data
is_double

# Define the previous solution
move <- function(is_double) {
  if (is_double[1] & is_double[2] & is_double[3]) {
    current <- 11 # Go To Jail
  }
}

# Define the improved solution
improved_move <- function(is_double) {
  if (is_double[1] && is_double[2] && is_double[3]) {
    current <- 11 # Go To Jail
  }
}

## microbenchmark both solutions
microbenchmark(move(is_double), improved_move(is_double), times = 1e5)

#####################################################################################################
# PARRALELL PROCESSING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Load the parallel package
library(parallel)

# Store the number of cores in the object no_of_cores
no_of_cores <- detectCores()

# Print no_of_cores
no_of_cores # 4 CORES

###################################################################################################
# CAN THIS LOOP RUN ON PARRALELL PROCESSING
total <- no_of_rolls <- 0 # Initialise
while(total < 10) {
  total <- total + sample(1:6, 1)
  
  if(total %% 2 == 0) total <- 0  # If even. Reset to 0
  
  no_of_rolls <- no_of_rolls + 1
}

# NO BECAUSE the ith value depends on the previous value....
#############################################################################
# can this loop run in parralell???????????????????????????????
play <- function() {
  total <- no_of_rolls <- 0
  while(total < 10) {
    total <- total + sample(1:6, 1)
    
    # If even. Reset to 0
    if(total %% 2 == 0) total <- 0 
    no_of_rolls <- no_of_rolls + 1
  }
  no_of_rolls
}
# and construct a loop to play the game:
  
  results <- numeric(100)
for(i in seq_along(results)) 
  results[i] <- play()
# yess  Yes, this is embarrassingly parallel. We can simulate the games in any order.
############################################################################################3
#parrallel programming
library("parallel")
dd <- read.csv("dd.csv")
  # Determine the number of available cores.
  detectCores()
  
  # Create a cluster via makeCluster
  cl <- makeCluster(2)
  
  # Parallelize this code
  parApply(cl, dd, 2, median)
  
  # Stop the cluster
  stopCluster(cl)

####################################################################
  play <- function() {
    total <- no_of_rolls <- 0
    while(total < 10) {
      total <- total + sample(1:6, 1)
      
      # If even. Reset to 0
      if(total %% 2 == 0) total <- 0 
      no_of_rolls <- no_of_rolls + 1
    }
    no_of_rolls
  }
  
  library("parallel")
  # Create a cluster via makeCluster (2 cores)
  cl <- makeCluster(2)
  
  # Export the play() function to the cluster
  clusterExport(cl, "play")
  
  # Re-write sapply as parSapply
  res <- parSapply(cl, 1:100, function(i) play())
  
  # Stop the cluster
  stopCluster(cl)
  
##################################################################################
  
  # Set the number of games to play
  no_of_games <- 1e5
  
  ## Time serial version
  system.time(serial <- sapply(1:no_of_games, function(i) play()))
  
  ## Set up cluster
  cl <- makeCluster(4)
  clusterExport(cl, "play")
  
  ## Time parallel version
  system.time(par <- parSapply(cl, 1:no_of_games, function(i) play()))
  
  ## Stop cluster
  stopCluster(cl)