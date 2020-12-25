# Workout Program Script
rm(list=ls())
setwd("C:/Users/blake/OneDrive/R Scripts/Workout Program")
xercises <- read.csv("WorkoutExercises.csv");colnames(xercises)[1]<- "Chest"
exercisesofday <- read.csv("ExerciseOfTheDay.csv");colnames(exercisesofday)[1] <- "Date"

library(lubridate)
# Make day of just full day exercises
exercisesofdayfull <- exercisesofday
exercisesofdayfull$Date <- mdy(exercisesofdayfull$Date)
exercisesofday$Date <- mdy(exercisesofday$Date)
#exercisesofdayfull$Date  <- format(as.Date(exercisesofdayfull$Date, format="%m/%d/%Y"),"%Y-%m-%d")
secondaryexercisesofday <- exercisesofday[exercisesofday$Exercise != "Full",];nrow(secondaryexercisesofday)

exercisesofday <- exercisesofday[exercisesofday$Exercise == "Full",];nrow(exercisesofday)


# Make a cosine vector to multiply 10000 to for the volume of the exercise
t <- seq(from =0,to=17.7,by = .1) #177 days of the year are full body exercises
y=cos(t/2) # make the wave length shorter only 2 periods of max out during the year
plot(t,y,type="l", xlab="time", ylab="Sine wave")
y= as.vector(y)
y = y[-length(y)];length(y)
y

exercisesofday$volume <- c(abs(y*10000)+700); exercisesofday$volume <- round(exercisesofday$volume,digits = 0)
exercisesofday
# Change format of date column 
# Final Rep scheme funciton just put in exercise of the day or any dataframe with date and volume column 
{
# Make random Rep scheme and give new database based on column 
RepScheme <- function(x) {
  benchmax = 185
  squatmax = 265
  deadliftmax = 315
  
  if(x < 11000 & x > 8000){
    BenchMaxTimeIntensityMain <- benchmax * runif(1, min=.80, max=.85)
    BenchMaxTimeIntensitySecond <- benchmax * runif(1, min=.60, max=.675)
    RepsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsbenchSecond <- floor(runif(length(exercisesofday$volume), min=8, max=12))
    SetsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=4))
    SetsbenchSecond <- floor(runif(length(exercisesofday$volume), min=7, max=11))
    
    squatMaxTimeIntensityMain <- squatmax * runif(1, min=.80, max=.85)
    squatMaxTimeIntensitySecond <- squatmax * runif(1, min=.60, max=.675)
    RepssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepssquatSecond <- floor(runif(length(exercisesofday$volume), min=8, max=12))
    SetssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=4))
    SetssquatSecond <- floor(runif(length(exercisesofday$volume), min=7, max=11))
    
    deadMaxTimeIntensityMain <- squatmax * runif(1, min=.80, max=.85)
    deadMaxTimeIntensitySecond <- squatmax * runif(1, min=.60, max=.675)
    RepsdeadMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsdeadSecond <- floor(runif(length(exercisesofday$volume), min=8, max=12))
    SetsdeadhMain <- floor(runif(length(exercisesofday$volume), min=1, max=4))
    SetsdeadSecond <- floor(runif(length(exercisesofday$volume), min=7, max=11))
    
    BenchMainDf <- data.frame(BenchMaxTimeIntensityMain = BenchMaxTimeIntensityMain,
                              BenchMaxTimeIntensitySecond =BenchMaxTimeIntensitySecond,
                              BenchMainVolume =BenchMaxTimeIntensityMain * (RepsbenchMain * SetsbenchMain), 
                              BenchSecondVolume = BenchMaxTimeIntensitySecond * (RepsbenchSecond * SetsbenchSecond),
                              RepsbenchMain = RepsbenchMain,
                              RepsbenchSecond = RepsbenchSecond,
                              SetsbenchMain = SetsbenchMain,SetsbenchSecond = SetsbenchSecond,
                              squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                              squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                              squatMaxTimeIntensitySecond = squatMaxTimeIntensitySecond,
                              squatMainVolume =squatMaxTimeIntensityMain * (RepssquatMain * SetssquatMain), 
                              squatSecondVolume = squatMaxTimeIntensitySecond * (RepssquatSecond * SetssquatSecond),
                              RepssquatMain=RepssquatMain,
                              RepssquatSecond=RepssquatSecond,
                              SetssquatMain=SetssquatMain,
                              SetssquatSecond=SetssquatSecond,
                              deadMaxTimeIntensityMain=deadMaxTimeIntensityMain,
                              deadMaxTimeIntensitySecond = deadMaxTimeIntensitySecond,
                              deadMainVolume =deadMaxTimeIntensityMain * (SetsdeadhMain * RepsdeadMain), 
                              deadSecondVolume =deadMaxTimeIntensitySecond * (SetsdeadSecond * RepsdeadSecond), 
                              RepsdeadMain=RepsdeadMain,
                              RepsdeadSecond=RepsdeadSecond,
                              SetsdeadhMain=SetsdeadhMain,
                              SetsdeadSecond=SetsdeadSecond)
    
    } else if (x < 8000 & x > 6000){
      BenchMaxTimeIntensityMain <- benchmax * runif(1, min=.825, max=.9)
      BenchMaxTimeIntensitySecond <- benchmax * runif(1, min=.70, max=.75)
      RepsbenchMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      RepsbenchSecond <- floor(runif(length(exercisesofday$volume), min=5, max=9))
      SetsbenchMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      SetsbenchSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
      
      squatMaxTimeIntensityMain <- squatmax * runif(1, min=.825, max=.9)
      squatMaxTimeIntensitySecond <- squatmax * runif(1, min=.70, max=.75)
      RepssquatMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      RepssquatSecond <- floor(runif(length(exercisesofday$volume), min=5, max=9))
      SetssquatMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      SetssquatSecond <- floor(runif(length(exercisesofday$volume), min=5, max=8))
      
      deadMaxTimeIntensityMain <- squatmax * runif(1, min=.825, max=.9)
      deadMaxTimeIntensitySecond <- squatmax * runif(1, min=.70, max=.75)
      RepsdeadMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      RepsdeadSecond <- floor(runif(length(exercisesofday$volume), min=5, max=9))
      SetsdeadhMain <- floor(runif(length(exercisesofday$volume), min=2, max=4))
      SetsdeadSecond <- floor(runif(length(exercisesofday$volume), min=5, max=8))
      
      BenchMainDf <- data.frame(BenchMaxTimeIntensityMain = BenchMaxTimeIntensityMain,
                                BenchMaxTimeIntensitySecond =BenchMaxTimeIntensitySecond,
                                BenchMainVolume =BenchMaxTimeIntensityMain * (RepsbenchMain * SetsbenchMain), 
                                BenchSecondVolume = BenchMaxTimeIntensitySecond * (RepsbenchSecond * SetsbenchSecond),
                                RepsbenchMain = RepsbenchMain,
                                RepsbenchSecond = RepsbenchSecond,
                                SetsbenchMain = SetsbenchMain,SetsbenchSecond = SetsbenchSecond,
                                squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                                squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                                squatMaxTimeIntensitySecond = squatMaxTimeIntensitySecond,
                                squatMainVolume =squatMaxTimeIntensityMain * (RepssquatMain * SetssquatMain), 
                                squatSecondVolume = squatMaxTimeIntensitySecond * (RepssquatSecond * SetssquatSecond),
                                RepssquatMain=RepssquatMain,
                                RepssquatSecond=RepssquatSecond,
                                SetssquatMain=SetssquatMain,
                                SetssquatSecond=SetssquatSecond,
                                deadMaxTimeIntensityMain=deadMaxTimeIntensityMain,
                                deadMaxTimeIntensitySecond = deadMaxTimeIntensitySecond,
                                deadMainVolume =deadMaxTimeIntensityMain * (SetsdeadhMain * RepsdeadMain), 
                                deadSecondVolume =deadMaxTimeIntensitySecond * (SetsdeadSecond * RepsdeadSecond), 
                                RepsdeadMain=RepsdeadMain,
                                RepsdeadSecond=RepsdeadSecond,
                                SetsdeadhMain=SetsdeadhMain,
                                SetsdeadSecond=SetsdeadSecond)
    
  }else if (x < 6000 & x > 3500){
    BenchMaxTimeIntensityMain <- benchmax * runif(1, min=.85, max=.925)
    BenchMaxTimeIntensitySecond <- benchmax * runif(1, min=.75, max=.82)
    RepsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsbenchSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetsbenchSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    squatMaxTimeIntensityMain <- squatmax * runif(1, min=.85, max=.925)
    squatMaxTimeIntensitySecond <- squatmax * runif(1, min=.75, max=.82)
    RepssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepssquatSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetssquatSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    deadMaxTimeIntensityMain <- squatmax * runif(1, min=.85, max=.925)
    deadMaxTimeIntensitySecond <- squatmax * runif(1, min=.75, max=.82)
    RepsdeadMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsdeadSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetsdeadhMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetsdeadSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    BenchMainDf <- data.frame(BenchMaxTimeIntensityMain = BenchMaxTimeIntensityMain,
                              BenchMaxTimeIntensitySecond =BenchMaxTimeIntensitySecond,
                              BenchMainVolume =BenchMaxTimeIntensityMain * (RepsbenchMain * SetsbenchMain), 
                              BenchSecondVolume = BenchMaxTimeIntensitySecond * (RepsbenchSecond * SetsbenchSecond),
                              RepsbenchMain = RepsbenchMain,
                              RepsbenchSecond = RepsbenchSecond,
                              SetsbenchMain = SetsbenchMain,SetsbenchSecond = SetsbenchSecond,
                              squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                              squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                              squatMaxTimeIntensitySecond = squatMaxTimeIntensitySecond,
                              squatMainVolume =squatMaxTimeIntensityMain * (RepssquatMain * SetssquatMain), 
                              squatSecondVolume = squatMaxTimeIntensitySecond * (RepssquatSecond * SetssquatSecond),
                              RepssquatMain=RepssquatMain,
                              RepssquatSecond=RepssquatSecond,
                              SetssquatMain=SetssquatMain,
                              SetssquatSecond=SetssquatSecond,
                              deadMaxTimeIntensityMain=deadMaxTimeIntensityMain,
                              deadMaxTimeIntensitySecond = deadMaxTimeIntensitySecond,
                              deadMainVolume =deadMaxTimeIntensityMain * (SetsdeadhMain * RepsdeadMain), 
                              deadSecondVolume =deadMaxTimeIntensitySecond * (SetsdeadSecond * RepsdeadSecond), 
                              RepsdeadMain=RepsdeadMain,
                              RepsdeadSecond=RepsdeadSecond,
                              SetsdeadhMain=SetsdeadhMain,
                              SetsdeadSecond=SetsdeadSecond)
  } else {
    BenchMaxTimeIntensityMain <- benchmax * runif(1, min=.85, max=.925)
    BenchMaxTimeIntensitySecond <- benchmax * runif(1, min=.75, max=.82)
    RepsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsbenchSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetsbenchMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetsbenchSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    squatMaxTimeIntensityMain <- squatmax * runif(1, min=.85, max=.925)
    squatMaxTimeIntensitySecond <- squatmax * runif(1, min=.75, max=.82)
    RepssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepssquatSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetssquatMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetssquatSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    deadMaxTimeIntensityMain <- squatmax * runif(1, min=.85, max=.925)
    deadMaxTimeIntensitySecond <- squatmax * runif(1, min=.75, max=.82)
    RepsdeadMain <- floor(runif(length(exercisesofday$volume), min=1, max=3))
    RepsdeadSecond <- floor(runif(length(exercisesofday$volume), min=3, max=5))
    SetsdeadhMain <- floor(runif(length(exercisesofday$volume), min=1, max=6))
    SetsdeadSecond <- floor(runif(length(exercisesofday$volume), min=6, max=9))
    
    BenchMainDf <- data.frame(BenchMaxTimeIntensityMain = BenchMaxTimeIntensityMain,
                              BenchMaxTimeIntensitySecond =BenchMaxTimeIntensitySecond,
                              BenchMainVolume =BenchMaxTimeIntensityMain * (RepsbenchMain * SetsbenchMain), 
                              BenchSecondVolume = BenchMaxTimeIntensitySecond * (RepsbenchSecond * SetsbenchSecond),
                              RepsbenchMain = RepsbenchMain,
                              RepsbenchSecond = RepsbenchSecond,
                              SetsbenchMain = SetsbenchMain,SetsbenchSecond = SetsbenchSecond,
                              squatMaxTimeIntensityMain = squatMaxTimeIntensityMain,
                              squatMaxTimeIntensitySecond = squatMaxTimeIntensitySecond,
                              squatMainVolume =squatMaxTimeIntensityMain * (RepssquatMain * SetssquatMain), 
                              squatSecondVolume = squatMaxTimeIntensitySecond * (RepssquatSecond * SetssquatSecond),
                              RepssquatMain=RepssquatMain,
                              RepssquatSecond=RepssquatSecond,
                              SetssquatMain=SetssquatMain,
                              SetssquatSecond=SetssquatSecond,
                              deadMaxTimeIntensityMain=deadMaxTimeIntensityMain,
                              deadMaxTimeIntensitySecond = deadMaxTimeIntensitySecond,
                              deadMainVolume =deadMaxTimeIntensityMain * (SetsdeadhMain * RepsdeadMain), 
                              deadSecondVolume =deadMaxTimeIntensitySecond * (SetsdeadSecond * RepsdeadSecond), 
                              RepsdeadMain=RepsdeadMain,
                              RepsdeadSecond=RepsdeadSecond,
                              SetsdeadhMain=SetsdeadhMain,
                              SetsdeadSecond=SetsdeadSecond)
    
  }
  exercisesofday <- cbind(exercisesofday,BenchMainDf)
    return(exercisesofday)
}
}

# Put the funciton around the dataframe and then 
finaldf <- RepScheme(exercisesofday)

for(i in 1:nrow(finaldf)){
  randomchest <- NULL;randomsquat <- NULL; randomdead <- NULL;#randomabs <- NULL;randomshoulders <- NULL;randombiceps <- NULL; randomtriceps <- NULL;randomback <- NULL;randomcalves<- NULL;randomforearms <- NULL;
  randomchest <- round(runif(1, 1, 11),digits=0); randomsquat <- round(runif(1, 1, 7),digits=0);randomdead <-round(runif(1, 1, 3),digits=0);# randomabs <- round(runif(1, 1, 5),digits=0);  randomshoulders <- round(runif(1, 1, 7),digits=0);  randombiceps <- round(runif(1, 1, 6),digits=0);  randomtriceps <- round(runif(1, 1, 7),digits=0);  randomback <- round(runif(1, 1, 11),digits=0);  randomcalves<- round(runif(1, 1, 14),digits=0);  randomforearms<- round(runif(1, 1, 3),digits=0)
  finaldf$benchexercise[i] <- xercises[randomchest,c(1)]
  finaldf$deadliftexercise[i] <- xercises[randomdead,c(9)]
  finaldf$squatexercise[i] <- xercises[randomsquat,c(10)]
  # finaldf$Absexercise[i] <- xercises[randomabs,c(2)]
  # finaldf$bicepexercise[i] <- xercises[randombiceps,c(4)]
  # finaldf$tricepexercise[i] <- xercises[randomtriceps,c(5)]
  # finaldf$shoulderexercise[i] <- xercises[randomtriceps,c(3)]
  # finaldf$backexercise[i] <- xercises[randomback,c(6)]
  # finaldf$calvesexercise[i] <- xercises[randomcalves,c(7)]
  # finaldf$forearmsexercise[i] <- xercises[randomforearms,c(8)]
  
}


for(i in 1:nrow(secondaryexercisesofday)){
  reps <- NULL;randomabs <- NULL;randomshoulders <- NULL;randombiceps <- NULL; randomtriceps <- NULL;randomback <- NULL;randomcalves<- NULL;randomforearms <- NULL;
  randomabs <- round(runif(1, 1, 5),digits=0);  randomshoulders <- round(runif(1, 1, 7),digits=0);  randombiceps <- round(runif(1, 1, 6),digits=0);  randomtriceps <- round(runif(1, 1, 7),digits=0);  randomback <- round(runif(1, 1, 11),digits=0);  randomcalves<- round(runif(1, 1, 14),digits=0);  randomforearms<- round(runif(1, 1, 3),digits=0)
  reps <- round(runif(1,1,8),digits=0)
  secondaryexercisesofday$Absexercise[i] <- xercises[randomabs,c(2)]
  secondaryexercisesofday$bicepexercise[i] <- xercises[randombiceps,c(4)]
  secondaryexercisesofday$tricepexercise[i] <- xercises[randomtriceps,c(5)]
  secondaryexercisesofday$shoulderexercise[i] <- xercises[randomtriceps,c(3)]
  secondaryexercisesofday$backexercise[i] <- xercises[randomback,c(6)]
  secondaryexercisesofday$calvesexercise[i] <- xercises[randomcalves,c(7)]
  secondaryexercisesofday$forearmsexercise[i] <- xercises[randomforearms,c(8)]
  secondaryexercisesofday$reps[i] <- xercises[reps,c(11)]
  
}

for(i in 1:length(secondaryexercisesofday$reps)){
if (secondaryexercisesofday$reps[i] == ""){
  secondaryexercisesofday$reps[i] = "5x10"
 }
}

TodaysWorkout <-function(x){

Sys.Date() %in% secondaryexercisesofday$Date
Sys.Date() %in% finaldf$Date

if(Sys.Date() %in% secondaryexercisesofday$Date == TRUE){
  secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0("Your Exercise day is : ",secondaryexercisesofdayz$Exercise))
    print(paste0("You will be doing: ",secondaryexercisesofdayz$reps))
    print(paste0("The Exercises will be: "))
    print(paste0(secondaryexercisesofdayz$Absexercise))
    print(paste0(secondaryexercisesofdayz$bicepexercise))
    print(paste0(secondaryexercisesofdayz$tricepexercise))
    print(paste0(secondaryexercisesofdayz$shoulderexercise))
    print(paste0(secondaryexercisesofdayz$backexercise))
    print(paste0(secondaryexercisesofdayz$calvesexercise))
    print(paste0(secondaryexercisesofdayz$forearmsexercise))
    
}else if(Sys.Date() %in% finaldf$Date == TRUE){
    finaldf <- finaldf[finaldf$Date == Sys.Date(),]
    cat(
      print("Your workout today is: "),
      print(paste0("Bench: ",finaldf$RepsbenchMain," Reps X ",finaldf$SetsbenchMain," Sets of ",round(finaldf$BenchMaxTimeIntensityMain,digits = 0)," lbs.")),
      print(paste0("Bench: ",finaldf$RepsbenchSecond," Reps X ",finaldf$SetsbenchSecond," Sets of ",round(finaldf$BenchMaxTimeIntensitySecond,digits = 0)," lbs.")),
      print(paste0("Squat: ",finaldf$RepssquatMain," Reps X ",finaldf$SetssquatMain," Sets of ",round(finaldf$squatMaxTimeIntensityMain,digits = 0)," lbs.")),
      print(paste0("Squat: ",finaldf$RepssquatSecond," Reps X ",finaldf$SetssquatSecond," Sets of ",round(finaldf$squatMaxTimeIntensitySecond,digits = 0)," lbs.")),
      print(paste0("Dead: ",finaldf$RepsdeadMain," Reps X ",finaldf$SetsdeadhMain," Sets of ",round(finaldf$deadMaxTimeIntensityMain,digits = 0)," lbs.")),
      print(paste0("Dead: ",finaldf$RepsdeadSecond," Reps X ",finaldf$SetsdeadSecond," Sets of ",round(finaldf$deadMaxTimeIntensitySecond,digits = 0)," lbs.")),
      sep = "\n")
    }else{
  print("Whoops not sure what workout is today...")
 }
}

############################################################################################################################################################################################################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################
# IF YOU JUST RUN THIS YOU WILL GET THE EXERCISE OF THE DAY!
TodaysWorkout() # This is the final function to read
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################################################################################################################################################################################################################################

# Because it can only send one line at a time i will use multiple functions
 TodaysWorkout1 <- function(x){
     print("Your workout today is: ")
     
 }
TodaysWorkout2 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
  finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Bench: ",finaldf$RepsbenchMain," Reps X ",finaldf$SetsbenchMain," Sets of ",round(finaldf$BenchMaxTimeIntensityMain,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0("Your Exercise day is : ",secondaryexercisesofdayz$Exercise))
    }
}

TodaysWorkout3 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
  finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Bench: ",finaldf$RepsbenchSecond," Reps X ",finaldf$SetsbenchSecond," Sets of ",round(finaldf$BenchMaxTimeIntensitySecond,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0("You will be doing: ",secondaryexercisesofdayz$reps))
  }
}

TodaysWorkout4 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
  finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Squat: ",finaldf$RepssquatMain," Reps X ",finaldf$SetssquatMain," Sets of ",round(finaldf$squatMaxTimeIntensityMain,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0("The Exercises will be ",secondaryexercisesofdayz$Absexercise))
  }
  }

TodaysWorkout5 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
    finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Squat: ",finaldf$RepssquatSecond," Reps X ",finaldf$SetssquatSecond," Sets of ",round(finaldf$squatMaxTimeIntensitySecond,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0(secondaryexercisesofdayz$bicepexercise))
    }
  }

TodaysWorkout6 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
  finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Dead: ",finaldf$RepsdeadMain," Reps X ",finaldf$SetsdeadhMain," Sets of ",round(finaldf$deadMaxTimeIntensityMain,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0(secondaryexercisesofdayz$tricepexercise))
  }
  }

TodaysWorkout7 <- function(x){
  Sys.Date() %in% secondaryexercisesofday$Date
  Sys.Date() %in% finaldf$Date
  if(Sys.Date() %in% finaldf$Date == TRUE){
    finaldf <- finaldf[finaldf$Date == Sys.Date(),]
  print(paste0("Dead: ",finaldf$RepsdeadSecond," Reps X ",finaldf$SetsdeadSecond," Sets of ",round(finaldf$deadMaxTimeIntensitySecond,digits = 0)," lbs."))
  }else{
    secondaryexercisesofdayz <- secondaryexercisesofday[secondaryexercisesofday$Date == Sys.Date(),]
    print(paste0(secondaryexercisesofdayz$shoulderexercise,secondaryexercisesofdayz$backexercise,secondaryexercisesofdayz$calvesexercise,secondaryexercisesofdayz$forearmexercise))
  
  }
}


# must get a new number
 #ACCOUNT SID Get new one
 #Authentication Token Get new one
 # Trial Number Get new one
 #+15674323743
 
 #a9F0cQpht0MlfV2B0sBXN1Ov-Vhp2v-AgFM49fg6

 install.packages("twilio")
 library(twilio)
 
 Sys.setenv(TWILIO_SID = "New one")
 Sys.setenv(TWILIO_TOKEN = "New one")
 usethis::edit_r_environ()
 
 from_number <- "+15674323743"
 to_number <-"+12695783791"

 # my_message <- tw_send_message(
 #   to = Sys.getenv("to_number"),
 #   from = Sys.getenv("from_number"),
 #   body = paste("I am sending this message from an R script!")
 # )
 # names(my_message)
 "+12697190436"
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout1()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout2()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout3()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout4()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout5()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout6()))
 )
 tw_send_message(
   to = "+12695783791",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout7()))
 )
 
 
 
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout1()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout2()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout3()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout4()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout5()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout6()))
 )
 tw_send_message(
   to =  "+12697190436",
   from = "+15674323743",
   body =   Reduce(paste, deparse(TodaysWorkout7()))
 )

