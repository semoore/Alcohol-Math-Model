# This file will run the drinking simulation 100 times for each monkey in trainSet
#and output the drinking classifications as a matrix.  The output will include a 
#column for the monkey drinking number, the number of LD classifications, the number
#of BD classifications, the number of HD classifications, and the number of VHD classifications

##Change source location to location of downloaded files in lines 13 and 14
##Change source location to csv data file locations in lines 26 and 27

# Uncomment line 13 to run "ProbsAndParams" if it has not been run in the current session


### Simulate all monkeys
##source('location of ProbsAndParams.R') # estimate probabilities
#source('location of OneMonkeySim_Fun.R')
AllResults=c()
trainSet = c(1,2,3,4,5,7,8,9,10,11,14,15,16,17,18,19,23,24,25,26,27,28,29)
for (j in trainSet){
  N = 365  # number of days 
  num_trials=100
  num_LD=0
  num_BD=0
  num_HD=0
  num_VHD=0
  sim_class_result=c()
  # Read in data
  #weights= read.csv(file = 'location of weight csv',head = TRUE,sep = ',')
  #filenames <- list.files("location of drink data", pattern="*.csv", full.names=TRUE)
  drinker <- lapply(filenames, read.csv)
  
  monkey_num1 = j
  weight1 = weights[monkey_num1,2]
  Probs = p[monkey_num1, ]# probabilities of state transitions from ProbsAndParams
  ThisMonkeyParams = Params[monkey_num1,]  # parameter values from ProbsAndParams
 
  for (i in 1:num_trials){
    sim1 <- OneMonkeySim(weight1,N,Probs,ThisMonkeyParams)
    sim_class_result<-c(sim_class_result,sim1$class)
    if(sim1$class=='LD'){num_LD=num_LD+1}
    if(sim1$class=='BD'){num_BD=num_BD+1}
    if(sim1$class=='HD'){num_HD=num_HD+1}
    if(sim1$class=='VHD'){num_VHD=num_VHD+1}
  }  
  print(c(monkey_num1,num_LD, num_BD,num_HD,num_VHD))
  print(sim_class_result)
  AllResults = rbind(AllResults,c(monkey_num1,num_LD, num_BD,num_HD,num_VHD))
  write.table(AllResults, file = "SimulationResults.csv", row.names=FALSE, na="",col.names=c('Monkey Number', 'Number LD', 'Number BD', 'Number HD', 'Number VHD'),sep=",")
  }