#This file will plot the actual drinking data and the simulated drinking data 
# for the monkey in monkeys and the number of days in N.

##Change source location to csv data file locations in lines 14 and 15
##Change source location to location of downloaded files in lines 21 and 22


monkeys=c(1,9,19,26)
monkey_class=c('BD','HD','LD','VHD')
class_num=1
N=180
  
for(k in monkeys){  
  # Read in data
  #weights= read.csv(file = 'location of weight data',head = TRUE,sep = ',')
  #filenames <- list.files("location of drink data", pattern="*.csv", full.names=TRUE)
  drinker <- lapply(filenames, read.csv)
  
  monkey_num1 = k
  weight1 = weights[monkey_num1,2]
  #source('location of ProbsAndParams.R')
  #source('location of OneMonkeySim_Fun.R')
  Probs = p[monkey_num1, ]# probabilities of state transitions from ProbsAndParams
  ThisMonkeyParams = Params[monkey_num1,]  # parameter values from ProbsAndParams
  sim1 <- OneMonkeySim(weight1,N,Probs,ThisMonkeyParams)
  
  
  j = monkey_num1
  
  day<-as.Date(drinker[[j]]$Date, "%Y-%m-%d") #check what format the date is entered in the csv file
  day_sec<-as.numeric(60*60*7*(day-min(day))) 
  
  timestart<-day_sec+rbind(drinker[[j]]$Start)
  timestop<-day_sec+rbind(drinker[[j]]$Stop)
  Drink_Length<-as.numeric(drinker[[j]]$Length)
  vol<-rbind(drinker[[j]]$Vol)
  rate<-vol/Drink_Length 
  
  # plot n drinking times from the data
  n = 1500
  dat_times = 0
  dat_vals = c(0,0)
  for (i in 1:(n/2-1)){
    dat_times = c(dat_times,timestart[i],timestart[i],timestop[i],timestop[i])
    dat_vals = c(dat_vals,rate[i],rate[i],0,0)
  }
  dat_times = c(dat_times,timestart[n/2],timestart[n/2],timestop[n/2],timestop[n/2])
  dat_vals = c(dat_vals,rate[n/2],rate[n/2],0)
  
  par(mar=c(8,8,8,8))
  plot(dat_times/(60),dat_vals,type = 'l',col = 'black',xlab = 'time in minutes',
       ylab = 'rate (ml/sec)',ylim = c(0,1.5),
       main = paste("Drinking Data Monkey:", j, " ", monkey_class[class_num]), cex.main=3,cex.lab=2.5,cex.axis=2.5, family = 'serif')
  
  # plot n drinking times in a simulation
  # make time values
  sim_times = 0
  last_time = 0
  for (i in 1:n){
    sim_times = c(sim_times,last_time+sim1$drink_times[i],last_time+sim1$drink_times[i])
    last_time = sim_times[length(sim_times)]
  }
  # make drinking rate values
  sim_vals = c()
  
  # noisy version
  sigmas = c((low_lim-low_rate)/2,(high_lim - med_rate)/2,(high_rate- high_lim)/2)
  for (i in 1:(n)){
    if (sim1$drink_vals[i] > 0){
      mean_val = sim1$drink_vals[i]
      state = which(c(low_rate,med_rate,high_rate)==mean_val)
      noisy_val = mean_val+rnorm(1,0,sigmas[state])
    } else {noisy_val = sim1$drink_vals[i]}   # leave 0's alone
    sim_vals = c(sim_vals,noisy_val,noisy_val)
  }
  sim_vals = c(sim_vals,0)
  
  par(mar=c(8,8,8,8))
  plot(sim_times/60,sim_vals,type = 'l',xlab = 'time in minutes',
       ylab = 'rate (ml/sec)',ylim = c(0,1.5),col = 'gray45',
       main = paste('Simulated Monkey:', j, " ", monkey_class[class_num]), cex.main=3,cex.lab=2.5,cex.axis=2.5, family = 'serif')
  
  class_num=class_num+1
}