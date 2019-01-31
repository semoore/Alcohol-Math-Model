## RUN PackagesNeeded IF NECESSARY 
#This file will read in the data in filenames and 
#calculate the probability of entering a bout as well as the probabilities of each drinking state

##Change source location to csv data file locations in lines 11 and 212

#To print parameters, uncomment the code at the end.

#Names of files to easily reference.  

#filenames1 <- list.files("drink data location", pattern="*.csv", full.names=TRUE)
#filenames2 <- list.files("bout data location", pattern="*.csv", full.names=TRUE)

drinker <- lapply(filenames1, read.csv)
bout <- lapply(filenames2, read.csv)


#Define Variables
numProbs=4 #1 prob for entering a bout and 3 prob, one for each drinking state, low, med, high
numSums=5 #2 sums for calculating bout, 3 sums, one for each drinking state
trainSet=31 #number of monkeys
numStates=7 #7 avg time spent in low, med, high, in a bout, not drinking after a bout, not drinking after a drink, drinking (regardless of state)  
low_rate =0.383 #low: (0,0.6) -used median values
med_rate=0.798 #med: [0.6,1.1]
high_rate=1.376 #high: above 1.1
low_lim = .6 # 
high_lim = 1.1

#Create Matrices
s <-matrix(data=NA, nrow=trainSet, ncol=numSums) #Will use this to store the sums of number of times starting to drink but not in a bout and number of times going into a bout as well as each drinking state
p <-matrix(data=NA, nrow=trainSet, ncol=numProbs) #Will use this to store the probabilities for each monkey 
Params <-c()

#Loop-for each monkey bin the data and calculate the sums
for(j in 1:trainSet){
  # Extract one monkey's drink data
  ThisStartStop = cbind(as.numeric(drinker[[j]]$Start),as.numeric(drinker[[j]]$Stop),as.numeric(drinker[[j]]$Vol))
  # Add time to first drink, and time to end of day for each day.
  # We do this by adding drinks of length 0 at beginning and end of day.
  # Put -1 in volume for start of day, -2 in volume for end of day.
  day<-as.Date(drinker[[j]]$Date, "%Y-%m-%d")
  DiffDay=diff(day)
  NewStartStop = c()
  day_sec =22*60*60  # number of seconds in 22 hours
  i = 1
  while (i < length(DiffDay)){
    # add a row of zeros at start of day
    NewStartStop = rbind(NewStartStop,c(0,0,-1))
    while ((DiffDay[i]==0) & (i <=length(DiffDay))){
      # Remove all drinks of length 0 and vol <= .2
      if ((ThisStartStop[i,1]==ThisStartStop[i,2]) &(ThisStartStop[i,3] <=.2)){
      } else{
      NewStartStop = rbind(NewStartStop,ThisStartStop[i,])
      }
      i = i+1
    }
    # When DiffDay[i] is 1, a new day starts in day[i+1]
    # Add last drink of that day
    NewStartStop = rbind(NewStartStop,ThisStartStop[i,])
    # add a row at end of day  
    NewStartStop = rbind(NewStartStop, c(day_sec,day_sec,-2))
    i=i+1
  }
  # Calculate Drink Lengths.  Make length =1 if it was 0
  AllDrinklength = NewStartStop[,2]-NewStartStop[,1]
  NewStartStop[AllDrinklength==0,2]=NewStartStop[AllDrinklength==0,2]+1 
  AllDrinklength[AllDrinklength==0]=1
  # Remove start and end of days by only keeping those lengths with volume > 0
  Drinklength = AllDrinklength[(NewStartStop[,3]>0)]
  vol = NewStartStop[(NewStartStop[,3]>0),3] 
  # Fix stop times for start and end of each day
  NewStartStop[NewStartStop[,3]<0,2]=  NewStartStop[NewStartStop[,3]<0,2]-1 
  # Calculate rates
  rate<-vol/(Drinklength) 

  # Calculate time between drinks.  
  # This includes time from start of each day to first drink, and from last drink to end of day
  L = length(NewStartStop[,1])  
  AllIDI = NewStartStop[2:L,1] - NewStartStop[1:(L-1),2] #IDI = start(i)-stop(i-1)
  # Remove negative intervals.  These occur between each new day, and sometimes at the end of the day, if the last drink extends past 22 hours
  I_neg = which(AllIDI < 0)
  PosIDI = AllIDI[-I_neg]
  # Remove corresponding entries from the drink length vector
  PosDrinklength = AllDrinklength[-(I_neg+1)]
  L_pos = length(PosDrinklength)
  # Find indices, number, length and length between  bouts
  BoutInd = which(PosIDI > 300)
  NumBouts = length(BoutInd)
  IBI = PosIDI[BoutInd]   
  BoutLengths = 0*BoutInd  # make a vector of zeros
  for (k in 1:length(BoutInd)){
    kk = BoutInd[k]   # IDI(i) is the time before drink i+1
    ThisBoutLength = PosDrinklength[kk+1]  # add length of drink k+1, first drink in bout
    kk = kk+1
    if (k < length(BoutInd)){
      while ((kk < BoutInd[k+1])&(kk < L_pos)){
        ThisBoutLength = ThisBoutLength + PosIDI[kk] + PosDrinklength[kk+1] #add next ID and drink
        kk = kk+1
      }
    } else{
      while (kk < L_pos){
        ThisBoutLength = ThisBoutLength + PosIDI[kk] + PosDrinklength[kk+1]
        kk = kk+1
      }
    }
    BoutLengths[k]=ThisBoutLength
  }

  #bin the data
  rbin <- function(r) {
    if (0.0 < r && r<(low_lim)) {
      return(low_rate)
    } 
    else if ((low_lim)<=r && r<=(high_lim)) {
      return(med_rate)
    }  
    return(high_rate)
  }
  
  rate_bin<-lapply(rate, rbin) #this categorizes the rates but does not include 0's for when the monkey is not drinking.
  
  x = c()#array of times in low_drinking state
  y = c()#array of times in med_drinking state
  z = c()#array of times in high_drinking state
  
  #loop-calculate the time in each interval
  for (i in 1:length(Drinklength)){
    if (rate_bin[i]==low_rate){
      x = c(x,Drinklength[i])
    }
    else if (rate_bin[i]==med_rate){
      y = c(y,Drinklength[i])
    }
    else {
      z = c(z,Drinklength[i])
    }
  }
  
  # test drink lengths in the three different states
  # fit to a weibull distribution
  Wx = fitdistr(x,densfun = "weibull",lower = 1)
  Wy = fitdistr(y,densfun = "weibull",lower = 1)
  Wz = fitdistr(z,densfun = "weibull",lower = 1)
  if (length(z) ==1){# if high-rate drinking state is entered only once
    z = rep.int(z,5) + rnorm(5,0,.1)} #repeat 5 times and add a bit of noise
  if (length(z)==0){  # if high-rate drinking state is never entered
    z = rep.int(1,5) +rnorm(5,0,.1)
  }
  Wz = fitdistr(z,densfun = "weibull",lower = 1)
  
  Wbl = fitdistr(BoutLengths, densfun="weibull", lower=1)
  Eibi = fitdistr(IBI-300,densfun='exponential') # all inter-bout intervals are > 300 sec
  IDI = PosIDI[PosIDI <=300]
  Eidi = fitdistr(IDI,densfun='exponential')
  Params=rbind(Params,c(Wx$estimate,Wy$estimate,Wz$estimate,Wbl$estimate,Eibi$estimate,Eidi$estimate))
  
  
  #-------------------------------------

  #Vector of the total counts for each monkey
  s[j,1] = sum(rate_bin==low_rate) #'0 to 0.2' = '0.2 to 0'
  s[j,2] = sum(rate_bin==med_rate) #'0 to 0.5'= '0.5 to 0'
  s[j,3] = sum(rate_bin==high_rate) #'0 to 1' = '1 to 0'
  #Vector of the total counts for each monkey
  s[j,4] =  NumBouts# number of times going into a bout
  s[j,5] =  length(PosIDI)# total number of drinks and bouts
  
  

} #end of for loop for each monkey

#Calculate Probabilities of Being in Each Drinking State (not including not drinking) for each monkey
for (i in 1:trainSet){
  p[i,1] = s[i, 1]/(s[i, 1]+s[i, 2]+s[i, 3]) #(num secs in low_drinking state)/(total secs drinking)
  p[i,2] = s[i, 2]/(s[i, 1]+s[i, 2]+s[i, 3]) #(num secs in med_drinking state)/(total secs drinking)
  p[i,3] = s[i, 3]/(s[i, 1]+s[i, 2]+s[i, 3]) #(num secs in high_drinking state)/(total secs drinking)
} #end of for loop for finding probabilities

#Calculate probability of going into in a bout
for (i in 1:trainSet){
  p[i,4] = s[i, 4]/(s[i, 5]) #(total times starting a bout)/(total drinks taken)
} #end of for loop for finding probabilities


