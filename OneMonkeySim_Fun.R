OneMonkeySim <- function(weight, num_days,Probs,Params){
  # Simulate one monkey drinking
  # This version uses the actual probabilities and parameters estimated from the data of a particular monkey
  # Inputs: 
  # weight = monkeys weight (for calculating daily ethanol intake, which is in g/kg)
  # Probs  = vector of four transition probabilities: non-drinking to states 2 (low drinking), 3 (medium drinking),4 (high drinking) and non-drinking to bout.  Probs[1:4]
  # Params  = vector of parameters for the following distributions:
  #      time in low, medium, high drink (Weibull:  k =shape, lambda = scale,)  Params[1:6]
  #      time in bout (Weibull: shape, scale )   Params[7,8]
  #      time in non-drinking state after a bout (or single drink) (exponential:rate. Add 300 sec - less than 300 seconds between ethanol consumption is considered a continuous bout) Params[9]
  #      time in non-drinking state within a bout (exponential: rate.  Add 5 sec - less than 5 seconds between consumption of ethanol is a continuous drink)    Params[10]
  # num_days = number of days to simulate (we use 22 hours, since drinking is not possible for 2 hours each day)
  # Ouput:
  # drink_times (in seconds)
  # drink_vals (in milliliters)
  # ADEI: average daily ethanol intake (in g/kg)
  # class: classification (light drinker, heavy drinker, very heavy drinker)
  
  # Now we simulate:
  #  All monkeys start in the non-drinking state, state 1
  # STEP 1  We use Probs[4] to determine whether the monkey goes into a bout.
  # If in bout we use Params[7,8] to determine length of bout
  # STEP 2  We use Probs[1:3] to determine whether the monkey goes into state 2 (low), 3 (med) or 4 (high)
  #  We use Parms[1:6] to determine how long each drink is
  # STEP 3 If in bout:  we test if at end of bout
  # If not at end of bout, use Params[10] to determine time until next drink, back to STEP 2
  # If not in bout, use Params[9] to determine time until next bout/drink, back to STEP 1
  
  # empty vectors for storing the simulation results
  drink_times = c()
  drink_vals = c()  # first time period is non-drinking
  num_bouts = 0 # number of actual bouts total
  
  ADEI = c()     # estimate of average daily ethanol consumption
  oneday = 22*60*60 # number of seconds in a day - use 7 hours of daylight drinking
  
  #Loop - loop through the number of days you want to simulate
  for (i in 1:num_days){
    day_time = 0  # no time has elapsed on this day
    s = 1     # start in state 1 -> non-drinking
    this_day_drink_times = c()
    this_day_drink_vals = c()
    #while loop - loop until you have calculated drinks for the entire day
    while (day_time < oneday){
      # STEP 1 - decide whether to enter a bout
      boutp = runif(1,0,1)
      if (boutp < Probs[4]){
        num_bouts = num_bouts+1
        bout_time = rweibull(1,shape = Params[7], scale = Params[8] )
        this_bout_time = 0
        # STEP 2 - choose next drinking state - in bout
        p_switch = runif(1,0,1)
        if (p_switch <Probs[1]){
          s = 2} else {
            if (p_switch < Probs[1]+Probs[2]){
              s = 3} else 
              {s = 4}
          }
        this_day_drink_vals = c(this_day_drink_vals, s)
        shape_param = 2*(s-1)-1
        scale_param = 2*(s-1)
        drink_time = rweibull(1,shape = Params[shape_param], scale = Params[scale_param]) 
        this_day_drink_times = c(this_day_drink_times, drink_time)
        this_bout_time = this_bout_time + drink_time
        while (this_bout_time < bout_time){
          # STEP 3 - choose time until next drink - in bout
          idi_time = rexp(1,rate = Params[10]) + 5 
          this_day_drink_vals = c(this_day_drink_vals,1)
          this_day_drink_times = c(this_day_drink_times,idi_time)
          this_bout_time = this_bout_time + idi_time
          # now take another drink
          p_switch = runif(1,0,1)
          if (p_switch <Probs[1]){
            s = 2} else {
              if (p_switch < Probs[1]+Probs[2]){
                s = 3} else 
                {s = 4}
            }
          this_day_drink_vals = c(this_day_drink_vals, s)
          shape_param = 2*(s-1)-1
          scale_param = 2*(s-1)
          drink_time = rweibull(1,shape = Params[shape_param], scale = Params[scale_param])
          this_day_drink_times = c(this_day_drink_times, drink_time)
          this_bout_time = this_bout_time + drink_time
        }# END while time in bout less than bout time
        day_time = day_time + this_bout_time
        # END if in bout
      }  else{ 
        # STEP 2 - if not in bout
        p_switch = runif(1,0,1)
        if (p_switch <Probs[1]){
          s = 2} else {
            if (p_switch < Probs[1]+Probs[2]){
              s = 3} else 
              {s = 4}
          }
        this_day_drink_vals = c(this_day_drink_vals, s)
        shape_param = 2*(s-1)-1
        scale_param = 2*(s-1)
        drink_time = rweibull(1,shape = Params[shape_param], scale = Params[scale_param])
        this_day_drink_times = c(this_day_drink_times, drink_time)
        day_time = day_time + drink_time
      }
      s = 1   # return to non-drinking state 
      this_day_drink_vals = c(this_day_drink_vals,s)
      # Choose time until next drink
      p_long = runif(1,0,1)     
      #Use if single IBI distribution
      ibi_time = rexp(1,rate = Params[9]) + 300 
      this_day_drink_times=c(this_day_drink_times,ibi_time)
      day_time = day_time+ibi_time
    } # end while loop (one day)
    # translate drinking rates
    for (i in 1:length(this_day_drink_vals)){
      state = this_day_drink_vals[i]
      rate = 0*(state==1)+low_rate*(state==2)+med_rate*(state==3)+high_rate*(state==4)
      this_day_drink_vals[i]=rate
    }
    daily_consumption = ((sum(this_day_drink_times*this_day_drink_vals))*.04)/(weight)  # g/kg
    ADEI = c(ADEI, daily_consumption)
    drink_times = c(drink_times,this_day_drink_times)
    drink_vals = c(drink_vals, this_day_drink_vals)
  } # end of loop through all days
  # Calculate drinking classification
  
  DrinkBins = hist(ADEI,c(0,2.01, 3.01,4.01,100),plot=FALSE)
  MeanADEI = mean(ADEI)
  
  if ((MeanADEI > 3) & (DrinkBins$counts[4]/num_days > .1)){
    class = 'VHD'} else{
      if ((DrinkBins$counts[3]+DrinkBins$counts[4])/num_days> .2){
        class = 'HD'} else{
          if (sum(DrinkBins$counts[2:4])/num_days > .55){
            class = 'BD'} else{ class = 'LD'}
        }
    }
  
  output<- list(drink_vals = drink_vals, drink_times = drink_times,ADEI=ADEI,DrinkBins=DrinkBins,class=class, num_bouts=num_bouts)
  return(output)
} # close of the function
