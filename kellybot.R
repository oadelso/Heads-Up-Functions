kellybot<-function(initialwealth,wealth,usercards,communitycards=0,behavior=1,kellyfrac=0.5,MCS=100,pot,raise,bigblind,moneyinpot){
  #simple bot that uses the kelly factor to determine wether to fold, call, or raise/re-raise
  #Input
  #'initialwealth' is the initial wealth of the agent at the start of the simulation
  #'wealth' is the current wealth of the agent
  #The hole cards (i.e., 'usercards') and available community cards (i.e.,'communitycards') are used to determine the probability
  #of winning using the 'oddsNL' R function, or the poker calculator.
  #'behavior' which will either be full kelly (behavior=1), or fraction kelly (behavior=2), or a combination of the two (behavior=3)
  #'raise' is the bet placed by another player in the table
  #'bigblind' is the smallest incremental amount to re raise
  #'moneyinpot' is the money already in the pot for that betting round
  #this is used in order to calculate the incremental amount that must be placed in the pot to continue in the game
  #depending on the decision to be made.
  #Output
  #A one by 2 matrix. The first element is the decision taken (i.e., 1=fold, 2=check/call, 3=raise)
  #the second element is the amount that was raised, if the decision is to fold or check/call, the value is 0.
  
  kellybot=rep(0,2); #first component will be the action, with 1=folding, 2=calling/checking, and 3=raising
                    #second component will be the amount of money if it needs to be raised
  
  bet=max(bigblind,raise-moneyinpot);
  
  b=pot/bet; #calculate odds
  
  #calculate probability of winning by the river round (=3)
  pw=oddsNL(usercards,0,communitycards,1,MCS,3);
  pw=pw[1];
  
  if (behavior==1){
    #use full kelly
    frac=(pw*(b+1)-1)/b; 
  }else if (behavior==2){
    #use fraction of kelly
    frac=kellyfrac*(pw*(b+1)-1)/b;
  }else if (behavior==3){
    if (wealth<initialwealth){
      #wealth is lower than initial wealth => risk seeking, use 1.5 kelly
      frac=1.5*(pw*(b+1)-1)/b;
      }else{
        #risk averse, use half kelly
      frac=0.5*(pw*(b+1)-1)/b;
      }
  }
  
  invest=floor(frac*wealth); #amount of money to allocate to this betting round
                            #rounded down to the lowest integer value
  
  if (invest>=raise+bigblind){ #The raise must increase by at least the bigblind
    kellybot[1]=3; #raise to what the kelly factor suggests
    kellybot[2]=invest; 
    if (bigblind>invest){
      #needs to go all in
      kellybot[1]=3; #all in
      kellybot[2]=wealth;
    }
  } else if (invest < raise){
    #depends if a raise has been made or not
    if (raise==0){
      kellybot[1]=2;#check
    }else{
      kellybot[1]=1;#fold
    }
  } else{
    kellybot[1]=2; #call
    kellybot[2]=raise;
  }
  return(kellybot)
}