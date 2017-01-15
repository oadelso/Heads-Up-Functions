HUtable<-function(games=10,bigblind=1,initialw=100){
  #Input
  #'games' is an intiger value defining the amount of games or matches which will be simulated
  #'bigblind' is an integer value defining the big blind amount for the game
  #'initialw' is the initial wealth for each agent in the Heads Up match
  #Output
  #A 'games' by 12 matrix
  #for each game, the matrix saves:
  #Two values, each being the ending wealth for each agent
  #The hole cards for each agent in each match of the simulations
  #the community cards (if applicable)
  #the round in which the match ended. For example, it would be 1 if one of the agents fold pre-flop, or
  #4 if the round ends in a showdown.

  wealth=rep(0,2); #stores the weath of each player
  wealth[1:2]=initialw; #initial wealth for players
  HUtable=matrix(0,games,12); #will save the resulting wealth at the end of each game (2 values), all cards
                              #(4+5=9 total), plus betting round in which game ended (1), total of 12 values.
  
  for (i in 1:games){
    pot=0;
    raise=0;
    play=rep(0,2); #play vector saves the play done by each of the players during the round
    moneyingame=rep(0,2); #saves the amount of money placed by each player in the pot per game
    
    #big and small blind alternate between each round
    sbindex=(i+1)%%2+1; #small blind index
    bbindex=i%%2+1;     #big blind index
    
    wealth[sbindex]=wealth[sbindex]-(bigblind)/2;
    moneyingame[sbindex]=bigblind/2;
    wealth[bbindex]=wealth[bbindex]-bigblind;
    moneyingame[bbindex]=bigblind;
    
    if (wealth[1]<0 | wealth[2]<0 ){
      #end game once any of the players goes broke
      #the money is returned to see the final status of each player's stack (AKA wealth)
      wealth[sbindex]=wealth[sbindex]+(bigblind)/2; 
      wealth[bbindex]=wealth[bbindex]+bigblind;
      communitycardsn=matrixtonumber(communitycards);
      playercardsn=matrixtonumber(playercards);
      gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
      HUtable[games,]=gameoutput; #to view the last round of the match
      break
    }
    pot=bigblind*(3/2);
    gamecards=cards(9); #four cards for the players (two per player), and the five community cards
    playercards=gamecards[,1:4];
    communitycards=gamecards[,5:9];
    #round 0, small blind is first to act
    betround=0; #pre flop bet round
    sbs=(sbindex-1)*2+1;
    sbe=(sbindex-1)*2+2;
    sbcards=playercards[,sbs:sbe]; #smallblind cards
    #small blind needs to decide whether to call, raise, or fold 

    behavior=1;
    kellyfrac=0.5;
    nsimulations=100; #less informed agent
    if (sbindex==1){
      #decide whether to change parameters for the kelly bot for each agent in the game
      behavior=3;
      kellyfrac=0.5;
      nsimulations=100; #better informed agent;
    }
  
    decision=kellybot(initialw,wealth[sbindex],sbcards,0,behavior,kellyfrac,nsimulations,pot,raise,bigblind,bigblind/2);
    if (decision[1]==1){
      #player on the small blind folds
      play[sbindex]=decision[1];
      wealth[bbindex]=wealth[bbindex]+pot;
      communitycardsn=matrixtonumber(communitycards);
      playercardsn=matrixtonumber(playercards);
      gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
    }else if (decision[1]==2){
      #player in the small blind calls
      wealth[sbindex]=wealth[sbindex]-bigblind/2;
      moneyingame[sbindex]=moneyingame[sbindex]+bigblind/2;
      pot=pot+bigblind/2; #increase the pot accordingly
      play[sbindex]=decision[1];
      #the other betting rounds are modeled in another function
      gameoutput=HUbettinground(bbindex,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyingame,initialw);
      wealth=gameoutput[1:2]; #part of output from HUbettinground
      if (wealth[1]<0 | wealth[2]<0 ){
        #end game once any of the players goes broke
        communitycardsn=matrixtonumber(communitycards);
        playercardsn=matrixtonumber(playercards);
        gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
        HUtable[games,]=gameoutput; #to view the last round of the match
        break
      }
      
    }else if (decision[1]==3){
      #small blind raises
      play[sbindex]=decision[1];
      raise=decision[2]; #the amount being raised by the small blind
      wealth[sbindex]=wealth[sbindex]-raise+moneyingame[sbindex];
      pot=pot+raise-moneyingame[sbindex]; #only the additional money goes into the pot
      moneyingame[sbindex]=raise;
      #the other betting rounds are modeled in another function
      gameoutput=HUbettinground(bbindex,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyingame,initialw);
      wealth=gameoutput[1:2];
      if (wealth[1]<0 | wealth[2]<0 ){
        #end game once any of the players goes broke
        communitycardsn=matrixtonumber(communitycards);
        playercardsn=matrixtonumber(playercards);
        gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
        HUtable[games,]=gameoutput; #to view the last round of the match
        break
      }
    }
   
  if (wealth[1]<0 | wealth[2]<0 ){
      #end game once any of the players goes broke
    communitycardsn=matrixtonumber(communitycards);
    playercardsn=matrixtonumber(playercards);
    gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
    HUtable[games,]=gameoutput; #to view the last round of the match
    break
  }  
  HUtable[i,]=gameoutput; #saves output of the ith game
  }
  HUtable
}