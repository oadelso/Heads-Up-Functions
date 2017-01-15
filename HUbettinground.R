HUbettinground<-function(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw){
  #Input
  #'bp' is the betting position, it will be either 1 or 2; it is the position of the agent that needs to act
  #'bbindex' is the agent with the big blind player position, either 1 or 2. After each round starts anew, bp should equal bbindex
  #'play' is 2 element vector containing the play of each player during the existing round (1=fold, 2=call/check, 3=raise)
  #'raise' is any existing raise in current betting round; it should be either zero or a multiple of the big blind amount
  #'betround' is either 1=flop, 2=turn, 3=river,4=showdown
  #'wealth' is the 2 element vector containing each player's current wealth
  #'pot' is the current size of the pot
  #'big blind' is the big blind value of the game
  #'playercards' and 'communitycards' are the matrices containing the game's cards in the format explained in the 'cards' R function 
  #'moneyintheround' is a 2 element vector containg the amount of money in the pot for a specific round for each player
  #'initialw' is the amount of money that each agent began with in the beginning of the simulation
  #Output
  #The wealth of each player
  #The hole cards of each player in integer format (i.e., an integer between 1 and 52, inclusive)
  #The community cards in the round in integer format
  #Betround currently being played in each match
  
  if (isTRUE(all.equal(play,c(2,2)))){
    #both players have called and the next round of betting starts, or previous player raised and current player called
    betround=betround+1;
    if (betround==4){
      #betting round is past the river, and each player must show its cards
      handrank1=handmaker(playercards[,1:2],communitycards,3); #handrank of first player
      handrank2=handmaker(playercards[,3:4],communitycards,3); #handrank of second player
      if (handrank1>handrank2){
        #player one wins the hand
        wealth[1]=wealth[1]+pot;
      }else if (handrank2>handrank1){
        wealth[2]=wealth[2]+pot;
      }else{
        #split the pot
        wealth[1]=wealth[1]+pot/2;
        wealth[2]=wealth[2]+pot/2;
      }
      #prepare output
      communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
      playercardsn=matrixtonumber(playercards); #stores the player cards in number format
      HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
    }
    else{
      #Next round starts, and player and moneyinround vectors, along with the raise variable need to be reset to 0
      play=rep(0,2);
      moneyinround=rep(0,2);
      raise=0;
      bp=bbindex; #the big blind is always the first to act on all other rounds following the flop
      HUbettinground=HUbettinground(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw);
    }
    
  } else if (play[1]==1){
    #one of the players has folded
    wealth[2]=wealth[2]+pot; #player 2 wins the pot
    #prepare output
    communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
    playercardsn=matrixtonumber(playercards); #stores the player cards in number format
    HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
  } else if (play[2]==1){
    wealth[1]=wealth[1]+pot; #player 1 wins the pot
    #prepare output
    communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
    playercardsn=matrixtonumber(playercards); #stores the player cards in number format
    HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
  } else{
    #next move needs to be made by the bp player
    hs=(bp-1)*2+1; #hand start index for the betting position, bp. bp is either 1 or 2
    he=(bp-1)*2+2; #hand end index for the betting position, bp. bp is either 1 or 2.
    tablecards=0;
    if (betround>0){
      ncc=betround+2; #number of community cars already known by the round; 3 for the flop, 4 for the turn, and 5 by the river.
      tablecards=communitycards[,1:ncc]; #community cards seen during the round 
    }
    
    behavior=1;
    kellyfrac=0.5;
    nsimulations=100; #less informed agent
    if (bp==1){
      #decide whether to change parameters for the kelly bot for each agent in the game
      behavior=3;
      kellyfrac=0.5;
      nsimulations=100; #better informed agent;
    }
    
    decision=kellybot(initialw,wealth[bp],playercards[,hs:he],tablecards,behavior,kellyfrac,nsimulations,pot,raise,bigblind,moneyinround[bp]); #decision to be made by the kelly criterion
    
    play[bp]=decision[1]; 
    if (play[bp]==3 | play[bp]==2){
      #player raises/calls
      if(play[bp]==2){
        #if player calls set play vector to c(2,2)
        play[bp%%2+1]=2;
      }
      raise=decision[2];
      wealth[bp]=wealth[bp]-raise+moneyinround[bp];
      pot=pot+raise-moneyinround[bp];
      moneyinround[bp]=raise; #NEEDS to be checked
    }
    #the other player's turn to fold/call/check or raise/re-raise
    bp=bp%%2+1;
    HUbettinground=HUbettinground(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw);
  }
  HUbettinground
}

