# Demo code for game ratings.

# the idea here is as follows: game ratings may not be efficient and
# accurate measures of gamer capability when team matches are considered.
# We would like to show that the combined capabilities of many players is *different* depending on the sort of game played,
# and show that an individual gamerscore must be chosen in order to work well with this aspect of team dynamics. 

# In this first demo though, we'll just show that a bunch of normally-distributed players will win or lose in a manner that is 
# dictated by some combination of their scores. So for this example and this example *only*, we'll allow secret Ability to equal Gamerscore

# So we will look at some pathologically contrived games with known dynamics,
# and simulate the following:
#     A pool of gamers with known "secret" Ability score that a Gamerscore will hopefully find, or already represented by Gamerscore
#     A game matching service with some scheme that matches gamers based on Gamerscore
#     An engine where many games are played and games resolved according to a contrived physical game model
#     An analysis portion considering wins and losses across all games, as well as several convolutions of gamerscore, to determine the most useful metric for team power
# We will try this for several distributions of gamer Ability and several different games
#     

mean_Ability = 200 # the average true gamer ability
std_Ability = 25
abilityForAdvantage = 25 #how many Ability points mean a "decided advantage"

gameType = 'gun_fight' # or 'block_on_hill', 'block_in_valley', or others
gamers_each_side = 5
number_total_gamers = 1000
number_games_played = 10000

## here's a rernormalized ERF
erfRenorm <- function(x){
  return(pnorm(x * sqrt(2)))
}

mod<-function(x,m){
  t1<-floor(x/m)
  return(x-t1*m)
}


playGame<-function(gamerAbilitiesTeam1,gamerAbilitiesTeam2,gameType){
  if(gameType == 'gun_fight'){  ########GUN FIGHT STYLE GAME
    #everyone starts out alive:
    team1_living = rep(1,gamers_each_side)
    team2_living = rep(1,gamers_each_side)
    #Then everyone lines up and takes turns shooting:
    while(sum(team1_living)>0 && sum(team2_living)>0){
      #all the living players take a shot at once:
      killedTeam1Player = 0
      killedTeam2Player = 0 #no one dead yet
      for(shooter in 1:gamers_each_side){
        if(team1_living[shooter] == 1){
          #pick an opponent (KLUDGEY - FIX)
          opponent = 0
          while(opponent == 0){
            lookFor = sample(1:gamers_each_side,1)
            if(team1_living[lookFor] == 1){opponent=lookFor}
          }
          #now take the shot!
          skillAdvantage = gamerAbilitiesTeam1[shooter]-gamerAbilitiesTeam2[opponent]
          if(runif(1, 0.0, 1.0)>erfRenorm(skillAdvantage/abilityForAdvantage)){killedTeam2Player = opponent}
        }
      }
      for(shooter in 1:gamers_each_side){
        if(team2_living[shooter] == 1){
          #pick an opponent (KLUDGEY - FIX)
          opponent = 0
          while(opponent == 0){
            lookFor = sample(1:gamers_each_side,1)
            if(team2_living[lookFor] == 1){opponent=lookFor}
          }
          #now take the shot!
          skillAdvantage = gamerAbilitiesTeam2[shooter]-gamerAbilitiesTeam1[opponent]
          if(runif(1, 0.0, 1.0)>erfRenorm(skillAdvantage/abilityForAdvantage)){killedTeam1Player = opponent}
        }
      }
      
      #now kill off the players who were hit
      if(killedTeam1Player){team1_living[killedTeam1Player] = 0}
      if(killedTeam2Player){team1_living[killedTeam2Player] = 0}
      
    }
    #someone is all dead, figure out who's alive; they won
    winner = if (sum(team1_living)>sum(team2_living)) 1 else 2
    return(winner)
  }else{ ########### NO GAME DEFINITION = COIN FLIP
    print('no game type declared, just flip a coin...')
    return(sample(1:2,1))
  }
}

#set all the gamer's intrinsic abilities
gamer_Ability <- rnorm(number_total_gamers, mean=mean_Ability, sd=std_Ability)
for (gamernum in 1:number_total_gamers){
  if (mod(gamernum,100) == 1){
    print(paste("Gamer number ", gamernum," has ability ",gamer_Ability[gamernum]))
  }
}

#NOT GENERALLY TRUE - for this initial test we're estimating the gamerScore to be perfectly accurate
gamer_Score = gamer_Ability

#gameresults <- data.frame(gamerSum=double(),
#                 gamerProduct=double(),
#                 gamewinner=integer(),
#                 stringsAsFactors=FALSE)

#this will hold the game results
gameresults = data.frame(matrix(vector(), 0, 5,
                       dimnames=list(c(), c("gamewinner", "gamerSumTeam1", "gamerProductTeam1", "gamerSumTeam2", "gamerProductTeam2"))),
                stringsAsFactors=F)

#now let's play some games
for (gamenum in 1:number_games_played){
  #pick some gamers according to a rule on their skill, group into teams (CURRENTLY TOTALLY RANDOM)
  gamerIndicesAll = sample(1:number_total_gamers,gamers_each_side*2)
  gamerIndicesTeam1 = gamerIndicesAll[1:gamers_each_side]
  gamerIndicesTeam2 = gamerIndicesAll[(gamers_each_side+1):(gamers_each_side*2)]
  gamerAbilitiesTeam1 = gamer_Ability[gamerIndicesTeam1]
  gamerAbilitiesTeam2 = gamer_Ability[gamerIndicesTeam2]
  #put these gamers together in a game and record who wins and the current gamer scores
  winSide = playGame(gamerAbilitiesTeam1,gamerAbilitiesTeam2,gameType) #returns either 1 or 2
  
  #make convolutions of gamer scores
  gamerSumTeam1 = sum(gamerAbilitiesTeam1)
  gamerProductTeam1 = prod(gamerAbilitiesTeam1)
  gamerSumTeam2 = sum(gamerAbilitiesTeam2)
  gamerProductTeam2 = prod(gamerAbilitiesTeam2)
    
  gameresults <- rbind(gameresults, c(winSide, gamerSumTeam1, gamerProductTeam1, gamerSumTeam2, gamerProductTeam2))

  #change the gamer scores based on who won (NOT YET IMPLEMENTED)
  
}

#now let's look at the results of these games, and see which sort of "scoring" is most important

#perhaps make some regression fits to find the best predictor

#play with it some!


print ('computations over')