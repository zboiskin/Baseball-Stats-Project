rm(tmp)
#new function to replace forloop
getGLS <- function(gls, category)
{
  desiredInfoFrame = do.call(rbind.data.frame, lapply(gls, function(gameList, category)
  {
    return(gameList[[category]])
  }, category))
  return(desiredInfoFrame)
}
categoryNames = names(gls2017[[1]])

#retrieves data from only one sheet
Home_Teams <-  data.frame(gls2017[[1]]$home_teams)
Away_Teams <- data.frame(gls2017[[1]]$away_teams)
Venues <- data.frame(gls2017[[1]]$venues)
GPlayers <- data.frame(gls2017[[1]]$players)
Game_Logs <- data.frame(gls2017[[1]]$game_logs)
Teams <- data.frame(gls2017[[1]]$teams)

#retrieves all desired category data from all sheets
Home_Teams <-getGLS(gls2017, 'home_teams')
Away_Teams <- getGLS(gls2017, 'away_teams')
Venues <- getGLS(gls2017, 'venues')
Players <-getGLS(gls2017, 'players')
Game_Logs <- getGLS(gls2017, 'game_logs')
Teams <- getGLS(gls2017, 'teams')

#creates unique observations for desired categories
Home_Teams <- unique(Home_Teams)
Away_Teams <- unique(Away_Teams)
Venues <- unique(Venues)
Players <- unique(Players)
Game_Logs <- unique(Game_Logs)
Teams <- unique(Teams)

#syntax for combining above 2 steps
Game_Logs <- unique(getGLS(gls = gls2017, category = 'game_logs'))

#combining desired data into one data frame
#matched players id with their team
PlayersxTeams<- merge(Players, Teams, by.x = "team_id", by.y = "id") 
#filtered out to only Rockies players
OnlyRockies <- PlayersxTeams[which(PlayersxTeams$hashtag == "Rockies"),] 
#matched Rockies players to which games they played
# Rockies_Logs <- merge(OnlyRockies, Game_Logs, by.x = "team_id", by.y = "player_id")
colnames(OnlyRockies)[which(colnames(OnlyRockies)%in%"id")] <- 'player_id'
Rockies_Logs <- merge(Game_Logs, OnlyRockies, by = 'player_id') 
#created table counting how many games each player id had
Games_Played <- table(Game_Logs$player_id) 
#convereted the table of players by game amount played into data frame
as.data.frame(Games_Played)
#assigned variable to amount of games played per player
Games_Played2 <- as.data.frame(Games_Played) 
#combines amount of games players played with main data frame
merge(Rockies_Logs, Games_Played2, by.x = "player_id", by.y = "Var1") 
#assigned name of new data frame with amount of games each player played
Rockies_Games_Played <- merge(Rockies_Logs, Games_Played2, by.x = "player_id", by.y = "Var1") 
#creates new data frame with only players that played 40+ games
Games_Played_100 <- Rockies_Games_Played[(Rockies_Games_Played$Freq >= 20),]
#Removing pitchers from the dataframe
No_Pitchers <- Games_Played_100[which(Games_Played_100$position_name != "Reliever"),]  
No_Pitchers1 <- No_Pitchers[which(No_Pitchers$position_name != "Starter"),] 
No_Pitchers2 <- No_Pitchers1[which(No_Pitchers1$position_name != "P"),] 
#of the 39 players on the Rockies roster, a sample of 21 players, who played more than 40 games, were selected
Player_Sample <- unique(No_Pitchers2$player_id)

No_Pitchers2 = No_Pitchers2[which(No_Pitchers2$player_id%in%Player_Sample),]
#test delete
Games_Played_100[ ,c('created_at.x', 'updated_at.x')] <- list(NULL)
#Creating data frame of only needed columns
colsToDrop <- c('bats', 'birth_date', 'captain', 'city', 'country', 'draft_overall_pick',
                'draft_round', 'draft_season', 'draft_team_name', 'high_school', 'team_id',
                'mlb_id', 'nickname', 'pro_debut', 'salary', 'salary_currency', 'school', 'slug.x', 'sport',
                'state', 'uniform_number', 'league_id.x', 'playing_position_id', 'created_at.y', 'updated_at.y',
                'mlbam_id', 'nickname', 'unit_of_height', 'unit_of_weight', 'color', 'colors', 'name.y', 'latitude',
                'longitude', 'slug.y', 'league_id.y', 'created_at', 'updated_at', 'division_id', 'catcher_interferences',
                'catcher_stealers_allowed', 'fielding_errors', 'outfield_assists','passed_balls','caught_stealing',
                'hit_by_pitch', 'innings_pitched', 'intentional_walks', 'intentional_walks_against', 'left_on_base',
                'rlisp_two_out', 'stolen_bases', 'balks', 'balls_thrown', 'batters_faced', 'blown_saves', 'complete_games',
                'earned_run_average', 'pitcher_fielding_errors', 'fly_balls_out', 'ground_ball_out', 'catcher_stealers_caught',
                'extra_base_hits', 'fly_ball_out', 'ground_ball_out', 'holds', 'inherited_runners_scored', 'inherited_runner_scoring_percentage',
                'losses', 'no_decision', 'outs_pitched', 'pickoffs', 'quality_starts', 'saves', 'shutouts', 'starting_pitches_thrown',
                'strikes_thrown', 'strike_percentage', 'whip', 'wild_pitches', 'wins', 'no_decisions', 'player_id')
Clean_Sample1 <- No_Pitchers2[ ,-which(colnames(No_Pitchers2)%in%colsToDrop)]
# Clean_Sample1 <- Clean_Sample1[, -c(51:68)]
Clean_Sample1
unique(Clean_Sample1$last_name)

# HOME GAMES #
#creating columns separately and then combining at the end
HHRs <- as.data.frame(aggregate(Home_Games$home_runs, list(Home_Games$last_name), sum))
HBatting_Avg <- as.data.frame(aggregate(Home_Games$batting_average, list(Home_Games$last_name), mean))
HHits <- as.data.frame(aggregate(Home_Games$hits, list(Home_Games$last_name), sum))
HRBI <- as.data.frame(aggregate(Home_Games$runs_batted_in, list(Home_Games$last_name), sum))
HAt_Bats <- as.data.frame(aggregate(Home_Games$at_bats, list(Home_Games$last_name), sum))

Final_Home <- cbind.data.frame(HHRs[,2] , HBatting_Avg[,2] , HHits[,2] , HRBI[,2] , HAt_Bats[ , 2])

Final_Home$Real_Batting_Avg <- (aggregate(Home_Games$hits, list(Home_Games$last_name), sum))[,2]/(aggregate(Home_Games$at_bats, list(Home_Games$last_name), sum))[,2]

#Away Games
#creating columns separately and then combining at the end
AHRs <- as.data.frame(aggregate(Home_Games$home_runs, list(Home_Games$last_name), sum))
ABatting_Avg <- as.data.frame(aggregate(Home_Games$batting_average, list(Home_Games$last_name), mean))
AHits <- as.data.frame(aggregate(Home_Games$hits, list(Home_Games$last_name), sum))
ARBI <- as.data.frame(aggregate(Home_Games$runs_batted_in, list(Home_Games$last_name), sum))
AAt_Bats <- as.data.frame(aggregate(Home_Games$at_bats, list(Home_Games$last_name), sum))

Final_Away <- cbind.data.frame(AHRs[,2] , ABatting_Avg[,2] , AHits[,2] , ARBI[,2] , AAt_Bats[ , 2])
head(Final_Away)

Final_Away$avg=Final_Away$Hits/Final_Away$At_Bats
Final_Away$Real_Batting_Avg <- (aggregate(Away_Games$hits, list(Away_Games$last_name), sum))[,2]/(aggregate(Away_Games$at_bats, list(Away_Games$last_name), sum))[,2]
Final_Away
#Renaming Columns
colnames(Final_Home) <- c("HRs", "Batting_Avg", "Hits", "RBIs", "At_Bats", "Real_Batting_Avg")
colnames(Final_Away) <- c("HRs", "Batting_Avg", "Hits", "RBIs", "At_Bats", "Real_Batting_Avg")

#T-test
t.test(Final_Home$HRs, Final_Away$HRs, alternative = "greater", conf.level = .8, sigma.x = sd(Final_Home$HRs), sigma.y = sd(Final_Away$HRs))  



t.test(Final_Home$`Real_Batting Avg`, Final_Away$`Real_Batting Avg`, alternative = "greater", conf.level = .8,
       sigma.x = sd(Final_Home$`Real_Batting Avg`), sigma.y = sd(Final_Away$`Real_Batting Avg`)) 


#Home
t.test(Final_Home$Batting_Avg, Final_Away$Real_Batting_Avg, alternative = "two.sided", 
       conf.level = .8, sigma.x = sd(Final_Home$Batting_Avg), sigma.y = sd(Final_Home$Real_Batting_Avg))  

#Away
t.test(Final_Away$Batting_Avg, Final_Away$Real_Batting_Avg, alternative = "two.sided", 
       conf.level = .8, sigma.x = sd(Final_Away$Batting_Avg), sigma.y = sd(Final_Away$Real_Batting_Avg))  

#Chi squared Test for independence 
chisq.test(Clean_Sample1$game_started, Clean_Sample1$humanized_salary)


#F-test
var.test(Final_Away$RBIs, Final_Home$RBIs, conf.level = .8)

#Plotting
library(ggplot2)

hist(Final_Away$Real_Batting_Avg)
hist(Final_Home$Real_Batting_Avg)
qplot(Final_Away$Real_Batting_Avg, geom="histogram") 
qplot(Final_Home$Real_Batting_Avg, geom="histogram") 

qplot(Final_Away$Real_Batting_Avg,
      geom="histogram",
      binwidth = 0.05,  
      main = "Away Real Batting Avg.", 
      xlab = "Avg",  
      fill=I("red"), 
      col=I("blue"), 
      alpha=I(.01),
      xlim=c(0.0,0.5))

qplot(Final_Home$Real_Batting_Avg,
      geom="histogram",
      binwidth = 0.05,  
      main = "Home Real Batting Avg.", 
      xlab = "Avg",  
      fill=I("red"), 
      col=I("blue"), 
      alpha=I(.01),
      xlim=c(0.0,0.5))