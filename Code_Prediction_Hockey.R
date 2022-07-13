# On importe les librairies nécessaires

library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(ROCR)
library(caret)
library (pROC)

############################################################################################
############################################################################################
#################### CREATION DE LA TABLE ET DES INDICATEURS ###############################
############################################################################################
############################################################################################

# On importe les données nécessaires

game_skater_stats <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/game_skater_stats.csv")
player_info <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/player_info.csv")
game_goalie_stats <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/game_goalie_stats.csv")
game <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/game.csv")
game_teams_stats <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/game_teams_stats.csv")
team_info <- read.csv("C:/Users/antoi/Documents/Medas/Master2/Statistiques_2/exos_data/team_info.csv")

# On dédoublonne certaines tables 
game <- subset(game, !duplicated(game_teams_stats[,"game_id"]))
game_teams_stats <- subset(game_teams_stats, !duplicated(game_teams_stats[,c("game_id","team_id")]))

# On ajoute certains champs, on filtre certaines tables et on créé de nouveaux champs
game_skater_stats<-merge(game_skater_stats,game[,c("game_id","season","date_time_GMT")],by="game_id")
game_skater_stats<-game_skater_stats[game_skater_stats$timeOnIce > 600,]
game_skater_stats$minsplayed <- game_skater_stats$timeOnIce/60
game_goalie_stats<-merge(game_goalie_stats,game[,c("game_id","season","date_time_GMT")],by="game_id")
game_goalie_stats<-game_goalie_stats[game_goalie_stats$timeOnIce > 600,]
game_goalie_stats$score <- game_goalie_stats$savePercentage
game_teams_stats<-merge(game_teams_stats,game[,c("game_id","season","date_time_GMT")],by="game_id")
game<-game[game$season > 20102011,]


# Création de tables vides pour les implémenter dans la future boucle
home_score <- data.frame(game_id = integer(),
                         score = double())
home_goal_score <- data.frame(game_id = integer(),
                              score = double())
home_stats <- data.frame(game_id = integer(),
                         home_goals_for10lastmatchs=double(),
                         home_shots_for10lastmatchs=double(),
                         home_hits_for10lastmatchs=double(),
                         home_pim_for10lastmatchs=double(),
                         home_powerPlayOpportunities_for10lastmatchs=double(),
                         home_powerPlayGoals_for10lastmatchs=double(),
                         home_faceOffWinPercentage_for10lastmatchs=double(),
                         home_giveaways_for10lastmatchs=double(),
                         home_takeaways_for10lastmatchs=double(),
                         home_blocked_for10lastmatchs=double())
away_score <- data.frame(game_id = integer(),
                         score = double())
away_goal_score <- data.frame(game_id = integer(),
                              score = double())
away_stats <- data.frame(game_id = integer(),
                         away_goals_for10lastmatchs=double(),
                         away_shots_for10lastmatchs=double(),
                         away_hits_for10lastmatchs=double(),
                         away_pim_for10lastmatchs=double(),
                         away_powerPlayOpportunities_for10lastmatchs=double(),
                         away_powerPlayGoals_for10lastmatchs=double(),
                         away_faceOffWinPercentage_for10lastmatchs=double(),
                         away_giveaways_for10lastmatchs=double(),
                         away_takeaways_for10lastmatchs=double(),
                         away_blocked_for10lastmatchs=double())
game <- na.omit(game)


# On réalise une boucle afin  d'obtenir les indicateurs que nous souhaitons utiliser

for(i in 1:dim(game)[1]){
  
# Pour chaque match on stocke la date, l'id des 2 équipes et du match dans une variable
  
  date<-game[i,"date_time_GMT"]
  team_home_id <-game[i,"home_team_id"]
  team_away_id <-game[i,"away_team_id"]
  game_id <-game[i,"game_id"]
  
  
#On selectionne les statistique des joueurs de l'équipe a domicile pour les matchs ayant une date inférieure 
# à celle du match qu'on souhaite prédire 
  score_team_home <- game_skater_stats[game_skater_stats$date_time_GMT< date & game_skater_stats$team_id ==team_home_id, ]

# On créé une table qui ne contient que les 10 derniers matchs de l'équipe à domicile 
  ref_matchs_home <- unique(score_team_home[,c("date_time_GMT","game_id")])  
  ref_matchs_home <- ref_matchs_home[order(ref_matchs_home$date_time_GMT,decreasing = TRUE),]
  ref_matchs_home<-ref_matchs_home[1:10,]
  
# On créé subjectivement un score : 
  score_team_home$score <- 60 +12*score_team_home$goals +8*score_team_home$assists -2*score_team_home$shots + 2 *score_team_home$hits -2*score_team_home$penaltyMinutes + 7 * score_team_home$faceOffWins - 2*score_team_home$faceoffTaken + 3*score_team_home$takeaways - 2*score_team_home$giveaways + 5* score_team_home$blocked-1*score_team_home$minsplayed
  score_team_home$score <- ifelse(score_team_home$score>100,100,score_team_home$score)
  score_team_home$score <- ifelse(score_team_home$score<0,0,score_team_home$score)
# On garde dans notre table seulement les variables qui nous intéressent :
  score_team_home2 <- subset(score_team_home, select=c(game_id, team_id, date_time_GMT,score))
# Meme chose pour le gardien, son score correspondra au % d'arret moyen sur les 10 derniers matchs :
  score_goal_home <- game_goalie_stats[game_goalie_stats$date_time_GMT< date & game_goalie_stats$team_id ==team_home_id, ]
  score_goal_home2 <- subset(score_goal_home, select=c(game_id, team_id, date_time_GMT,score))
  score_goal_home2 <- na.omit(score_goal_home2)
# On concatène la table des joueurs avec celle du gardien :
  score_team_home2 <- rbind(score_team_home2,score_goal_home2)
# On vérifie qu'on a des données dans la table gardien avant de créer notre indicateur du % de sauvetage moyen sur les 10
# derniers matchs 
  if (nrow(score_goal_home2)!=0){
# On regarde le % d'arrets moyens par match
    score_goal_home3 <- aggregate(score ~ game_id, data = score_goal_home2, mean,na.action=na.pass)
# On garde uniquement les 10 derniers matchs
    score_goal_home_fin <- ref_matchs_home %>% 
      inner_join(y = score_goal_home3, 
                 by = c("game_id" = "game_id"))
# On effectue ensuite une moyenne de ces 10 derniers match et on indique à quel match on va venir greffer ce score
    score_goal_home_fin$game_id2 <- game_id
    score_goal_home4 <- aggregate(score ~ game_id2, data = score_goal_home_fin, mean,na.action=na.pass)
    score_goal_home4 <- score_goal_home4 %>% rename(game_id=game_id2)
    score_goal_home4 <- score_goal_home4 %>% rename(home_goal_score_for10lastmatchs=score)
# On implémente une table de ce score et de l'id de la game auquel il va falloir le lier à la fin de la boucle
    home_goal_score <- rbind(home_goal_score,score_goal_home4)
    home_goal_score <- home_goal_score[,c("game_id","home_goal_score_for10lastmatchs")]
  }
    
  score_team_home2 <- na.omit(score_team_home2)

# On effectue maintenant la meme action en faisant la moyenne du score de tous les joueurs de l'equipe cette fois et non
# seulement le gardien de buts afin d'avoir un indicateur de la note moyenne des joueurs d'une équipe lors des 10 derniers matchs
  if (nrow(score_team_home2)!=0){
    score_team_home3 <- aggregate(score ~ game_id, data = score_team_home2, mean,na.action=na.pass)
    
    score_team_home_fin <- ref_matchs_home %>% 
      inner_join(y = score_team_home3, 
                 by = c("game_id" = "game_id"))
    
    score_team_home_fin$game_id2 <- game_id
    score_team_home4 <- aggregate(score ~ game_id2, data = score_team_home_fin, mean,na.action=na.pass)
    score_team_home4 <- score_team_home4 %>% rename(game_id=game_id2)
    score_team_home4 <- score_team_home4 %>% rename(home_score_for10lastmatchs=score)
    home_score <- rbind(home_score,score_team_home4)
    home_score <- home_score[,c("game_id","home_score_for10lastmatchs")]
  }

  # On réalise la meme chose sur la table des statistique de l'équipe globale afin de créer plusieurs indicateurs sur les 
  # 10 derniers matchs : 
  
  stats_matchs_home <- game_teams_stats[game_teams_stats$date_time_GMT< date & game_teams_stats$team_id ==team_home_id, ]
  stats_matchs_home_fin <- ref_matchs_home %>% 
    inner_join(y = stats_matchs_home, 
               by = c("game_id" = "game_id"))
  if (nrow(stats_matchs_home_fin)!=0){
    stats_matchs_home_fin$game_id2 <- game_id
    stats_home <- aggregate(cbind(goals,shots,hits,pim,powerPlayOpportunities,powerPlayGoals,faceOffWinPercentage,giveaways,takeaways,blocked) ~ game_id2, data = stats_matchs_home_fin, mean,na.action=na.pass)
    stats_home$home_percentage_won_for10lastmatchs <- (sum(stats_matchs_home_fin$won, na.rm = TRUE))/(nrow(stats_matchs_home_fin))*100
    stats_home <- stats_home %>% rename(game_id=game_id2)
    stats_home <- stats_home %>% rename(home_goals_for10lastmatchs=goals,home_shots_for10lastmatchs=shots,home_hits_for10lastmatchs=hits,
                                        home_pim_for10lastmatchs=pim,home_powerPlayOpportunities_for10lastmatchs=powerPlayOpportunities,
                                        home_powerPlayGoals_for10lastmatchs=powerPlayGoals,home_faceOffWinPercentage_for10lastmatchs=faceOffWinPercentage,
                                        home_giveaways_for10lastmatchs=giveaways,home_takeaways_for10lastmatchs=takeaways,home_blocked_for10lastmatchs=blocked)
    home_stats <- rbind(home_stats,stats_home)
    home_stats <- home_stats[,c("game_id","home_goals_for10lastmatchs","home_shots_for10lastmatchs","home_hits_for10lastmatchs","home_pim_for10lastmatchs",
                                "home_powerPlayOpportunities_for10lastmatchs","home_powerPlayGoals_for10lastmatchs","home_faceOffWinPercentage_for10lastmatchs",
                                "home_giveaways_for10lastmatchs","home_takeaways_for10lastmatchs","home_blocked_for10lastmatchs","home_percentage_won_for10lastmatchs")]
  }
  
  
  # On effectue la meme chose que ci dessus avec l'équipe à l'extérieur cette fois ci :
  

  score_team_away <- game_skater_stats[game_skater_stats$date_time_GMT< date & game_skater_stats$team_id ==team_away_id, ]
  ref_matchs_away <- unique(score_team_away[,c("date_time_GMT","game_id")])  
  ref_matchs_away <- ref_matchs_away[order(ref_matchs_away$date_time_GMT,decreasing = TRUE),]
  ref_matchs_away<-ref_matchs_away[1:10,]
  score_team_away$score <- 60 +12*score_team_away$goals +8*score_team_away$assists -2*score_team_away$shots + 2 *score_team_away$hits -2*score_team_away$penaltyMinutes + 7 * score_team_away$faceOffWins - 2*score_team_away$faceoffTaken + 3*score_team_away$takeaways - 2*score_team_away$giveaways + 5* score_team_away$blocked-1*score_team_away$minsplayed
  score_team_away$score <- ifelse(score_team_away$score>100,100,score_team_away$score)
  score_team_away$score <- ifelse(score_team_away$score<0,0,score_team_away$score)
  score_team_away2 <- subset(score_team_away, select=c(game_id, team_id, date_time_GMT,score))
  
  score_goal_away <- game_goalie_stats[game_goalie_stats$date_time_GMT< date & game_goalie_stats$team_id ==team_away_id, ]
  
  score_goal_away2 <- subset(score_goal_away, select=c(game_id, team_id, date_time_GMT,score))
  score_goal_away2 <- na.omit(score_goal_away2)
  score_team_away2 <- rbind(score_team_away2,score_goal_away2)
  
  if (nrow(score_goal_away2)!=0){
    score_goal_away3 <- aggregate(score ~ game_id, data = score_goal_away2, mean,na.action=na.pass)
    score_goal_away_fin <- ref_matchs_away %>% 
      inner_join(y = score_goal_away3, 
                 by = c("game_id" = "game_id"))
    score_goal_away_fin$game_id2 <- game_id
    score_goal_away4 <- aggregate(score ~ game_id2, data = score_goal_away_fin, mean,na.action=na.pass)
    score_goal_away4 <- score_goal_away4 %>% rename(game_id=game_id2)
    score_goal_away4 <- score_goal_away4 %>% rename(away_goal_score_for10lastmatchs=score)
    away_goal_score <- rbind(away_goal_score,score_goal_away4)
    away_goal_score <- away_goal_score[,c("game_id","away_goal_score_for10lastmatchs")]
  }
  
  score_team_away2 <- na.omit(score_team_away2)
  if (nrow(score_team_away2)!=0){
    score_team_away3 <- aggregate(score ~ game_id, data = score_team_away2, mean,na.action=na.pass)
    
    score_team_away_fin <- ref_matchs_away %>% 
      inner_join(y = score_team_away3, 
                 by = c("game_id" = "game_id"))
    
    score_team_away_fin$game_id2 <- game_id
    score_team_away4 <- aggregate(score ~ game_id2, data = score_team_away_fin, mean,na.action=na.pass)
    score_team_away4 <- score_team_away4 %>% rename(game_id=game_id2)
    score_team_away4 <- score_team_away4 %>% rename(away_score_for10lastmatchs=score)
    away_score <- rbind(away_score,score_team_away4)
    away_score <- away_score[,c("game_id","away_score_for10lastmatchs")]
  }
  #team stats pour la l'équipe à l'extérieure : 
  stats_matchs_away <- game_teams_stats[game_teams_stats$date_time_GMT< date & game_teams_stats$team_id ==team_away_id, ]
  stats_matchs_away_fin <- ref_matchs_away %>% 
    inner_join(y = stats_matchs_away, 
               by = c("game_id" = "game_id"))
  if (nrow(stats_matchs_away_fin)!=0){
    stats_matchs_away_fin$game_id2 <- game_id
    stats_away <- aggregate(cbind(goals,shots,hits,pim,powerPlayOpportunities,powerPlayGoals,faceOffWinPercentage,giveaways,takeaways,blocked) ~ game_id2, data = stats_matchs_away_fin, mean,na.action=na.pass)
    stats_away$away_percentage_won_for10lastmatchs <- (sum(stats_matchs_away_fin$won, na.rm = TRUE))/(nrow(stats_matchs_away_fin))*100
    stats_away <- stats_away %>% rename(game_id=game_id2)
    stats_away <- stats_away %>% rename(away_goals_for10lastmatchs=goals,away_shots_for10lastmatchs=shots,away_hits_for10lastmatchs=hits,away_pim_for10lastmatchs=pim,away_powerPlayOpportunities_for10lastmatchs=powerPlayOpportunities,away_powerPlayGoals_for10lastmatchs=powerPlayGoals,away_faceOffWinPercentage_for10lastmatchs=faceOffWinPercentage,away_giveaways_for10lastmatchs=giveaways,away_takeaways_for10lastmatchs=takeaways,away_blocked_for10lastmatchs=blocked)
    away_stats <- rbind(away_stats,stats_away)
    away_stats <- away_stats[,c("game_id","away_goals_for10lastmatchs","away_shots_for10lastmatchs","away_hits_for10lastmatchs","away_pim_for10lastmatchs",
                                "away_powerPlayOpportunities_for10lastmatchs","away_powerPlayGoals_for10lastmatchs","away_faceOffWinPercentage_for10lastmatchs",
                                "away_giveaways_for10lastmatchs","away_takeaways_for10lastmatchs","away_blocked_for10lastmatchs","away_percentage_won_for10lastmatchs")]
  }
}

# Une fois la boucle fini nous obtenons 6 tables avec a chaque fois l'id de la game et un ou plusieurs indicateurs concernant 
# cette dernière, on les lie toute à la table game du début :

game_final <- game %>% 
  left_join(y = home_score, 
            by = c("game_id" = "game_id"))
game_final <- game_final %>% 
  left_join(y = away_score, 
            by = c("game_id" = "game_id"))
game_final <- game_final %>% 
  left_join(y = home_stats, 
            by = c("game_id" = "game_id"))
game_final <- game_final %>% 
  left_join(y = away_stats, 
            by = c("game_id" = "game_id"))
game_final <- game_final %>% 
  left_join(y = away_goal_score, 
            by = c("game_id" = "game_id"))
game_final <- game_final %>% 
  left_join(y = home_goal_score, 
            by = c("game_id" = "game_id"))

# On enlève les valeurs nulles ainsi que les doublons :

game_final <- na.omit(game_final)
game_final <- subset(game_final, !duplicated(game_final[,"game_id"]))

#######################################################################
#######################################################################
################## CREATION DE CHAMPS POUR L'ANALYSE ##################
#######################################################################
#######################################################################

# On créé encore de nouveaux champs utiles à notre étude et on en transforme certains en facteurs :

game_final$home_winner <- ifelse(game_final$outcome == "home win REG", 1, ifelse(game_final$outcome == "home win OT",1,0))
game_final$winner <- ifelse(game_final$home_winner == 1, 'home', 'away')

game_final$diff_goals_last10matchs <- game_final$home_goals_for10lastmatchs-game_final$away_goals_for10lastmatchs
game_final$diff_shots_last10matchs <- game_final$home_shots_for10lastmatchs-game_final$away_shots_for10lastmatchs
game_final$diff_hits_last10matchs <- game_final$home_hits_for10lastmatchs-game_final$away_hits_for10lastmatchs
game_final$diff_pim_last10matchs <- game_final$home_pim_for10lastmatchs-game_final$away_pim_for10lastmatchs
game_final$diff_powerPlayOpportunities_last10matchs <- game_final$home_powerPlayOpportunities_for10lastmatchs-game_final$away_powerPlayOpportunities_for10lastmatchs
game_final$diff_powerPlayGoal_last10matchs <- game_final$home_powerPlayGoals_for10lastmatchs-game_final$away_powerPlayGoals_for10lastmatchs
game_final$diff_faceOffWinPercentage_last10matchs <- game_final$home_faceOffWinPercentage_for10lastmatchs-game_final$away_faceOffWinPercentage_for10lastmatchs
game_final$diff_giveaways_last10matchs <- game_final$home_giveaways_for10lastmatchs-game_final$away_giveaways_for10lastmatchs
game_final$diff_takeaways_last10matchs <- game_final$home_takeaways_for10lastmatchs-game_final$away_takeaways_for10lastmatchs
game_final$diff_blocked_last10matchs <- game_final$home_blocked_for10lastmatchs-game_final$away_blocked_for10lastmatchs
game_final$diff_percentage_won_last10matchs <- game_final$home_percentage_won_for10lastmatchs-game_final$away_percentage_won_for10lastmatchs
game_final$diff_percentage_save_last10matchs <- game_final$home_goal_score_for10lastmatchs-game_final$away_goal_score_for10lastmatchs
game_final$class_diff_percentage_won_last10matchs <- ifelse(between(game_final$diff_percentage_won_last10matchs,-80,-60), "1-inférieur à -60", ifelse(between(game_final$diff_percentage_won_last10matchs,-60,-40),"2-entre -60 et -40",ifelse(between(game_final$diff_percentage_won_last10matchs,-40,-20),"3-entre -40 et -20",ifelse(between(game_final$diff_percentage_won_last10matchs,-20,0),"4-entre -20 et 0",ifelse(between(game_final$diff_percentage_won_last10matchs,0,20),"5-entre 0 et 20",ifelse(between(game_final$diff_percentage_won_last10matchs,20,40),"6-entre 20 et 40",ifelse(between(game_final$diff_percentage_won_last10matchs,40,60),"7-entre 40 et 60","8-supérieur à 60")))))))
game_final$class_diff_percentage_won_last10matchs <- as.factor(game_final$class_diff_percentage_won_last10matchs )
game_final$class_diff_goals_last10matchs <- ifelse(between(game_final$diff_goals_last10matchs,-4,-2), "1-entre 4 et 2 buts de moins", ifelse(between(game_final$diff_goals_last10matchs,-2,0),"2-entre 0 et 2 buts de moins",ifelse(between(game_final$diff_goals_last10matchs,0,2),"3-entre 0 et 2 buts de plus","4-entre 2 et 4 buts de plus")))
game_final$class_diff_goals_last10matchs <- as.factor(game_final$class_diff_goals_last10matchs )
game_final$class_diff_shots_last10matchs <- ifelse(between(game_final$diff_shots_last10matchs,-14,-10), "1-entre 10 et 14 tirs de moins", ifelse(between(game_final$diff_shots_last10matchs,-10,-6),"2-entre 6 et 10 tirs de moins",ifelse(between(game_final$diff_shots_last10matchs,-6,-2),"3-entre 2 et 6 tirs de moins",ifelse(between(game_final$diff_shots_last10matchs,-2,2),"4-entre 2 tirs de moins et 2 tirs de plus",ifelse(between(game_final$diff_shots_last10matchs,2,6),"5-entre 2 et 6 tirs de plus",ifelse(between(game_final$diff_shots_last10matchs,6,10),"6-entre 6 et 10 tirs de plus","7-entre 10 et 16 tirs de plus"))))))
game_final$class_diff_shots_last10matchs <- as.factor(game_final$class_diff_shots_last10matchs )
game_final$home_winner <- as.factor(game_final$home_winner )


#######################################################################
#######################################################################
####################### STATS DESC ####################################
#######################################################################
#######################################################################

# On réalise plusieurs graphiques afin de regarder l'impact de plusieurs variables sur la victoire :

ggplot(game_final, aes(winner)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

count_table <- game_final %>% count(home_winner, class_diff_percentage_won_last10matchs,sort=TRUE)
count_table2 <- game_final %>% count(home_winner, class_diff_goals_last10matchs,sort=TRUE)
count_table3 <- game_final %>% count(home_winner, class_diff_shots_last10matchs,sort=TRUE)

ggplot(count_table, aes(fill=home_winner, y=n, x=class_diff_percentage_won_last10matchs)) + 
  geom_bar(position="fill", stat="identity")

ggplot(count_table2, aes(fill=home_winner, y=n, x=class_diff_goals_last10matchs)) + 
  geom_bar(position="fill", stat="identity")

ggplot(count_table3, aes(fill=home_winner, y=n, x=class_diff_shots_last10matchs)) + 
  geom_bar(position="fill", stat="identity")

########################
########################
#### Echantillonage ####
########################
########################

app <- sample(nrow(game_final), nrow(game_final)*0.7, replace=FALSE)
pred <- c(1:nrow(game_final))
pred<-pred[-app]

game_final_app<- game_final[app,]
game_final_pred<- game_final[pred,]


########################
########################
##### Modélisation #####
########################
########################

#########################
#### Modèle numéro 1 #### 
#########################

res_glm<-glm(home_winner~diff_goals_last10matchs+diff_shots_last10matchs+diff_hits_last10matchs+diff_pim_last10matchs+diff_powerPlayOpportunities_last10matchs+diff_powerPlayGoal_last10matchs+diff_faceOffWinPercentage_last10matchs+diff_giveaways_last10matchs+diff_takeaways_last10matchs+diff_blocked_last10matchs+diff_percentage_won_last10matchs, family = binomial(link=logit), data= game_final_app)
summary(res_glm)

# MAtrice de confusion / apprentissage
appren.p <- cbind(game_final_app, predict(res_glm, newdata = game_final_app, type = "link", 
                                          se = TRUE))
appren.p <- within(appren.p, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$home_winner))


#taux de bien classé
(m.confusion[1,1]+m.confusion[2,2])/sum(m.confusion)

# MAtrice de confusion / test
appren.p <- cbind(game_final_pred, predict(res_glm, newdata = game_final_pred, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$home_winner)))

(m.confusion[1,1]+m.confusion[2,2])/sum(m.confusion)



# Courbe ROC

pred <- prediction(appren.p$PredictedProb, appren.p$home_winner)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(res_glm) #residus de deviance
res_pear <- residuals(res_glm,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(res_glm) #residu de deviance standardises
H <- influence(res_glm)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(res_glm),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(res_glm),rstudent(res_glm),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(res_glm),type="h",ylab="Distance de Cook") 





fitControl <- trainControl ( method= "none" )
m_lr <- train (home_winner~diff_goals_last10matchs+diff_shots_last10matchs+diff_hits_last10matchs+diff_pim_last10matchs+diff_powerPlayOpportunities_last10matchs+diff_powerPlayGoal_last10matchs+diff_faceOffWinPercentage_last10matchs+diff_giveaways_last10matchs+diff_takeaways_last10matchs+diff_blocked_last10matchs+diff_percentage_won_last10matchs,data=game_final_app,method= "glm" , trControl= fitControl) 

#Courbe lift 
score <- predict(m_lr,game_final_pred,type="prob")[,"1"] 
print(quantile(score)) 




#tableau de données pour le scoring 
liftdata <- data.frame(classe=game_final_pred$home_winner) 
liftdata$score <- score 

#Courbe lift
lift_obj <- lift(classe ~ score, data=liftdata, class="1") 
print(lift_obj) 
plot(lift_obj) 

#objet roc 
roc_obj <- roc ( game_final_pred$home_winner == "1" ,score) 
#plot de l'objet roc 
plot ( 1 - roc_obj$specificities,roc_obj$sensitivities,type= "l" )
abline(0,1)

print(roc_obj$auc)


#########################
#### Modèle numéro 2 #### 
#########################

res_glm2<-glm(home_winner~diff_percentage_save_last10matchs+diff_goals_last10matchs+diff_shots_last10matchs+diff_percentage_won_last10matchs, family = binomial(link=logit), data= game_final_app)
summary(res_glm2)

# MAtrice de confusion / apprentissage
appren.p2 <- cbind(game_final_app, predict(res_glm2, newdata = game_final_app, type = "link", 
                                          se = TRUE))
appren.p2 <- within(appren.p2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p2 <- cbind(appren.p2, pred.chd = factor(ifelse(appren.p2$PredictedProb > 0.5, 1, 0)))
m.confusion2 <- as.matrix(table(appren.p2$pred.chd, appren.p2$home_winner))


#taux de bien classé
(m.confusion2[1,1]+m.confusion2[2,2])/sum(m.confusion2)

# MAtrice de confusion / test
appren.p2 <- cbind(game_final_pred, predict(res_glm2, newdata = game_final_pred, type = "link", 
                                           se = TRUE))
appren.p2 <- within(appren.p2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p2 <- cbind(appren.p2, pred.chd = factor(ifelse(appren.p2$PredictedProb > 0.5, 1, 0)))
(m.confusion2 <- as.matrix(table(appren.p2$pred.chd, appren.p2$home_winner)))

(m.confusion2[1,1]+m.confusion2[2,2])/sum(m.confusion2)



# Courbe ROC

pred2 <- prediction(appren.p2$PredictedProb, appren.p2$home_winner)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2)

# Analyse des résidus
res_dev2 <- residuals(res_glm2) #residus de deviance
res_pear2 <- residuals(res_glm2,type="pearson") #residus de Pearson
res_dev_stand2 <- rstandard(res_glm2) #residu de deviance standardises
H2 <- influence(res_glm2)$hat #diagonale de la hat matrix
res_pear_stand2 <- res_pear2/sqrt(1-H2) #residu de Pearson standardises
plot(rstudent(res_glm2),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(res_glm2),rstudent(res_glm2),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(res_glm2),type="h",ylab="Distance de Cook") 





fitControl <- trainControl ( method= "none" )
m_lr2 <- train (home_winner~diff_percentage_save_last10matchs+diff_goals_last10matchs+diff_shots_last10matchs+diff_percentage_won_last10matchs,data=game_final_app,method= "glm" , trControl= fitControl) 

#Courbe lift 
score2 <- predict(m_lr2,game_final_pred,type="prob")[,"1"] 
print(quantile(score2)) 




#tableau de données pour le scoring 
liftdata2 <- data.frame(classe=game_final_pred$home_winner) 
liftdata2$score <- score2 

#Courbe lift
lift_obj2 <- lift(classe ~ score2, data=liftdata2, class="1") 
print(lift_obj2) 
plot(lift_obj2) 

#objet roc 
roc_obj2 <- roc ( game_final_pred$home_winner == "1" ,score2) 
#plot de l'objet roc 
plot ( 1 - roc_obj2$specificities,roc_obj2$sensitivities,type= "l" )
abline(0,1)

print(roc_obj2$auc)



##########################################################################
##########################################################################
##########################################################################
##########################################################################


# On change certains paramètre de la table principale :

game_final2<-game_final[game_final$season > 20182019,]


# On refait l'échantillonage :

app <- sample(nrow(game_final2), nrow(game_final2)*0.7, replace=FALSE)
pred <- c(1:nrow(game_final2))
pred<-pred[-app]

game_final_app<- game_final2[app,]
game_final_pred<- game_final2[pred,]

#########################
#### Modèle numéro 3 #### 
#########################

res_glm3<-glm(home_winner~class_diff_shots_last10matchs+class_diff_goals_last10matchs+class_diff_percentage_won_last10matchs, family = binomial(link=logit), data= game_final_app)
summary(res_glm3)

# MAtrice de confusion / apprentissage
appren.p3 <- cbind(game_final_app, predict(res_glm3, newdata = game_final_app, type = "link", 
                                           se = TRUE))
appren.p3 <- within(appren.p3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p3 <- cbind(appren.p3, pred.chd = factor(ifelse(appren.p3$PredictedProb > 0.5, 1, 0)))
m.confusion3 <- as.matrix(table(appren.p3$pred.chd, appren.p3$home_winner))


#taux de bien classé
(m.confusion3[1,1]+m.confusion3[2,2])/sum(m.confusion3)

# MAtrice de confusion / test
appren.p3 <- cbind(game_final_pred, predict(res_glm3, newdata = game_final_pred, type = "link", 
                                            se = TRUE))
appren.p3 <- within(appren.p3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p3 <- cbind(appren.p3, pred.chd = factor(ifelse(appren.p3$PredictedProb > 0.5, 1, 0)))
(m.confusion3 <- as.matrix(table(appren.p3$pred.chd, appren.p3$home_winner)))

(m.confusion3[1,1]+m.confusion3[2,2])/sum(m.confusion3)



# Courbe ROC

pred3 <- prediction(appren.p3$PredictedProb, appren.p3$home_winner)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3)

# Analyse des résidus
res_dev3 <- residuals(res_glm3) #residus de deviance
res_pear3 <- residuals(res_glm3,type="pearson") #residus de Pearson
res_dev_stand3 <- rstandard(res_glm3) #residu de deviance standardises
H3 <- influence(res_glm3)$hat #diagonale de la hat matrix
res_pear_stand3 <- res_pear3/sqrt(1-H3) #residu de Pearson standardises
plot(rstudent(res_glm3),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(res_glm3),rstudent(res_glm3),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(res_glm3),type="h",ylab="Distance de Cook") 





fitControl <- trainControl ( method= "none" )
m_lr3 <- train (home_winner~class_diff_shots_last10matchs+class_diff_goals_last10matchs+class_diff_percentage_won_last10matchs,data=game_final_app,method= "glm" , trControl= fitControl) 

#Courbe lift 
score3 <- predict(m_lr3,game_final_pred,type="prob")[,"1"] 
print(quantile(score3)) 




#tableau de données pour le scoring 
liftdata3 <- data.frame(classe=game_final_pred$home_winner) 
liftdata3$score <- score3 

#Courbe lift
lift_obj3 <- lift(classe ~ score3, data=liftdata3, class="1") 
print(lift_obj3) 
plot(lift_obj3) 

#objet roc 
roc_obj3 <- roc ( game_final_pred$home_winner == "1" ,score3) 
#plot de l'objet roc 
plot ( 1 - roc_obj3$specificities,roc_obj3$sensitivities,type= "l" )
abline(0,1)

print(roc_obj3$auc)














