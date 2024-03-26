summary(SoccerData)
View(SoccerData)

library(dplyr)
library(ggplot2)

is_tourn <- SoccerData %>%
  filter((tournament == "Oceania Nations Cup") | (tournament == "Cyprus International Tournament"))

ggplot(is_tourn, aes(tournament,home_score))+
  geom_boxplot()

tapply(is_tourn$home_score, is_tourn$tournament, mean)
tapply(is_tourn$home_score, is_tourn$tournament, length)

france <- SoccerData %>%
  filter(home_team == "France")
         
tapply(france$home_score, france$home_team, mean)
tapply(france$home_score, france$home_team, length)         

t.test(home_score~france, data=SoccerData)
t.test(home_score~is_tourn$tournament, data=is_tourn)


france <- SoccerData$home_team == "Mexico"
tapply(SoccerData$home_score, france, mean)
tapply(SoccerData$home_score, france, length)

t.test(home_score~france, data=SoccerData)

set.seed(22)                                      
num_sim <- 1000
diffs <- numeric(num_sim)                         
for(i in 1:num_sim){
  fr <- sample(1:20003,176)                          
  fr_mean <- mean(SoccerData$home_score[fr])      
  not_fr_mean <- mean(SoccerData$home_score[-fr]) 
  diffs[i] <- not_fr_mean - fr_mean           
}

fr_mean <- mean(SoccerData$home_score[france])
not_fr_mean <- mean(SoccerData$home_score[!france])
obs_diff <- not_fr_mean - fr_mean
pval <- mean((diffs >= obs_diff) | (diffs <= -obs_diff))
pval

