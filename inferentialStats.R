summary(SoccerData)
View(SoccerData)

library(dplyr)
library(ggplot2)

if_tourn <- SoccerData %>%
  filter((tournament == "COSAFA Cup") | (tournament == "FIFA World Cup"))

check <- SoccerData %>%
  filter(tournament == "COSAFA Cup")
hscore <- check$home_score

quantile(hscore)
2+3*IQR(hscore)
max(hscore)

fifa <- SoccerData %>%
  filter(tournament == "FIFA World Cup")
score <- fifa$home_score

2+3*IQR(score)
max(score)

max(if_tourn$home_score)


ggplot(if_tourn, aes(tournament,home_score))+
  geom_boxplot()

tapply(if_tourn$home_score, if_tourn$tournament, mean)
tapply(if_tourn$home_score, if_tourn$tournament, length)

ggplot(data = if_tourn, aes(x = home_score)) +
  geom_histogram(bins=3) +
  facet_grid(~tournament)

ggplot(data = if_tourn, aes(sample = home_score)) + 
  stat_qq()+
  stat_qq_line() +
  facet_grid(~tournament)

set.seed(22)                                      
num_sim <- 1000
diffs <- numeric(num_sim)                         
for(i in 1:num_sim){
  ocean <- sample(1:320,281)                          
  o_mean <- mean(if_tourn$home_score[ocean])      
  not_o_mean <- mean(if_tourn$home_score[-ocean]) 
  diffs[i] <- not_o_mean - o_mean           
}

o_mean <- mean(if_tourn$home_score[if_tourn$tournament == "COSAFA Cup"])
not_o_mean <- mean(if_tourn$home_score[if_tourn$tournament != "COSAFA Cup"])
obs_diff <- not_o_mean - o_mean
pval <- mean((diffs >= obs_diff) | (diffs <= -obs_diff))
pval


t.test(home_score~if_tourn$tournament == "COSAFA Cup", data=if_tourn)

