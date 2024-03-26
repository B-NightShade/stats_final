summary(SoccerData)
View(SoccerData)

unique(SoccerData$home_team)
unique(SoccerData$tournament)

library("dplyr")
library(ggplot2)

ht <- SoccerData %>%
  count(home_team)

print(ht, n=nrow(ht))

summary(SoccerData$home_score)
sd(SoccerData$home_score)
ggplot(SoccerData, aes(x = home_score))+
  geom_histogram(binwidth = 1)


popular <- SoccerData %>%
  filter(home_team == "England"  | home_team == "Argentina" 
         | home_team == "France" | home_team == "Germany" )
ggplot(popular, aes(x=home_team, y=home_score))+
  geom_boxplot()
ggplot(popular, aes(x=home_score))+
  geom_histogram(binwidth = 1)+
  facet_grid(~home_team)

table_pop <- table(popular$home_team)
prop.table(table_pop)

at <- SoccerData %>%
  count(away_team)

print(at, n=nrow(at))

summary(SoccerData$away_score)
sd(SoccerData$away_score)
ggplot(SoccerData, aes(x = away_score))+
  geom_histogram(binwidth = 1)


four <- SoccerData %>%
  filter(away_team == "Nepal"  | away_team == "Palestine" 
         | away_team == "France" | away_team == "Germany")
ggplot(four, aes(x=away_team, y=away_score))+
  geom_boxplot()
ggplot(four, aes(x=away_score))+
  geom_histogram(binwidth = 1)+
  facet_grid(~away_team)

table_four <- table(four$away_team)
prop.table(table_four)

type2 <- SoccerData %>%
  count(home_team) 
type <- SoccerData %>%
  count(tournament)
View(type2)
print(type, n=nrow(type))

table_tourn <- table(popular$tournament)
prop.table(table_tourn)


table_t <- table(four$tournament)
prop.table(table_t)



tourn <- popular %>%
  filter((tournament == "Friendly") | (tournament == "FIFA World Cup") | 
           (tournament == "UEFA Euro"))

ggplot(fifa, aes(fill = home_team, y = home_score, x = date))+
  geom_bar(position = "stack", stat="identity")

ggplot(tourn, aes(y = home_score, x = tournament, fill = home_team))+
  geom_bar(stat="identity")


table_test <- table(tourn$tournament, tourn$home_team)
mosaicplot(table_test, color=TRUE)

away_tourn <- four %>%
  filter((tournament == "Friendly") | (tournament == "FIFA World Cup") | 
           (tournament == "UEFA Euro"))

relationship <- table(away_tourn$tournament, away_tourn$away_team)
mosaicplot(relationship, color=TRUE)


three_t <- SoccerData %>%
  filter((tournament == "Friendly") | (tournament == "FIFA World Cup") | 
           (tournament == "UEFA Euro"))

ggplot(three_t, aes(x=tournament, y=home_score))+
  geom_boxplot()

ggplot(three_t, aes(x=tournament, y=away_score))+
  geom_boxplot()

FIFA <- SoccerData %>%
  filter(tournament ==  "Three Nations Cup")
ggplot(FIFA, aes(x=tournament, y=home_score))+
  geom_boxplot()


ran_t <- SoccerData %>%
  filter((tournament == "Three Nations Cup") | (tournament == "Oceania Nations Cup") | 
           (tournament == "COSAFA Cup" ))

ggplot(ran_t, aes(x=tournament, y=home_score))+
  geom_boxplot()

ggplot(ran_t, aes(x=tournament, y=away_score))+
  geom_boxplot()

