# setting directory
setwd('/Users/jinyuntae/Desktop/Team_project/industrial_statistics_university_project')
df = read.csv('data/Challenger_Ranked_Games.csv', header = T)
df = data.frame(df)
df

# select sample of df
temp_sample_num = sample(nrow(df), 200)
temp_sample_num

sample_df = df[temp_sample_num,]
sample_df

# separate blue and red team
blue_sample = sample_df[c('gameId', 'gameDuraton', 'blueWins', 'blueFirstBlood', 'blueFirstTower', 'blueFirstBaron', 'blueFirstDragon', 'blueFirstInhibitor', 'blueDragonKills', 'blueBaronKills', 'blueTowerKills', 'blueInhibitorKills', 'blueWardPlaced', 'blueWardkills', 'blueKills', 'blueDeath', 'blueAssist', 'blueChampionDamageDealt', 'blueTotalGold', 'blueTotalMinionKills', 'blueTotalLevel', 'blueAvgLevel', 'blueJungleMinionKills', 'blueKillingSpree', 'blueTotalHeal', 'blueObjectDamageDealt')]
red_sample = sample_df[c('gameId', 'gameDuraton', 'redWins', 'redFirstBlood', 'redFirstTower', 'redFirstBaron', 'redFirstDragon', 'redFirstInhibitor', 'redDragonKills', 'redBaronKills', 'redTowerKills', 'redInhibitorKills', 'redWardPlaced', 'redWardkills', 'redKills', 'redDeath', 'redAssist', 'redChampionDamageDealt', 'redTotalGold', 'redTotalMinionKills', 'redTotalLevel', 'redAvgLevel', 'redJungleMinionKills', 'redKillingSpree', 'redTotalHeal', 'redObjectDamageDealt')]

blue_sample
red_sample

# analysis 1-1 => logistic regression (categorical and numeric value)
reg_blue_result <- glm(blueWins ~ blueFirstBlood+blueFirstTower+blueFirstInhibitor+blueFirstDragon+blueFirstBaron+blueFirstInhibitor+blueDragonKills+blueBaronKills+blueTowerKills+blueInhibitorKills, data=blue_sample, family = "binomial")
summary(reg_blue_result) #except gameDuraton
reg_red_result <- glm(redWins ~ redFirstBlood+redFirstTower+redFirstInhibitor+redFirstDragon+redFirstBaron+redFirstInhibitor+redDragonKills+redBaronKills+redTowerKills+redInhibitorKills, data=red_sample, family = "binomial")
summary(reg_red_result) # except gameDuraton

step(reg_blue_result, direction = "backward")
step(reg_red_result)

