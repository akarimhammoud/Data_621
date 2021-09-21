library(mice)
library(here)
library(dplyr)

# load data
data <- read.csv(here('data','hw1_train_data.csv'))

#imput data by regression: 
data_imp <- mice(data, method = "norm.predict", m = 1)

#complete data 
data_complete <- complete(data_imp)

new_data <- data_complete %>% 
  rowwise() %>%
  mutate(TEAM_BATTING_AB = sum( TEAM_BATTING_H,TEAM_BATTING_BB,TEAM_BATTING_SO, na.rm=TRUE),
  TEAM_BATTING_OB = sum( TEAM_BATTING_H,TEAM_BATTING_BB, na.rm=TRUE),
  TEAM_BATTING_OBP = TEAM_BATTING_OB/TEAM_BATTING_AB)

new_data <- as.data.frame(new_data)
train1 <- new_data %>% dplyr::sample_frac(.75)
test1  <- dplyr::anti_join(new_data, train1, by = 'X')

cases = dim(train1)[1]
features = dim(train1)[2]
cat('Training data for this project is', cases, 'cases and', features, 'features')

### First Model 
#Select the desired data for the model
rmdata <- train1 %>%
  select(TEAM_BATTING_OBP, TEAM_PITCHING_HR, TEAM_BATTING_SO, TEAM_PITCHING_SO, TEAM_FIELDING_E, TEAM_FIELDING_DP, TARGET_WINS)

#Build the third model and produce a summary
GModel1 <- lm(TARGET_WINS ~ TEAM_BATTING_OBP + TEAM_PITCHING_HR + TEAM_BATTING_SO + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data = rmdata)
summary(GModel1)

#Predict with the second model training data
GModel1_predictions = predict(GModel1,test1)

#Evaluate the second model results using RMSE
rmse(test1$TARGET_WINS, GModel1_predictions)

### Second Model: Total Bases Plus
#In baseball statistics, total bases refers to the number of bases a player has gained with hits, 
#i.e. the sum of his/her hits weighted by 1 for a single, 2 for a double, 3 for a triple and 4 for a home run.

#Only bases attained from hits count toward this total. Total bases can be calculated from commonly used 
#baseball statistics by using the formula {\displaystyle TB=1B+2x2B+3x3B+4xHR}. Singles (1B) can be represented as 
#{\displaystyle 1B=H-2B-3B-HR} which, when combined with the given TB formula, allows for the reduced formula found 
#at the top of the page.


data2 <- data_complete %>% 
  rowwise() %>%
  mutate(TEAM_BATTING_1B = TEAM_BATTING_H - (TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR),
         TEAM_BATTING_TB = TEAM_BATTING_1B + (2 * TEAM_BATTING_2B) + (3 * TEAM_BATTING_3B) + (4 * TEAM_BATTING_HR))

data2 <- as.data.frame(data2)
train2 <- data2 %>% dplyr::sample_frac(.75)
test2  <- dplyr::anti_join(data2, train2, by = 'X')

### Second Model 
#Select the desired data for the model
#All Offensive Stats
rmdata2 <- train2 %>%
  select(TEAM_BATTING_TB, TEAM_BATTING_BB, TEAM_BATTING_SO, TEAM_BASERUN_SB, TARGET_WINS)

#Build the second model and produce a summary
GModel2 <- lm(TARGET_WINS ~ TEAM_BATTING_TB + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB, data = rmdata2)
summary(GModel2)

#Predict with the second model training data
GModel2_predictions = predict(GModel2,test2)

#Evaluate the second model results using RMSE
rmse(test2$TARGET_WINS, GModel2_predictions)

### BSR Model (SaberMetrics)
#Base runs (BsR) is a baseball statistic invented by sabermetrician David Smyth to estimate the number of runs a team "should have" 
#scored given their component offensive statistics, as well as the number of runs a hitter or pitcher creates or allows. 
#It measures essentially the same thing as Bill James runs created, but as sabermetrician Tom M. Tango points out, base 
#runs models the reality of the run-scoring process "significantly better than any other run estimator".

# The simplest, uses only the most common batting statistics[2]

# A = H + BB - HR
# B = (1.4 * TB - .6 * H - 3 * HR + .1 * BB) * 1.02
# C = AB - H
# D = HR


data3 <- data_complete %>% 
  rowwise() %>%
  mutate(TEAM_BATTING_AB = sum( TEAM_BATTING_H,TEAM_BATTING_BB,TEAM_BATTING_SO, na.rm=TRUE),
         TEAM_BATTING_1B = TEAM_BATTING_H - (TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR),
         TEAM_BATTING_TB = TEAM_BATTING_1B + (2 * TEAM_BATTING_2B) + (3 * TEAM_BATTING_3B) + (4 * TEAM_BATTING_HR),
         BSR_A = TEAM_BATTING_H + TEAM_BATTING_BB - TEAM_BATTING_HR,
         BSR_B = (( 1.4 * TEAM_BATTING_TB) - ( 0.6 * TEAM_BATTING_H) - (3 * TEAM_BATTING_HR) + (0.1 * TEAM_BATTING_BB)) * 1.02,
         BSR_C = TEAM_BATTING_AB - TEAM_BATTING_H,
         BSR = ((BSR_A*BSR_B)/(BSR_B + BSR_C)) + TEAM_BATTING_HR
         )

data3 <- as.data.frame(data3)
train3 <- data3 %>% dplyr::sample_frac(.75)
test3  <- dplyr::anti_join(data3, train3, by = 'X')

### Third Model 
#BSR
rmdata3 <- train3 %>%
  select(BSR, TEAM_PITCHING_SO, TEAM_FIELDING_E, TEAM_FIELDING_DP, TARGET_WINS)

#Build the second model and produce a summary
GModel3 <- lm(TARGET_WINS ~ BSR + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data = rmdata3)
summary(GModel3)

#Predict with the second model training data
GModel3_predictions = predict(GModel3,test3)

#Evaluate the second model results using RMSE
rmse(test3$TARGET_WINS, GModel3_predictions)

### All Three Models 

summary(GModel1)

#Evaluate the second model results using RMSE
rmse(test1$TARGET_WINS, GModel1_predictions)

summary(GModel2)

#Evaluate the second model results using RMSE
rmse(test2$TARGET_WINS, GModel2_predictions)

summary(GModel3)

#Evaluate the third model results using RMSE
rmse(test3$TARGET_WINS, GModel3_predictions)
