library(tidyverse)
library(ggplot2)
library(mgcv)
library(modelr)
library(broom)


battedballs <- read_csv("batted_balls.csv") #batted balls file
weights <- read_csv("woba_weights") #2022 linear weights file


#Clean, explore data
glimpse(battedballs) #variables, types

battedballs <- subset(battedballs,select=-c(PitchUID,BatterID_TM,Bearing,
                                            ContactPositionX,ContactPositionY,ContactPositionZ)) #remove unnecessary variables

battedballs <- battedballs %>% #Replace "NULL" values with NA
  mutate(TaggedPitchType=replace(TaggedPitchType,TaggedPitchType=="NULL",NA),
         TaggedHitType=replace(TaggedHitType,TaggedHitType=="NULL",NA),
         PlayResult=replace(PlayResult,PlayResult=="NULL",NA),
         ExitSpeed=replace(ExitSpeed,ExitSpeed=="NULL",NA),
         Angle=replace(Angle,Angle=="NULL",NA),
         Direction=replace(Direction,Direction=="NULL",NA),
         HitSpinRate=replace(HitSpinRate,HitSpinRate=="NULL",NA),
         HitSpinAxis=replace(HitSpinAxis,HitSpinAxis=="NULL",NA),
         Distance=replace(Distance,Distance=="NULL",NA),
         HangTime=replace(HangTime,HangTime=="NULL",NA))

bip <- battedballs %>%
  filter(!is.na(TaggedHitType)) %>%
  mutate(ExitSpeed = round(ExitSpeed))

q <- quantile(bip$ExitSpeed, probs=c(.25,.75), na.rm = FALSE) #remove outliers for exit speeds
iqr <- IQR(bip$ExitSpeed)
up <- q[2] + 1.5*iqr
low <- q[1] - 1.5*iqr
bip <- subset(bip, ExitSpeed > low & ExitSpeed < up)

boxplot(bip$ExitSpeed)

#Calculate Exit Velocity Stats
bip <- bip %>% #detect "barrels", defined as at least 98 mph, at which the angle is 26-30*, with angle range increasing for every increase in velo mph
  mutate(Barrel = case_when((ExitSpeed == 98 & Angle >= 26 & Angle <= 30) ~ 1,
                             (ExitSpeed == 99 & Angle >= 25 & Angle <= 31) ~ 1,
                             (ExitSpeed == 100 & Angle >= 24 & Angle <= 33) ~ 1,
                             (ExitSpeed == 101 & Angle >= 23 & Angle <= 34) ~ 1,
                             (ExitSpeed == 102 & Angle >= 22 & Angle <= 36) ~ 1,
                             (ExitSpeed == 103 & Angle >= 21 & Angle <= 37) ~ 1,
                             (ExitSpeed == 104 & Angle >= 20 & Angle <= 38) ~ 1,
                             (ExitSpeed == 105 & Angle >= 19 & Angle <= 39) ~ 1,
                             (ExitSpeed == 106 & Angle >= 18 & Angle <= 40) ~ 1,
                             (ExitSpeed == 107 & Angle >= 17 & Angle <= 41) ~ 1,
                             (ExitSpeed == 108 & Angle >= 16 & Angle <= 42) ~ 1,
                             (ExitSpeed == 109 & Angle >= 15 & Angle <= 43) ~ 1,
                             (ExitSpeed == 110 & Angle >= 14 & Angle <= 44) ~ 1,
                             (ExitSpeed == 111 & Angle >= 13 & Angle <= 45) ~ 1,
                             (ExitSpeed == 112 & Angle >= 12 & Angle <= 46) ~ 1,
                             (ExitSpeed == 113 & Angle >= 11 & Angle <= 47) ~ 1,
                             (ExitSpeed == 114 & Angle >= 10 & Angle <= 48) ~ 1,
                             (ExitSpeed == 115 & Angle >= 9 & Angle <= 49) ~ 1,
                             (ExitSpeed >= 116 & Angle >= 8 & Angle <= 50) ~ 1,
                             TRUE ~ 0))



bip <- bip %>% #give each outcome of the at-bat a woba weight
  mutate(woba_bip = case_when((PlayResult == "Out") ~ 0,
                          (PlayResult == "FieldersChoice") ~ 0,
                          (PlayResult == "Sacrifice") ~ 0,
                          (PlayResult == "Single") ~ weights$wt_1B[1],
                          (PlayResult == "Double") ~ weights$wt_2B[1],
                          (PlayResult == "Triple") ~ weights$wt_3B[1],
                          (PlayResult == "HomeRun") ~ weights$wt_HR[1],
                          TRUE ~ NA_real_))

bip <- bip %>% #optimal angle, where a fitting angle is dependent on the EV of the hit 
  mutate(OptimalAngle = case_when((ExitSpeed >= 60 & ExitSpeed < 75 & Angle >= 18 & Angle < 28) ~ 1,
                            (ExitSpeed >= 75 & ExitSpeed < 95 & Angle >= 10 & Angle < 18) ~ 1,
                            (ExitSpeed >= 95 & Angle >= 8 & Angle <= 32) ~ 1,
                            TRUE ~ 0))

#figure out weighting for each distribution (balls hit 95+ mph should be weighted more than balls hit 70, 80, etc.)

##Optimal Angle
# Soft hit balls: 18 and 28 degrees .804
softHitsInRange <- (bip %>% filter(ExitSpeed >= 60 & ExitSpeed < 75 & Angle >= 18 & Angle < 28, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
softHitsOutRange <- (bip %>% filter(Angle < 18 | Angle >= 28, ExitSpeed >= 60 & ExitSpeed < 75, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
softHitsRangeDiff = softHitsInRange - softHitsOutRange #.612

# Medium hit balls: 10 and 17 degrees .812
mediumHitsInRange <- (bip %>% filter(ExitSpeed >= 75 & ExitSpeed < 95 & Angle >= 10 & Angle < 17, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
mediumHitsOutRange <- (bip %>% filter(Angle < 10 | Angle >= 17, ExitSpeed >= 75 & ExitSpeed < 95, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
mediumHitsRangeDiff = mediumHitsInRange - mediumHitsOutRange #.577

# Hard hit balls: 8 and 32 degrees 1.04
hardHitsInRange <- (bip %>% filter(ExitSpeed >= 95 & Angle >= 8 & Angle < 32, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
hardHitsOutRange <- (bip %>% filter(Angle < 8 | Angle >= 32, ExitSpeed >= 95, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
hardHitsRangeDiff = hardHitsInRange - hardHitsOutRange #.427

##Attempt at trying to weigh different hit balls, was not successful
softHitsInRange2 <- (bip %>% filter(ExitSpeed < 99 & Angle >= 10 & Angle < 24, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
softHitsOutRange2 <- (bip %>% filter(Angle < 10 | Angle > 24, ExitSpeed < 99, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
softHitsRangeDiff2 = softHitsInRange2 - softHitsOutRange2 #.388

hardHitsInRange2 <- (bip %>% filter(ExitSpeed >= 99 & Angle >= 20 & Angle <= 35, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
hardHitsOutRange2 <- (bip %>% filter(Angle < 20 | Angle > 35, ExitSpeed >= 99, !is.na(woba_bip)) %>% summarise(woba = mean(woba_bip)))$woba[1]
hardHitsRangeDiff2 = hardHitsInRange2 - hardHitsOutRange2 #.680



## BATTED BALLS (EACH WITH INDIVIDUAL WOBA VALUES) VISUALS - DISTRIBUTION GRAPHS
runvalueballs <- bip %>%
  filter(!is.na(woba_bip),
         Angle >= -30 & Angle <= 50,
         ExitSpeed >= 30 & ExitSpeed <= 120) %>%
  mutate(EVgroup = case_when((ExitSpeed >= 60 & ExitSpeed < 75 ) ~ "60-75",
                              (ExitSpeed >= 75 & ExitSpeed < 95) ~ "75-95",
                              (ExitSpeed >= 95) ~ "95+"))


LASummary <- runvalueballs %>%
  group_by(round(Angle)) %>%
  summarise(wOBA = mean(woba_bip))

LASummary2 <- filter(runvalueballs, !is.na(EVgroup)) %>%
  group_by(round(Angle),EVgroup) %>%
  summarise(wOBA = mean(woba_bip))

EVSummary <- runvalueballs %>%
  group_by(round(ExitSpeed)) %>%
  summarise(wOBA = mean(woba_bip))

ggplot(EVSummary, aes(x = `round(ExitSpeed)`, y = wOBA)) + # Exit Velo scatterplot
  geom_point() +
  geom_hline(yintercept=.372)

ggplot(LASummary, aes(x = `round(Angle)`, y = wOBA)) + # Launch angle line line
  geom_line() +
  geom_vline(xintercept = 8, color = "red") +
  geom_vline(xintercept = 32,  color = "red") + 
  xlab("Launch Angle") +
  ylab("wOBAcon") +
  labs(title = "wOBAcon Based on Launch Angle", subtitle = "Balls hit between -30° and 50° (Years 2019 - 2022)") +
  theme_classic()

ggplot(LASummary2, aes(x = `round(Angle)`, y = wOBA, color = EVgroup)) + # Launch angle split by 2 groups
  geom_line() +
  geom_vline(xintercept = 8) +
  geom_vline(xintercept = 32) +
  xlab("Launch Angle") +
  ylab("wOBAcon") +
  labs(title = "wOBAcon Based on Launch Angle and Exit Velocity", subtitle = "Balls hit between -30° and 50° (Years 2019 - 2022)") +
  scale_color_discrete(name = "Exit Velocity Group (mph)")+
  theme_classic()


boxplot(bip$ExitSpeed)

#Getting into player-wide data
players <- bip %>% #summarize batted ball rates
  group_by(team_name,player_name) %>%
  summarise(AvgEV = round(mean(ExitSpeed),2),
            MedEV = median(ExitSpeed),
            Perc75 = quantile(ExitSpeed,probs=.75),
            Perc85 = quantile(ExitSpeed,probs=.85),
            Perc95 = quantile(ExitSpeed,probs=.95),
            MaxEV = max(ExitSpeed),
            HardHitRate = round(sum(ExitSpeed >= 95)/n(),2),
            BarrelRate = round(sum(Barrel)/n(),2),
            Barrel75 = round(sum(Barrel75)/n(),2),
            SweetSpotRate = round(sum(Angle >= 8 & Angle<=32)/n(),2),
            wOBAconTrackedBalls = mean(woba_bip),
            OptimalAngleRate = round(sum(OptimalAngle)/n(),2),
            Sample = n())


#Filtered data

#check different percentiles
quantile(ThirdQuartStat$Perc75,probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
quantile(players$Sample,probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
quantile(bip$ExitSpeed,probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
quantile(players$BarrelRate,probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))


#edit player names
players$player_name <- sub(" Jr.","",players$player_name)
players$player_name <- sub(" III","",players$player_name)
players$player_name <- sub(" II","",players$player_name)

#Combine batted ball and wOBA stats
woba <- read_csv("2022_woba_stats.csv")
woba$Player <- sub(" Jr.","",woba$Player)
woba$Player <- sub(" III","",woba$Player)
woba$Player <- sub(" II","",woba$Player)
woba$Player <- sub("(\\w+),\\s(\\w+)","\\2 \\1", woba$Player)
quantile(wobatest$AB,probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) #see percentiles of AB size


#Combine batted ball player data frame with woba stats

combine <- left_join(players, woba, by = c("player_name"="Player"))

filtercombined <- combine %>% #633 players -> 538
  filter(!is.na(wOBA),
         AB >= 90, # ~50th percentile 
         Sample >= 30) # around 65th percentile

##OUTPUTTING LINEAR REGRESSIONS IN CHARTS
#wOBA
hitmetrics <- c("AvgEV","MedEV","Perc75","Perc95","MaxEV","HardHitRate","BarrelRate","SweetSpotRate")

rsquared <- c()
#wOBA
for(i in 1:length(hitmetrics)){
  model <- lm(formula(paste("wOBA", "~", hitmetrics[i])), filtercombined)
  value <- summary(model)$adj.r.squared
  rsquared <- append(rsquared,value)
}

wOBARegression <- data.frame(
  'Hitting Metric' = hitmetrics,
  'Adj.R-Squared' = round(rsquared,2)
)

#wOBAcon
rsquared2 <- c()
for(i in 1:length(hitmetrics)){
  model <- lm(formula(paste("wOBAcon", "~", hitmetrics[i])), filtercombined)
  value <- summary(model)$adj.r.squared
  rsquared2 <- append(rsquared2,value)
}

wOBAconRegression <- data.frame(
  'Hitting Metric' = hitmetrics,
  'Adj.R-Squared' = round(rsquared2,2)
)

  #Optimal Angle linear models
angleModel <- lm(wOBAcon ~ OptimalAngleRate,filtercombined) #3 "buckets" unweighted - .317
summary(angleModel)
angleModel2 <- lm(wOBAcon ~ OptimalAngleRate2,filtercombined) #3 buckets weighted .1688, failure of weighing method
summary(angleModel2) 


#DIFFERENT PERCENTILE EVs (from 75% to 95%)

EVPercentiles <- function(df,woba) { #input batted ball data, woba stats
  rsquared2 <- c()
  percentiles <- c()
  
    for(i in 50:95) {
      summary <- df %>% #summarize batted ball rates
        group_by(team_name,player_name)%>%
        summarise(value = quantile(ExitSpeed,probs=as.numeric(paste(".",i,sep = ""))),
                  Sample = n()) %>%
        left_join(woba, by = c("player_name"="Player"))  %>% #join woba stats
        filter(!is.na(wOBA),
               AB >= 90, # ~50th percentile 
               Sample >= 30) # around 65th percentile
        
      model <- lm(wOBAcon ~ value, summary) #extract R^2 for EV
      value <- summary(model)$adj.r.squared
      
      rsquared2 <- append(rsquared2,value)
      percentiles <- append(percentiles,i)
      
    }
  
  PercRegression <- data.frame(
    'EV Percentile' = percentiles,
    'Adj.R-Squared' = round(rsquared2,4))

  PercRegression <<- PercRegression
}


EVPercentiles(bip,woba)

percmax <- subset(PercRegression, Adj.R.Squared == max(Adj.R.Squared)) #find highest correlation

ggplot(PercRegression, #visualize distribution of percentiles
       aes(x=EV.Percentile,y=Adj.R.Squared))+
  geom_line()+
  labs(title = "R-Squared Results between Exit Velocity Percentiles \n and wOBAcon in 2022",
       subtitle = "Minimum 90 at-bats and 30 balls-in-play",
       x = "Exit Velocity Percentile", y = "Adjusted R-Squared") + 
  theme_classic() +
  geom_point(data=percmax, color = "red") +
  geom_text(data = percmax, aes(label=paste("R^2 = ", round(Adj.R.Squared,3))), hjust = -.1, vjust = 0)


##Testing Different Models


Perc85OptimalModel <- gam(wOBAcon ~ s(Perc85,OptimalAngleRate), filtercombined, method="REML",family="gaussian") #.491 R-squared
summary(Perc85OptimalModel)

HardSweetModel <- gam(wOBAcon ~ s(HardHitRate,SweetSpotRate), filtercombined, method="REML",family="gaussian") #.453% R-squared
summary(HardSweetModel)

HardSweetModel <- lm(wOBAcon ~ s(HardHitRate,SweetSpotRate), filtercombined, method="REML",family="gaussian") #.453% R-squared
summary(HardSweetModel)

Perc95SweetModel2 <- gam(wOBAcon ~ s(Perc95,SweetSpotRate), filtercombined, method="REML",family="gaussian") #.465 R-squared
summary(Perc95SweetModel2)
AvgEVSweetModel2 <- gam(wOBAcon ~ s(AvgEV,SweetSpotRate), filtercombined, method="REML",family="gaussian") #.376 R-squared
summary(AvgEVSweetModel2)

##VISUALS
#batted balls visuals
ggplot(filter(bip, PlayResult %in% c("Out","Single","Double","Triple","HomeRun")), #all batted balls
  aes(x = Angle, y = ExitSpeed, color = PlayResult)) +
  geom_point() +
  labs(title = "Launch Angle and Exit Velocity for Division 1 Batted Balls",
       subtitle = "(2019 - 2022)",
       color = "Play Result")+
  xlab("Launch Angle") +
  ylab("Exit Velocity") +
  scale_color_discrete(breaks = c("Out","Single","Double","Triple","HomeRun"),
                       labels = c("Out","Single","Double","Triple","Home Run"))


bip$bins <- cut(bip$Angle,breaks=seq(-20,50,5)) 

ggplot(filter(bip, PlayResult %in% c("Out","Single","Double","Triple","HomeRun"), !is.na(bins)), #proportional graph for angles
       aes(x = bins, fill = factor(PlayResult, levels = c("HomeRun","Triple","Double","Single","Out"))))+
  geom_bar(position="fill") + 
  labs(title = "Launch Angle by Play Result") +
  xlab("Launch Angle (bins of 5°)") +
  ylab("Proportion") +
  scale_fill_discrete(name = "Play Result",
                      breaks = c("Out","Single","Double","Triple","HomeRun"),
                      labels = c("Out","Single","Double","Triple","Home Run")) +
  theme_classic()

##Calculate wOBA by bin
wOBALaunch <- bip %>%
  group_by(bins, PlayResult) %>%
  summarise(count = n())
  

#GAM Model showing predicted wOBAcon based on 85th percentile and sweet spot rate

  #first showing nonlinear nature of both variables
ggplot(filtercombined, aes(x = Perc85, y = wOBAcon)) + 
  geom_point() +
  geom_smooth(stat= "smooth")

ggplot(filtercombined, aes(x = OptimalAngleRate, y = wOBAcon)) + 
  geom_point() +
  geom_smooth(stat= "smooth")

Perc85OptimalModel <- gam(wOBAcon ~ s(Perc85, OptimalAngleRate), filtercombined, method="REML",family="gaussian") #.491 R-squared
summary(Perc85OptimalModel)

model_predict <- predict.gam(Perc85OptimalModel)
names(model_predict)

grid_hats %>%
  ggplot(aes(Perc85,OptimalAngleRate, fill = .fitted)) +
  labs(title = "Predicted wOBAcon by Optimal Angle Rate \n and 85th Percentile Exit Velocity",
       subtitle = "(General Additive Model)") +
  xlab("EV 85 (in mph)") +
  ylab("Optimal Angle Rate") +
  geom_tile() +
  scale_fill_gradient(name = "wOBAcon", low = "gray", high = "blue") +
  theme_classic()
  
predictions <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(1, 2, 3, 4, 5),
  .fitted = c(0.2, 0.4, 0.6, 0.8, 1.0)
)

# Create the ggplot object
ggplot(predictions, aes(x = x, y = y, fill = .fitted)) +
  geom_tile() +  # Use tiles to create the rectangular grid
  scale_fill_gradient(low = "blue", high = "red") +  # Gradient color scheme
  labs(fill = "Predicted Value")  # Label for the color legend

plot <- ggplot(NULL,aes(x=Perc85,y=OptimalAngleRate)) +
  geom_rect(xmin= 88,
            xmax = 95,
            ymin=.06,
            ymax= .44,color="blue",alpha=0) +
  scale_x_continuous("85th EV Percentile", limits = c(83,100)) +
  scale_y_continuous("Optimal Angle Rate", limits = c(0,.5))

grid <- subset(filtercombined, select = -c(team_name,player_name)) %>%
  modelr::data_grid(Perc85 = seq_range(Perc85, n = 100),
                    OptimalAngleRate = seq_range(OptimalAngleRate, n = 100))


grid_hats <- Perc85OptimalModel %>%
  broom::augment(type.predict = "response", newdata = grid)

GamVisual <- plot %+% grid_hats +
  ggtitle("Predicted wOBAcon by Battic Metrics") +
  geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray", high = "blue")


ggplot(grid_hats,aes(x=Perc85,y=OptimalAngleRate)) +
  geom_rect(xmin= 88,
            xmax = 95,
            ymin=.06,
            ymax= .44,color="blue",alpha=0) +
  coord_equal()+
  scale_x_continuous("85th EV Percentile", limits = c(83,100)) +
  scale_y_continuous("Optimal Angle Rate", limits = c(0,.5)) +
  geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray", high = "blue")















