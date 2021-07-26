chart_col_1 <- "#008B61"
chart_col_2 <- "#BA328B"
chart_back <- "#DBDBD3"
options(digits = 3)

# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("stringer", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("adabag", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readr)
library(lubridate)
library(RCurl)
library(caret)
library(stringr)
library(rpart)
library(rpart.plot)
library(mice)
library(VIM)
library(randomForest)
library(adabag)
library(gbm)
library(e1071)
library(gridExtra)
library(grid)

# Download data from GitHub
url <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/accidents.csv")
accidents <- read.csv(text = url)
url <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/casualties.csv")
casualties <- read.csv(text = url)
url <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/vehicles.csv")
vehicles <- read.csv(text = url)

#Join three datasets into one
full_data <- casualties %>% left_join(vehicles, by = c("Accident_Index", "Vehicle_Reference")) %>% 
  left_join(accidents, by = "Accident_Index")

### Change casualty severity to 2 levels
full_data <- full_data %>% 
  mutate(Casualty_Severity = as.factor(ifelse(Casualty_Severity == 3, 0, 1))) 

# Replace all the missing data values with NAs (includes unknown, not known etc)
full_data[full_data == -1] <- NA
full_data$Sex_of_Driver <- na_if(full_data$Sex_of_Driver,3)
full_data$Sex_of_Driver_B <- na_if(full_data$Sex_of_Driver_B,3)
full_data$Road_Type <- na_if(full_data$Road_Type,9)
full_data$Weather_Conditions <- na_if(full_data$Weather_Conditions, 8)
full_data$Weather_Conditions <- na_if(full_data$Weather_Conditions, 9)

## Split into train and test sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = full_data$Casualty_Severity, times = 1, p = 0.1, list = FALSE)
train_set <- full_data[-test_index,]
test_set <- full_data[test_index,]

# Show basic info about each variable
data.frame(Type = sapply(train_set, class), 
           Distinct_values = sapply(train_set, n_distinct), 
           Missing_values = sapply(train_set, function(x) sum(is.na(x))))

# Show the imbalance in the target variable
train_set %>% group_by(Casualty_Severity) %>% summarise(count = n())

# Chart to show number of drivers who aren't legally allowed to drive
train_set %>% ggplot(aes(x = Age_of_Driver)) + geom_bar(fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(0,25,5)) +
  xlim(1,26) +
  labs(x = "Age of driver", y = "Number of casualties")

# If driver age is below 16, then set to 16
train_set$Age_of_Driver <- ifelse(train_set$Age_of_Driver < 16, 16, train_set$Age_of_Driver)
train_set$Age_of_Driver_B <- ifelse(train_set$Age_of_Driver_B < 16, 16, train_set$Age_of_Driver_B)

# Check distribution of casualties by date
chart1 <- train_set %>% mutate(weekno = week(Date)) %>%  ggplot(aes(x = weekno)) + 
  geom_bar(fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid")) +
  scale_x_continuous(breaks = seq(0,52,10)) +  
  labs(x = "Week number", y = "Number of casualties")

chart2 <- train_set %>% mutate(weekno = week(Date)) %>% group_by(weekno) %>% 
  summarise(accidents = n()) %>% 
  ggplot(aes(y = accidents)) + 
  geom_boxplot(fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text.x=element_blank()) +
  labs(x = "", y = "Number of casualties")

grid.arrange(chart1, chart2, ncol = 2)

# Check distribution of casualties by time of day
train_set %>% mutate(hour = hour(hms(Time))) %>%  ggplot(aes(x = hour)) + 
  geom_bar(fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(0,23,4)) +  
  labs(x = "Hour of the day", y = "Number of casualties")

# Relationship between casualty severity and speed limit
train_set %>% ggplot(aes(x = as.factor(Speed_limit))) + 
  geom_bar(aes(fill = Casualty_Severity), position = "fill")+
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)), 
        legend.text=element_text(size = rel(1.5)),  
        legend.title=element_text(size = rel(1.5))) + 
  labs(x = "Road speed limit (mph)", y = "Proportion of casualties")

# Relationship between casualty severity and driver age
train_set %>% ggplot(aes(x = Age_of_Driver)) + 
  geom_histogram(aes(fill = Casualty_Severity), position = "fill") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)), 
        legend.text=element_text(size = rel(1.5)),  
        legend.title=element_text(size = rel(1.5))) +
  labs(x = "Age of driver", y = "Proportion of casualties")

# Relationship between casualty severity and sex of driver
cont_table <- table(sex = train_set$Sex_of_Driver, severity = train_set$Casualty_Severity)
male <- cont_table[1,2] / sum(cont_table[1,])
female <- cont_table[2,2] / sum(cont_table[2,])

# Chi squared test to see if difference between sexes is statistically significant
chisq.test(cont_table)

# Use imputation to fill in missing data
train_set$Time <- as.numeric(hms(train_set$Time)) # Convert time to numeric to allow imputation
imp_init = mice(train_set, maxit=0) 
imp_meth = imp_init$method
imp_pred_matrix = imp_init$predictorMatrix
imp_pred_matrix[, c("Accident_Index", "Vehicle_Reference", "Casualty_Reference")]=0

set.seed(1)
imputed = mice(train_set, method=imp_meth, predictorMatrix=imp_pred_matrix, m=5)
train_set <- complete(imputed)

# Encoding time as cyclical variables
train_set <- train_set %>% mutate(Time_sin = sin((Time/(24*60*60)) * pi * 2), 
                                  Time_cos = cos((Time/(24*60*60)) * pi * 2),
                                  Dow_sin = sin(((wday(Date)-1) / 7) * pi * 2), 
                                  Dow_cos = cos(((wday(Date)-1) / 7) * pi * 2))

train_set %>% ggplot(aes(Time_sin, Time_cos)) + 
  geom_point(alpha = 0.002, fill = chart_col_1, colour = chart_col_1, size = 1) + 
  annotate(geom = "Text", x = sin(0 * pi * 2), y = cos(0 * pi * 2) - 0.1, label = "0") +
  annotate(geom = "Text", x = sin(0.25 * pi * 2) - 0.1, y = cos(0.25 * pi * 2) , label = "6") +
  annotate(geom = "Text", x = sin(0.5 * pi * 2), y = cos(0.5 * pi * 2) + 0.1, label = "12") +
  annotate(geom = "Text", x = sin(0.75 * pi * 2) + 0.1, y = cos(0.75 * pi * 2), label = "18") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.3, 
                                        linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),  axis.title.y = element_text(size = rel(1.5)))

# Encoding day of week as cyclical variables
train_set %>% ggplot(aes(Dow_sin, Dow_cos)) + 
  geom_point(alpha = 0.005, fill = chart_col_1, colour = chart_col_1, size = 5) + 
  annotate(geom = "Text", x = sin(0 * pi * 2), y = cos(0 * pi * 2) - 0.1, label = "Sun") +
  annotate(geom = "Text", x = sin(1/7 * pi * 2), y = cos(1/7 * pi * 2) - 0.1, label = "Mon") +
  annotate(geom = "Text", x = sin(2/7 * pi * 2), y = cos(2/7 * pi * 2) - 0.1, label = "Tue") +
  annotate(geom = "Text", x = sin(3/7 * pi * 2), y = cos(3/7 * pi * 2) - 0.1, label = "Wed") +
  annotate(geom = "Text", x = sin(4/7 * pi * 2), y = cos(4/7 * pi * 2) - 0.1, label = "Thu") +
  annotate(geom = "Text", x = sin(5/7 * pi * 2), y = cos(5/7 * pi * 2) - 0.1, label = "Fri") +
  annotate(geom = "Text", x = sin(6/7 * pi * 2), y = cos(6/7 * pi * 2) - 0.1, label = "Sat") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.3, 
                                        linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)))

# Change road class to combine motorways with A(M) roads as they are equivalent
train_set$Road_Class <- ifelse(train_set$Road_Class == 1, 1, train_set$Road_Class - 1)

# Encode light conditions
train_set$Daylight <- ifelse(train_set$Light_Conditions == 1, 1,0)
train_set$Street_Light <- ifelse(train_set$Light_Conditions == 4, 1, 0)

# Encode weather conditions
train_set$Raining <- ifelse(train_set$Weather_Conditions %in% c(2,5), 1,0)
train_set$Snowing <- ifelse(train_set$Weather_Conditions %in% c(3,6), 1, 0)
train_set$Foggy <- ifelse(train_set$Weather_Conditions == 7, 1, 0)
train_set$Windy <- ifelse(train_set$Weather_Conditions %in% c(4,5,6), 1, 0)

# Encode road type 
train_set$Road_Type <- as.factor(case_when(train_set$Road_Type == 1 ~ "aa_Roundabout",
                                           train_set$Road_Type == 2 ~ "One_way",
                                           train_set$Road_Type == 3 ~ "Dual_c",
                                           train_set$Road_Type == 6 ~ "Single_c",
                                           train_set$Road_Type == 7 ~ "Slip"))

dmy <- dummyVars("~Road_Type",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode road conditions 
train_set$Road_Surface_Conditions <- as.factor(case_when(
  train_set$Road_Surface_Conditions == 1 ~ "Road_Dry",
  train_set$Road_Surface_Conditions == 2 ~ "Road_Wet",
  train_set$Road_Surface_Conditions == 3 ~ "Road_Snowy",
  train_set$Road_Surface_Conditions == 4 ~ "Road_Icey",
  train_set$Road_Surface_Conditions == 5 ~ "Road_Flooded"))

dmy <- dummyVars("~Road_Surface_Conditions",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode junction detail
train_set$Junction_Detail <- as.factor(case_when(
  train_set$Junction_Detail == 0 ~ "aa_No_junc",
  train_set$Junction_Detail == 1 ~ "Junc_Roundabout",
  train_set$Junction_Detail == 2 ~ "Junc_Mini_roundabout",
  train_set$Junction_Detail == 3 ~ "Junc_T",
  train_set$Junction_Detail == 5 ~ "Junc_Slip",
  train_set$Junction_Detail == 6 ~ "Junc_Crossroads",
  train_set$Junction_Detail == 7 ~ "Junc_More_than_4",
  train_set$Junction_Detail == 8 ~ "Junc_Private_drive",
  train_set$Junction_Detail == 9 ~ "Junc_Other"))

dmy <- dummyVars("~Junction_Detail",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode vehicle type
train_set$Vehicle_Type <- as.factor(case_when(
  train_set$Vehicle_Type == 1 ~ "aa_pedal_bike",
  train_set$Vehicle_Type %in% c(2,3,4,5) ~ "Motorbike",
  train_set$Vehicle_Type %in% c(8,9) ~ "Car",
  train_set$Vehicle_Type == 19 ~ "LGV",
  train_set$Vehicle_Type %in% c(20,21) ~ "HGV"))

dmy <- dummyVars("~Vehicle_Type",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

train_set$Vehicle_Type_B <- as.factor(case_when(
  train_set$Vehicle_Type_B == 1 ~ "aa_pedal_bike_B",
  train_set$Vehicle_Type_B %in% c(2,3,4,5) ~ "Motorbike_B",
  train_set$Vehicle_Type_B %in% c(8,9) ~ "Car_B",
  train_set$Vehicle_Type_B == 19 ~ "LGV_B",
  train_set$Vehicle_Type_B %in% c(20,21) ~ "HGV_B"))

dmy <- dummyVars("~Vehicle_Type_B",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode vehicle manoeuvre
train_set$Vehicle_Manoeuvre <- as.factor(case_when(
  train_set$Vehicle_Manoeuvre == 1 ~ "Reversing",
  train_set$Vehicle_Manoeuvre == 2 ~ "Parked",
  train_set$Vehicle_Manoeuvre == 3 ~ "Held_up",
  train_set$Vehicle_Manoeuvre == 4 ~ "Stopping",
  train_set$Vehicle_Manoeuvre == 5 ~ "Moving_off",
  train_set$Vehicle_Manoeuvre == 6 ~ "U_turn",
  train_set$Vehicle_Manoeuvre == 7 ~ "Left_turn",
  train_set$Vehicle_Manoeuvre == 8 ~ "Wait_turn_left",
  train_set$Vehicle_Manoeuvre == 9 ~ "Right_turn",
  train_set$Vehicle_Manoeuvre == 10 ~ "Wait_turn_right",
  train_set$Vehicle_Manoeuvre == 11 ~ "Change_lane_left",
  train_set$Vehicle_Manoeuvre == 12 ~ "Change_lane_right",
  train_set$Vehicle_Manoeuvre == 13 ~ "Overtake_moving",
  train_set$Vehicle_Manoeuvre == 14 ~ "Overtake_static",
  train_set$Vehicle_Manoeuvre == 15 ~ "Overtake_nearside",
  train_set$Vehicle_Manoeuvre == 16 ~ "Left_bend",
  train_set$Vehicle_Manoeuvre == 17 ~ "Right_bend",
  train_set$Vehicle_Manoeuvre == 18 ~ "aa_Ahead_other"))

dmy <- dummyVars("~Vehicle_Manoeuvre",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

train_set$Vehicle_Manoeuvre_B <- as.factor(case_when(
  train_set$Vehicle_Manoeuvre_B == 1 ~ "Reversing_B",
  train_set$Vehicle_Manoeuvre_B == 2 ~ "Parked_B",
  train_set$Vehicle_Manoeuvre_B == 3 ~ "Held_up_B",
  train_set$Vehicle_Manoeuvre_B == 4 ~ "Stopping_B",
  train_set$Vehicle_Manoeuvre_B == 5 ~ "Moving_off_B",
  train_set$Vehicle_Manoeuvre_B == 6 ~ "U_turn_B",
  train_set$Vehicle_Manoeuvre_B == 7 ~ "Left_turn_B",
  train_set$Vehicle_Manoeuvre_B == 8 ~ "Wait_turn_left_B",
  train_set$Vehicle_Manoeuvre_B == 9 ~ "Right_turn_B",
  train_set$Vehicle_Manoeuvre_B == 10 ~ "Wait_turn_right_B",
  train_set$Vehicle_Manoeuvre_B == 11 ~ "Change_lane_left_B",
  train_set$Vehicle_Manoeuvre_B == 12 ~ "Change_lane_right_B",
  train_set$Vehicle_Manoeuvre_B == 13 ~ "Overtake_moving_B",
  train_set$Vehicle_Manoeuvre_B == 14 ~ "Overtake_static_B",
  train_set$Vehicle_Manoeuvre_B == 15 ~ "Overtake_nearside_B",
  train_set$Vehicle_Manoeuvre_B == 16 ~ "Left_bend_B",
  train_set$Vehicle_Manoeuvre_B == 17 ~ "Right_bend_B",
  train_set$Vehicle_Manoeuvre_B == 18 ~ "aa_Ahead_other_B"))

dmy <- dummyVars("~Vehicle_Manoeuvre_B",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode skidding and overturning

train_set$Skidded <- ifelse(train_set$Skidding_and_Overturning %in% c(1,2), 1, 0)
train_set$Skidded_B <- ifelse(train_set$Skidding_and_Overturning_B %in% c(1,2), 1, 0)

train_set$Overturned <- ifelse(train_set$Skidding_and_Overturning %in% c(2,4,5), 1, 0)
train_set$Overturned_B <- ifelse(train_set$Skidding_and_Overturning_B %in% c(2,4,5), 1, 0)

train_set$Jacknifed <- ifelse(train_set$Skidding_and_Overturning %in% c(3,4), 1, 0)
train_set$Jacknifed_B <- ifelse(train_set$Skidding_and_Overturning_B %in% c(3,4), 1, 0)

# Encode leaving carriageway

train_set$Straight <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(3), 1, 0)
train_set$Straight_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(3), 1, 0)

train_set$Offside <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(4,5,6,7,8), 1, 0)
train_set$Offside_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(4,5,6,7,8), 1, 0)

train_set$Nearside <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(1,2), 1, 0)
train_set$Nearside_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(1,2), 1, 0)

train_set$Rebound <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(2,5,8), 1, 0)
train_set$Rebound_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(2,5,8), 1,0)

train_set$Central_res <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(4,5), 1, 0)
train_set$Central_res_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(4,5), 1, 0)

train_set$Over_central <- ifelse(train_set$Vehicle_Leaving_Carriageway %in% c(6), 1, 0)
train_set$Over_central_B <- ifelse(train_set$Vehicle_Leaving_Carriageway_B %in% c(6), 1, 0)

# Encode hit object

train_set$Hit_object <- ifelse(train_set$Hit_Object_off_Carriageway == 0 , 0, 1)
train_set$Hit_object_B <- ifelse(train_set$Hit_Object_off_Carriageway_B == 0, 0, 1)

# Encode point of impact

train_set$First_Point_of_Impact <- as.factor(case_when(
  train_set$First_Point_of_Impact == 0 ~ "aa_no_impact",
  train_set$First_Point_of_Impact == 1 ~ "Impact_Front",
  train_set$First_Point_of_Impact == 2 ~ "Impact_Back",
  train_set$First_Point_of_Impact == 3 ~ "Impact_Offside",
  train_set$First_Point_of_Impact == 4 ~ "Impact_Nearside"))

dmy <- dummyVars("~First_Point_of_Impact",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

train_set$First_Point_of_Impact_B <- as.factor(case_when(
  train_set$First_Point_of_Impact_B == 0 ~ "aa_no_impact_B",
  train_set$First_Point_of_Impact_B == 1 ~ "Impact_Front_B",
  train_set$First_Point_of_Impact_B == 2 ~ "Impact_Back_B",
  train_set$First_Point_of_Impact_B == 3 ~ "Impact_Offside_B",
  train_set$First_Point_of_Impact_B == 4 ~ "Impact_Nearside_B"))

dmy <- dummyVars("~First_Point_of_Impact_B",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode sex of driver

train_set$Sex_of_Driver <- as.factor(ifelse(train_set$Sex_of_Driver == 1, "Male_driver",
                                            ifelse(train_set$Sex_of_Driver == 2, "Female_driver",NA)))

dmy <- dummyVars("~Sex_of_Driver",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

train_set$Sex_of_Driver_B <- as.factor(ifelse(train_set$Sex_of_Driver_B == 1, "Male_driver_B",
                                              ifelse(train_set$Sex_of_Driver_B == 2, "Female_driver_B",NA)))

dmy <- dummyVars("~Sex_of_Driver_B",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode combined casualty class and car passenger fields

train_set$Casualty_Location <- as.factor(
  ifelse(train_set$Casualty_Class == 1, "Driver",
         ifelse(train_set$Casualty_Class == 3, "aa_Pedestrian",
                ifelse(train_set$Car_Passenger == 1, "Front_Passenger","Rear_Passenger"))))

dmy <- dummyVars("~Casualty_Location",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

# Encode sex of casualty
train_set$Sex_of_Casualty <- as.factor(ifelse(train_set$Sex_of_Casualty == 1, "Male_cas",
                                              ifelse(train_set$Sex_of_Casualty == 2, "Female_cas",NA)))

dmy <- dummyVars("~Sex_of_Casualty",data=train_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=train_set))
train_set <- bind_cols(train_set, trfs)

### Change casualty severity to a factor
train_set$Casualty_Severity <- as.factor(train_set$Casualty_Severity)

# Remove original columns where we now have one hot encoding
train_set <- train_set %>% select(Age_of_Casualty, Casualty_Severity, Age_of_Driver, Age_of_Driver_B,
                                  Road_Class, Speed_limit, Time_sin:Male_driver_B, Driver:Male_cas)

# Split out validation set from training set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = train_set$Casualty_Severity, times = 1, p = 0.1, list = FALSE)
validate_set <- train_set[test_index,]
train_set <- train_set[-test_index,]

# run basic model
prob_slight <- sum(train_set$Casualty_Severity == 1) / nrow(train_set)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
guess_class <- data.frame(Casualty_Severity = as.factor(
  sample(c(0,1), size = nrow(validate_set), replace = TRUE, prob = c(1- prob_slight, prob_slight))))
cm <- confusionMatrix(data = guess_class$Casualty_Severity, reference = validate_set$Casualty_Severity, positive = "1")

# CM for basic model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion

# summary results for basic model
results <- data.frame(Method = "Basic guess", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                      Balanced = cm$byClass[11], Pos_pred = cm$byClass[3])
row.names(results) <- NULL
results

# run initial random forest
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
rf_fit <- randomForest(Casualty_Severity ~ ., data = train_set, ntree = 50)
rf_class <- predict(rf_fit, validate_set, type = "class")
cm <- confusionMatrix(data = rf_class, reference = validate_set$Casualty_Severity, positive = "1")

max(predict(rf_fit, validate_set, type = "prob")[,2])

# CM for initial random forest
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

# summary results for initial random forest
results_temp <- data.frame(Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                           Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]) 

row.names(results_temp) <- NULL
results_temp

### Create a balanced dataset
imbal <- sum(train_set$Casualty_Severity == 1) / sum(train_set$Casualty_Severity == 0)
train_sev_0 <- train_set %>% filter(Casualty_Severity == 0)
train_sev_1 <- train_set %>% filter(Casualty_Severity == 1)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
index <- createDataPartition(y = train_sev_0$Casualty_Severity, times = 1, p = imbal, list = FALSE)
train_sev_0 <- train_sev_0[index,]
train_set <- bind_rows(train_sev_0, train_sev_1)

### Run random forest with balanced training data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
rf_fit <- randomForest(Casualty_Severity ~ ., data = train_set)
rf_class <- predict(rf_fit, validate_set, type = "class")
cm <- confusionMatrix(data = rf_class, reference = validate_set$Casualty_Severity, positive = "1")

### Create a balanced dataset
imbal <- sum(train_set$Casualty_Severity == 1) / sum(train_set$Casualty_Severity == 0)
train_sev_0 <- train_set %>% filter(Casualty_Severity == 0)
train_sev_1 <- train_set %>% filter(Casualty_Severity == 1)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
index <- createDataPartition(y = train_sev_0$Casualty_Severity, times = 1, p = imbal, list = FALSE)
train_sev_0 <- train_sev_0[index,]
train_set <- bind_rows(train_sev_0, train_sev_1)

### Run random forest with balanced training data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
rf_fit <- randomForest(Casualty_Severity ~ ., data = train_set)
rf_class <- predict(rf_fit, validate_set, type = "class")
cm <- confusionMatrix(data = rf_class, reference = validate_set$Casualty_Severity, positive = "1")

#CM and summary results for random forest with balanced dataset
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results_temp <- data.frame(Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                           Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]) 

row.names(results_temp) <- NULL
results_temp 

# Tune the mtry parameter
control <- trainControl(method = "cv", number = 5, p = .9) ## This will be used for all cross validation
tunegrid <- expand.grid(.mtry=c(5:15) )
set.seed(1, sample.kind="Rounding")
rf_fit <- train(Casualty_Severity ~ ., data=train_set, method="rf", 
                metric="Accuracy", tuneGrid=tunegrid, trControl=control ,ntree=50)
rf_class <- predict(rf_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = rf_class, reference = validate_set$Casualty_Severity, positive = "1")

# CM and summary results for tuned random forest (all features)
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion

results_temp <- data.frame(Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                           Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]) 

row.names(results_temp) <- NULL
results_temp 

# Identify all variables that are at least 10% of the importance of the most important feature
key_vars <- row.names(as.data.frame(rf_fit$finalModel$importance[rf_fit$finalModel$importance > max(rf_fit$finalModel$importance) * 0.1,]))

# Show these variables
data.frame(Importance = 1:length(key_vars), Feature = key_vars)  

# Remove features from training and validation set that are not on that list
train_set <- train_set %>% select("Casualty_Severity", key_vars)
validate_set <- validate_set %>% select("Casualty_Severity", key_vars)

# Tune random forest again, now with smaller set of features
tunegrid <- expand.grid(.mtry=c(5:15) )
set.seed(1, sample.kind="Rounding")
rf_fit <- train(Casualty_Severity ~ ., data=train_set, method="rf", 
                metric="Accuracy", tuneGrid=tunegrid, trControl=control ,ntree=50)
rf_class <- predict(rf_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = rf_class, reference = validate_set$Casualty_Severity, positive = "1")

class_pred <- data.frame(rf = ifelse(rf_class == 0, 0, 1))

# CM and summary results for the final random forest
results <- rbind(results, data.frame(Method = "Random Forest", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL

confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion
results 

# Run initial KNN, no scaling
knn_fit <- knn3(Casualty_Severity ~ ., data = train_set)
knn_class <- predict(knn_fit, validate_set, type = "class")
cm <- confusionMatrix(data = knn_class, reference = validate_set$Casualty_Severity, positive = "1")

# CM and summary results for initial KNN
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results_temp <- data.frame(Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                           Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]) 

row.names(results_temp) <- NULL
results_temp 

# only scale the features, not the target variable
train_scale <- as.data.frame(scale(train_set[,2:ncol(train_set)]))
train_scale <- data.frame(Casualty_Severity = train_set$Casualty_Severity, train_scale)
validate_scale <- scale(validate_set[,2:ncol(train_set)])
validate_scale <- data.frame(Casualty_Severity = validate_set$Casualty_Severity, validate_scale)

# Tune the KNN model, now with scaled data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
tunegrid <-  data.frame(k = seq(10,100,10))
knn_fit <- train(Casualty_Severity ~ ., data = train_scale, method = "knn",
                 trControl = control, tuneGrid = tunegrid)

knn_class <- predict(knn_fit, validate_scale, type = "raw")

cm <- confusionMatrix(data = knn_class, reference = validate_scale$Casualty_Severity, positive = "1")
class_pred <- data.frame(class_pred, knn = ifelse(knn_class == 0, 0, 1))

# CM and summary results for final KNN model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results <- rbind(results, data.frame(Method = "KNN", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL
results 

# CM and summary results for final KNN model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results <- rbind(results, data.frame(Method = "KNN", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL

results 

# Run logistic regression model
log_fit <- glm(Casualty_Severity ~ ., data = train_set, family = "binomial")
log_prob <- predict(log_fit, newdata = validate_set, type = "response")
log_class <- as.factor(ifelse(log_prob > 0.5, 1, 0))
cm <- confusionMatrix(data = log_class, reference = validate_set$Casualty_Severity, positive = "1")
class_pred <- data.frame(class_pred, log = ifelse(log_class == 0, 0, 1))

# CM and summary results for logistic regression model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results <- rbind(results, data.frame(Method = "Logistic regression", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL

results 

# Run LDA model
lda_fit <- train(Casualty_Severity ~ ., method = "lda", data = train_set)
lda_class <- predict(lda_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = lda_class, reference = validate_set$Casualty_Severity, positive = "1")
class_pred <- data.frame(class_pred, lda = ifelse(lda_class == 0, 0, 1))

# CM and summary results for LDA model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion

results <- rbind(results, data.frame(Method = "LDA", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL
results 

# Run SVM model with each kernel type, default parameter settings
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_set_slim <- train_set[createDataPartition(y = train_set$Casualty_Severity, times = 1, p = 0.1, list = FALSE),]

svm_fit <- svm(Casualty_Severity ~ ., data = train_set_slim, kernel="linear")
svm_class <- predict(svm_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = svm_class, reference = validate_set$Casualty_Severity, positive = "1")

svm_results <- data.frame(Method = "Linear", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2], Balanced = cm$byClass[11], Pos_pred = cm$byClass[3])

svm_fit <- svm(Casualty_Severity ~ ., data = train_set_slim, kernel="radial")
svm_class <- predict(svm_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = svm_class, reference = validate_set$Casualty_Severity, positive = "1")
svm_results <- rbind(svm_results, data.frame(Method = "Radial", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2], Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))

svm_fit <- svm(Casualty_Severity ~ ., data = train_set_slim, kernel="polynomial")
svm_class <- predict(svm_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = svm_class, reference = validate_set$Casualty_Severity, positive = "1")
svm_results <- rbind(svm_results, data.frame(Method = "Polynomial", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2], Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))

svm_fit <- svm(Casualty_Severity ~ ., data = train_set_slim, kernel="sigmoid")
svm_class <- predict(svm_fit, validate_set, type = "raw")
cm <- confusionMatrix(data = svm_class, reference = validate_set$Casualty_Severity, positive = "1")
svm_results <- rbind(svm_results, data.frame(Method = "Sigmoid", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2], Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))

row.names(svm_results) <- NULL
svm_results 

# Tune the SVM model, now we have chosen the kernel
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
tunegrid <- expand.grid(C = seq(0.25, 1.5, 0.25), sigma = seq(0.005, 0.05, 0.005))
svm_fit <- train(Casualty_Severity ~ ., data = train_set_slim, method = "svmRadial", preProcess = c("center","scale"), trControl = control,   tuneGrid = tunegrid)

svm_class <- predict(svm_fit, validate_set, type = "raw")

cm <- confusionMatrix(data = svm_class, reference = validate_set$Casualty_Severity, positive = "1")
class_pred <- data.frame(class_pred, svm = ifelse(svm_class == 0, 0, 1))

# Chart parameter tuning against model accuracy
plot(svm_fit)

# CM and summary results for SVM
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results <- rbind(results, data.frame(Method = "SVM", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL
results 

# Build the ensembles
class_pred <- class_pred %>% mutate(ens_3 = as.factor(ifelse(rf + log + lda  >= 2, 1, 0)),
                                    vote_count_3 = as.factor(rf + log + lda ),
                                    ens_5 = as.factor(ifelse(rf + knn + log + lda + svm >= 3, 1, 0)),
                                    vote_count_5 = as.factor(rf + knn + log + lda + svm ))
cm <- confusionMatrix(data = class_pred$ens_3, reference = validate_set$Casualty_Severity, positive = "1")

# CM and summary results for 3 model ensemble
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion

results <- rbind(results, data.frame(Method = "Ensemble 3", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL
results 

# CM and summary results for 5 model ensemble
cm <- confusionMatrix(data = class_pred$ens_5, reference = validate_set$Casualty_Severity, positive = "1")

confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results <- rbind(results, data.frame(Method = "Ensemble 5", Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                                     Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]))
row.names(results) <- NULL
results 

# Charts showing validation set observations, split by number of models calling a serious injury and the true severity
ens_class_count <- data.frame(pred = class_pred$vote_count_5, actual = validate_set$Casualty_Severity)

chart1 <- ens_class_count %>% ggplot(aes(x = pred)) + geom_bar(aes(fill = actual), position = "fill") +
  scale_fill_discrete(name = "Actual \nseverity") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid")) +
  labs(x = "Number of positive predictions", y = "Proportion of casualties")

chart2 <- ens_class_count %>% ggplot(aes(x = pred)) + geom_bar(aes(fill = actual)) +
  scale_fill_discrete(name = "Actual \nseverity") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid")) +
  labs(x = "Number of positive predictions", y = "Number of casualties")

grid.arrange(chart1, chart2, ncol = 2)

# If driver age is below 16, then set to 16
test_set$Age_of_Driver <- ifelse(test_set$Age_of_Driver < 16, 16, test_set$Age_of_Driver)
test_set$Age_of_Driver_B <- ifelse(test_set$Age_of_Driver_B < 16, 16, test_set$Age_of_Driver_B)

# Impute values where missing
test_set$Time <- as.numeric(hms(test_set$Time)) # Convert time to numeric to allow imputation
imp_init = mice(test_set, maxit=0) 
imp_meth = imp_init$method
imp_pred_matrix = imp_init$predictorMatrix

imp_pred_matrix[, c("Accident_Index", "Vehicle_Reference", "Casualty_Reference")]=0
set.seed(1)
imputed = mice(test_set, method=imp_meth, predictorMatrix=imp_pred_matrix, m=5)
test_set <- complete(imputed)

# Encoding time and day of week as cyclical variables
test_set <- test_set %>% mutate(Time_sin = sin((Time/(24*60*60)) * pi * 2), 
                                Time_cos = cos((Time/(24*60*60)) * pi * 2),
                                Dow_sin = sin(((wday(Date)-1) / 7) * pi * 2), 
                                Dow_cos = cos(((wday(Date)-1) / 7) * pi * 2))

# Change road class to be 1 to 5
test_set$Road_Class <- ifelse(test_set$Road_Class == 1, 1, test_set$Road_Class - 1)

# Encode light conditions
test_set$Daylight <- ifelse(test_set$Light_Conditions == 1, 1,0)
test_set$Street_Light <- ifelse(test_set$Light_Conditions == 4, 1, 0)

# Encode weather conditions
test_set$Raining <- ifelse(test_set$Weather_Conditions %in% c(2,5), 1,0)
test_set$Snowing <- ifelse(test_set$Weather_Conditions %in% c(3,6), 1, 0)
test_set$Foggy <- ifelse(test_set$Weather_Conditions == 7, 1, 0)
test_set$Windy <- ifelse(test_set$Weather_Conditions %in% c(4,5,6), 1, 0)

# Encode road type 
test_set$Road_Type <- as.factor(case_when(test_set$Road_Type == 1 ~ "aa_Roundabout",
                                          test_set$Road_Type == 2 ~ "One_way",
                                          test_set$Road_Type == 3 ~ "Dual_c",
                                          test_set$Road_Type == 6 ~ "Single_c",
                                          test_set$Road_Type == 7 ~ "Slip"))

dmy <- dummyVars("~Road_Type",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode road conditions 
test_set$Road_Surface_Conditions <- as.factor(case_when(
  test_set$Road_Surface_Conditions == 1 ~ "Road_Dry",
  test_set$Road_Surface_Conditions == 2 ~ "Road_Wet",
  test_set$Road_Surface_Conditions == 3 ~ "Road_Snowy",
  test_set$Road_Surface_Conditions == 4 ~ "Road_Icey",
  test_set$Road_Surface_Conditions == 5 ~ "Road_Flooded"))

dmy <- dummyVars("~Road_Surface_Conditions",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode junction detail
test_set$Junction_Detail <- as.factor(case_when(
  test_set$Junction_Detail == 0 ~ "aa_No_junc",
  test_set$Junction_Detail == 1 ~ "Junc_Roundabout",
  test_set$Junction_Detail == 2 ~ "Junc_Mini_roundabout",
  test_set$Junction_Detail == 3 ~ "Junc_T",
  test_set$Junction_Detail == 5 ~ "Junc_Slip",
  test_set$Junction_Detail == 6 ~ "Junc_Crossroads",
  test_set$Junction_Detail == 7 ~ "Junc_More_than_4",
  test_set$Junction_Detail == 8 ~ "Junc_Private_drive",
  test_set$Junction_Detail == 9 ~ "Junc_Other"))

dmy <- dummyVars("~Junction_Detail",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode vehicle type
test_set$Vehicle_Type <- as.factor(case_when(
  test_set$Vehicle_Type == 1 ~ "aa_pedal_bike",
  test_set$Vehicle_Type %in% c(2,3,4,5) ~ "Motorbike",
  test_set$Vehicle_Type %in% c(8,9) ~ "Car",
  test_set$Vehicle_Type == 19 ~ "LGV",
  test_set$Vehicle_Type %in% c(20,21) ~ "HGV"))

dmy <- dummyVars("~Vehicle_Type",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

test_set$Vehicle_Type_B <- as.factor(case_when(
  test_set$Vehicle_Type_B == 1 ~ "aa_pedal_bike_B",
  test_set$Vehicle_Type_B %in% c(2,3,4,5) ~ "Motorbike_B",
  test_set$Vehicle_Type_B %in% c(8,9) ~ "Car_B",
  test_set$Vehicle_Type_B == 19 ~ "LGV_B",
  test_set$Vehicle_Type_B %in% c(20,21) ~ "HGV_B"))

dmy <- dummyVars("~Vehicle_Type_B",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode vehicle manoeuvre
test_set$Vehicle_Manoeuvre <- as.factor(case_when(
  test_set$Vehicle_Manoeuvre == 1 ~ "Reversing",
  test_set$Vehicle_Manoeuvre == 2 ~ "Parked",
  test_set$Vehicle_Manoeuvre == 3 ~ "Held_up",
  test_set$Vehicle_Manoeuvre == 4 ~ "Stopping",
  test_set$Vehicle_Manoeuvre == 5 ~ "Moving_off",
  test_set$Vehicle_Manoeuvre == 6 ~ "U_turn",
  test_set$Vehicle_Manoeuvre == 7 ~ "Left_turn",
  test_set$Vehicle_Manoeuvre == 8 ~ "Wait_turn_left",
  test_set$Vehicle_Manoeuvre == 9 ~ "Right_turn",
  test_set$Vehicle_Manoeuvre == 10 ~ "Wait_turn_right",
  test_set$Vehicle_Manoeuvre == 11 ~ "Change_lane_left",
  test_set$Vehicle_Manoeuvre == 12 ~ "Change_lane_right",
  test_set$Vehicle_Manoeuvre == 13 ~ "Overtake_moving",
  test_set$Vehicle_Manoeuvre == 14 ~ "Overtake_static",
  test_set$Vehicle_Manoeuvre == 15 ~ "Overtake_nearside",
  test_set$Vehicle_Manoeuvre == 16 ~ "Left_bend",
  test_set$Vehicle_Manoeuvre == 17 ~ "Right_bend",
  test_set$Vehicle_Manoeuvre == 18 ~ "aa_Ahead_other"))

dmy <- dummyVars("~Vehicle_Manoeuvre",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

test_set$Vehicle_Manoeuvre_B <- as.factor(case_when(
  test_set$Vehicle_Manoeuvre_B == 1 ~ "Reversing_B",
  test_set$Vehicle_Manoeuvre_B == 2 ~ "Parked_B",
  test_set$Vehicle_Manoeuvre_B == 3 ~ "Held_up_B",
  test_set$Vehicle_Manoeuvre_B == 4 ~ "Stopping_B",
  test_set$Vehicle_Manoeuvre_B == 5 ~ "Moving_off_B",
  test_set$Vehicle_Manoeuvre_B == 6 ~ "U_turn_B",
  test_set$Vehicle_Manoeuvre_B == 7 ~ "Left_turn_B",
  test_set$Vehicle_Manoeuvre_B == 8 ~ "Wait_turn_left_B",
  test_set$Vehicle_Manoeuvre_B == 9 ~ "Right_turn_B",
  test_set$Vehicle_Manoeuvre_B == 10 ~ "Wait_turn_right_B",
  test_set$Vehicle_Manoeuvre_B == 11 ~ "Change_lane_left_B",
  test_set$Vehicle_Manoeuvre_B == 12 ~ "Change_lane_right_B",
  test_set$Vehicle_Manoeuvre_B == 13 ~ "Overtake_moving_B",
  test_set$Vehicle_Manoeuvre_B == 14 ~ "Overtake_static_B",
  test_set$Vehicle_Manoeuvre_B == 15 ~ "Overtake_nearside_B",
  test_set$Vehicle_Manoeuvre_B == 16 ~ "Left_bend_B",
  test_set$Vehicle_Manoeuvre_B == 17 ~ "Right_bend_B",
  test_set$Vehicle_Manoeuvre_B == 18 ~ "aa_Ahead_other_B"))

dmy <- dummyVars("~Vehicle_Manoeuvre_B",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode skidding and overturning

test_set$Skidded <- ifelse(test_set$Skidding_and_Overturning %in% c(1,2), 1, 0)
test_set$Skidded_B <- ifelse(test_set$Skidding_and_Overturning_B %in% c(1,2), 1, 0)

test_set$Overturned <- ifelse(test_set$Skidding_and_Overturning %in% c(2,4,5), 1, 0)
test_set$Overturned_B <- ifelse(test_set$Skidding_and_Overturning_B %in% c(2,4,5), 1, 0)

test_set$Jacknifed <- ifelse(test_set$Skidding_and_Overturning %in% c(3,4), 1, 0)
test_set$Jacknifed_B <- ifelse(test_set$Skidding_and_Overturning_B %in% c(3,4), 1, 0)

# Encode leaving carriageway

test_set$Straight <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(3), 1, 0)
test_set$Straight_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(3), 1, 0)

test_set$Offside <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(4,5,6,7,8), 1, 0)
test_set$Offside_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(4,5,6,7,8), 1, 0)

test_set$Nearside <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(1,2), 1, 0)
test_set$Nearside_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(1,2), 1, 0)

test_set$Rebound <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(2,5,8), 1, 0)
test_set$Rebound_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(2,5,8), 1,0)

test_set$Central_res <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(4,5), 1, 0)
test_set$Central_res_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(4,5), 1, 0)

test_set$Over_central <- ifelse(test_set$Vehicle_Leaving_Carriageway %in% c(6), 1, 0)
test_set$Over_central_B <- ifelse(test_set$Vehicle_Leaving_Carriageway_B %in% c(6), 1, 0)

# Encode hit object

test_set$Hit_object <- ifelse(test_set$Hit_Object_off_Carriageway == 0 , 0, 1)
test_set$Hit_object_B <- ifelse(test_set$Hit_Object_off_Carriageway_B == 0, 0, 1)

# Encode point of impact

test_set$First_Point_of_Impact <- as.factor(case_when(
  test_set$First_Point_of_Impact == 0 ~ "aa_no_impact",
  test_set$First_Point_of_Impact == 1 ~ "Impact_Front",
  test_set$First_Point_of_Impact == 2 ~ "Impact_Back",
  test_set$First_Point_of_Impact == 3 ~ "Impact_Offside",
  test_set$First_Point_of_Impact == 4 ~ "Impact_Nearside"))

dmy <- dummyVars("~First_Point_of_Impact",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

test_set$First_Point_of_Impact_B <- as.factor(case_when(
  test_set$First_Point_of_Impact_B == 0 ~ "aa_no_impact_B",
  test_set$First_Point_of_Impact_B == 1 ~ "Impact_Front_B",
  test_set$First_Point_of_Impact_B == 2 ~ "Impact_Back_B",
  test_set$First_Point_of_Impact_B == 3 ~ "Impact_Offside_B",
  test_set$First_Point_of_Impact_B == 4 ~ "Impact_Nearside_B"))

dmy <- dummyVars("~First_Point_of_Impact_B",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode sex of driver

test_set$Sex_of_Driver <- as.factor(ifelse(test_set$Sex_of_Driver == 1, "Male_driver",
                                           ifelse(test_set$Sex_of_Driver == 2, "Female_driver",NA)))

dmy <- dummyVars("~Sex_of_Driver",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

test_set$Sex_of_Driver_B <- as.factor(ifelse(test_set$Sex_of_Driver_B == 1, "Male_driver_B",
                                             ifelse(test_set$Sex_of_Driver_B == 2, "Female_driver_B",NA)))

dmy <- dummyVars("~Sex_of_Driver_B",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode combined casualty class and car passenger fields

test_set$Casualty_Location <- as.factor(
  ifelse(test_set$Casualty_Class == 1, "Driver",
         ifelse(test_set$Casualty_Class == 3, "aa_Pedestrian",
                ifelse(test_set$Car_Passenger == 1, "Front_Passenger","Rear_Passenger"))))

dmy <- dummyVars("~Casualty_Location",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

# Encode sex of casualty
test_set$Sex_of_Casualty <- as.factor(ifelse(test_set$Sex_of_Casualty == 1, "Male_cas",
                                             ifelse(test_set$Sex_of_Casualty == 2, "Female_cas",NA)))

dmy <- dummyVars("~Sex_of_Casualty",data=test_set, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=test_set))
test_set <- bind_cols(test_set, trfs)

### Change casualty severity to a factor

test_set$Casualty_Severity <- as.factor(test_set$Casualty_Severity)

# Run the individual models against the test data
test_set <- test_set %>% select("Casualty_Severity", key_vars)

rf_class_test <- predict(rf_fit, test_set, type = "raw")
class_pred <- data.frame(rf = ifelse(rf_class_test == 0, 0, 1))

test_scale <- scale(test_set[,2:ncol(test_set)])
test_scale <- data.frame(Casualty_Severity = test_set$Casualty_Severity, test_scale)
knn_class_test <- predict(knn_fit, test_scale, type = "raw")
class_pred <- data.frame(class_pred, knn = ifelse(knn_class_test == 0, 0, 1))

log_prob_test <- predict(log_fit, newdata = test_set, type = "response")
log_class_test <- as.factor(ifelse(log_prob_test > 0.5, 1, 0))
class_pred <- data.frame(class_pred, log = ifelse(log_class_test == 0, 0, 1))

lda_class_test <- predict(lda_fit, test_set, type = "raw")
class_pred <- data.frame(class_pred, lda = ifelse(lda_class_test == 0, 0, 1))

svm_class_test <- predict(svm_fit, test_set, type = "raw")
class_pred <- data.frame(class_pred, svm = ifelse(svm_class_test == 0, 0, 1))

# Build the ensemble
class_pred <- class_pred %>% mutate(maj_vote = as.factor(ifelse(rf + knn + log + lda + svm >= 3, 1, 0)),
                                    vote_count = as.factor(rf + knn + log + lda + svm))

cm <- confusionMatrix(data = class_pred$maj_vote, reference = test_set$Casualty_Severity, positive = "1")

# CM and summary results for final model
confusion <- data.frame(matrix(data = cm$table, ncol = 2))
rownames(confusion) <- c("Prediction 0","Prediction 1" )
colnames(confusion) <- c("Reference 0", "Reference 1")
confusion 

results_final <- data.frame(Overall = cm$overall[1], Sensitivity = cm$byClass[1], Specificity = cm$byClass[2],
                            Balanced = cm$byClass[11], Pos_pred = cm$byClass[3]) 

row.names(results_final) <- NULL
results_final 

# Charts showing test set observations, split by number of models calling a serious injury and the true severity
ens_class_count <- data.frame(pred = class_pred$vote_count, actual = test_set$Casualty_Severity)

chart1 <- ens_class_count %>% ggplot(aes(x = pred)) + geom_bar(aes(fill = actual), position = "fill") +
  scale_fill_discrete(name = "Actual \nseverity") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid")) +
  labs(x = "Number of positive predictions", y = "Proportion of casualties")

chart2 <- ens_class_count %>% ggplot(aes(x = pred)) + geom_bar(aes(fill = actual)) +
  scale_fill_discrete(name = "Actual \nseverity") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid")) +
  labs(x = "Number of positive predictions", y = "Number of casualties")

grid.arrange(chart1, chart2, ncol = 2)

##########
end_tm <- Sys.time()
end_tm - start_tm