library(tidyverse)
library(readr)
library(lubridate)
library(RCurl)
library(caret)
library(stringr)

x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/accidents.csv")
accidents <- read.csv(text = x)
x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/casualties.csv")
casualties <- read.csv(text = x)
x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/vehicles.csv")
vehicles <- read.csv(text = x)

view(accidents)
view(casualties)
view(vehicles)

accidents %>% mutate(year = year(Date)) %>% group_by(year) %>% summarise(count= n())
chart_col_1 <- "#008B61"
chart_back <- "#DBDBD3"

#############################################
## Encoding for accidents

# Encoding time as cyclical variable

accidents$Time <- hms(accidents$Time)
accidents <- accidents %>% mutate(Time_sin = sin((period_to_seconds(Time)/(24*60*60)) * pi * 2), 
                                  Time_cos = cos((period_to_seconds(Time)/(24*60*60)) * pi * 2))

accidents %>% ggplot(aes(Time_sin, Time_cos)) + 
  geom_point(alpha = 0.005, fill = chart_col_1, colour = chart_col_1, size = 1) + 
  annotate(geom = "Text", x = sin(0 * pi * 2), y = cos(0 * pi * 2) - 0.1, label = "0") +
  annotate(geom = "Text", x = sin(0.25 * pi * 2) - 0.1, y = cos(0.25 * pi * 2) , label = "6") +
  annotate(geom = "Text", x = sin(0.5 * pi * 2), y = cos(0.5 * pi * 2) + 0.1, label = "12") +
  annotate(geom = "Text", x = sin(0.75 * pi * 2) + 0.1, y = cos(0.75 * pi * 2), label = "18") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.3, 
  linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)))

accidents <- accidents %>% mutate(Dow_sin = sin(((wday(Date)-1) / 7) * pi * 2), 
                                  Dow_cos = cos(((wday(Date)-1) / 7) * pi * 2))

accidents %>% ggplot(aes(Dow_sin, Dow_cos)) + 
  geom_point(alpha = 0.005, fill = chart_col_1, colour = chart_col_1, size = 5) + 
  annotate(geom = "Text", x = sin(0 * pi * 2), y = cos(0 * pi * 2) - 0.1, label = "Sun") +
  annotate(geom = "Text", x = sin(1/7 * pi * 2), y = cos(1/7 * pi * 2) - 0.1, label = "Mon") +
  annotate(geom = "Text", x = sin(2/7 * pi * 2), y = cos(2/7 * pi * 2) - 0.1, label = "Tue") +
  annotate(geom = "Text", x = sin(3/7 * pi * 2), y = cos(3/7 * pi * 2) - 0.1, label = "Wed") +
  annotate(geom = "Text", x = sin(4/7 * pi * 2), y = cos(4/7 * pi * 2) - 0.1, label = "Thu") +
  annotate(geom = "Text", x = sin(5/7 * pi * 2), y = cos(5/7 * pi * 2) - 0.1, label = "Fri") +
  annotate(geom = "Text", x = sin(6/7 * pi * 2), y = cos(6/7 * pi * 2) - 0.1, label = "Sat") +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.3, 
                                        linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)))


# Change road class to be 1 to 5
accidents$Road_Class <- ifelse(accidents$Road_Class == 1, 1, accidents$Road_Class - 1)

# Encode light conditions
accidents$Daylight <- ifelse(accidents$Light_Conditions == 1, 1, 
                      ifelse(accidents$Light_Conditions == -1,NA,0))

accidents$Street_Light <- ifelse(accidents$Light_Conditions == 4, 1, 
                          ifelse(accidents$Light_Conditions == -1,NA,0))
# Encode weather conditions
accidents$Raining <- ifelse(accidents$Weather_Conditions %in% c(2,5), 1, 
                     ifelse(accidents$Weather_Conditions %in% c(8,9,-1),NA,0))
accidents$Snowing <- ifelse(accidents$Weather_Conditions %in% c(3,6), 1, 
                     ifelse(accidents$Weather_Conditions %in% c(8,9,-1),NA,0))
accidents$Foggy <- ifelse(accidents$Weather_Conditions == 7, 1, 
                   ifelse(accidents$Weather_Conditions %in% c(8,9,-1),NA,0))
accidents$Windy <- ifelse(accidents$Weather_Conditions %in% c(4,5,6), 1, 
                     ifelse(accidents$Weather_Conditions %in% c(8,9,-1),NA,0))

# Encode road type 
accidents$Road_Type <- as.factor(ifelse(accidents$Road_Type == 1, "aa_Roundabout",
                                   ifelse(accidents$Road_Type == 2, "One_way",
                                   ifelse(accidents$Road_Type == 3, "Dual_c",
                                   ifelse(accidents$Road_Type == 6, "Single_c",
                                   ifelse(accidents$Road_Type == 7, "Slip",NA))))))

dmy <- dummyVars("~Road_Type",data=accidents, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=accidents))
accidents <- bind_cols(accidents, trfs)

# Encode road conditions 
accidents$Road_Surface_Conditions <- as.factor(ifelse(accidents$Road_Surface_Conditions == 1, "Road_Dry",
                                               ifelse(accidents$Road_Surface_Conditions == 2, "Road_Wet",
                                               ifelse(accidents$Road_Surface_Conditions == 3, "Road_Snowy",
                                               ifelse(accidents$Road_Surface_Conditions == 4, "Road_Icey",
                                               ifelse(accidents$Road_Surface_Conditions == 5, "Road_Flooded",NA))))))

dmy <- dummyVars("~Road_Surface_Conditions",data=accidents, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=accidents))
accidents <- bind_cols(accidents, trfs)

# Encode junction detail
accidents$Junction_Detail <- as.factor(ifelse(accidents$Junction_Detail == 0, "aa_No_junc",
                                       ifelse(accidents$Junction_Detail == 1, "Junc_Roundabout",
                                       ifelse(accidents$Junction_Detail == 2, "Junc_Mini_roundabout",
                                       ifelse(accidents$Junction_Detail == 3, "Junc_T",
                                       ifelse(accidents$Junction_Detail == 5, "Junc_Slip",
                                       ifelse(accidents$Junction_Detail == 6, "Junc_Crossroads",
                                       ifelse(accidents$Junction_Detail == 7, "Junc_More_than_4",
                                       ifelse(accidents$Junction_Detail == 8, "Junc_Private_drive",
                                       ifelse(accidents$Junction_Detail == 9, "Junc_Other",NA))))))))))

dmy <- dummyVars("~Junction_Detail",data=accidents, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=accidents))
accidents <- bind_cols(accidents, trfs)

# accidents <- accidents %>% mutate(Hour = as.numeric(substr(Time,1,2)), Hour_Block = as.factor(floor(hour/3)+1))
# dmy <- dummyVars("~Hour_Block",data=accidents, fullRank = TRUE, sep = "_")
# trfs <- data.frame(predict(dmy,newdata=accidents))
# accidents <- bind_cols(accidents, trfs)

accidents <- accidents %>% select(Accident_Index:Road_Class, Speed_limit, Time_sin:Junc_T)

accident_cols <- names(accidents)
write.csv(accident_cols, "accident_columns.csv")

#############################################
## Encoding for vehicles

#vehicles %>% group_by(driver_age_band, driver_age_band_other) %>% summarise(count=n()) %>% arrange(desc(count))

vehicles <- vehicles %>% rename(Age_of_Driver_B = Age_of_Driver_other)

# Encode vehicle type
vehicles$Vehicle_Type <- as.factor(ifelse(vehicles$Vehicle_Type == 1, "aa_pedal_bike",
                                   ifelse(vehicles$Vehicle_Type %in% c(2,3,4,5), "Motorbike",
                                   ifelse(vehicles$Vehicle_Type %in% c(8,9), "Car",
                                   ifelse(vehicles$Vehicle_Type == 19, "LGV",
                                   ifelse(vehicles$Vehicle_Type %in% c(20,21), "HGV",NA))))))

dmy <- dummyVars("~Vehicle_Type",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

vehicles$Vehicle_Type_other <- as.factor(ifelse(vehicles$Vehicle_Type_other == 1, "aa_pedal_bike_B",
                                         ifelse(vehicles$Vehicle_Type_other %in% c(2,3,4,5), "Motorbike_B",
                                         ifelse(vehicles$Vehicle_Type_other %in% c(8,9), "Car_B",
                                         ifelse(vehicles$Vehicle_Type_other == 19, "LGV_B",
                                         ifelse(vehicles$Vehicle_Type_other %in% c(20,21), "HGV_B",NA))))))

dmy <- dummyVars("~Vehicle_Type_other",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

# Encode vehicle manoeuvre
vehicles$Vehicle_Manoeuvre <- as.factor(ifelse(vehicles$Vehicle_Manoeuvre == 1, "Reversing",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 2, "Parked",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 3, "Held_up",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 4, "Stopping",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 5, "Moving_off",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 6, "U_turn",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 7, "Left_turn",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 8, "Wait_turn_left",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 9, "Right_turn",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 10, "Wait_turn_right",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 11, "Change_lane_left",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 12, "Change_lane_right",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 13, "Overtake_moving",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 14, "Overtake_static",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 15, "Overtake_nearside",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 16, "Left_bend",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 17, "Right_bend",
                                        ifelse(vehicles$Vehicle_Manoeuvre == 18, "aa_Ahead_other",NA)))))))))))))))))))

dmy <- dummyVars("~Vehicle_Manoeuvre",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

vehicles$Vehicle_Manoeuvre_other <- as.factor(ifelse(vehicles$Vehicle_Manoeuvre_other == 1, "Reversing_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 2, "Parked_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 3, "Held_up_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 4, "Stopping_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 5, "Moving_off_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 6, "U_turn_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 7, "Left_turn_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 8, "Wait_turn_left_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 9, "Right_turn_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 10, "Wait_turn_right_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 11, "Change_lane_left_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 12, "Change_lane_right_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 13, "Overtake_moving_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 14, "Overtake_static_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 15, "Overtake_nearside_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 16, "Left_bend_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 17, "Right_bend_B",
                                              ifelse(vehicles$Vehicle_Manoeuvre_other == 18, "aa_Ahead_other_B",NA)))))))))))))))))))

dmy <- dummyVars("~Vehicle_Manoeuvre_other",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

# Encode skidding and overturning

vehicles$Skidded <- ifelse(vehicles$Skidding_and_Overturning %in% c(1,2), 1, 
                    ifelse(vehicles$Skidding_and_Overturning %in% c(0,3,4,5), 0, NA))

vehicles$Skidded_B <- ifelse(vehicles$Skidding_and_Overturning_other %in% c(1,2), 1, 
                      ifelse(vehicles$Skidding_and_Overturning_other %in% c(0,3,4,5), 0, NA))

vehicles$Overturned <- ifelse(vehicles$Skidding_and_Overturning %in% c(2,4,5), 1, 
                       ifelse(vehicles$Skidding_and_Overturning %in% c(0,1,3), 0, NA))

vehicles$Overturned_B <- ifelse(vehicles$Skidding_and_Overturning_other %in% c(2,4,5), 1, 
                         ifelse(vehicles$Skidding_and_Overturning_other %in% c(0,1,3), 0, NA))

vehicles$Jacknifed <- ifelse(vehicles$Skidding_and_Overturning %in% c(3,4), 1, 
                      ifelse(vehicles$Skidding_and_Overturning %in% c(0,1,2,5), 0, NA))

vehicles$Jacknifed_B <- ifelse(vehicles$Skidding_and_Overturning_other %in% c(3,4), 1, 
                        ifelse(vehicles$Skidding_and_Overturning_other %in% c(0,1,2,5), 0, NA))

# Encode leaving carriageway

vehicles$Straight <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(3), 1, 
                     ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,1,2,4,5,6,7,8), 0, NA))

vehicles$Straight_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(3), 1, 
                       ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,1,2,4,5,6,7,8), 0, NA))

vehicles$Offside <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(4,5,6,7,8), 1, 
                    ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,1,2,3), 0, NA))

vehicles$Offside_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(4,5,6,7,8), 1, 
                      ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,1,2,3), 0, NA))

vehicles$Nearside <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(1,2), 1, 
                     ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,3,4,5,6,7,8), 0, NA))

vehicles$Nearside_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(1,2), 1, 
                       ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,3,4,5,6,7,8), 0, NA))

vehicles$Rebound <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(2,5,8), 1, 
                    ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,1,3,4,6,7), 0, NA))

vehicles$Rebound_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(2,5,8), 1, 
                      ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,1,3,4,6,7), 0, NA))

vehicles$Central_res <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(4,5), 1, 
                        ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,1,2,3,6,7,8), 0, NA))

vehicles$Central_res_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(4,5), 1, 
                          ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,1,2,3,6,7,8), 0, NA))

vehicles$Over_central <- ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(6), 1, 
                         ifelse(vehicles$Vehicle_Leaving_Carriageway %in% c(0,1,2,3,4,5,7,8), 0, NA))

vehicles$Over_central_B <- ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(6), 1, 
                           ifelse(vehicles$Vehicle_Leaving_Carriageway_other %in% c(0,1,2,3,4,5,7,8), 0, NA))

# Encode hit object

vehicles$Hit_object <- ifelse(vehicles$Hit_Object_off_Carriageway == 0 , 0, 
                       ifelse(vehicles$Hit_Object_off_Carriageway > 0, 1, NA))

vehicles$Hit_object_B <- ifelse(vehicles$Hit_Object_off_Carriageway_other == 0, 0, 
                         ifelse(vehicles$Hit_Object_off_Carriageway_other > 0, 1, NA))

# Encode point of impact

vehicles$First_Point_of_Impact <- as.factor(ifelse(vehicles$First_Point_of_Impact == 0, "aa_no_impact",
                                            ifelse(vehicles$First_Point_of_Impact == 1, "Impact_Front",
                                            ifelse(vehicles$First_Point_of_Impact == 2, "Impact_Back",
                                            ifelse(vehicles$First_Point_of_Impact == 3, "Impact_Offside",
                                            ifelse(vehicles$First_Point_of_Impact == 4, "Impact_Nearside",NA))))))

dmy <- dummyVars("~First_Point_of_Impact",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

vehicles$First_Point_of_Impact_other <- as.factor(ifelse(vehicles$First_Point_of_Impact_other == 0, "aa_no_impact_B",
                                                  ifelse(vehicles$First_Point_of_Impact_other == 1, "Impact_Front_B",
                                                  ifelse(vehicles$First_Point_of_Impact_other == 2, "Impact_Back_B",
                                                  ifelse(vehicles$First_Point_of_Impact_other == 3, "Impact_Offside_B",
                                                  ifelse(vehicles$First_Point_of_Impact_other == 4, "Impact_Nearside_B",NA))))))

dmy <- dummyVars("~First_Point_of_Impact_other",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

# Encode sex of driver

vehicles$Sex_of_Driver <- as.factor(ifelse(vehicles$Sex_of_Driver == 1, "Male_driver",
                                    ifelse(vehicles$Sex_of_Driver == 2, "Female_driver",NA)))

dmy <- dummyVars("~Sex_of_Driver",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

vehicles$Sex_of_Driver_other <- as.factor(ifelse(vehicles$Sex_of_Driver_other == 1, "Male_driver_B",
                                           ifelse(vehicles$Sex_of_Driver_other == 2, "Female_driver_B",NA)))

dmy <- dummyVars("~Sex_of_Driver_other",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=vehicles))
vehicles <- bind_cols(vehicles, trfs)

# Encode driver age band

# vehicles$driver_age_band <- as.factor(ifelse(vehicles$Age_of_Driver == -1, NA, 
#                             str_c( "Driver_age_", floor(vehicles$Age_of_Driver/10)*10)))
# 
# dmy <- dummyVars("~driver_age_band",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
# trfs <- data.frame(predict(dmy,newdata=vehicles))
# vehicles <- bind_cols(vehicles, trfs)
# 
# vehicles$driver_age_band_other <- as.factor(ifelse(vehicles$Age_of_Driver_other == -1, NA, 
#                                   str_c( "Driver_age_other_", floor(vehicles$Age_of_Driver_other/10)*10)))
# 
# dmy <- dummyVars("~driver_age_band_other",data=vehicles, fullRank = TRUE, levelsOnly = TRUE)
# trfs <- data.frame(predict(dmy,newdata=vehicles))
# vehicles <- bind_cols(vehicles, trfs)

vehicles <- vehicles %>% select(Accident_Index, Vehicle_Reference, Age_of_Driver, Age_of_Driver_other:Male_driver_B)

vehicles_cols <- names(vehicles)
write.csv(vehicles_cols, "vehicles_columns.csv")

#############################################
## Encoding for casualties

# Encode combined casualty class and car passenger fields

casualties$Casualty_Location <- as.factor(ifelse(casualties$Casualty_Class == 1, "Driver",
                                          ifelse(casualties$Casualty_Class == 3, "aa_Pedestrian",
                                          ifelse(casualties$Car_Passenger == 1, "Front_Passenger",
                                          ifelse(casualties$Car_Passenger == 2, "Rear_Passenger",NA)))))

dmy <- dummyVars("~Casualty_Location",data=casualties, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=casualties))
casualties <- bind_cols(casualties, trfs)

# # Encode casualty class
# 
# casualties$Casualty_Class <- as.factor(ifelse(casualties$Casualty_Class == 1, "Driver",
#                                        ifelse(casualties$Casualty_Class == 2, "Passenger",
#                                        ifelse(casualties$Casualty_Class == 3, "aa_Pedestrian",NA))))
# 
# dmy <- dummyVars("~Casualty_Class",data=casualties, fullRank = TRUE, levelsOnly = TRUE)
# trfs <- data.frame(predict(dmy,newdata=casualties))
# casualties <- bind_cols(casualties, trfs)

# Encode sex of casualty
casualties$Sex_of_Casualty <- as.factor(ifelse(casualties$Sex_of_Casualty == 1, "Male_cas",
                                        ifelse(casualties$Sex_of_Casualty == 2, "Female_cas",NA)))

dmy <- dummyVars("~Sex_of_Casualty",data=casualties, fullRank = TRUE, levelsOnly = TRUE)
trfs <- data.frame(predict(dmy,newdata=casualties))
casualties <- bind_cols(casualties, trfs)

casualties %>% group_by(Casualty_Class, Car_Passenger, casualty_location) %>% summarise(count = n())

# Encode car passenger (position)
# casualties$Car_Passenger <- as.factor(ifelse(casualties$Car_Passenger == 0, "aa_not_car_passenger",
#                                       ifelse(casualties$Car_Passenger == 1, "Front_pas",
#                                       ifelse(casualties$Car_Passenger == 2, "Rear_pas",NA))))
# 
# dmy <- dummyVars("~Car_Passenger",data=casualties, fullRank = TRUE, levelsOnly = TRUE)
# trfs <- data.frame(predict(dmy,newdata=casualties))
# casualties <- bind_cols(casualties, trfs)

# Encode age band for casualty

# casualties$age_band <- as.factor(ifelse(casualties$Age_of_Casualty == -1, NA, 
#                                   str_c( "Cas_age_", floor(casualties$Age_of_Casualty/10)*10)))
# 
# dmy <- dummyVars("~age_band",data=casualties, fullRank = TRUE, levelsOnly = TRUE)
# trfs <- data.frame(predict(dmy,newdata=casualties))
# casualties <- bind_cols(casualties, trfs)

casualties <-  casualties %>% select(Accident_Index:Casualty_Reference, Age_of_Casualty, Casualty_Severity, 
                                     Driver:Male_cas)

casualties_cols <- names(casualties)
write.csv(casualties_cols, "casualties_columns.csv")

#############################################
## EDA for accidents

accidents$Road_Class <- as.factor(accidents$Road_Class)
accidents$Road_Type <- as.factor(accidents$Road_Type)
accidents$Speed_limit <- as.factor(accidents$Speed_limit)
accidents$Junction_Detail <- as.factor(accidents$Junction_Detail)
accidents$Light_Conditions <- as.factor(accidents$Light_Conditions)
accidents$Weather_Conditions <- as.factor(accidents$Weather_Conditions)
accidents$Road_Surface_Conditions <- as.factor(accidents$Road_Surface_Conditions)



accidents %>% ggplot(aes(x = Road_Class)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,50000,10000))

accidents %>% ggplot(aes(x = Road_Type)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,100000,20000))

accidents %>% ggplot(aes(x = Speed_limit)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,100000,20000))

accidents %>% ggplot(aes(x = Junction_Detail)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,100000,20000))

accidents %>% ggplot(aes(x = Light_Conditions)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,100000,20000))

accidents %>% ggplot(aes(x = Weather_Conditions)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,120000,20000))

accidents %>% ggplot(aes(x = Road_Surface_Conditions)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,120000,20000))

accidents %>% ggplot(aes(x = hour)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,12000,2000))

accidents %>% ggplot(aes(x = hour_block)) + geom_bar(colour = chart_col_1, fill = chart_col_1) +
  theme(panel.background = element_rect(fill = chart_back, colour = "black", size = 0.5, linetype = "solid"), axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  scale_y_continuous(breaks = seq(0,120000,5000))

