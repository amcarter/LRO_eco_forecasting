library(readr)

## OXYGEN ##

O2_Franklin_Basin <- read_csv("LRO Data August/Franklin Basin/fb_disox.csv", 
                              skip = 81)

O2_Main_St <- read_csv("LRO Data August/Main Street/ms_disox.csv", 
                       skip = 81)

O2_Mendon_Rd <- read_csv("LRO Data August/Mendon Rd/mr_disox.csv", 
                      skip = 81)

O2_Tony_Grove <- read_csv("LRO Data August/Tony Grove/tg_disox.csv", 
                          skip = 81)

O2_Water_Lab_West_Bridge <- read_csv("LRO Data August/Water Lab/wl_disox.csv", 
                         skip = 81)


Oxygen_Franklin <- data.frame(
  date = O2_Franklin_Basin$ResultTime,
  DO = O2_Franklin_Basin$Result,
  source = "Franklin"
)
Oxygen_Main_St <- data.frame(
  date = O2_Main_St$ResultTime,
  DO = O2_Main_St$Result,
  source = "Main St"
)
Oxygen_Mendon <- data.frame(
  date = O2_Mendon_Rd$ResultTime,
  DO = O2_Mendon_Rd$Result,
  source = "Mendon"
)
Oxygen_TonyGrove <- data.frame(
  date = O2_Tony_Grove$ResultTime,
  DO = O2_Tony_Grove$Result,
  source = "Tony Grove"
)
Oxygen_Water_Lab <- data.frame(
  date = O2_Water_Lab_West_Bridge$ResultTime,
  DO = O2_Water_Lab_West_Bridge$Result,
  source = "Water Lab"
)

Logan_River_Oxygen <- rbind(Oxygen_Franklin,
                            Oxygen_Main_St,
                            Oxygen_Mendon,
                            Oxygen_TonyGrove,
                            Oxygen_Water_Lab)

#Saving Oxygen Data Set
#save(Logan_River_Oxygen, file = "Logan_River_Oxygen.RData")




## PH ##

pH_Franklin_Basin <- read_csv("LRO Data August/Franklin Basin/fb_ph.csv", 
                              skip = 81)

pH_Main_St <- read_csv("LRO Data August/Main Street/ms_ph.csv", 
                              skip = 81)

pH_Mendon_Rd <- read_csv("LRO Data August/Mendon Rd/mr_ph.csv", 
                         skip = 81)

pH_Tony_Grove <- read_csv("LRO Data August/Tony Grove/tg_ph.csv", 
                          skip = 81)

pH_Water_Lab_West_Bridge <- read_csv("LRO Data August/Water Lab/wl_ph.csv", 
                                     skip = 81)

pH_Franklin <- data.frame(
  date = pH_Franklin_Basin$ResultTime,
  pH = pH_Franklin_Basin$Result,
  source = "Franklin"
)
pH_Main_St <- data.frame(
  date = pH_Main_St$ResultTime,
  pH = pH_Main_St$Result,
  source = "Main St"
)
pH_Mendon_Rd <- data.frame(
  date = pH_Mendon_Rd$ResultTime,
  pH = pH_Mendon_Rd$Result,
  source = "Mendon"
)
pH_TonyGrove <- data.frame(
  date = pH_Tony_Grove$ResultTime,
  pH = pH_Tony_Grove$Result,
  source = "Tony Grove"
)
pH_Water_Lab <- data.frame(
  date = pH_Water_Lab_West_Bridge$ResultTime,
  pH = pH_Water_Lab_West_Bridge$Result,
  source = "Water Lab"
)

Logan_River_pH <- rbind(pH_Franklin,
                        pH_Main_St,
                        pH_Mendon_Rd,
                        pH_TonyGrove,
                        pH_Water_Lab)

#Saving PH Data Set
#save(Logan_River_pH, file = "Logan_River_pH.RData")



## Turbidity ##

turbidity_Franklin_Basin <- read_csv("LRO Data August/Franklin Basin/fb_turb.csv", 
                                     skip = 81)

turbidity_Main_St <- read_csv("LRO Data August/Main Street/ms_turb.csv", 
                                     skip = 81)

turbidity_Mendon_Rd <- read_csv("LRO Data August/Mendon Rd/mr_turb.csv",
                                skip = 81)

turbidity_Tony_Grove <- read_csv("LRO Data August/Tony Grove/tg_turb.csv", 
                                 skip = 81)

turbidity_Water_Lab_West_Bridge <- read_csv("LRO Data August/Water Lab/wl_turb.csv", 
                                            skip = 81)


turbidity_Franklin <- data.frame(
  date = turbidity_Franklin_Basin$ResultTime,
  turbidity = turbidity_Franklin_Basin$Result,
  source = "Franklin"
)
turbidity_Main_St <- data.frame(
  date = turbidity_Main_St$ResultTime,
  turbidity = turbidity_Main_St$Result,
  source = "Main St"
)
turbidity_Mendon_Rd <- data.frame(
  date = turbidity_Mendon_Rd$ResultTime,
  turbidity = turbidity_Mendon_Rd$Result,
  source = "Mendon"
)
turbidity_TonyGrove <- data.frame(
  date = turbidity_Tony_Grove$ResultTime,
  turbidity = turbidity_Tony_Grove$Result,
  source = "Tony Grove"
)
turbidity_Water_Lab <- data.frame(
  date = turbidity_Water_Lab_West_Bridge$ResultTime,
  turbidity = turbidity_Water_Lab_West_Bridge$Result,
  source = "Water Lab"
)

Logan_River_turbidity <- rbind(turbidity_Franklin,
                               turbidity_Main_St,
                               turbidity_Mendon_Rd,
                               turbidity_TonyGrove,
                               turbidity_Water_Lab)

#Saving Turbidity Data Set
#save(Logan_River_turbidity, file = "Logan_River_turbidity.RData")



## Discharge ##

discharge_Franklin_Basin <- read_csv("LRO Data August/Franklin Basin/fb_discharge.csv", 
                                     skip = 81)

discharge_Main_St <- read_csv("LRO Data August/Main Street/ms_discharge.csv", 
                                     skip = 81)

discharge_Mendon_Rd <- read_csv("LRO Data August/Mendon Rd/mr_discharge.csv",
                                skip = 81)

discharge_Tony_Grove <- read_csv("LRO Data August/Tony Grove/tg_discharge.csv", 
                                 skip = 81)

discharge_Water_Lab_West_Bridge <- read_csv("LRO Data August/Water Lab/wl_discharge.csv", 
                                            skip = 81)

discharge_Franklin <- data.frame(
  date = discharge_Franklin_Basin$ResultTime,
  discharge = discharge_Franklin_Basin$Result,
  source = "Franklin"
)
discharge_Main_St <- data.frame(
  date = discharge_Main_St$ResultTime,
  discharge = discharge_Main_St$Result,
  source = "Main St"
)
discharge_Mendon <- data.frame(
  date = discharge_Mendon_Rd$ResultTime,
  discharge = discharge_Mendon_Rd$Result,
  source = "Mendon"
)
discharge_TonyGrove <- data.frame(
  date = discharge_Tony_Grove$ResultTime,
  discharge = discharge_Tony_Grove$Result,
  source = "Tony Grove"
)
discharge_Water_Lab <- data.frame(
  date = discharge_Water_Lab_West_Bridge$ResultTime,
  discharge = discharge_Water_Lab_West_Bridge$Result,
  source = "Water Lab"
)

Logan_River_discharge <- rbind(discharge_Franklin,
                               discharge_Main_St,
                               discharge_Mendon,
                               discharge_TonyGrove,
                               discharge_Water_Lab)


#Saving Data Set
#save(Logan_River_discharge, file = "Logan_River_discharge.RData")



## water temp ##

watertemp_Franklin_Basin <- read_csv("LRO Data August/Franklin Basin/fb_watertemp.csv", 
                                      skip = 81)

watertemp_Main_St <- read_csv("LRO Data August/Main Street/ms_watertemp.csv", 
                                      skip = 81)

watertemp_Mendon_Rd <- read_csv("LRO Data August/Mendon Rd/mr_watertemp.csv",
                                 skip = 81)

watertemp_Tony_Grove <- read_csv("LRO Data August/Tony Grove/tg_watertemp.csv", 
                                  skip = 81)

watertemp_Water_Lab_West_Bridge <- read_csv("LRO Data August/Water Lab/wl_watertemp.csv", 
                                             skip = 81)

watertemp_Franklin <- data.frame(
  date = watertemp_Franklin_Basin$ResultTime,
  watertemp = watertemp_Franklin_Basin$Result,
  source = "Franklin"
)
watertemp_Main_St <- data.frame(
  date = watertemp_Main_St$ResultTime,
  watertemp = watertemp_Main_St$Result,
  source = "Main St"
)
watertemp_Mendon <- data.frame(
  date = watertemp_Mendon_Rd$ResultTime,
  watertemp = watertemp_Mendon_Rd$Result,
  source = "Mendon"
)
watertemp_TonyGrove <- data.frame(
  date = watertemp_Tony_Grove$ResultTime,
  watertemp = watertemp_Tony_Grove$Result,
  source = "Tony Grove"
)
watertemp_Water_Lab <- data.frame(
  date = watertemp_Water_Lab_West_Bridge$ResultTime,
  watertemp = watertemp_Water_Lab_West_Bridge$Result,
  source = "Water Lab"
)

Logan_River_watertemp<- rbind(watertemp_Franklin,
                               watertemp_Main_St,
                               watertemp_Mendon,
                               watertemp_TonyGrove,
                               watertemp_Water_Lab)

#Saving Data Set
#save(Logan_River_watertemp, file = "Logan_River_watertemp.RData")



### TEST SET SPLIT ###

#main file
library(dplyr)
Logan_River_Main_Updated <- Logan_River_discharge |>
  full_join(Logan_River_Oxygen, by = c("date", "source")) |>
  full_join(Logan_River_pH, by = c("date", "source")) |>
  full_join(Logan_River_turbidity, by = c("date", "source")) |>
  full_join(Logan_River_watertemp, by = c("date", "source"))



#adding in the Franklin Basin PAR data
Franklin_Basin_PAR <-  read_csv("LRO Data August/Franklin Basin/fb_incoming_PAR.csv", 
                                skip = 81)[,-3]

PAR_Franklin <- data.frame(
  date = Franklin_Basin_PAR$ResultTime,
  PAR = Franklin_Basin_PAR$Result,
  source = "Franklin"
)

Logan_River_Main_Updated <- Logan_River_Main_Updated |>
  full_join(PAR_Franklin, by = c("date", "source")) 


#Saving Combined Data Set
save(Logan_River_Main_Updated, file = "Logan_River_Main_Updated.RData")




#Splitting Data set to use last two years as testing data (testing starts on 2/15/2024)

library(lubridate)
two_years <- Sys.Date() %m-% years(2)

Logan_River_Updated_train <- Logan_River_Main_Updated[Logan_River_Main_Updated$date < two_years, ]
Logan_River_Updated_test <- Logan_River_Main_Updated[Logan_River_Main_Updated$date >= two_years, ]


save(Logan_River_Updated_train, file = "Logan_River_Updated_Train.RData")
save(Logan_River_Updated_test, file = "Logan_River_Updated_Test.RData")


#removing extra Variables

rm(list = setdiff(ls(), c("Logan_River_Main_Updated", 
                          "Logan_River_Updated_train",
                          "Logan_River_Updated_test")))


