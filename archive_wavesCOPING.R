## Create wave column in COPING

##End dates here are 'expiry' dates, however some participants requested extra time and for those who were sent questionnaires by text message, no expiration date could be set. For the purposes of waves,I will effectively ignore expiration dates to capture these people.

##FORTNIGHTLY:
#Wave 3, follow up A = 19th May - 26th May ##May1 
#Wave 4, follow up B = 2nd June - 9th June (?) ##June1
#Wave 5, follow up A = 16th June - 23 June at 5PM (Set A_ongoing) or Friday, 26 June (setA_1) ##June2
#Wave 6, follow up B = 30th June - 7th July ##July1
#Wave 7, follow up A = 14th July - 21st July ##July2
#Wave 8, follow up B = 28th July - 4th August ##July_Aug

##MONTHLY
#Wave 9, follow up A = 25th August- 15th Sept ##August-September
#Wave 10, follow up B = 22nd Sept - 13th Oct ##September-October
#Wave 11, follow up A = 20th Oct - 10th Nov ##October-November
#Wave 12, follow up B, = 17th Nov - 8th Dec ##November - December
#WAVE 13, follow up A = 15th Dec - 5th Jan ##December - Jan
#(Note that waves continue but data extracted in Dec)

#**EDEQ only in follow up B, TAF in both A and B

# MAY 2 (wave 2a) - FOLLOW UP A
start3 <- as.POSIXct("2020-05-19")
end3 <-  as.POSIXct("2020-06-02")  

# JUNE 1 (wave 2b) - FOLLOW UP B
start4 <- as.POSIXct("2020-06-02")
end4 <-  as.POSIXct("2020-06-16")  

# JUNE 2 (wave 3a) - FOLLOW UP A
start5 <- as.POSIXct("2020-06-16")
end5 <-  as.POSIXct("2020-06-30") 

# JULY 1 (wave 3b) - FOLLOW UP B
start6 <- as.POSIXct("2020-06-30")
end6 <-  as.POSIXct("2020-07-14") 

# JULY 2 (wave 4a) - FOLLOW UP A
start7 <- as.POSIXct("2020-07-14")
end7 <-  as.POSIXct("2020-07-28") #**I think startDate_wavesA_taf_ongoing starts from here?

# JULY - AUG (wave 4b) - FOLLOW UP B
start8 <- as.POSIXct("2020-07-28") #**Some people in 'startDate_wavesA_taf' filled out follow up A TAF on 29th July, but this date falls within follow up B? What should we do with people who filled them in really late?
end8 <-  as.POSIXct("2020-08-25") 

# AUG - SEPT (wave 5) - FOLLOW UP A
start9 <- as.POSIXct("2020-08-25")
end9 <-  as.POSIXct("2020-09-22") 

# SEPT - OCT (wave 6) - FOLLOW UP B
start10 <- as.POSIXct("2020-09-22")
end10 <-  as.POSIXct("2020-10-20") 

# OCT - NOV (wave 7) - FOLLOW UP A
start11 <- as.POSIXct("2020-10-20")
end11 <-  as.POSIXct("2020-11-17") 

# NOV - DEC (wave 8) - FOLLOW UP B
start12 <- as.POSIXct("2020-11-17")
end12 <-  as.POSIXct("2020-12-15") 

# DEC - JAN (wave 9) - FOLLOW UP A
start13 <- as.POSIXct("2020-12-15")
end13 <-  as.POSIXct("2021-01-11") 


#startDate_wavesA_taf - earlier waves (until 29th July?)
#startDate_wavesA_taf_ongoing - later waves ()

#startDate_wavesB_taf
#startDate_wavesB_edeq

##**How much does it matter if someone is in 'A' or 'B' - is the date more important? e.g. can someone who was considered follow up A but completed q during timepoints considered as within 'follow up B', be recoded as follow up B?
#**Need to check wave_A_taf code - numbers don't look right..Looking at those who are NA for wave_A_taf, their 'startDate_wavesA_taf_ongoing' include these dates: 29th July, 18th December, 20th December. And their 'startDate_wavesA_taf' includes 30th June and 29th July...these all fall in follow up B?

##Follow up A and B can overlap in time, but follow up A can not overlap with the timing of another follow up A


#Should create wave variable within each dataset first
#Then merge by ID and waves (each participant should have ONE data entry for each wave, but multiple wave data entries)
#Consider then splitting by wave (i.e. so have 'TAF_WAVE1A', 'TAF_WAVE1B' etc etc)

taf.coping.followupa.raw.id <- 
  taf.coping.followupa.raw.id %>%
  mutate(wave_taf =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave 2a",
                                 startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave 3a",
                                 startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave 4a",
                                 startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave 5",
                                 startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave 7",
                                 startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave 9"))

taf.coping.followupb.raw.id <- 
  taf.coping.followupb.raw.id %>%
  mutate(wave_taf =  case_when(startDate_waves>= start4 & startDate_waves < end4 ~ ".Wave 2b",
                                 startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 3b",
                                 startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 4b",
                                 startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave 6",
                                 startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave 8"))


edeq.coping.followupb.raw.id <- 
  edeq.coping.followupb.raw.id %>%
  mutate(wave_edeq =  case_when(startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 2b",
                                  startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 3b",
                                  startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 4b",
                                  startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave 6",
                                  startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave 8"))

edeq.coping.followupb.screener.raw.id <- 
  edeq.coping.followupb.screener.raw.id %>%
  mutate(wave_edeq =  case_when(startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 2b",
                                startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 3b",
                                startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 4b",
                                startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave 6",
                                startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave 8"))
