

###FORTNIGHTLY
##First 2 waves are not in COPING
#Wave 1, follow up A = 21st april 
#Wave 2, follow up B = 5th May

##End dates here are 'expiry' dates, however some participants requested extra time and for those who were sent questionnaires by text message, no expiration date could be set. For the purposes of waves,I will effectively ignore expiration dates to capture these people.

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

##All fortnightly waves are labelled 1a, 1b, 2a, 2b, etc. Thus, all waves labelled under one number (e.g. 1a and 1b) represent one month, which corresponds to the labelling of the monthly waves (wave 5, wave 6, etc etc) which are not split into 'a' and 'b'

## APRIL 1
start1 <- as.POSIXct("2020-04-01")
end1 <-  as.POSIXct("2020-04-30") 

# MAY 2
start2 <- as.POSIXct("2020-05-01")
end2 <-  as.POSIXct("2020-05-31")  ## both RAMP and COPING ended by now, new RAMP started at 30 June

# JUNE 3 
start2 <- as.POSIXct("2020-06-01")
end2 <-  as.POSIXct("2020-06-30")  ## both RAMP and COPING ended by now, new RAMP started at 30 June

#July 4
start3 <- as.POSIXct("2020-06-30")
end3 <-  as.POSIXct("2020-07-27")

#August 5
start4 <- as.POSIXct("2020-07-28")
end4 <-  as.POSIXct("2020-09-01")

#September 6
start5 <- as.POSIXct("2020-09-01")
end5 <-  as.POSIXct("2020-10-01")

#October 7
start6 <- as.POSIXct("2020-10-01")
end6 <-   as.POSIXct("2020-11-01") 

#November 8
start7 <- as.POSIXct("2020-11-01")
end7 <-   as.POSIXct("2020-12-01") 

#December 9 
start8 <- as.POSIXct("2020-12-01")
end8 <-   as.POSIXct("2021-01-01") 

#January 10
start9 <- as.POSIXct("2021-01-01")
end9 <-   as.POSIXct("2021-02-01") 


##Follow up A and B can overlap in time, but follow up A can not overlap with the timing of another follow up A


#Should create wave variable within each dataset first
#Then merge by ID and waves (each participant should have ONE data entry for each wave, but multiple wave data entries)


taf.ramp.followupa.raw.id <-  taf.ramp.followupa.raw.id %>%
  mutate(wave_taf =  case_when(startDate_waves >= start1 & startDate_waves < end1 ~ ".Wave 1",
                               startDate_waves >= start2 & startDate_waves < end2 ~ ".Wave 2",
                               startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave 3",
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 4",
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave 5",
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 6",
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave 7",
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 8",
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave 9"))

taf.ramp.followupb.raw.id <- taf.ramp.followupb.raw.id %>%
  mutate(wave_taf =  case_when(startDate_waves >= start1 & startDate_waves < end1 ~ ".Wave 1",
                               startDate_waves >= start2 & startDate_waves < end2 ~ ".Wave 2",
                               startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave 3",
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 4",
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave 5",
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 6",
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave 7",
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 8",
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave 9"))


edeq.ramp.followupb.raw.id <- edeq.ramp.followupb.raw.id %>%
  mutate(wave_edeq =  case_when(startDate_waves >= start1 & startDate_waves < end1 ~ ".Wave 1",
                                startDate_waves >= start2 & startDate_waves < end2 ~ ".Wave 2",
                                startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave 3",
                                startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 4",
                                startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave 5",
                                startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 6",
                                startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave 7",
                                startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 8",
                                startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave 9"))

edeq.ramp.followupb.screener.raw.id <- edeq.ramp.followupb.screener.raw.id %>%
  mutate(wave_edeq =  case_when(startDate_waves >= start1 & startDate_waves < end1 ~ ".Wave 1",
                                startDate_waves >= start2 & startDate_waves < end2 ~ ".Wave 2",
                                startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave 3",
                                startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave 4",
                                startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave 5",
                                startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave 6",
                                startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave 7",
                                startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave 8",
                                startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave 9"))
