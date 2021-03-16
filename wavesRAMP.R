

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
