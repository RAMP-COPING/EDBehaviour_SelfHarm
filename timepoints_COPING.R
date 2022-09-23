
## Create wave column in COPING

## End dates here are NOT 'expiry' dates, because some participants requested extra time and for those who were sent questionnaires by text message, no expiration date could be set. For the purposes of waves, I will effectively ignore expiration dates to capture these people, and instead use the start date of the next timepoint as the end date for the previous wave. Code should be inclusive of the start date but NOT the end date, i.e., should mean that we capture people who filled in the questionnaire ON the start date (all must be BEFORE the end date, not ON the end date)
 
## Follow up A and B can overlap in time, but follow up A can not overlap with the timing of another follow up A

### FORTNIGHTLY; All fortnightly waves are labelled 1a, 1b, 2a, 2b, etc. Thus, all waves labelled under one number (e.g. 1a and 1b) represent one month, which corresponds to the labelling of the monthly waves (wave 5a, wave 6b, etc etc). The letters indicate the questionnaire that got sent out at that time.

# Wave 1a and 1b do not exist in COPING as it started a bit later than RAMP

# Wave 2a, follow up A = 19th May - 2nd June
# Wave 2b, follow up B = 2nd June - 16th June 
# Wave 3a, follow up A = 16th June - 30th June 
# Wave 3b, follow up B = 30th June - 14th July 
# Wave 4a, follow up A = 14th July - 28th July 
# Wave 4b, follow up B = 28th July - 25th August 

### MONTHLY

# Wave 5a, follow up A = 25th August- 22nd Sept 
# Wave 6b, follow up B = 22nd Sept - 20th Oct 
# Wave 7a, follow up A = 20th Oct - 17th Nov 
# Wave 8b, follow up B, = 17th Nov - 15th Dec 

#**Diverging timepoints from here onwards*** I am using COPING timepoints; RAMP participants with duplicate entries in a single timepoint will get collapsed into a single entry, where 1 trumps 0, i.e., 0 & 0 = 0, 1 & 0 = 1, 1 & 1 = 1

# Wave 9a, follow up A = 15th Dec - 12th Jan 
# Wave 10b, follow up B = 12TH Jan - 9TH Feb
# Wave 11a, follow up A = 9th Feb - 9th March
# Wave 12b, follow up B = 9th March - 6th April


# MAY 2 (wave 2a) - FOLLOW UP A
start3 <- as.POSIXct("2020-05-19")
end3 <-  as.POSIXct("2020-06-02")  

# JUNE 1 (wave 2b) - FOLLOW UP B ## COPING STARTS HERE
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
end7 <-  as.POSIXct("2020-07-28") 

# JULY - AUG (wave 4b) - FOLLOW UP B
start8 <- as.POSIXct("2020-07-28")
end8 <-  as.POSIXct("2020-08-25") 

# AUG - SEPT (wave 5a) - FOLLOW UP A
start9 <- as.POSIXct("2020-08-25")
end9 <-  as.POSIXct("2020-09-22") 

# SEPT - OCT (wave 6b) - FOLLOW UP B
start10 <- as.POSIXct("2020-09-22")
end10 <-  as.POSIXct("2020-10-20") 

# OCT - NOV (wave 7a) - FOLLOW UP A
start11 <- as.POSIXct("2020-10-20")
end11 <-  as.POSIXct("2020-11-17") 

# NOV - DEC (wave 8b) - FOLLOW UP B
start12 <- as.POSIXct("2020-11-17")
end12 <-  as.POSIXct("2020-12-15") 

# DEC - JAN (wave 9a) - FOLLOW UP A
start13 <- as.POSIXct("2020-12-15")
end13 <-  as.POSIXct("2021-01-12") 

# JAN - FEB (wave 10b) - FOLLOW UP B
start14 <- as.POSIXct("2021-01-12")
end14 <-  as.POSIXct("2021-02-09") 

# FEB - MARCH (wave 11a) - FOLLOW UP A
start15 <- as.POSIXct("2021-02-09")
end15 <-  as.POSIXct("2021-03-09") 

# MARCH - APRIL (wave 11a) - FOLLOW UP B
start16 <- as.POSIXct("2021-03-09")
end16 <-  as.POSIXct("2021-04-06")


# NEW TIMEPOINTS AS OF 20/10/21
# APRIL - MAY (wave 13a) - FOLLOW UP A
start17 <- as.POSIXct("2021-04-06")
end17 <-  as.POSIXct("2021-05-04")

# MAY - JUNE (wave 14b) - FOLLOW UP B
start18 <- as.POSIXct("2021-05-04")
end18 <-  as.POSIXct("2021-06-01")

# JUNE - LATE JUNE (wave 15a) - FOLLOW UP A
start19 <- as.POSIXct("2021-06-01")
end19 <-  as.POSIXct("2021-06-29")

# LATE JUNE - JULY (wave 16b) - FOLLOW UP B
start20 <- as.POSIXct("2021-06-29")
end20 <-  as.POSIXct("2021-07-27")


# Should create wave variable within each dataset first

# Then merge by ID and waves (each participant should have ONE data entry for each wave, but multiple wave data entries). Duplicated entires will be collapsed (as explained above)

#Wave entries labelled with 'QA' at the end means someone has filled in an A questionnaire in a 'B' timeframe (e.g., filled it in late or requested an extension). What is important is the timeframe in which they filled out the questionnaire NOT whether the questionnaire is A or B, so we still need to capture these people. If we don't include this, we would lose these data 

taf.coping.followupa.raw.id <-  taf.coping.followupa.raw.id %>%
  
  mutate(wave_taf =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                               
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                               
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                               
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                               
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                               
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                               
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                               
                               startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                               
                               startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                               
                               startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                               
                               startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                               
                               startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                               
                               startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                               
                               startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                               
                               startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                               
                               startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                               
                               startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                               
                               startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                               
                               startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))

taf.coping.followupb.raw.id <- taf.coping.followupb.raw.id %>%
  
  mutate(wave_taf =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                               
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                               
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                               
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                               
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                               
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                               
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                               
                               startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                               
                               startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                               
                               startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                               
                               startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                               
                               startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                               
                               startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                               
                               startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                               
                               startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                               
                               startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                               
                               startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                               
                               startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                               
                               startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))

# NB The EDEQ questionnaire is only in follow up B
## I will use the end date of the A questionnaire in-between to capture people who answered follow up B late (i.e., during an A timepoint)
edeq.coping.followupb.screener.raw.id <- edeq.coping.followupb.screener.raw.id %>%
 
   mutate(wave_edeq =  case_when(
                               startDate_waves >= start4 & startDate_waves < end5 ~ ".Wave_2b",
                                
                                startDate_waves >= start6 & startDate_waves < end7 ~ ".Wave_3b",
                                
                                startDate_waves >= start8 & startDate_waves < end9 ~ ".Wave_4b",
                                
                                startDate_waves >= start10 & startDate_waves < end11 ~ ".Wave_6b",
                                
                                startDate_waves >= start12 & startDate_waves < end13 ~ ".Wave_8b",
                                
                                startDate_waves >= start14 & startDate_waves < end15 ~ ".Wave_10b",
                                
                                startDate_waves >= start16 & startDate_waves < end17 ~ ".Wave_12b",
                             
                             startDate_waves >= start18 & startDate_waves < end19 ~ ".Wave_14b",
                             
                             startDate_waves >= start20 & startDate_waves < end21 ~ ".Wave_16b"
                            
                             ))


# NB The VIRUS questionnaire is only in follow up B
## I will use the end date of the A questionnaire in-between to capture people who answered follow up B late (i.e., during an A timepoint)
coping.followupb.virus.id <- coping.followupb.virus.id %>%
  mutate(wave_virus =  case_when(
    startDate_waves >= start4 & startDate_waves < end5 ~ ".Wave_2b",
    
    startDate_waves >= start6 & startDate_waves < end7 ~ ".Wave_3b",
    
    startDate_waves >= start8 & startDate_waves < end9 ~ ".Wave_4b",
    
    startDate_waves >= start10 & startDate_waves < end11 ~ ".Wave_6b",
    
    startDate_waves >= start12 & startDate_waves < end13 ~ ".Wave_8b",
    
    startDate_waves >= start14 & startDate_waves < end15 ~ ".Wave_10b",
    
    startDate_waves >= start16 & startDate_waves < end17 ~ ".Wave_12b",
    
    startDate_waves >= start18 & startDate_waves < end19 ~ ".Wave_14b",
    
    startDate_waves >= start20 & startDate_waves < end21 ~ ".Wave_16b"
    
    ))



coping.dem.followupa.raw.id <-  coping.dem.followupa.raw.id %>%
  
  mutate(wave_dem =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                               
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                               
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                               
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                               
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                               
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                               
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                               
                               startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                               
                               startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                               
                               startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                               
                               startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                               
                               startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                               
                               startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                               
                               startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                               
                               startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                               
                               startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                               
                               startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                               
                               startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                               
                               startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))

coping.dem.followupb.raw.id <- coping.dem.followupb.raw.id %>%
  
  mutate(wave_dem =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                               
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                               
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                               
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                               
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                               
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                               
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                               
                               startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                               
                               startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                               
                               startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                               
                               startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                               
                               startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                               
                               startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                               
                               startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                               
                               startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                               
                               startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                               
                               startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                               
                               startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                               
                               startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))

# NB The LOSS questionnaire is only in follow up B
## I will use the end date of the A questionnaire in-between to capture people who answered follow up B late (i.e., during an A timepoint)
coping.loss.followupb.raw.id <- coping.loss.followupb.raw.id %>%
  mutate(wave_loss =  case_when(
    startDate_waves >= start4 & startDate_waves < end5 ~ ".Wave_2b",
    
    startDate_waves >= start6 & startDate_waves < end7 ~ ".Wave_3b",
    
    startDate_waves >= start8 & startDate_waves < end9 ~ ".Wave_4b",
    
    startDate_waves >= start10 & startDate_waves < end11 ~ ".Wave_6b",
    
    startDate_waves >= start12 & startDate_waves < end13 ~ ".Wave_8b",
    
    startDate_waves >= start14 & startDate_waves < end15 ~ ".Wave_10b",
    
    startDate_waves >= start16 & startDate_waves < end17 ~ ".Wave_12b",
    
    startDate_waves >= start18 & startDate_waves < end19 ~ ".Wave_14b",
    
    startDate_waves >= start20 & startDate_waves < end21 ~ ".Wave_16b"
    ))



coping.followupa.resp.id <-  coping.followupa.resp.id %>%
  
  mutate(wave_resp =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                               
                               startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                               
                               startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                               
                               startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                               
                               startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                               
                               startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                               
                               startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                               
                               startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                               
                               startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                               
                               startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                               
                               startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                               
                               startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                               
                               startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                               
                               startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                               
                               startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                               
                               startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                               
                               startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                               
                               startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                               
                               startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))


coping.followupb.resp.id <-  coping.followupb.resp.id %>%
  
  mutate(wave_resp =  case_when(startDate_waves >= start3 & startDate_waves < end3 ~ ".Wave_2a",
                                
                                startDate_waves >= start4 & startDate_waves < end4 ~ ".Wave_2b",
                                
                                startDate_waves >= start5 & startDate_waves < end5 ~ ".Wave_3a",
                                
                                startDate_waves >= start6 & startDate_waves < end6 ~ ".Wave_3b",
                                
                                startDate_waves >= start7 & startDate_waves < end7 ~ ".Wave_4a",
                                
                                startDate_waves >= start8 & startDate_waves < end8 ~ ".Wave_4b",
                                
                                startDate_waves >= start9 & startDate_waves < end9 ~ ".Wave_5a",
                                
                                startDate_waves >= start10 & startDate_waves < end10 ~ ".Wave_6b",
                                
                                startDate_waves >= start11 & startDate_waves < end11 ~ ".Wave_7a",
                                
                                startDate_waves >= start12 & startDate_waves < end12 ~ ".Wave_8b",
                                
                                startDate_waves >= start13 & startDate_waves < end13 ~ ".Wave_9a",
                                
                                startDate_waves >= start14 & startDate_waves < end14 ~ ".Wave_10b",
                                
                                startDate_waves >= start15 & startDate_waves < end15 ~ ".Wave_11a",
                                
                                startDate_waves >= start16 & startDate_waves < end16 ~ ".Wave_12b",
                                
                                startDate_waves >= start17 & startDate_waves < end17 ~ ".Wave_13a",
                                
                                startDate_waves >= start18 & startDate_waves < end18 ~ ".Wave_14b",
                                
                                startDate_waves >= start19 & startDate_waves < end19 ~ ".Wave_15a",
                                
                                startDate_waves >= start20 & startDate_waves < end20 ~ ".Wave_16b",
                                
                                startDate_waves >= start21 & startDate_waves < end21 ~ ".Wave_17a"
  ))