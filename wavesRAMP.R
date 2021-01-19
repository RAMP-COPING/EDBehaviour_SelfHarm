## Create wave column in RAMP

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

# June
start2 <- as.POSIXct("2020-06-01")
end2 <-  as.POSIXct("2020-06-29")  ## both RAMP and COPING ended by now, new RAMP started at 30 June

#July
start3 <- as.POSIXct("2020-06-30")
end3 <-  as.POSIXct("2020-07-27")

#August
start4 <- as.POSIXct("2020-07-28")
end4 <-  as.POSIXct("2020-09-01")

#September
start5 <- as.POSIXct("2020-09-01")
end5 <-  as.POSIXct("2020-10-01")

#October
start6 <- as.POSIXct("2020-10-01")
end6 <-   as.POSIXct("2020-11-01") 

#November
start7 <- as.POSIXct("2020-11-01")
end7 <-   as.POSIXct("2020-12-01") 

#December
start8 <- as.POSIXct("2020-12-01")
end8 <-   as.POSIXct("2021-01-01") 

dat.raw <- 
  dat.raw %>%
  mutate(wave_A_taf =  case_when(startDate_wavesA_taf >= start2 & startDate_wavesA_taf < end2 ~ "June 2020",
                           startDate_wavesA_taf >= start3 & startDate_wavesA_taf < end3 ~ "July 2020",
                           startDate_wavesA_taf >= start4 & startDate_wavesA_taf < end4 ~ "August 2020",
                           startDate_wavesA_taf >= start5 & startDate_wavesA_taf < end5 ~ "September 2020",
                           startDate_wavesA_taf >= start6 & startDate_wavesA_taf < end6 ~ "October 2020",
                           startDate_wavesA_taf >= start7 & startDate_wavesA_taf < end7 ~ "November 2020",
                           startDate_wavesA_taf >= start8 & startDate_wavesA_taf < end8 ~ "December 2020"))
         
dat.raw <- 
  dat.raw %>%
  mutate(wave_B_taf =  case_when(startDate_wavesB_taf >= start2 & startDate_wavesB_taf < end2 ~ "June 2020",
                                 startDate_wavesB_taf >= start3 & startDate_wavesB_taf < end3 ~ "July 2020",
                                 startDate_wavesB_taf >= start4 & startDate_wavesB_taf < end4 ~ "August 2020",
                                 startDate_wavesB_taf >= start5 & startDate_wavesB_taf < end5 ~ "September 2020",
                                 startDate_wavesB_taf >= start6 & startDate_wavesB_taf < end6 ~ "October 2020",
                                 startDate_wavesB_taf >= start7 & startDate_wavesB_taf < end7 ~ "November 2020",
                                 startDate_wavesB_taf >= start8 & startDate_wavesB_taf < end8 ~ "December 2020"))


dat.raw<- 
  dat.raw %>%
  mutate(wave_B_edeq =  case_when(startDate_wavesB_edeq >= start2 & startDate_wavesB_edeq < end2 ~ "June 2020",
                                 startDate_wavesB_edeq >= start3 & startDate_wavesB_edeq < end3 ~ "July 2020",
                                 startDate_wavesB_edeq >= start4 & startDate_wavesB_edeq < end4 ~ "August 2020",
                                 startDate_wavesB_edeq >= start5 & startDate_wavesB_edeq < end5 ~ "September 2020",
                                 startDate_wavesB_edeq >= start6 & startDate_wavesB_edeq < end6 ~ "October 2020",
                                 startDate_wavesB_edeq >= start7 & startDate_wavesB_edeq < end7 ~ "November 2020",
                                 startDate_wavesB_edeq >= start8 & startDate_wavesB_edeq < end8 ~ "December 2020"))
