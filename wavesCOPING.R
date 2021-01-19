## Create wave column in COPING

##FORTNIGHTLY:
#Wave 1, follow up A = 19th May - 26th May ##May1
#Wave 2, follow up B = 2nd June - 9th June (?) ##June1
#Wave 3, follow up A = 16th June - 23 June at 5PM (Set A_ongoing) or Friday, 26 June (setA_1) ##June2
#Wave 4, follow up B = 30th June - 7th July ##July1
#Wave 5, follow up A = 14th July - 21st July ##July2
#Wave 6, follow up B = 28th July - 4th August ##July_Aug

##MONTHLY
#Wave 7, follow up A = 25th August- 15th Sept ##August-September
#Wave 8, follow up B = 22nd Sept - 13th Oct ##September-October
#Wave 9, follow up A = 20th Oct - 10th Nov ##October-November
#Wave 10, follow up B, = 17th Nov - 8th Dec ##November - December
#WAVE 11, follow up A = 15th Dec - 5th Jan ##December - Jan
#(Note that waves continue but data extracted in Dec)

#**EDEQ only in follow up B, TAF in both A and B

# May1
start1 <- as.POSIXct("2020-05-19")
end1 <- as.POSIXct("2020-05-26")

#June1
start2 <- as.POSIXct("2020-06-02")
end2 <-  as.POSIXct("2020-06-09")  

#June2
start3 <- as.POSIXct("2020-06-16")
end3 <-  as.POSIXct("2020-06-23")

#July1
start4 <- as.POSIXct("2020-06-30")
end4 <-  as.POSIXct("2020-07-07")

#July2
start5 <- as.POSIXct("2020-07-14")
end5 <-  as.POSIXct("2020-07-21")

#July_Aug
start6 <- as.POSIXct("2020-07-28")
end6 <-   as.POSIXct("2020-08-04") 

#Aug_Sept
start7 <- as.POSIXct("2020-08-25")
end7 <-   as.POSIXct("2020-09-15") 

#Sept_Oct
start8 <- as.POSIXct("2020-09-22")
end8 <-   as.POSIXct("2021-10-13") 

#Oct_Nov
start9 <- as.POSIXct("2020-10-20")
end9 <-   as.POSIXct("2021-11-10") 

#Nov_Dec
start10 <- as.POSIXct("2020-11-17")
end10 <-   as.POSIXct("2021-12-08") 

#Dec_Jan
start11 <- as.POSIXct("2020-12-15")
end11 <-   as.POSIXct("2021-01-05") 


dat.raw.no.dup <- 
  dat.raw.no.dup %>%
  mutate(wave_A_taf =  case_when(startDate_wavesA_taf >= start2 & startDate_wavesA_taf < end2 ~ "June 2020",
                                 startDate_wavesA_taf >= start3 & startDate_wavesA_taf < end3 ~ "July 2020",
                                 startDate_wavesA_taf >= start4 & startDate_wavesA_taf < end4 ~ "August 2020",
                                 startDate_wavesA_taf >= start5 & startDate_wavesA_taf < end5 ~ "September 2020",
                                 startDate_wavesA_taf >= start6 & startDate_wavesA_taf < end6 ~ "October 2020",
                                 startDate_wavesA_taf >= start7 & startDate_wavesA_taf < end7 ~ "November 2020",
                                 startDate_wavesA_taf >= start8 & startDate_wavesA_taf < end8 ~ "December 2020"))

dat.raw.no.dup <- 
  dat.raw.no.dup %>%
  mutate(wave_B_taf =  case_when(startDate_wavesB_taf >= start2 & startDate_wavesB_taf < end2 ~ "June 2020",
                                 startDate_wavesB_taf >= start3 & startDate_wavesB_taf < end3 ~ "July 2020",
                                 startDate_wavesB_taf >= start4 & startDate_wavesB_taf < end4 ~ "August 2020",
                                 startDate_wavesB_taf >= start5 & startDate_wavesB_taf < end5 ~ "September 2020",
                                 startDate_wavesB_taf >= start6 & startDate_wavesB_taf < end6 ~ "October 2020",
                                 startDate_wavesB_taf >= start7 & startDate_wavesB_taf < end7 ~ "November 2020",
                                 startDate_wavesB_taf >= start8 & startDate_wavesB_taf < end8 ~ "December 2020"))


dat.raw.no.dup <- 
  dat.raw.no.dup %>%
  mutate(wave_B_edeq =  case_when(startDate_wavesB_edeq >= start2 & startDate_wavesB_edeq < end2 ~ "June 2020",
                                  startDate_wavesB_edeq >= start3 & startDate_wavesB_edeq < end3 ~ "July 2020",
                                  startDate_wavesB_edeq >= start4 & startDate_wavesB_edeq < end4 ~ "August 2020",
                                  startDate_wavesB_edeq >= start5 & startDate_wavesB_edeq < end5 ~ "September 2020",
                                  startDate_wavesB_edeq >= start6 & startDate_wavesB_edeq < end6 ~ "October 2020",
                                  startDate_wavesB_edeq >= start7 & startDate_wavesB_edeq < end7 ~ "November 2020",
                                  startDate_wavesB_edeq >= start8 & startDate_wavesB_edeq < end8 ~ "December 2020"))
