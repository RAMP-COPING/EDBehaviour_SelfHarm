## Create wave column in RAMP


## Roughly by month.Some day discerpancies from pure calendar month to make sure there is no confusion with 
### survey send out dates

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
