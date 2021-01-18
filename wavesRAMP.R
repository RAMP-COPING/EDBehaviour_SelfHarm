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

dat.raw.no.dup <- 
  dat.raw.no.dup %>%
  mutate(wave =  case_when(startDate >= start2 & startDate < end2 ~ "June 2020",
                           startDate >= start3 & startDate < end3 ~ "July 2020",
                           startDate >= start4 & startDate < end4 ~ "August 2020",
                           startDate >= start5 & startDate < end5 ~ "September 2020",
                           startDate >= start6 & startDate < end6 ~ "October 2020",
                           startDate >= start7 & startDate < end7 ~ "November 2020",
                           startDate >= start8 & startDate < end8 ~ "December 2020")
