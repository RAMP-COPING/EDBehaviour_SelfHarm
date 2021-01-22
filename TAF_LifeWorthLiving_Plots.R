
#PLOTTING
#First, Remove IDs that have multiple data for one wave RAMP for each variable, i.e. one person should not have multiple data for one wave [e.g. duplicated ID in wave 1a dataset], but one person will have multiple wave data [e.g. wave 1a, wave 1b, etc])

##RAMP - Life worth living
###Remove duplicate IDs
if(RAMP == TRUE) {
  DupIDdat <- dat.raw.taf.ramp %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.worth_living_life_people,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 1a` > 1 | DupIDdat$`Wave 1b` > 1 |
                           DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 |
                           DupIDdat$`Wave 9` > 1)]
  
  dupids <- unique(dupids)
  
  ##Now drop from  data
  
  dat.raw.taf.ramp <- dat.raw.taf.ramp[(dat.raw.taf.ramp$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.ramp)
}

##COPING - Life worth living
###Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- dat.raw.taf.coping %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.worth_living_life_people,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 |
                           DupIDdat$`Wave 9` > 1)]
  
  dupids <- unique(dupids)
  
  ##Now drop from  data
  
  dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}



##Plot by wave and question:  RAMP - Life worth living (lifetime)
if(RAMP == TRUE) {
ggplot(dat.raw.taf.ramp,
       aes(taf.worth_living_life_people, 
           fill = wave_taf)) +
  geom_bar(size = 1,
           shape = 23,
           colour = "black",
           position = position_dodge()) +
  labs(y = "Number of participants",
       x = "RAMP: Many people have thoughts that life is not worth living. Have you felt that way?",
       title = "RAMP: Life not worth living (lifetime)")
}


##Plot by wave and question:  RAMP - Life worth living (lifetime)
if(COPING == TRUE) {
ggplot(dat.raw.taf.coping,
       aes(taf.worth_living_life_people, 
           fill = wave_taf)) +
  geom_bar(size = 1,
           shape = 23,
           colour = "black",
           position = position_dodge()) +
  labs(y = "Number of participants",
       x = "RAMP: Many people have thoughts that life is not worth living. Have you felt that way?",
       title = "COPING: Life not worth living (lifetime)")
}
