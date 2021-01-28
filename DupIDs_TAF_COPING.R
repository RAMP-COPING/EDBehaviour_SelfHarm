

##COPING TAF- Life worth living (lifetime)

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
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 )]

  dupids1 <- unique(dupids)
  
  ##Now drop from  data
  
 # dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}

##COPING TAF - Life worth living (2 weeks)
###Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- dat.raw.taf.coping %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.felt_weeks_past,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 )]
                      
  
  dupids2 <- unique(dupids)
  
  ##Now drop from  data
  
 # dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}


##COPING TAF - Contemplated harming self (lifetime)
###Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- dat.raw.taf.coping %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.have_you_contemplated_harming_yourself,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 )]
                         
  
  dupids3 <- unique(dupids)
  
  ##Now drop from  data
  
 # dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}

##COPING TAF - Contemplated harming self (2 weeks)
###Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- dat.raw.taf.coping %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.felt_weeks_past.1,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 )]
  
  dupids4 <- unique(dupids)
  
  ##Now drop from  data
  
 # dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}

##COPING TAF - Deliberately harmed self (2 weeks)
###Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- dat.raw.taf.coping %>%
    pivot_wider(id_cols = ID,
                names_from = wave_taf,
                values_from = taf.meant_life_end_weeks,
                values_fn = length)
  
  dupids <- DupIDdat$ID[(DupIDdat$`Wave 2a` > 1 | DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3a` > 1 | DupIDdat$`Wave 3b` > 1 |
                           DupIDdat$`Wave 4a` > 1 | DupIDdat$`Wave 4b` > 1 |
                           DupIDdat$`Wave 5` > 1 | DupIDdat$`Wave 6` > 1 |
                           DupIDdat$`Wave 7` > 1 | DupIDdat$`Wave 8` > 1 )]
  
  dupids5 <- unique(dupids)
  
  ##Now drop from  data
  
  #dat.raw.taf.coping <- dat.raw.taf.coping[(dat.raw.taf.coping$ID %!in% dupids),]
  
  ##Check number of rows
  nrow(dat.raw.taf.coping)
}