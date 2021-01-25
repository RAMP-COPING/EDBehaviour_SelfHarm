edeq.coping.followupb.raw.id

##COPING EDEQ- 1.Dietary restriction 
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.deliberately_limit_amount_influence,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                             DupIDdat$`Wave 3b` > 1 |
                              DupIDdat$`Wave 4b` > 1 |
                               DupIDdat$`Wave 6` > 1 |
                            DupIDdat$`Wave 8` > 1 )]
  
dupids <- unique(dupids)
  
##Now drop from  data
  
edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]
  
##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ- 2. Preoccupation
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.calories_made_body_shape,
                values_fn = length)
  
  dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                             DupIDdat$`Wave 3b` > 1 |
                              DupIDdat$`Wave 4b` > 1 |
                               DupIDdat$`Wave 6` > 1 |
                            DupIDdat$`Wave 8` > 1 )]
  
dupids <- unique(dupids)
  
##Now drop from  data
  
edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]
  
##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ- 3. Weight concern
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.weight_influenced_person,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                             DupIDdat$`Wave 3b` > 1 |
                              DupIDdat$`Wave 4b` > 1 |
                               DupIDdat$`Wave 6` > 1 |
                            DupIDdat$`Wave 8` > 1 )]
  
dupids <- unique(dupids)
  
##Now drop from  data
  
edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]
  
##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ- 4. Body dissatisfaction
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.dissatisfied_shape_weight,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  5. Distress
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.eating_behaviours_distressed_concerns,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  6. Vigilance about weight or shape
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.actively_been_monitoring_your_shape_or_weight,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  7. Binge eating with a loss of control
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.regard_lost_control_eaten,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  8. Self-induced vomiting
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.sick_means_made_shape,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  9. Purging behaviour
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.diuretics_laxatives_means_shape,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  10. Excessive exercise
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.compulsive_driven_burn_calories,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  11. Grazing
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.nibbled_picked_snacks_grazed,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  12. Emotions leading to increased eating
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.felt_anxious_blue_bored,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}

##COPING EDEQ -  13. Emotions leading to decreased eating
##Remove duplicate IDs
if(COPING == TRUE) {
  DupIDdat <- edeq.coping.followupb.raw.id %>%
    pivot_wider(id_cols = ID,
                names_from = wave_edeq,
                values_from = edeq.felt_anxious_blue_bored.1,
                values_fn = length)
  
dupids <- DupIDdat$ID[( DupIDdat$`Wave 2b` > 1 |
                           DupIDdat$`Wave 3b` > 1 |
                            DupIDdat$`Wave 4b` > 1 |
                             DupIDdat$`Wave 6` > 1 |
                          DupIDdat$`Wave 8` > 1 )]

dupids <- unique(dupids)

##Now drop from  data

edeq.coping.followupb.raw.id <- edeq.coping.followupb.raw.id[(edeq.coping.followupb.raw.id$ID %!in% dupids),]

##Check number of rows
nrow(edeq.coping.followupb.raw.id)
}