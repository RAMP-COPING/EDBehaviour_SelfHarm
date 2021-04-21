
### Controls
##EDGI 
#These individuals have no self report diagnosis of any kind that we measure.

if(EDGI == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control_numeric =
        if_else(
          depressive_disorders_numeric == "0" &
            anxiety_disorders_numeric == "0" &
            eating_disorders_numeric == "0" &
            obsessive_compulsive_disorders_numeric == "0" &
            psychotic_disorders_numeric == "0" &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == "0" &
            mhd.posttraumatic_stress_disorder_ptsd_numeric == "0" &
            autism_spectrum_disorder_numeric == "0" &
            mhd.attention_deficit_hyperactivity_disorder_numeric == "0" &
            mhd.personality_disorder_numeric == "0" &
            mhd.dont_know_eating_disorder_numeric  == "0" & #**Need to ask why these are here??
            mhd.prefer_not_to_answer_eating_disorder_numeric  == "0" &
            mhd.dont_know_numeric  == "0" &
            mhd.prefer_not_to_answer_numeric  == "0" &
            mhd.dont_know.1_numeric == "0" &
            mhd.prefer_not_to_answer.1_numeric  == "0" &
            (mhd.none_of_the_above.1_numeric == "1" | is.na(mhd.none_of_the_above.1_numeric)) &
            (mhd.none_of_the_above.2_numeric == "1" | is.na(mhd.none_of_the_above.2_numeric)) &
            (mhd.none_of_the_above_eating_disorder_numeric == "1" | is.na(mhd.none_of_the_above_eating_disorder_numeric)), #Check this - this would mean 'controls' may have answered 'None of the above' to all but one, in which the remaining one is NA. Does this really make them a control?
          true = 1,
          false = 0,
          NA_real_)
    )
  
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control =
        recode_factor(control_numeric,
                      "0" = "No control",
                      "1" = "Control",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(control,
         cumul = F)
}



### Controls
## NBR controls **NEED to check this for NBR - getting no controls?
#first need to recode PTSD variable

if(NBR == TRUE) {
  
  #change ptsd variable to numeric [wrongly coded] 2 == DIAGNOSIS OF PTSD, 1 == NO DIAGNOSIS OF PTSD
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.posttraumatic_stress_disorder_ptsd_numeric =
        case_when(
          mhd.posttraumatic_stress_disorder_ptsd_numeric == "Not PTSD" ~ 0,
          mhd.posttraumatic_stress_disorder_ptsd_numeric == "PTSD" ~ 1,
          is.na(mhd.posttraumatic_stress_disorder_ptsd_numeric) ~ NA_real_))
}


#These individuals have no self report diagnosis of any kind that we measure.

if(NBR == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control_numeric =
        if_else(
          depressive_disorders_numeric == "0" &
            anxiety_disorders_numeric == "0" &
            eating_disorders_numeric == "0" &
            obsessive_compulsive_disorders_numeric == "0" &
            psychotic_disorders_numeric == "0" &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == "0" &
            mhd.posttraumatic_stress_disorder_ptsd_numeric == "0" &
            autism_spectrum_disorder_numeric == "0" &
            mhd.attention_deficit_hyperactivity_disorder_numeric == "0" &
            mhd.personality_disorder_numeric == "0" &
            mhd.dont_know_eating_disorder_numeric  == "0" & #**Need to ask why these are here??
            mhd.prefer_not_to_answer_eating_disorder_numeric  == "0" &
            mhd.dont_know_numeric  == "0" &
            mhd.prefer_not_to_answer_numeric  == "0" &
            mhd.dont_know.1_numeric == "0" &
            mhd.prefer_not_to_answer.1_numeric  == "0" &
            (mhd.none_of_the_above.1_numeric == "1" | is.na(mhd.none_of_the_above.1_numeric)) &
            (mhd.none_of_the_above.2_numeric == "1" | is.na(mhd.none_of_the_above.2_numeric)) &
            (mhd.none_of_the_above_eating_disorder_numeric == "1" | is.na(mhd.none_of_the_above_eating_disorder_numeric)), #Check this - this would mean 'controls' may have answered 'None of the above' to all but one, in which the remaining one is NA. Does this really make them a control?
          true = 1,
          false = 0,
          NA_real_)
    )
  
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control =
        recode_factor(control_numeric,
                      "0" = "No control",
                      "1" = "Control",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(control,
         cumul = F)
}



if(EDGI == TRUE | NBR == TRUE) {
  mhd.raw.id %>%
    group_by(control) %>%
    count(
      mhd.none_of_the_above.1,
      mhd.none_of_the_above.2,
      mhd.none_of_the_above_eating_disorder
    )
}

##GLAD controls
if(GLAD == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control_numeric =
        if_else(
          depressive_disorders_numeric == "0" &
            anxiety_disorders_numeric == "0" &
            eating_disorders_numeric == "0" &
            obsessive_compulsive_disorders_numeric == "0" &
            psychotic_disorders_numeric == "0" &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == "0" &
            mhd.posttraumatic_stress_disorder_ptsd_numeric == "0" &
            autism_spectrum_disorder_numeric == "0" &
            mhd.attention_deficit_hyperactivity_disorder_numeric == "0" &
            mhd.personality_disorder_numeric == "0" &
            mhd.dont_know_numeric == "0" &
            mhd.prefer_not_to_answer_numeric == "0" &
            mhd.dont_know.1_numeric == "0" &
            mhd.prefer_not_to_answer.1_numeric == "0" &
            mhd.dont_know_eating_disorder_numeric == "0" &
            mhd.prefer_not_to_answer_eating_disorder_numeric == "0" &
            mhd.none_of_the_above.1_numeric != "0" &
            mhd.none_of_the_above.2_numeric != "0" &
            mhd.none_of_the_above.3_numeric != "0" &
            mhd.none_of_the_above.4_numeric != "0" &
            mhd.none_of_the_above_eating_disorder_numeric != "0", #Check this - this would mean 'controls' may have answered 'None of the above' to all but one, in which the remaining one is NA. Does this really make them a control?
          true = 1,
          false = 0,
          NA_real_)
    )
  
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control =
        recode_factor(control_numeric,
                      "0" = "No control",
                      "1" = "Control",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(control,
         cumul = F)
}


if(GLAD == TRUE) {
  mhd.raw.id %>%
    select(
      control,
      mhd.none_of_the_above.1,
      mhd.none_of_the_above.2,
      mhd.none_of_the_above.3,
      mhd.none_of_the_above.4,
      mhd.none_of_the_above_eating_disorder
    )
  mhd.raw.id %>%
    group_by(control) %>%
    count(
      mhd.none_of_the_above.1,
      mhd.none_of_the_above.2,
      mhd.none_of_the_above.3,
      mhd.none_of_the_above.4,
      mhd.none_of_the_above_eating_disorder 
    )
}


##RAMP controls
if(RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control_numeric =
        if_else(
          depressive_disorders_numeric == "0" &
            anxiety_disorders_numeric == "0" &
            eating_disorders_numeric == "0" &
            obsessive_compulsive_disorders_numeric == "0" &
            psychotic_disorders_numeric == "0" &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == "0" &
            mhd.posttraumatic_stress_disorder_ptsd_numeric == "0" &
            autism_spectrum_disorder_numeric == "0" &
            mhd.attention_deficit_hyperactivity_disorder_numeric == "0" &
            mhd.personality_disorder_numeric == "0" &
            mhd.dont_know_numeric == "0" &
            mhd.prefer_not_to_answer_numeric == "0" &
            mhd.dont_know.1_numeric == "0" &
            mhd.prefer_not_to_answer.1_numeric == "0" &
            mhd.dont_know_numeric == "0" &
            mhd.prefer_not_to_answer_numeric == "0" &
            mhd.dont_know.1_numeric == "0" &
            mhd.prefer_not_to_answer.1_numeric == "0" &
            mhd.none_of_the_above_numeric  != "0" &
            mhd.none_of_the_above.1_numeric != "0",
          true = 1,
          false = 0,
          NA_real_)
    )
  
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      control =
        recode_factor(control_numeric,
                      "0" = "No control",
                      "1" = "Control",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(control,
         cumul = F)
}


if(RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      control,
      mhd.none_of_the_above,
      mhd.none_of_the_above.1
    )
  mhd.raw.id %>%
    group_by(control) %>%
    count(
      mhd.none_of_the_above,
      mhd.none_of_the_above.1
    )
}


#**Check - does this work with EDGI? NBR?

if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id %>%
    select(
      control,
      depressive_disorders_numeric,
      anxiety_disorders_numeric,
      eating_disorders_numeric,
      obsessive_compulsive_disorders_numeric,
      psychotic_disorders_numeric,
      mhd.mania_hypomania_bipolar_or_manicdepression_numeric,
      mhd.posttraumatic_stress_disorder_ptsd_numeric,
      autism_spectrum_disorder_numeric,
      mhd.attention_deficit_hyperactivity_disorder_numeric,
      mhd.personality_disorder_numeric
    )
}

