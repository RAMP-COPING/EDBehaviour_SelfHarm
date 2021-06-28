# MHD - MENTAL HEALTH DIAGNOSIS
# Columns to exclude from add_numeric
exclude_cols <- c("cohort_name",
                  "ID")

## GLAD
if(GLAD == TRUE) {
  glad.mhd.raw <- readRDS(file = "../data_raw/glad/mhd_glad.rds")
  dim(glad.mhd.raw)
  colnames(glad.mhd.raw)
}


if(GLAD == TRUE) {
  glad.mhd.raw.id <- glad.mhd.raw %>%
    drop_na(externalDataReference) %>% # Drop NAs
    distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    # mutate(ID = as.numeric(ID)) %>%
    add_column(cohort_name = "coping_glad", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      #startDate_prepandemic = startDate,
      starts_with("mhd."),
      mhd.depression_signup = mhd.mdd,
      mhd.pmdd_signup = mhd.pmdd,
      mhd.bipolar_disorder_signup = mhd.bipolar_disorder,
      mhd.gad_signup = mhd.gad,
      mhd.social_anxiety_signup = mhd.social_anxiety,
      mhd.specific_phobia_signup = mhd.specific_phobia,
      mhd.agoraphobia_signup = mhd.agoraphobia,
      mhd.panic_disorder_signup = mhd.panic_disorder,
      mhd.panic_attacks_signup = mhd.panic_attacks,
      mhd.ptsd_signup = mhd.ptsd,
      mhd.ocd_signup = mhd.ocd,
      mhd.bdd_signup = mhd.bdd,
      mhd.other_ocd_signup = mhd.other_ocd,
      mhd.anorexia_signup = mhd.an,
      mhd.atypical_anorexia_signup = mhd.atypical_an,
      mhd.bulimia_signup = mhd.bn,
      mhd.bed_signup = mhd.bed,
      mhd.schizophrenia_signup = mhd.schizophrenia,
      mhd.psychosis_signup = mhd.psychosis,
      mhd.personality_disorder_signup = mhd.personality_disorder,
      mhd.asd_signup = mhd.asd,
      mhd.addadhd_signup = mhd.addadhd,
      mhd.other_signup = mhd.other,
      mhd.perinatal_depression, #Not in glad coping mhd (so don't need to make signup copy)
      mhd.schizoaffective_disorder = mhd.schizoaffective, #Not in glad coping mhd (so don't need to make signup copy)
      mhd.none_of_the_above.1 = mhd.none_of_the_above,
      mhd.none_of_the_above.2 = mhd.none_of_the_above.1
    ) %>%
    add_numeric(., exclude = exclude_cols) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(glad.mhd.raw.id)
  # Inspect colnames
  colnames(glad.mhd.raw.id)
  #Differences
  dim(glad.mhd.raw)[1]-dim(glad.mhd.raw.id)[1]
}

if(GLAD == TRUE) {
  mhd.raw.id <- glad.mhd.raw.id
}


## GLAD
if(GLAD == TRUE) {
  glad.coping.mhd.raw <- readRDS(file = "../data_raw/glad/mhd_coping_glad.rds")
  dim(glad.coping.mhd.raw)
  colnames(glad.coping.mhd.raw)
}


if(GLAD == TRUE) {
  glad.coping.mhd.raw.id <- glad.coping.mhd.raw  %>%
    drop_na(externalDataReference) %>% # Drop NAs
    distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    # mutate(ID = as.numeric(ID)) %>%
    add_column(cohort_name = "coping_glad", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      #startDate_baseline = startDate,
      mhd.depression_coping = mhd.depression,
      mhd.pregnancy_depression = mhd.postnatal_depression, #Only perinatal depression in glad signup so don't need to make coping copy
      mhd.pmdd_coping = mhd.pmdd,
      mhd.bdd_coping = mhd.bdd,
      mhd.ptsd_coping = mhd.ptsd,
      mhd.psychosis_coping = mhd.psychosis_type_psychotic_illness,
      mhd.personality_disorder_coping = mhd.personality_disorder,
      mhd.asd_coping = mhd.autism_spectrum_disorder_asd,
      mhd.addadhd_coping = mhd.attention_deficit_hyperactivity_disorder,
      mhd.other_coping = mhd.other,
      mhd.bipolar_disorder_coping = mhd.mania_hypomania_bipolar_or_manicdepression, 
      mhd.gad_coping = mhd.anxiety_nerves_or_generalised_anxiety_disorder,
      mhd.social_anxiety_coping = mhd.social_anxiety_or_social_phobia,
      mhd.specific_phobia_coping = mhd.specific_phobia,
      mhd.agoraphobia_coping = mhd.agoraphobia,
      mhd.panic_disorder_coping = mhd.panic_disorder,
      mhd.panic_attacks_coping = mhd.panic_attacks,
      mhd.schizophrenia_coping = mhd.schizophrenia,
      mhd.ocd_coping = mhd.obsessivecompulsive_disorder_ocd,
      mhd.other_ocd_coping = mhd.other_ocd,
      mhd.anorexia_coping = mhd.anorexia_nervosa,
      mhd.atypical_anorexia_coping = mhd.atypical_anorexia_nervosa,
      mhd.bulimia_coping = mhd.bulimia_nervosa,
      mhd.atypical_bulimia_nervosa_unc = mhd.atypical_bulimia_nervosa, #Not in glad signup so don't need to make coping copy
      mhd.bed_coping = mhd.bingeeating_disorder,
      mhd.atypical_bingeeating_disorder_unc = mhd.atypical_bingeeating_disorder, #Not in glad signup so don't need to make coping copy
      mhd.purging_disorder_unc = mhd.purging_disorder, #Not in glad signup so don't need to make coping copy
      mhd.night_eating_syndrome_unc = mhd.night_eating_syndrome, #Not in glad signup so don't need to make coping copy
      mhd.pica_unc = mhd.pica, #Not in glad signup so don't need to make coping copy
      mhd.avoidantrestrictive_food_intake_disorder_unc = mhd.avoidantrestrictive_food_intake_disorder, #Not in glad signup so don't need to make coping copy
      mhd.rumination_disorder_unc = mhd.rumination_disorder,  #Not in glad signup so don't need to make coping copy
      mhd.feeding_eating_disorder_unc = mhd.feeding_eating_disorder, #Not in glad signup so don't need to make coping copy
      mhd.other_eating_disorder_unc = mhd.other_eating_disorder, #Not in glad signup so don't need to make coping copy
      mhd.none_of_the_above_eating_disorder_unc = mhd.none, 
      mhd.dont_know_eating_disorder_unc = mhd.dont_know,
      mhd.prefer_not_to_answer_eating_disorder_unc = mhd.prefer_not_to_answer,
      mhd.none_of_the_above.3 = mhd.none_of_the_above,
      mhd.none_of_the_above.4 = mhd.none_of_the_above.1,
      mhd.suspected_eating_disorder_diagnosed
    ) %>%
    add_numeric(., exclude = exclude_cols) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(glad.coping.mhd.raw.id)
  # Inspect colnames
  colnames(glad.coping.mhd.raw.id)
  #Differences
  dim(glad.coping.mhd.raw)[1]-dim(glad.coping.mhd.raw.id)[1]
}

#Combine mhd datasets glad
if(GLAD == TRUE){
  mhd.raw.id <- left_join(
    x = glad.coping.mhd.raw.id,
    y = glad.mhd.raw.id,
    by = c("ID", "cohort_name")
  )
  # Inspect dimensions
  dim(mhd.raw.id)
  # Inspect colnames
  colnames(mhd.raw.id)
  
}

##GLAD + COPING, recoding
#For some disorders, participants are asked both in GLAD and COPING whether or not they have a mental health diagnosis. The code below captures anyone who endorsed a mental health disorder in at least one of these questions.

#Depression
if(GLAD == TRUE){
  
  #Depression
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.depression_numeric =
        case_when(
          (mhd.depression_coping_numeric  == "1" & mhd.depression_signup_numeric == "1" ~ 1),
          (mhd.depression_coping_numeric  == "1" | mhd.depression_signup_numeric == "1" ~ 1),
          (mhd.depression_coping_numeric  == "0" & mhd.depression_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.depression =
        recode_factor(mhd.depression_numeric,
                      "0" = "Not Depression",
                      "1" = "Depression",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.depression,
         cumul = F)
}

#PMDD
if(GLAD == TRUE){
  
  #Premenstrual dysphoric disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.premenstrual_dysphoric_disorder_pmdd_numeric =
        case_when(
          (mhd.pmdd_coping_numeric  == "1" & mhd.pmdd_signup_numeric == "1" ~ 1),
          (mhd.pmdd_coping_numeric  == "1" | mhd.pmdd_signup_numeric == "1" ~ 1),
          (mhd.pmdd_coping_numeric  == "0" & mhd.pmdd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.premenstrual_dysphoric_disorder_pmdd =
        recode_factor(mhd.premenstrual_dysphoric_disorder_pmdd_numeric,
                      "0" = "Not PMDD",
                      "1" = "PMDD",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.premenstrual_dysphoric_disorder_pmdd,
         cumul = F)
  
}


#BDD
if(GLAD == TRUE){
  
  #Body dysmorphic disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.body_dysmorphic_disorder_bdd_numeric =
        case_when(
          (mhd.bdd_coping_numeric  == "1" & mhd.bdd_signup_numeric == "1" ~ 1),
          (mhd.bdd_coping_numeric  == "1" | mhd.bdd_signup_numeric == "1" ~ 1),
          (mhd.bdd_coping_numeric  == "0" & mhd.bdd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.body_dysmorphic_disorder_bdd =
        recode_factor(mhd.body_dysmorphic_disorder_bdd_numeric,
                      "0" = "Not BDD",
                      "1" = "BDD",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.body_dysmorphic_disorder_bdd,
         cumul = F)
  
}


#PTSD
if(GLAD == TRUE){
  
  #Post traumatic stress disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.posttraumatic_stress_disorder_ptsd_numeric =
        case_when(
          (mhd.ptsd_coping_numeric  == "1" & mhd.ptsd_signup_numeric == "1" ~ 1),
          (mhd.ptsd_coping_numeric  == "1" | mhd.ptsd_signup_numeric == "1" ~ 1),
          (mhd.ptsd_coping_numeric  == "0" & mhd.ptsd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.posttraumatic_stress_disorder_ptsd =
        recode_factor(mhd.posttraumatic_stress_disorder_ptsd_numeric,
                      "0" = "Not PTSD",
                      "1" = "PTSD",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.posttraumatic_stress_disorder_ptsd,
         cumul = F)
  
}

#Psychosis
if(GLAD == TRUE){
  
  #Psychotic type illness
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.type_psychosis_psychotic_illness_numeric =
        case_when(
          (mhd.psychosis_coping_numeric  == "1" & mhd.psychosis_signup_numeric == "1" ~ 1),
          (mhd.psychosis_coping_numeric  == "1" | mhd.psychosis_signup_numeric == "1" ~ 1),
          (mhd.psychosis_coping_numeric  == "0" & mhd.psychosis_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.type_psychosis_psychotic_illness =
        recode_factor(mhd.type_psychosis_psychotic_illness_numeric,
                      "0" = "Any other type of psychosis or psychotic illness",
                      "1" = "Not Any other type of psychosis or psychotic illness",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.type_psychosis_psychotic_illness,
         cumul = F)
  
}

#Personality disorder
if(GLAD == TRUE){
  
  #Personality disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.personality_disorder_numeric =
        case_when(
          (mhd.personality_disorder_coping_numeric  == "1" & mhd.personality_disorder_signup_numeric == "1" ~ 1),
          (mhd.personality_disorder_coping_numeric  == "1" | mhd.personality_disorder_signup_numeric == "1" ~ 1),
          (mhd.personality_disorder_coping_numeric  == "0" & mhd.personality_disorder_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.personality_disorder =
        recode_factor(mhd.personality_disorder_numeric,
                      "0" = "Not Personality disorder",
                      "1" = "Personality disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.personality_disorder,
         cumul = F)
  
}

#Autism spectrum disorder
if(GLAD == TRUE){
  
  #Autism spectrum disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric =
        case_when(
          (mhd.asd_coping_numeric  == "1" & mhd.asd_signup_numeric == "1" ~ 1),
          (mhd.asd_coping_numeric  == "1" | mhd.asd_signup_numeric == "1" ~ 1),
          (mhd.asd_coping_numeric  == "0" & mhd.asd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.autism_aspergers_or_autistic_spectrum_disorder =
        recode_factor(mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric,
                      "0" = "Not Autism spectrum disorder",
                      "1" = "Autism spectrum disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.autism_aspergers_or_autistic_spectrum_disorder,
         cumul = F)
  
}

#Attention deficit hyperactivity disorder
if(GLAD == TRUE){
  
  #ADHD
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.attention_deficit_hyperactivity_disorder_numeric =
        case_when(
          (mhd.addadhd_coping_numeric  == "1" & mhd.addadhd_signup_numeric == "1" ~ 1),
          (mhd.addadhd_coping_numeric  == "1" | mhd.addadhd_signup_numeric == "1" ~ 1),
          (mhd.addadhd_coping_numeric  == "0" & mhd.addadhd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.attention_deficit_hyperactivity_disorder =
        recode_factor(mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric,
                      "0" = "Not Attention deficit hyperactivity disorder",
                      "1" = "Attention deficit hyperactivity disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.attention_deficit_hyperactivity_disorder,
         cumul = F)
  
}

#Attention deficit hyperactivity disorder
if(GLAD == TRUE){
  
  #ADHD
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.attention_deficit_hyperactivity_disorder_numeric =
        case_when(
          (mhd.addadhd_coping_numeric  == "1" & mhd.addadhd_signup_numeric == "1" ~ 1),
          (mhd.addadhd_coping_numeric  == "1" | mhd.addadhd_signup_numeric == "1" ~ 1),
          (mhd.addadhd_coping_numeric  == "0" & mhd.addadhd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.attention_deficit_hyperactivity_disorder =
        recode_factor(mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric,
                      "0" = "Not Attention deficit hyperactivity disorder",
                      "1" = "Attention deficit hyperactivity disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.attention_deficit_hyperactivity_disorder,
         cumul = F)
  
}

#Other mental health or neurodevelopmental disorder
if(GLAD == TRUE){
  
  #Other
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.other_please_tell_us_more_numeric =
        case_when(
          (mhd.other_coping_numeric  == "1" & mhd.other_signup_numeric == "1" ~ 1),
          (mhd.other_coping_numeric  == "1" | mhd.other_signup_numeric == "1" ~ 1),
          (mhd.other_coping_numeric  == "0" & mhd.other_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.other_please_tell_us_more =
        recode_factor(mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric,
                      "0" = "Not Other mental health or neurodevelopmental disorder",
                      "1" = "Other mental health or neurodevelopmental disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.other_please_tell_us_more,
         cumul = F)
  
}

#Mania, hypomania, bipolar or manic-depression
if(GLAD == TRUE){
  
  #Bipolar disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.mania_hypomania_bipolar_or_manicdepression_numeric =
        case_when(
          (mhd.bipolar_disorder_coping_numeric  == "1" & mhd.bipolar_disorder_signup_numeric == "1" ~ 1),
          (mhd.bipolar_disorder_coping_numeric  == "1" | mhd.bipolar_disorder_signup_numeric == "1" ~ 1),
          (mhd.bipolar_disorder_coping_numeric  == "0" & mhd.bipolar_disorder_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.mania_hypomania_bipolar_or_manicdepression =
        recode_factor(mhd.mania_hypomania_bipolar_or_manicdepression_numeric,
                      "0" = "Not Mania, hypomania, bipolar or manic-depression",
                      "1" = "Mania, hypomania, bipolar or manic-depression",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.mania_hypomania_bipolar_or_manicdepression,
         cumul = F)
  
}

#Anxiety, nerves or generalised anxiety disorder
if(GLAD == TRUE){
  
  #Anxiety, nerves or generalised anxiety disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric =
        case_when(
          (mhd.gad_coping_numeric  == "1" & mhd.gad_signup_numeric == "1" ~ 1),
          (mhd.gad_coping_numeric  == "1" | mhd.gad_signup_numeric == "1" ~ 1),
          (mhd.gad_coping_numeric  == "0" & mhd.gad_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.anxiety_nerves_or_generalised_anxiety_disorder =
        recode_factor(mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric,
                      "0" = "Not Anxiety, nerves or generalised anxiety disorder",
                      "1" = "Anxiety, nerves or generalised anxiety disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.anxiety_nerves_or_generalised_anxiety_disorder,
         cumul = F)
  
}

#Social anxiety or social phobia
if(GLAD == TRUE){
  
  #Social anxiety
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.social_anxiety_or_social_phobia_numeric =
        case_when(
          (mhd.social_anxiety_coping_numeric  == "1" & mhd.social_anxiety_signup_numeric == "1" ~ 1),
          (mhd.social_anxiety_coping_numeric  == "1" | mhd.social_anxiety_signup_numeric == "1" ~ 1),
          (mhd.social_anxiety_coping_numeric  == "0" & mhd.social_anxiety_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.social_anxiety_or_social_phobia =
        recode_factor(mhd.social_anxiety_or_social_phobia_numeric,
                      "0" = "Not Social anxiety or social phobia",
                      "1" = "Social anxiety or social phobia",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.social_anxiety_or_social_phobia,
         cumul = F)
  
}

#Specific phobia
if(GLAD == TRUE){
  
  #Social anxiety
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.specific_phobia_e.g._phobia_of_flying_numeric =
        case_when(
          (mhd.specific_phobia_coping_numeric  == "1" & mhd.specific_phobia_signup_numeric == "1" ~ 1),
          (mhd.specific_phobia_coping_numeric  == "1" | mhd.specific_phobia_signup_numeric == "1" ~ 1),
          (mhd.specific_phobia_coping_numeric  == "0" & mhd.specific_phobia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.specific_phobia_e.g._phobia_of_flying =
        recode_factor(mhd.specific_phobia_e.g._phobia_of_flying_numeric,
                      "0" = "Not Specific phobia",
                      "1" = "Specific phobia",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.specific_phobia_e.g._phobia_of_flying,
         cumul = F)
  
}

#Agoraphobia
if(GLAD == TRUE){
  
  #Agoraphobia
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.agoraphobia_numeric =
        case_when(
          (mhd.agoraphobia_coping_numeric  == "1" & mhd.agoraphobia_signup_numeric == "1" ~ 1),
          (mhd.agoraphobia_coping_numeric  == "1" | mhd.agoraphobia_signup_numeric == "1" ~ 1),
          (mhd.agoraphobia_coping_numeric  == "0" & mhd.agoraphobia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.agoraphobia =
        recode_factor(mhd.agoraphobia_numeric,
                      "0" = "Not Agoraphobia",
                      "1" = "Agoraphobia",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.agoraphobia,
         cumul = F)
  
}

#Panic disorder
if(GLAD == TRUE){
  
  #Panic disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.panic_disorder_numeric =
        case_when(
          (mhd.panic_disorder_coping_numeric  == "1" & mhd.panic_disorder_signup_numeric == "1" ~ 1),
          (mhd.panic_disorder_coping_numeric  == "1" | mhd.panic_disorder_signup_numeric == "1" ~ 1),
          (mhd.panic_disorder_coping_numeric  == "0" & mhd.panic_disorder_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.panic_disorder =
        recode_factor(mhd.panic_disorder_numeric,
                      "0" = "Not Panic disorder",
                      "1" = "Panic disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.panic_disorder,
         cumul = F)
  
}

#Panic attacks
if(GLAD == TRUE){
  
  #Panic attacks
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.panic_attacks_numeric =
        case_when(
          (mhd.panic_attacks_coping_numeric  == "1" & mhd.panic_attacks_signup_numeric == "1" ~ 1),
          (mhd.panic_attacks_coping_numeric  == "1" | mhd.panic_attacks_signup_numeric == "1" ~ 1),
          (mhd.panic_attacks_coping_numeric  == "0" & mhd.panic_attacks_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.panic_attacks =
        recode_factor(mhd.panic_attacks_numeric,
                      "0" = "Not Panic attacks",
                      "1" = "Panic attacks",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.panic_attacks,
         cumul = F)
  
}

#Schizophrenia
if(GLAD == TRUE){
  
  #Panic attacks
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.schizophrenia_numeric =
        case_when(
          (mhd.schizophrenia_coping_numeric  == "1" & mhd.schizophrenia_signup_numeric == "1" ~ 1),
          (mhd.schizophrenia_coping_numeric  == "1" | mhd.schizophrenia_signup_numeric == "1" ~ 1),
          (mhd.schizophrenia_coping_numeric  == "0" & mhd.schizophrenia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.schizophrenia =
        recode_factor(mhd.schizophrenia_numeric,
                      "0" = "Not Schizophrenia",
                      "1" = "Schizophrenia",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.schizophrenia,
         cumul = F)
  
}

#Obsessive-compulsive disorder
if(GLAD == TRUE){
  
  #OCD
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.obsessivecompulsive_disorder_ocd_numeric =
        case_when(
          (mhd.ocd_coping_numeric  == "1" & mhd.ocd_signup_numeric == "1" ~ 1),
          (mhd.ocd_coping_numeric  == "1" | mhd.ocd_signup_numeric == "1" ~ 1),
          (mhd.ocd_coping_numeric  == "0" & mhd.ocd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.obsessivecompulsive_disorder_ocd =
        recode_factor(mhd.obsessivecompulsive_disorder_ocd_numeric,
                      "0" = "Not OCD",
                      "1" = "OCD",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.obsessivecompulsive_disorder_ocd,
         cumul = F)
  
}

#Other related obsessive-compulsive disorders
if(GLAD == TRUE){
  
  #Other OCD
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.obsessive_compulsive_related_disorders_numeric =
        case_when(
          (mhd.other_ocd_coping_numeric  == "1" & mhd.other_ocd_signup_numeric == "1" ~ 1),
          (mhd.other_ocd_coping_numeric  == "1" | mhd.other_ocd_signup_numeric == "1" ~ 1),
          (mhd.other_ocd_coping_numeric  == "0" & mhd.other_ocd_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.obsessive_compulsive_related_disorders =
        recode_factor(mhd.obsessive_compulsive_related_disorders_numeric,
                      "0" = "Not Other OCD",
                      "1" = "Other OCD",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.obsessive_compulsive_related_disorders,
         cumul = F)
  
}

#Anorexia nervosa
if(GLAD == TRUE){
  
  #Anorexia nervosa
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.anorexia_nervosa_unc_numeric =
        case_when(
          (mhd.anorexia_coping_numeric  == "1" & mhd.anorexia_signup_numeric == "1" ~ 1),
          (mhd.anorexia_coping_numeric  == "1" | mhd.anorexia_signup_numeric == "1" ~ 1),
          (mhd.anorexia_coping_numeric  == "0" & mhd.anorexia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.anorexia_nervosa_unc =
        recode_factor(mhd.anorexia_nervosa_unc_numeric,
                      "0" = "Not Anorexia nervosa",
                      "1" = "Anorexia nervosa",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.anorexia_nervosa_unc,
         cumul = F)
  
}

#Atypical anorexia nervosa
if(GLAD == TRUE){
  
  #Atypical anorexia nervosa
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.atypical_anorexia_nervosa_unc_numeric =
        case_when(
          (mhd.atypical_anorexia_coping_numeric  == "1" & mhd.atypical_anorexia_signup_numeric == "1" ~ 1),
          (mhd.atypical_anorexia_coping_numeric  == "1" | mhd.atypical_anorexia_signup_numeric == "1" ~ 1),
          (mhd.atypical_anorexia_coping_numeric  == "0" & mhd.atypical_anorexia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.atypical_anorexia_nervosa_unc =
        recode_factor(mhd.atypical_anorexia_nervosa_unc_numeric,
                      "0" = "Not Atypical anorexia nervosa",
                      "1" = "Atypical anorexia nervosa",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.atypical_anorexia_nervosa_unc,
         cumul = F)
  
}

#Bulimia nervosa
if(GLAD == TRUE){
  
  #Bulimia nervosa
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.bulimia_nervosa_unc_numeric =
        case_when(
          (mhd.bulimia_coping_numeric  == "1" & mhd.bulimia_signup_numeric == "1" ~ 1),
          (mhd.bulimia_coping_numeric  == "1" | mhd.bulimia_signup_numeric == "1" ~ 1),
          (mhd.bulimia_coping_numeric  == "0" & mhd.bulimia_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.bulimia_nervosa_unc =
        recode_factor(mhd.bulimia_nervosa_unc_numeric,
                      "0" = "Not Bulimia nervosa",
                      "1" = "Bulimia nervosa",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.bulimia_nervosa_unc,
         cumul = F)
  
}

#Binge-eating disorder
if(GLAD == TRUE){
  
  #Binge-eating disorder
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.bingeeating_disorder_unc_numeric =
        case_when(
          (mhd.bed_coping_numeric  == "1" & mhd.bed_signup_numeric == "1" ~ 1),
          (mhd.bed_coping_numeric  == "1" | mhd.bed_signup_numeric == "1" ~ 1),
          (mhd.bed_coping_numeric  == "0" & mhd.bed_signup_numeric == "0" ~ 0)
        ))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.bingeeating_disorder_unc =
        recode_factor(mhd.bingeeating_disorder_unc_numeric,
                      "0" = "Not Binge-eating disorder",
                      "1" = "Binge-eating disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(mhd.bingeeating_disorder_unc,
         cumul = F)
  
}


## EDGI
if(EDGI == TRUE) {
  edgi.mhd.raw <- readRDS(file = "../data_raw/edgi/mhd_edgi.rds")
  dim(edgi.mhd.raw)
  colnames(edgi.mhd.raw)
}

if(EDGI == TRUE) {
  edgi.mhd.raw.id <- edgi.mhd.raw %>%
    drop_na(externalDataReference) %>% # Drop NAs
    distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    # mutate(ID = as.numeric(ID)) %>%
    add_column(cohort_name = "coping_edgi", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      #startDate_prepandemic = startDate,
      mhd.anorexia_nervosa_unc = mhd.anorexia_nervosa,
      mhd.atypical_anorexia_nervosa_unc = mhd.atypical_anorexia_nervosa,
      mhd.bulimia_nervosa_unc = mhd.bulimia_nervosa,
      mhd.atypical_bulimia_nervosa_unc = mhd.atypical_bulimia_nervosa,
      mhd.bingeeating_disorder_unc = mhd.bingeeating_disorder,
      mhd.atypical_bingeeating_disorder_unc = mhd.atypical_bingeeating_disorder,
      mhd.purging_disorder_unc = mhd.purging_disorder,
      mhd.night_eating_syndrome_unc = mhd.night_eating_syndrome,
      mhd.pica_unc = mhd.pica,
      mhd.avoidantrestrictive_food_intake_disorder_unc = mhd.avoidantrestrictive_food_intake_disorder,
      mhd.rumination_disorder_unc = mhd.rumination_disorder,
      mhd.feeding_eating_disorder_unc = mhd.feeding_eating_disorder,
      mhd.other_eating_disorder_unc = mhd.other_eating_disorder,
      mhd.none_of_the_above_eating_disorder_unc = mhd.none,
      mhd.dont_know_eating_disorder_unc = mhd.dont_know,
      mhd.prefer_not_to_answer_eating_disorder_unc = mhd.prefer_not_to_answer,
      mhd.suspected_eating_disorder_diagnosed = mhd.suspected_eating_disorder_diagnosed
    ) %>%
    add_numeric(., exclude = exclude_cols) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(edgi.mhd.raw.id)
  # Inspect colnames
  colnames(edgi.mhd.raw.id)
  #Differences
  dim(edgi.mhd.raw.id)[1]-dim(edgi.mhd.raw.id)[1]
}


if(EDGI == TRUE) {
  mhd.raw.id <- edgi.mhd.raw.id
}

##EDGI MHD 2
if(EDGI == TRUE) {
  edgi.mhd2.raw <- readRDS(file = "../data_raw/edgi/demographics_edgi.rds")
  dim(edgi.mhd2.raw)
  colnames(edgi.mhd2.raw)
}

if(EDGI == TRUE){
  exclude_cols_mhd2 <- c("ID",
                         "cohort_name",
                         "startDate.prepandemic",
                         "age_unc",
                         "EduYrs")
  
  edgi.mhd2.raw.id <- edgi.mhd2.raw %>%
    drop_na(externalDataReference) %>% # Drop NAs
    distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(externalDataReference, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #mutate(ID = as.numeric(ID)) %>%
    add_column(cohort_name = "coping_edgi", .before = "startDate") %>% #create new column 
    select(
      ID = externalDataReference, # ID
      cohort_name, # Sample
      #startDate_prepandemic = startDate,
      mhd.depression = demographics.depression,
      mhd.pregnancy_depression = demographics.postnatalantenatal_depression,
      mhd.premenstrual_dysphoric_disorder_pmdd = demographics.pmdd,
      mhd.anxiety_nerves_or_generalised_anxiety_disorder = demographics.anxiety_nerves_or_generalised_anxiety_disorder,
      mhd.social_anxiety_or_social_phobia = demographics.social_anxiety_or_social_phobia,
      mhd.specific_phobia_e.g._phobia_of_flying = demographics.specific_phobia, # fear instead of phobia
      mhd.agoraphobia = demographics.agoraphobia,
      mhd.panic_disorder = demographics.panic_disorder,
      mhd.panic_attacks = demographics.panic_attacks,
      mhd.ptsd = demographics.ptsd,
      mhd.obsessivecompulsive_disorder_ocd = demographics.obsessivecompulsive_disorder_ocd,
      mhd.obsessive_compulsive_related_disorders = demographics.other_ocd,
      mhd.body_dysmorphic_disorder_bdd = demographics.bdd,
      mhd.mania_hypomania_bipolar_or_manicdepression = demographics.mania_hypomania_bipolar_or_manicdepression,
      mhd.schizophrenia = demographics.schizophrenia,
      mhd.type_psychosis_psychotic_illness = demographics.psychosis_type_psychotic_illness, # word type psychosis changed order compared to GLAD
      mhd.personality_disorder_diagnosed = demographics.diagnosed_personality_disorder, # named differently in EDGI
      mhd.posttraumatic_stress_disorder_ptsd = demographics.ptsd,
      mhd.autism_aspergers_or_autistic_spectrum_disorder = demographics.autism_spectrum_disorder_asd, # different name in EDGI
      mhd.personality_disorder = demographics.personality_disorder, # routing variable in EDGI is named differently; renamed here to match GLAD
      mhd.attention_deficit_hyperactivity_disorder = demographics.attention_deficit_hyperactivity_disorder,
      mhd.none_of_the_above.1 = demographics.none_of_the_above.1,
      mhd.dont_know = demographics.dont_know,
      mhd.prefer_not_to_answer = demographics.prefer_not_to_answer.1,
      mhd.none_of_the_above.2 = demographics.none_of_the_above.2,
      mhd.dont_know.1 = demographics.dont_know.7,
      mhd.prefer_not_to_answer.1 = demographics.prefer_not_to_answer.4,
      mhd.other_please_tell_us_more = demographics.other.2
    ) %>%
    add_column(
      mhd.schizoaffective_disorder = NA_character_
    ) %>%
    add_numeric(., exclude = exclude_cols_mhd2) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(edgi.mhd2.raw.id)
  # Inspect colnames
  colnames(edgi.mhd2.raw.id)
  #Differences
  dim(edgi.mhd2.raw)[1]-dim(edgi.mhd2.raw.id)[1]
}


if(EDGI == TRUE){
  mhd.raw.id <- left_join(
    x = edgi.mhd.raw.id,
    y = edgi.mhd2.raw.id,
    by = c("cohort_name", "ID")
  )
  # Inspect dimensions
  dim(mhd.raw.id)
  # Inspect colnames
  colnames(mhd.raw.id) 
}

## NBR
if(NBR == TRUE) {
  nbr.mhd.raw <- readRDS(file = "../data_raw/nbr/mhd_coping_nbr.rds")
  dim(nbr.mhd.raw)
  colnames(nbr.mhd.raw)
}

if(NBR == TRUE) {
  exclude_cols <- c("cohort_name",
                    "ID")
  nbr.mhd.raw.id <- nbr.mhd.raw %>%
    drop_na(subjectid) %>% # Drop NAs
    distinct(subjectid, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(subjectid, into = c("Sample", "ID.intm"), sep = 6) %>% # Split ID in Sample and Number
    #separate(ID.intm, into = "ID", sep = 7) %>%
    add_column(cohort_name = "nbr", .after = "subjectid") %>% #create new column 
    select(
      ID = subjectid, # ID
      cohort_name, # Sample
      #startDate_baseline = startDate,
      starts_with("mhd."),
      mhd.suspected_eating_disorder_diagnosed = mhd.suspected_eating_disorder_diagnosed,
      mhd.pregnancy_depression = mhd.postnatal_depression,
      mhd.posttraumatic_stress_disorder_ptsd_numeric = mhd.ptsd,
      mhd.specific_phobia_e.g._phobia_of_flying = mhd.specific_phobia,
      mhd.premenstrual_dysphoric_disorder_pmdd = mhd.pmdd,
      mhd.body_dysmorphic_disorder_bdd = mhd.bdd,
      mhd.obsessive_compulsive_related_disorders = mhd.other_ocd,
      mhd.purging_disorder_unc = mhd.purging_disorder,
      mhd.night_eating_syndrome_unc = mhd.night_eating_syndrome,
      mhd.pica_unc = mhd.pica,
      mhd.avoidantrestrictive_food_intake_disorder_unc = mhd.avoidantrestrictive_food_intake_disorder,
      mhd.rumination_disorder_unc = mhd.rumination_disorder,
      mhd.feeding_eating_disorder_unc = mhd.feeding_eating_disorder,
      mhd.other_eating_disorder_unc = mhd.other_eating_disorder,
      mhd.type_psychosis_psychotic_illness = mhd.psychosis_type_psychotic_illness,
      mhd.autism_aspergers_or_autistic_spectrum_disorder = mhd.autism_spectrum_disorder_asd,
      mhd.other_please_tell_us_more = mhd.other,
      mhd.anorexia_nervosa_unc = mhd.anorexia_nervosa,
      mhd.atypical_anorexia_nervosa_unc = mhd.atypical_anorexia_nervosa,
      mhd.atypical_bulimia_nervosa_unc = mhd.atypical_bulimia_nervosa,
      mhd.atypical_bingeeating_disorder_unc = mhd.atypical_bingeeating_disorder,
      mhd.bulimia_nervosa_unc = mhd.bulimia_nervosa,
      mhd.bingeeating_disorder_unc = mhd.bingeeating_disorder,
      mhd.other_eating_disorder_unc = mhd.other_eating_disorder,
      mhd.none_of_the_above_eating_disorder_unc = mhd.none,
      mhd.dont_know_eating_disorder_unc = mhd.dont_know.2,
      mhd.prefer_not_to_answer_eating_disorder_unc = mhd.prefer_not_to_answer.2,
      mhd.none_of_the_above.1 = mhd.none_of_the_above,
      mhd.none_of_the_above.2 = mhd.none_of_the_above.1
    ) %>%
    add_column(
      mhd.schizoaffective_disorder = NA #NBR doesn't ask about schizoaffective disorder. This means the column das not exist in the data set
    ) %>%
    add_numeric(., exclude = exclude_cols) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(nbr.mhd.raw.id)
  # Inspect colnames
  colnames(nbr.mhd.raw.id)
  #Differences
  dim(nbr.mhd.raw)[1]-dim(nbr.mhd.raw.id)[1]
}


if(NBR == TRUE) {
  mhd.raw.id <- nbr.mhd.raw.id
}

## RAMP

if(RAMP == TRUE) {
  ramp.mhd.raw <- readRDS(file = "../data_raw/ramp/health_mhq_ramp.rds")
  dim(ramp.mhd.raw)
  colnames(ramp.mhd.raw)
}

if(RAMP == TRUE) {
  
  exclude_cols <- c("cohort_name",
                    "ID")
  
  ramp.mhd.raw.id <- ramp.mhd.raw %>%
    drop_na(`Login ID`) %>% # Drop NAs
    distinct(`Login ID`, .keep_all = TRUE) %>% # Remove duplicates based on ID
    #separate(`Login ID`, into = c("Sample", "ID"), sep = 4) %>% # Split ID in Sample and Number
    #separate(ID, into = "ID", sep = 7) %>%
    add_column(cohort_name = "ramp", .before = "startDate") %>% #create new column 
    select(
      ID = `Login ID`, # ID
      cohort_name, # Sample
      #startDate_baseline = startDate,
      mhd.depression = mhd.depression,
      mhd.pregnancy_depression = mhd.pregnancy_depression,
      mhd.premenstrual_dysphoric_disorder_pmdd = mhd.premenstrual_dysphoric_disorder_pmdd,
      mhd.mania_hypomania_bipolar_or_manicdepression = mhd.mania_hypomania_bipolar_or_manicdepression,
      mhd.anxiety_nerves_or_generalised_anxiety_disorder = mhd.anxiety_nerves_or_generalised_anxiety_disorder,
      mhd.social_anxiety_or_social_phobia = mhd.social_anxiety_or_social_phobia,
      mhd.specific_phobia_e.g._phobia_of_flying = mhd.specific_phobia_e.g._phobia_of_flying,
      mhd.agoraphobia = mhd.agoraphobia,
      mhd.panic_disorder = mhd.panic_disorder,
      mhd.panic_attacks = mhd.panic_attacks,
      mhd.posttraumatic_stress_disorder_ptsd = mhd.posttraumatic_stress_disorder_ptsd,
      mhd.obsessivecompulsive_disorder_ocd= mhd.obsessivecompulsive_disorder_ocd,
      mhd.body_dysmorphic_disorder_bdd = mhd.body_dysmorphic_disorder_bdd,
      mhd.obsessive_compulsive_related_disorders = mhd.skin_picking_obsessive_compulsive,
      mhd.anorexia_nervosa = mhd.anorexia_nervosa,
      mhd.atypical_anorexia_nervosa = mhd.atypical_anorexia_nervosa,
      mhd.bulimia_nervosa = mhd.bulimia_nervosa,
      mhd.bingeeating_disorder= mhd.psychological_overeating_or_bingeeating_disorder,
      mhd.schizophrenia = mhd.schizophrenia,
      mhd.schizoaffective_disorder = mhd.schizoaffective_disorder,
      mhd.type_psychosis_psychotic_illness = mhd.psychosis_type_psychotic_illness,
      mhd.personality_disorder = mhd.personality_disorder,
      mhd.autism_aspergers_or_autistic_spectrum_disorder = mhd.autism_aspergers_or_autistic_spectrum_disorder,
      mhd.attention_deficit_hyperactivity_disorder = mhd.attention_deficit_hyperactivity_disorder,
      mhd.personality_disorder_diagnosed = mhd.personality_disorder_diagnosed,
      mhd.none_of_the_above = mhd.none_of_these,
      mhd.dont_know = mhd.dont_know,
      mhd.prefer_not_to_answer = mhd.prefer_not_to_answer,
      mhd.none_of_the_above.1 = mhd.none_of_these.1,
      mhd.dont_know.1 = mhd.dont_know.1,
      mhd.prefer_not_to_answer.1 = mhd.prefer_not_to_answer.1,
      mhd.other_please_tell_us_more.1 = mhd.other_please_tell_us_more
    ) %>%
    add_numeric(., exclude = exclude_cols) %>%
    mutate_if(is.numeric, ~na_if(., -88)) %>% # Recode missing values to NAs in numeric variables
    mutate_if(is.numeric, ~na_if(., -99)) %>%
    mutate_if(is.numeric, ~na_if(., -77)) %>%
    mutate_if(is.factor, ~na_if(., "Seen but not answered")) %>% # Recode missing values to NAs in factor variables
    mutate_if(is.factor, ~na_if(., "Don't know")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to say")) %>%
    mutate_if(is.factor, ~na_if(., "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Seen but not answered")) %>% # Drop empty factor levels
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Don't know")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to answer")) %>%
    mutate_if(is.factor, ~forcats::fct_drop(., only = "Prefer not to say"))
  # Inspect dimensions
  dim(ramp.mhd.raw.id)
  # Inspect colnames
  colnames(ramp.mhd.raw.id)
  #Differences
  dim(ramp.mhd.raw)[1]-dim(ramp.mhd.raw.id)[1]
}

if(RAMP == TRUE) {
  mhd.raw.id <- ramp.mhd.raw.id
  nrow(mhd.raw.id)
}


## Grouping mental health disorders
#1. Depressive disorder group: major depression, antenatal/postnatal depression, premenstrual depression. The logic for this has been double checked for males - KT/MD.
#2. Anxiety group: generalised anxiety, social anxiety, specific phobia, agoraphobia, panic_disorder, panic_attacks.
#3. Agoraphobia and panic_disorder group: panic_disorder, agoraphobia, panic_attacks.
#4. Eating disorders group: anorexianervosa, atypical anorexia nervosa, bulimia nervosa, overeating or bingeeating.
#5. Obsessive complusive disorders: obsessive compulaive disorder, obsessive compulsive related disorders.
#6. Psychotic disorders: schizophrenia, schizoaffective disorder, general psychosis or psychotic illness.
#7. Personality disorders: cluster A, cluster B, cluster C
#8. Control group: no diagnosed disorders.

### Depressive disorders
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      depressive_disorders_numeric =
        case_when(
          (mhd.depression_numeric == "0" &
             (mhd.pregnancy_depression_numeric == "0" | is.na(mhd.pregnancy_depression_numeric)) &
             (mhd.premenstrual_dysphoric_disorder_pmdd_numeric == "0" | is.na(mhd.premenstrual_dysphoric_disorder_pmdd_numeric))) ~ 0,
          mhd.depression_numeric == "1" ~ 1,
          mhd.pregnancy_depression_numeric == "1" ~ 1,
          mhd.premenstrual_dysphoric_disorder_pmdd_numeric == "1" ~ 1))
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      depressive_disorders =
        recode_factor(depressive_disorders_numeric,
                      "0" = "No depressive disorder",
                      "1" = "Depressive disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(depressive_disorders,
         cumul = F)
  
}

if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      depressive_disorders,
      depressive_disorders_numeric,
      mhd.depression_numeric,
      mhd.pregnancy_depression_numeric,
      mhd.premenstrual_dysphoric_disorder_pmdd_numeric,
      mhd.depression,
      mhd.pregnancy_depression,
      mhd.premenstrual_dysphoric_disorder_pmdd
    )
}

### Anxiety disorders
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      anxiety_disorders_numeric =
        case_when(
          mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric == "1" ~ 1,
          mhd.social_anxiety_or_social_phobia_numeric == "1" ~ 1,
          mhd.specific_phobia_e.g._phobia_of_flying_numeric == "1" ~ 1,
          mhd.agoraphobia_numeric == "1" ~ 1,
          mhd.panic_disorder_numeric == "1" ~ 1,
          mhd.panic_attacks_numeric == "1" ~ 1,
          (mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric == "0" &
             mhd.social_anxiety_or_social_phobia_numeric == "0" &
             mhd.specific_phobia_e.g._phobia_of_flying_numeric == "0" &
             mhd.agoraphobia_numeric == "0" &
             mhd.panic_disorder_numeric == "0" &
             mhd.panic_attacks_numeric == "0") ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      anxiety_disorders =
        recode_factor(anxiety_disorders_numeric,
                      "0" = "No anxiety disorder",
                      "1" = "Anxiety disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(anxiety_disorders,
         cumul = F)
}

#Check anxiety disorders
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      anxiety_disorders,
      anxiety_disorders_numeric,
      mhd.anxiety_nerves_or_generalised_anxiety_disorder_numeric,
      mhd.social_anxiety_or_social_phobia_numeric,
      mhd.specific_phobia_e.g._phobia_of_flying_numeric,
      mhd.agoraphobia_numeric,
      mhd.panic_disorder_numeric,
      mhd.panic_attacks_numeric,
      mhd.anxiety_nerves_or_generalised_anxiety_disorder,
      mhd.social_anxiety_or_social_phobia,
      mhd.specific_phobia_e.g._phobia_of_flying,
      mhd.agoraphobia,
      mhd.panic_disorder,
      mhd.panic_attacks
    )
}

#Agoraphobia and panic disorder group
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      agoraphobia_panic_disorder_numeric =
        case_when(
          mhd.agoraphobia_numeric == "1" ~ 1,
          mhd.panic_disorder_numeric == "1" ~ 1,
          mhd.panic_attacks_numeric == "1" ~ 1,
          (mhd.agoraphobia_numeric == "0" &
             mhd.panic_disorder_numeric == "0" &
             mhd.panic_attacks_numeric == "0") ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      agoraphobia_panic_disorder =
        recode_factor(agoraphobia_panic_disorder_numeric,
                      "0" = "No agoraphobia/panic disorder",
                      "1" = "Agoraphobia/panic disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(agoraphobia_panic_disorder)
}


#Check agoraphobia_panic_disorder
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      agoraphobia_panic_disorder,
      agoraphobia_panic_disorder_numeric,
      mhd.agoraphobia_numeric,
      mhd.panic_disorder_numeric,
      mhd.panic_attacks_numeric,
      mhd.agoraphobia,
      mhd.panic_disorder,
      mhd.panic_attacks
    )
}


#Depression and anxiety comorbidity
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      depression_and_anxiety_numeric =
        case_when(
          depressive_disorders_numeric == 0 &
            anxiety_disorders_numeric == 0 ~ 0,
          depressive_disorders_numeric == 1 &
            anxiety_disorders_numeric == 1 ~ 1,
          depressive_disorders_numeric == 1 &
            anxiety_disorders_numeric == 0 ~ 2,
          depressive_disorders_numeric == 0 &
            anxiety_disorders_numeric == 1 ~ 3
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      depression_and_anxiety =
        recode_factor(depression_and_anxiety_numeric,
                      "0" = "No depressive or anxiety disorder",
                      "1" = "Depressive and anxiety disorder",
                      "2" = "Only depressive disorder",
                      "3" = "Only anxiety disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(depression_and_anxiety,
         cumul = F)
}


### Eating disorders
#There is routing questionnaire if participants SUSPECT  an eating disorder and afterwards they get asked if they have been DIAGNOSED with the specific disorders. Therefore, mhd.suspected_eating_disorder_diagnosed_numeric needs to be recoded as 0 otherwise they would be NAs.
if(NBR == TRUE | EDGI == TRUE | GLAD == TRUE) { 
  #Recode as 0 if ppt responds no to screening question (mhd.suspected_eating_disorder_diagnosed_numeric)
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      mhd.anorexia_nervosa_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.anorexia_nervosa_unc_numeric == "0" ~ 0,
                  mhd.anorexia_nervosa_unc_numeric == "1" ~ 1),
      mhd.atypical_anorexia_nervosa_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.atypical_anorexia_nervosa_unc_numeric == "0" ~ 0,
                  mhd.atypical_anorexia_nervosa_unc_numeric == "1" ~ 1),
      mhd.bulimia_nervosa_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.bulimia_nervosa_unc_numeric == "0" ~ 0,
                  mhd.bulimia_nervosa_unc_numeric == "1" ~ 1),
      mhd.atypical_bulimia_nervosa_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.atypical_bulimia_nervosa_unc_numeric == "0" ~ 0,
                  mhd.atypical_bulimia_nervosa_unc_numeric == "1" ~ 1),
      mhd.bingeeating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.bingeeating_disorder_unc_numeric == "0" ~ 0,
                  mhd.bingeeating_disorder_unc_numeric == "1" ~ 1),
      mhd.atypical_bingeeating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.atypical_bingeeating_disorder_unc_numeric == "0" ~ 0,
                  mhd.atypical_bingeeating_disorder_unc_numeric == "1" ~ 1),
      mhd.purging_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.purging_disorder_unc_numeric == "0" ~ 0,
                  mhd.purging_disorder_unc_numeric == "1" ~ 1),
      mhd.night_eating_syndrome_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.night_eating_syndrome_unc_numeric == "0" ~ 0,
                  mhd.night_eating_syndrome_unc_numeric == "1" ~ 1),
      mhd.pica_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.pica_unc_numeric == "0" ~ 0,
                  mhd.pica_unc_numeric == "1" ~ 1),
      mhd.avoidantrestrictive_food_intake_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.avoidantrestrictive_food_intake_disorder_unc_numeric == "0" ~ 0,
                  mhd.avoidantrestrictive_food_intake_disorder_unc_numeric == "1" ~ 1),
      mhd.rumination_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.rumination_disorder_unc_numeric == "0" ~ 0,
                  mhd.rumination_disorder_unc_numeric == "1" ~ 1),
      mhd.feeding_eating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.feeding_eating_disorder_unc_numeric == "0" ~ 0,
                  mhd.feeding_eating_disorder_unc_numeric == "1" ~ 1), 
      mhd.none_of_the_above_eating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.none_of_the_above_eating_disorder_unc_numeric == "0" ~ 0,
                  mhd.none_of_the_above_eating_disorder_unc_numeric == "1" ~ 1),
      mhd.dont_know_eating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.dont_know_eating_disorder_unc_numeric == "0" ~ 0,
                  mhd.dont_know_eating_disorder_unc_numeric == "1" ~ 1),
      mhd.prefer_not_to_answer_eating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.prefer_not_to_answer_eating_disorder_unc_numeric == "0" ~ 0,
                  mhd.prefer_not_to_answer_eating_disorder_unc_numeric == "1" ~ 1),
      mhd.other_eating_disorder_numeric =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ 0,
                  mhd.other_eating_disorder_unc_numeric == "0" ~ 0,
                  mhd.other_eating_disorder_unc_numeric == "1" ~ 1),
      mhd.anorexia_nervosa =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Anorexia nervosa",
                  mhd.anorexia_nervosa_unc_numeric == "0" ~ "Not Anorexia nervosa",
                  mhd.anorexia_nervosa_unc_numeric == "1" ~ "Anorexia nervosa"),
      mhd.atypical_anorexia_nervosa =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Atypical anorexia nervosa",
                  mhd.atypical_anorexia_nervosa_unc_numeric == "0" ~ "Not Atypical anorexia nervosa",
                  mhd.atypical_anorexia_nervosa_unc_numeric == "1" ~ "Atypical anorexia nervosa"),
      mhd.bulimia_nervosa =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Bulimia nervosa",
                  mhd.bulimia_nervosa_unc_numeric == "0" ~ "Not Bulimia nervosa",
                  mhd.bulimia_nervosa_unc_numeric == "1" ~ "Bulimia nervosa"),
      mhd.atypical_bulimia_nervosa =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Atypical bulimia nervosa",
                  mhd.atypical_bulimia_nervosa_unc_numeric == "0" ~ "Atypical bulimia nervosa",
                  mhd.atypical_bulimia_nervosa_unc_numeric == "1" ~ "Atypical bulimia nervosa"),
      mhd.bingeeating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Binge-eating disorder",
                  mhd.bingeeating_disorder_unc_numeric == "0" ~ "Not Binge-eating disorder",
                  mhd.bingeeating_disorder_unc_numeric == "1" ~ "Binge-eating disorder"),
      mhd.atypical_bingeeating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Atypical binge-eating disorder",
                  mhd.atypical_bingeeating_disorder_unc_numeric == "0" ~ "Not Atypical binge-eating disorder",
                  mhd.atypical_bingeeating_disorder_unc_numeric == "1" ~ "Atypical binge-eating disorder"),
      mhd.purging_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not purging disorder",
                  mhd.purging_disorder_unc_numeric == "0" ~ "Not purging disorder",
                  mhd.purging_disorder_unc_numeric == "1" ~ "Purging disorder"),
      mhd.night_eating_syndrome =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not night eating syndrome",
                  mhd.night_eating_syndrome_unc_numeric == "0" ~ "Not night eating syndrome",
                  mhd.night_eating_syndrome_unc_numeric == "1" ~ "Night eating syndrome"),
      mhd.pica =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not pica",
                  mhd.pica_unc_numeric == "0" ~ "Not pica",
                  mhd.pica_unc_numeric == "1" ~ "Pica"),
      mhd.avoidantrestrictive_food_intake_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not ARFID",
                  mhd.avoidantrestrictive_food_intake_disorder_unc_numeric == "0" ~ "Not ARFID",
                  mhd.avoidantrestrictive_food_intake_disorder_unc_numeric == "1" ~ "ARFID"),
      mhd.rumination_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not rumination disorder",
                  mhd.rumination_disorder_unc_numeric == "0" ~ "Not rumination disorder",
                  mhd.rumination_disorder_unc_numeric == "1" ~ "Rumination disorder"),
      mhd.feeding_eating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not feeding disorder",
                  mhd.feeding_eating_disorder_unc_numeric == "0" ~ "Not feeding disorder",
                  mhd.feeding_eating_disorder_unc_numeric == "1" ~ "Feeding disorder"), 
      mhd.none_of_the_above_eating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not None of the above",
                  mhd.none_of_the_above_eating_disorder_unc == "0" ~ "Not None of the above",
                  mhd.none_of_the_above_eating_disorder_unc == "1" ~ "None of the above"),
      mhd.dont_know_eating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Don't know",
                  mhd.dont_know_eating_disorder_numeric == "0" ~ "Not Don't know",
                  mhd.dont_know_eating_disorder_numeric == "1" ~ "Don't know"),
      mhd.prefer_not_to_answer_eating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Prefer not to answer",
                  mhd.prefer_not_to_answer_eating_disorder_unc == "0" ~ "Not Prefer not to answer",
                  mhd.prefer_not_to_answer_eating_disorder_unc == "1" ~ "Prefer not to answer"),
      mhd.other_eating_disorder =
        case_when(mhd.suspected_eating_disorder_diagnosed_numeric == "0" ~ "Not Other eating disorder:",
                  mhd.other_eating_disorder_unc_numeric == "0" ~ "Not Other eating disorder:",
                  mhd.other_eating_disorder_unc_numeric == "1" ~ "Other eating disorder:")
    )
}

###EDGI, GLAD, NBR eating disorders
if(EDGI == TRUE | NBR == TRUE | GLAD == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      eating_disorders_diagnosed_numeric =
        case_when(
          mhd.anorexia_nervosa_numeric == "1" ~ 1,
          mhd.atypical_anorexia_nervosa_numeric == "1" ~ 1,
          mhd.bulimia_nervosa_numeric == "1" ~ 1,
          mhd.atypical_bulimia_nervosa_numeric == "1" ~ 1, # additional diagnosis in EDGI but not in GLAD
          mhd.bingeeating_disorder_numeric == "1" ~ 1,
          mhd.atypical_bingeeating_disorder_numeric == "1" ~ 1, # additional diagnosis in EDGI
          mhd.purging_disorder_numeric == "1" ~ 1,
          mhd.night_eating_syndrome_numeric  == "1" ~ 1,
          mhd.pica_numeric == "1" ~ 1,
          mhd.avoidantrestrictive_food_intake_disorder_numeric == "1" ~ 1,
          mhd.rumination_disorder_numeric == "1" ~ 1,
          mhd.feeding_eating_disorder_numeric == "1" ~ 1,
          mhd.other_eating_disorder_numeric == "1" ~ 1,
        
          (mhd.anorexia_nervosa_numeric == "0" &
             mhd.atypical_anorexia_nervosa_numeric == "0" &
             mhd.atypical_anorexia_nervosa_numeric == "0" &
             mhd.bulimia_nervosa_numeric == "0" &
             mhd.atypical_bulimia_nervosa_numeric == "0" &
             mhd.bingeeating_disorder_numeric == "0" &
             mhd.atypical_bingeeating_disorder_numeric == "0" &
             mhd.purging_disorder_numeric == "0" &
             mhd.night_eating_syndrome_numeric  == "0" &
             mhd.pica_numeric == "0" &
             mhd.avoidantrestrictive_food_intake_disorder_numeric == "0" &
             mhd.rumination_disorder_numeric == "0" &
             mhd.feeding_eating_disorder_numeric == "0" &
             mhd.other_eating_disorder_numeric == "0") ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      eating_disorders_diagnosed =
        recode_factor(eating_disorders_diagnosed_numeric,
                      "0" = "No eating disorder",
                      "1" = "Eating disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(eating_disorders_diagnosed,
         cumul = F)
}


###RAMP eating disorders
if(RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      eating_disorders_diagnosed_numeric =
        case_when(
          mhd.anorexia_nervosa_numeric == "1" ~ 1,
          mhd.atypical_anorexia_nervosa_numeric == "1" ~ 1,
          mhd.bulimia_nervosa_numeric == "1" ~ 1,
          mhd.bingeeating_disorder_numeric == "1" ~ 1,
          (mhd.anorexia_nervosa_numeric == "0" &
             mhd.atypical_anorexia_nervosa_numeric == "0" &
             mhd.bulimia_nervosa_numeric == "0" &
             mhd.bingeeating_disorder_numeric == "0") ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      eating_disorders_diagnosed =
        recode_factor(eating_disorders_diagnosed_numeric,
                      "0" = "No eating disorder",
                      "1" = "Eating disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(eating_disorders_diagnosed,
         cumul = F)
}


### Obsessive compulsive disorders
if(NBR == TRUE | RAMP == TRUE| EDGI == TRUE | GLAD == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      obsessive_compulsive_disorders_numeric =
        case_when(
          mhd.obsessivecompulsive_disorder_ocd_numeric == "1" ~ 1,
          mhd.obsessive_compulsive_related_disorders_numeric == "1" ~ 1,
          mhd.body_dysmorphic_disorder_bdd_numeric == "1" ~ 1,
          (mhd.obsessivecompulsive_disorder_ocd_numeric == "0" &
             mhd.obsessive_compulsive_related_disorders_numeric == "0" &
             mhd.body_dysmorphic_disorder_bdd_numeric == "0") ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      obsessive_compulsive_disorders =
        recode_factor(obsessive_compulsive_disorders_numeric,
                      "0" = "No obsessive compulsive disorder",
                      "1" = "Obsessive compulsive disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(obsessive_compulsive_disorders)
}


#Check obsessive_compulsive_disorders}
if(NBR == TRUE | RAMP == TRUE| EDGI == TRUE | GLAD == TRUE) {
  mhd.raw.id %>%
    select(
      obsessive_compulsive_disorders,
      obsessive_compulsive_disorders_numeric,
      mhd.obsessivecompulsive_disorder_ocd,
      mhd.obsessivecompulsive_disorder_ocd_numeric,
      mhd.obsessive_compulsive_related_disorders,
      mhd.obsessive_compulsive_related_disorders_numeric,
      mhd.body_dysmorphic_disorder_bdd,
      mhd.body_dysmorphic_disorder_bdd_numeric
    )
}

### Psychotic disorders
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      psychotic_disorders_numeric =
        case_when(
          mhd.schizophrenia_numeric == "1" ~ 1,
          mhd.schizoaffective_disorder_numeric == "1" ~ 1,
          mhd.type_psychosis_psychotic_illness_numeric == "1" ~ 1,
          (mhd.schizophrenia_numeric == "0" &
             (mhd.schizoaffective_disorder_numeric == "0" | is.na(mhd.schizoaffective_disorder_numeric == "0") & #NA included because some cohorts don't have schizoaffective disorder
                mhd.type_psychosis_psychotic_illness_numeric == "0") ~ 0)
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      psychotic_disorders =
        recode_factor(psychotic_disorders_numeric,
                      "0" = "No psychotic disorder",
                      "1" = "Psychotic disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(psychotic_disorders)
}

# Overlap between self-report schizophrenia and bipolar disorder
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      bipolar_and_schizophrenia_numeric =
        case_when(
          psychotic_disorders_numeric == 0 &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == 0 ~ 0,
          psychotic_disorders_numeric == 1 &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == 1 ~ 1,
          psychotic_disorders_numeric == 1 &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == 0 ~ 2,
          psychotic_disorders_numeric == 0 &
            mhd.mania_hypomania_bipolar_or_manicdepression_numeric == 1 ~ 3
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      bipolar_and_schizophrenia =
        recode_factor(bipolar_and_schizophrenia_numeric,
                      "0" = "No psychotic or bipolar disorder",
                      "1" = "Psychotic and bipolar disorder",
                      "2" = "Only psychotic disorder",
                      "3" = "Only bipolar disorder",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(bipolar_and_schizophrenia,
         cumul = F)
}


#Check psychotic_disorders
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      psychotic_disorders,
      psychotic_disorders_numeric,
      bipolar_and_schizophrenia,
      bipolar_and_schizophrenia_numeric,
      mhd.schizophrenia_numeric,
      mhd.schizoaffective_disorder_numeric,
      mhd.mania_hypomania_bipolar_or_manicdepression,
      mhd.mania_hypomania_bipolar_or_manicdepression_numeric,
      mhd.type_psychosis_psychotic_illness_numeric,
      mhd.schizophrenia,
      mhd.schizoaffective_disorder,
      mhd.type_psychosis_psychotic_illness
    )
}


### Autism spectrum disorder
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE){
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      autism_spectrum_disorder_numeric =
        case_when(
          mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric == 0 ~ 0,
          mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric == 1 ~ 1
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      autism_spectrum_disorder =
        recode_factor(
          autism_spectrum_disorder_numeric,
          `0` = "No autism spectrum disorder",
          `1` = "Autism spectrum disorder"
        )
    )
  mhd.raw.id %>%
    freq(autism_spectrum_disorder)
}


#Check autism_spectrum_disorder
if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE) {
  mhd.raw.id %>%
    select(
      autism_spectrum_disorder,
      autism_spectrum_disorder_numeric,
      mhd.autism_aspergers_or_autistic_spectrum_disorder,
      mhd.autism_aspergers_or_autistic_spectrum_disorder_numeric
    ) %>%
    filter(autism_spectrum_disorder == "Autism spectrum disorder")
}

### Personality disorders
#Cluster a
#1. Paranoid                        
#2. Schizoid                      
#3. Schizotypal                      
if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_a_numeric =
        case_when(
          mhd.personality_disorder_diagnosed_numeric == "1" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "2" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "3" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "4" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "5" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "6" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "7" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "8" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "9" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "10" ~ 0,
          mhd.personality_disorder_numeric == "0" ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_a =
        recode_factor(personality_cluster_a_numeric,
                      "0" = "No personality disorder cluster A",
                      "1" = "Personality disorder cluster A",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(personality_cluster_a,
         cumul = F)
}


if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id %>%
    select(
      personality_cluster_a,
      personality_cluster_a_numeric,
      mhd.personality_disorder_diagnosed,
      mhd.personality_disorder_diagnosed_numeric
    )
}


#Cluster B
#4. Antisocial                       
#5. Borderline                     
#6. Histrionic                        
#7. Narcissistic                      

if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_b_numeric =
        case_when(
          mhd.personality_disorder_diagnosed_numeric == "1" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "2" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "3" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "4" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "5" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "6" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "7" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "8" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "9" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "10" ~ 0,
          mhd.personality_disorder_numeric == "0" ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_b =
        recode_factor(personality_cluster_b_numeric,
                      "0" = "No personality disorder cluster B",
                      "1" = "Personality disorder cluster B",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(personality_cluster_b,
         cumul = F)
}

#Check personality_cluster_b
if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id %>%
    select(
      personality_cluster_b,
      personality_cluster_b_numeric,
      mhd.personality_disorder_diagnosed,
      mhd.personality_disorder_diagnosed_numeric
    )
}


#Cluster C
#8. Avoidant/anxious                
#9. Dependent                        
#10. Obsessive-compulsive
if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_c_numeric =
        case_when(
          mhd.personality_disorder_diagnosed_numeric == "1" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "2" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "3" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "4" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "5" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "6" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "7" ~ 0,
          mhd.personality_disorder_diagnosed_numeric == "8" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "9" ~ 1,
          mhd.personality_disorder_diagnosed_numeric == "10" ~ 1,
          mhd.personality_disorder_numeric == "0" ~ 0
        )
    )
  mhd.raw.id <- mhd.raw.id %>%
    mutate(
      personality_cluster_c =
        recode_factor(personality_cluster_c_numeric,
                      "0" = "No personality disorder cluster C",
                      "1" = "Personality disorder cluster C",
                      missing = NA_character_
        )
    )
  mhd.raw.id %>%
    freq(personality_cluster_c,
         cumul = F)
}


#Check personality_cluster_c
if(GLAD == TRUE | NBR == TRUE | RAMP == TRUE | EDGI == TRUE) {
  mhd.raw.id %>%
    select(
      personality_cluster_c,
      personality_cluster_c_numeric,
      mhd.personality_disorder_diagnosed,
      mhd.personality_disorder_diagnosed_numeric
    )
  
  
  if(GLAD == TRUE | EDGI == TRUE | NBR == TRUE | RAMP == TRUE){
    mhd.raw.id <- mhd.raw.id %>%
      mutate(
        PERSONALITY_DISORDER_NUMERIC =
          case_when(
            mhd.personality_disorder_numeric == 1 |
              personality_cluster_c_numeric == 1 |
              personality_cluster_b_numeric == 1|
              personality_cluster_a_numeric == 1 ~ 1,
            
            mhd.personality_disorder_numeric == 0 &
              personality_cluster_c_numeric == 0 &
              personality_cluster_b_numeric == 0 &
              personality_cluster_a_numeric == 0 ~ 0
          )
      )
    mhd.raw.id <- mhd.raw.id %>%
      mutate(
        PERSONALITY_DISORDER_FACTOR =
          recode_factor(
            PERSONALITY_DISORDER_NUMERIC,
            `0` = "No personality disorder",
            `1` = "Personality disorder"
          )
      )
    mhd.raw.id %>%
      freq(PERSONALITY_DISORDER_FACTOR)
  }
}