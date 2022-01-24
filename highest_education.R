
if(GLAD == TRUE) {
  # Create numeric version of the highest education variable
  highest_education_glad_coping <- highest_education_glad_coping %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.phd == "1" ~ 5,
          employ.masters_degree_or_equivalent == "1" ~ 5,
          employ.postgraduate_degree_or_equivalent == "1" ~ 5,
          employ.bachelors_degree_or_equivalent == "1" ~ 5,
          employ.other_professional_qualifications_ == "1" ~ 5,
          employ.a_levelsas_levels_or_equivalent == "1" ~ 4,
          employ.nvq_or_hnd_or_hnc_or_equivalent == "1" ~ 3,
          employ.o_levelsgcses_or_equivalent == "1" ~ 2,
          employ.cses_or_equivalent == "1" ~ 2,
          employ.none_of_the_above == "1" ~ 1
        )
    )
  
  
  # Recode the numeric version into a factor
  highest_education_glad_coping  <- highest_education_glad_coping %>%
    mutate(
      highest_education =
        recode_factor(
          highest_education_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University/Other professional qualifications")
    )
  
  
  highest_education_glad_coping  %>%
    freq(highest_education)
  
  
}

# Create numeric version of the highest education variable
if(EDGI == TRUE){
  highest_education_edgi_coping <- highest_education_edgi_coping %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.phd == "1" ~ 5,
          employ.masters_degree_or_equivalent == "1" ~ 5,
          employ.postgraduate_degree_or_equivalent == "1" ~ 5,
          employ.bachelors_degree_or_equivalent == "1" ~ 5,
          employ.other_professional_qualifications_ == "1" ~ 5,
          employ.a_levelsas_levels_or_equivalent == "1" ~ 4,
          employ.nvq_or_hnd_or_hnc_or_equivalent == "1" ~ 3,
          employ.o_levelsgcses_or_equivalent == "1" ~ 2,
          employ.cses_or_equivalent == "1" ~ 2,
          
          employ.none_of_the_above == "1" ~ 1
        )
    )
  


# Recode the numeric version into a factor
highest_education_edgi_coping <- highest_education_edgi_coping %>%
  mutate(
    highest_education =
      recode_factor(
        highest_education_numeric,
        `1` = "No formal qualifications",
        `2` = "GCSE/CSE",
        `3` = "NVQ",
        `4` = "A-levels",
        `5` = "University/Other professional qualifications")
  )
highest_education_edgi_coping %>%
  freq(highest_education)
}


if(NBR == TRUE){
highest_education_nbr <- highest_education_nbr %>%
  mutate(
    highest_education_numeric =
      case_when(
        employ.phd == "1" ~ 5,
        employ.masters_degree_or_equivalent == "1" ~ 5,
        employ.postgraduate_degree_or_equivalent == "1" ~ 5,
        employ.bachelors_degree_or_equivalent == "1" ~ 5,
        employ.other_professional_qualifications_ == "1" ~ 5,
        employ.a_levelsas_levels_or_equivalent == "1" ~ 4,
        employ.nvq_or_hnd_or_hnc_or_equivalent == "1" ~ 3,
        employ.o_levelsgcses_or_equivalent == "1" ~ 2,
        employ.cses_or_equivalent == "1" ~ 2,
        employ.none_of_the_above == "1" ~ 1
      )
  )


# Recode the numeric version into a factor
highest_education_nbr <- highest_education_nbr %>%
  mutate(
    highest_education =
      recode_factor(
        highest_education_numeric,
        `1` = "No formal qualifications",
        `2` = "GCSE/CSE",
        `3` = "NVQ",
        `4` = "A-levels",
        `5` = "University/Other professional qualifications")
  )
highest_education_nbr %>%
  freq(highest_education)
}

if(RAMP == TRUE){

highest_education_ramp <- highest_education_ramp %>%
  mutate(
    highest_education_numeric =
      case_when(
        employ.what_is_your_highest_level_of_education == "6" ~ 5, # PhD
        employ.what_is_your_highest_level_of_education == "5" ~ 5, # Postgraduate degree or equivalent
        employ.what_is_your_highest_level_of_education == "4" ~ 5, # Master's degree or equivalent
        employ.what_is_your_highest_level_of_education == "3" ~ 5, # Bachelor's degree or equivalent
        employ.what_is_your_highest_level_of_education == "2" ~ 4, # A levels
        employ.what_is_your_highest_level_of_education == "1" ~ 2, # GCSEs
        employ.what_is_your_highest_level_of_education == "0" ~ 1 # No formal education
      )
  )
# Recode the numeric version into a factor
highest_education_ramp <- highest_education_ramp %>%
  mutate(
    highest_education =
      recode_factor(
        highest_education_numeric,
        `1` = "No formal qualifications",
        `2` = "GCSE/CSE",
        #`3` = "NVQ", # Not in RAMP
        `4` = "A-levels",
        `5` = "University/Other professional qualifications")
  )

highest_education_ramp %>%
  freq(highest_education)
}