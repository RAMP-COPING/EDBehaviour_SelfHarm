## Highest education (prepandemic, GLAD)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
#Assumes University degree > Other professional qualification/A level > NVQ > GCSE/O Level/CSE > None of the above

if(GLAD == TRUE) {
  #create numeric version of the highest education variable
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      highest_education_prepan_numeric =
        case_when(
          dem.college_or_university_degree_numeric == "1" ~ 5,
          #dem.other_professional_qualifications_numeric == "1" ~ 5, #Not in EDGI
          dem.a_levelsas_levels_or_equivalent_numeric == "1" ~ 4,
          dem.nvq_or_hnd_or_hnc_or_equivalent_numeric == "1" ~ 3,
          dem.o_levelsgcses_or_equivalent_numeric == "1" ~ 2,
          dem.cses_or_equivalent_numeric == "1" ~ 2,
          dem.none_of_the_above_education_numeric == "1" ~ 1
        )
    )
  #recode the numeric version into a factor
  glad.dem3.raw.id <- glad.dem3.raw.id %>%
    mutate(
      highest_education_prepan =
        recode_factor(
          highest_education_prepan_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University")
    )
  glad.dem3.raw.id %>%
    freq(highest_education_prepan)
}


#Check highest education coding GLAD
if(GLAD == TRUE) {
  glad.dem3.raw.id %>%
    select(highest_education_prepan,
           dem.college_or_university_degree,
           dem.a_levelsas_levels_or_equivalent,
           dem.nvq_or_hnd_or_hnc_or_equivalent,
           dem.o_levelsgcses_or_equivalent,
           # dem.other_professional_qualifications_numeric,
           dem.cses_or_equivalent,
           dem.none_of_the_above_education
    )
}


## Highest education (prepandemic, EDGI)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
#Assumes University degree > A level > NVQ > GCSE/O Level/CSE > no qualifications
if(EDGI == TRUE) {
  
  #create numeric version of the highest education variable
  edgi.dem3.raw.id <- edgi.dem3.raw.id %>%
    mutate(
      highest_education_prepan_numeric =
        case_when(
          demographics.collegeuniversity_numeric == "1" ~ 5,
          demographics.asa_levels_numeric == "1" ~ 4,
          demographics.nvq_numeric == "1" ~ 3,
          demographics.cse_numeric == "1" ~ 2,
          demographics.gcse_numeric == "1" ~ 2,
          demographics.none_of_the_above_numeric == "1" ~ 1
        )
    )
  #recode the numeric version into a factor
  edgi.dem3.raw.id <- edgi.dem3.raw.id %>%
    mutate(
      highest_education_prepan =
        recode_factor(
          highest_education_prepan_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University")
    )
  edgi.dem3.raw.id %>%
    freq(highest_education_prepan)
}



#Check highest education coding EDGI
if(EDGI == TRUE) {
  edgi.dem3.raw.id %>%
    select(highest_education_prepan,
           demographics.collegeuniversity,
           demographics.asa_levels_numeric,
           demographics.nvq_numeric,
           demographics.cse_numeric,
           demographics.gcse_numeric,
           demographics.none_of_the_above_numeric
    )
}


if(EDGI == TRUE) {
  summarytools::dfSummary(
    edgi.dem3.raw.id,
    graph.col = F,
    valid.col = F,
    labels.col = F)
}


## Highest education (baseline, GLAD)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
#Assumes University degree > A level > NVQ > GCSE/O Level/CSE > No qualifications
if(GLAD == TRUE) {
  #create numeric version of the highest education variable
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id  %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.phd_numeric == "1" ~ 5,
          employ.masters_degree_or_equivalent_numeric == "1" ~ 5,
          employ.postgraduate_degree_or_equivalent_numeric == "1" ~ 5,
          employ.bachelors_degree_or_equivalent_numeric == "1" ~ 5,
          employ.a_levelsas_levels_or_equivalent_numeric == "1" ~ 4,
          employ.nvq_or_hnd_or_hnc_or_equivalent_numeric == "1" ~ 3,
          employ.o_levelsgcses_or_equivalent_numeric == "1" ~ 2,
          employ.cses_or_equivalent_numeric == "1" ~ 2,
          employ.none_of_the_above_numeric == "1" ~ 1
        )
    )
  
  
  #recode the numeric version into a factor
  glad.coping.dem4.employ.raw.id  <- glad.coping.dem4.employ.raw.id %>%
    mutate(
      highest_education =
        recode_factor(
          highest_education_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University")
    )
  glad.coping.dem4.employ.raw.id  %>%
    freq(highest_education)
}


#Check highest education coding
if(GLAD == TRUE) {
  glad.coping.dem4.employ.raw.id %>%
    select(highest_education,
           highest_education_numeric,
           employ.phd,
           employ.masters_degree_or_equivalent,
           employ.postgraduate_degree_or_equivalent,
           employ.bachelors_degree_or_equivalent,
           employ.a_levelsas_levels_or_equivalent,
           employ.nvq_or_hnd_or_hnc_or_equivalent,
           employ.o_levelsgcses_or_equivalent,
           employ.cses_or_equivalent
    )
}

## Highest education (baseline, EDGI)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
#Assumes University degree > A level > NVQ > GCSE/O Level/CSE > no qualifications
if(EDGI == TRUE) {
  
  #create numeric version of the highest education variable
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.phd_numeric == "1" ~ 5,
          employ.masters_degree_or_equivalent_numeric == "1" ~ 5,
          employ.postgraduate_degree_or_equivalent_numeric == "1" ~ 5,
          employ.bachelors_degree_or_equivalent_numeric == "1" ~ 5,
          employ.a_levelsas_levels_or_equivalent_numeric == "1" ~ 4,
          employ.nvq_or_hnd_or_hnc_or_equivalent_numeric == "1" ~ 3,
          employ.o_levelsgcses_or_equivalent_numeric == "1" ~ 2,
          employ.cses_or_equivalent_numeric == "1" ~ 2,
          employ.none_of_the_above_numeric == "1" ~ 1
        )
    )
  
  
  #recode the numeric version into a factor
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      highest_education =
        recode_factor(
          highest_education_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University")
    )
  edgi.coping.dem4.employ.raw.id %>%
    freq(highest_education)
}

#Check highest education coding
if(EDGI == TRUE) {
  edgi.coping.dem4.employ.raw.id %>%
    select(highest_education,
           highest_education_numeric,
           employ.phd,
           employ.masters_degree_or_equivalent,
           employ.postgraduate_degree_or_equivalent,
           employ.bachelors_degree_or_equivalent,
           employ.a_levelsas_levels_or_equivalent,
           employ.nvq_or_hnd_or_hnc_or_equivalent,
           employ.o_levelsgcses_or_equivalent,
           employ.cses_or_equivalent
    )
}

## Highest education (baseline, NBR)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
#Assumes University degree > A level > NVQ > GCSE/O Level/CSE > None of above
if(NBR == TRUE) {
  #create numeric version of the highest education variable
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.phd_numeric == "1" ~ 5,
          employ.masters_degree_or_equivalent_numeric == "1" ~ 5,
          employ.postgraduate_degree_or_equivalent_numeric == "1" ~ 5,
          employ.bachelors_degree_or_equivalent_numeric == "1" ~ 5,
          employ.a_levelsas_levels_or_equivalent_numeric == "1" ~ 4,
          employ.nvq_or_hnd_or_hnc_or_equivalent_numeric == "1" ~ 3,
          employ.o_levelsgcses_or_equivalent_numeric == "1" ~ 2,
          employ.cses_or_equivalent_numeric == "1" ~ 2,
          employ.none_of_the_above_numeric == "1" ~ 1
        )
    )
  
  
  #recode the numeric version into a factor
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      highest_education =
        recode_factor(
          highest_education_numeric,
          `1` = "No formal qualifications",
          `2` = "GCSE/CSE",
          `3` = "NVQ",
          `4` = "A-levels",
          `5` = "University")
    )
  nbr.dem.employ.raw.id %>%
    freq(highest_education)
}


#Check highest education coding
if(NBR == TRUE) {
  nbr.dem.employ.raw.id %>%
    select(highest_education,
           highest_education_numeric,
           employ.phd,
           employ.masters_degree_or_equivalent,
           employ.postgraduate_degree_or_equivalent,
           employ.bachelors_degree_or_equivalent,
           employ.a_levelsas_levels_or_equivalent,
           employ.nvq_or_hnd_or_hnc_or_equivalent,
           employ.o_levelsgcses_or_equivalent,
           employ.cses_or_equivalent
    )
}


## Highest education (baseline, RAMP)
if(RAMP == TRUE){
  #create numeric version of the highest education variable
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      highest_education_numeric =
        case_when(
          employ.what_is_your_highest_level_of_education == "PhD" ~ 4,
          employ.what_is_your_highest_level_of_education == "Postgraduate degree or equivalent" ~ 4,
          employ.what_is_your_highest_level_of_education == "Master's degree or equivalent" ~ 4,
          employ.what_is_your_highest_level_of_education == "Bachelor's degree or equivalent" ~ 4,
          employ.what_is_your_highest_level_of_education == "A-level or equivalent" ~ 3,
          employ.what_is_your_highest_level_of_education == "GCSE or equivalent" ~ 1,
          employ.what_is_your_highest_level_of_education == "None of these" ~ 5
        )
    )
  #recode the numeric version into a factor
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      highest_education =
        recode_factor(
          highest_education_numeric,
          `1` = "GCSE/CSE",
          `2` = NA_character_,
          `3` = "A-levels",
          `4` = "University",
          `5` = "No formal qualifications")
    )
  ramp.dem.employ.raw.id %>%
    freq(highest_education)
}


#Check highest education coding for RAMP
if(RAMP == TRUE) {
  ramp.dem.employ.raw.id %>%
    select(highest_education,
           highest_education_numeric,
           employ.what_is_your_highest_level_of_education,
           employ.what_is_your_highest_level_of_education_numeric
    )
}
