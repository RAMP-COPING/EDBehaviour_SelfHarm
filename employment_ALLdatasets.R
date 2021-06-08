## Employment prior to pandemic (baseline, GLAD)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(GLAD == TRUE) {
  #create numeric version of the highest education variable
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id %>%
    mutate(
      employment_prior_covid_numeric =
        case_when(
          employ.fulltime_employed_numeric == "1" ~ 1,
          employ.parttime_employed_numeric == "1" ~ 1,
          employ.unemployed_numeric == "1" ~ 2,
          employ.zerohours_contract_numeric == "1" ~ 1,
          employ.selfemployed_numeric == "1" ~ 1,
          employ.contract_or_freelance_work_numeric == "1" ~ 1,
          employ.small_business_owner_numeric == "1" ~ 1,
          employ.retired_numeric  ==  "1" ~ 4,
          employ.student_gcse_or_a_level_numeric == "1" ~ 3,
          employ.student_university_numeric == "1" ~ 3
        )
    )
  #recode the numeric version into a factor
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id %>%
    mutate(
      employment_prior_covid =
        recode_factor(
          employment_prior_covid_numeric,
          `1` = "Employed",
          `2` = "Unemployed",
          `3` = "Student",
          `4` = "Retired")
    )
  glad.coping.dem4.employ.raw.id %>%
    freq(employment_prior_covid)
}


#Check Employment prior to pandemic coding
if(GLAD == TRUE) {
  glad.coping.dem4.employ.raw.id %>%
    select(employment_prior_covid,
           employment_prior_covid_numeric,
           employ.fulltime_employed_numeric,
           employ.parttime_employed_numeric,
           employ.unemployed_numeric,
           employ.zerohours_contract_numeric,
           employ.selfemployed_numeric,
           employ.contract_or_freelance_work_numeric,
           employ.small_business_owner_numeric,
           employ.retired_numeric,
           employ.student_gcse_or_a_level_numeric,
           employ.student_university_numeric
    )
}

## Employment prior to pandemic (baseline, EDGI)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(EDGI == TRUE) {
  #create numeric version of the highest education variable
  
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      employment_prior_covid_numeric =
        case_when(
          employ.fulltime_employed_numeric == "1" ~ 1,
          employ.parttime_employed_numeric == "1" ~ 1,
          employ.unemployed_numeric == "1" ~ 2,
          employ.zerohours_contract_numeric == "1" ~ 1,
          employ.selfemployed_numeric == "1" ~ 1,
          employ.contract_or_freelance_work_numeric == "1" ~ 1,
          employ.small_business_owner_numeric == "1" ~ 1,
          employ.retired_numeric  ==  "1" ~ 4,
          employ.student_gcse_or_a_level_numeric == "1" ~ 3,
          employ.student_university_numeric == "1" ~ 3
        )
    )
  #recode the numeric version into a factor
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      employment_prior_covid =
        recode_factor(
          employment_prior_covid_numeric,
          `1` = "Employed",
          `2` = "Unemployed",
          `3` = "Student",
          `4` = "Retired")
    )
  edgi.coping.dem4.employ.raw.id %>%
    freq(employment_prior_covid)
}


#Check Employment prior to pandemic coding
if(EDGI == TRUE) {
  edgi.coping.dem4.employ.raw.id %>%
    select(employment_prior_covid,
           employment_prior_covid_numeric,
           employ.fulltime_employed_numeric,
           employ.parttime_employed_numeric,
           employ.unemployed_numeric,
           employ.zerohours_contract_numeric,
           employ.selfemployed_numeric,
           employ.contract_or_freelance_work_numeric,
           employ.small_business_owner_numeric,
           employ.retired_numeric,
           employ.student_gcse_or_a_level_numeric,
           employ.student_university_numeric
    )
}

## Employment prior to pandemic (baseline, nbr)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(NBR == TRUE) {
  #create numeric version of the highest education variable
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      employment_prior_covid_numeric =
        case_when(
          employ.fulltime_employed_numeric == "1" ~ 1,
          employ.parttime_employed_numeric == "1" ~ 1,
          employ.unemployed_numeric == "1" ~ 2,
          employ.zerohours_contract_numeric == "1" ~ 1,
          employ.selfemployed_numeric == "1" ~ 1,
          employ.contract_or_freelance_work_numeric == "1" ~ 1,
          employ.small_business_owner_numeric == "1" ~ 1,
          employ.retired_numeric  ==  "1" ~ 4,
          employ.student_gcse_or_a_level_numeric == "1" ~ 3,
          employ.student_university_numeric == "1" ~ 3
        )
    )
  #recode the numeric version into a factor
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      employment_prior_covid =
        recode_factor(
          employment_prior_covid_numeric,
          `1` = "Employed",
          `2` = "Unemployed",
          `3` = "Student",
          `4` = "Retired")
    )
  nbr.dem.employ.raw.id %>%
    freq(employment_prior_covid)
}


#Check Employment prior to pandemic coding
if(NBR == TRUE) {
  nbr.dem.employ.raw.id %>%
    select(employment_prior_covid,
           employment_prior_covid_numeric,
           employ.fulltime_employed_numeric,
           employ.parttime_employed_numeric,
           employ.unemployed_numeric,
           employ.zerohours_contract_numeric,
           employ.selfemployed_numeric,
           employ.contract_or_freelance_work_numeric,
           employ.small_business_owner_numeric,
           employ.retired_numeric,
           employ.student_gcse_or_a_level_numeric,
           employ.student_university_numeric
    )
}


## Employment prior to pandemic (baseline, RAMP)
if(RAMP == TRUE) {
  #create numeric version of the highest education variable
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      employment_prior_covid_numeric =
        case_when(
          employ.fulltime_employed_numeric == "1" ~ 1,
          employ.parttime_employed_numeric == "1" ~ 1,
          employ.unemployed_numeric == "1" ~ 2,
          employ.zerohours_contract_numeric == "1" ~ 1,
          employ.selfemployed_numeric == "1" ~ 1,
          employ.contract_or_freelance_work_numeric == "1" ~ 1,
          employ.small_business_owner_numeric == "1" ~ 1,
          employ.retired_numeric  ==  "1" ~ 4,
          employ.student_gcse_or_a_level_numeric == "1" ~ 3,
          employ.student_university_numeric == "1" ~ 3
        )
    )
  #recode the numeric version into a factor
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      employment_prior_covid =
        recode_factor(
          employment_prior_covid_numeric,
          `1` = "Employed",
          `2` = "Unemployed",
          `3` = "Student",
          `4` = "Retired")
    )
  ramp.dem.employ.raw.id %>%
    freq(employment_prior_covid)
}


#Check Employment prior to pandemic coding for RAMP
if(RAMP == TRUE) {
  ramp.dem.employ.raw.id %>%
    select(employment_prior_covid,
           employment_prior_covid_numeric,
           employ.fulltime_employed_numeric,
           employ.parttime_employed_numeric,
           employ.unemployed_numeric,
           employ.zerohours_contract_numeric,
           employ.selfemployed_numeric,
           employ.contract_or_freelance_work_numeric,
           employ.small_business_owner_numeric,
           employ.retired_numeric,
           employ.student_gcse_or_a_level_numeric,
           employ.student_university_numeric
    )
}


## Employment change (baseline, GLAD)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(GLAD == TRUE) {
  #create numeric version of the highest education variable
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id %>%
    mutate(
      employment_change_numeric =
        case_when(
          employ.reduction_in_hours_numeric == "1" ~ 1,
          employ.reduction_in_salary_numeric == "1" ~ 1,
          employ.became_unemployed_numeric == "1" ~ 1,
          employ.benefits_decreased_numeric == "1" ~ 1,
          employ.increased_hours_numeric == "1" ~ 2,
          employ.increased_salary_numeric == "1" ~ 3,
          employ.became_employed_numeric == "1" ~ 3,
          employ.benefits_increased_numeric  ==  "1" ~ 2,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 3,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 3,
          employ.paid_leave_furloughed_numeric == "1" ~ 3,
          employ.my_employment_status_has_not_changed_numeric == "1" ~ 4
        )
    )
  
  #recode the numeric version into a factor
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id %>%
    mutate(
      employment_change =
        recode_factor(
          employment_change_numeric,
          `1` = "Decreased employment",
          `2` = "Increased employment",
          `3` = "Furloughed",
          `4` = "Employment not changed")
    )
  glad.coping.dem4.employ.raw.id %>%
    freq(employment_change)
}



#Check Employment change coding
if(GLAD == TRUE) {
  glad.coping.dem4.employ.raw.id %>%
    select(employment_change,
           employment_change_numeric,
           employ.reduction_in_hours_numeric,
           employ.reduction_in_salary_numeric,
           employ.became_unemployed_numeric,
           employ.benefits_decreased_numeric,
           employ.increased_hours_numeric,
           employ.increased_salary_numeric,
           employ.became_employed_numeric,
           employ.benefits_increased_numeric,
           employ.furloughed_or_paid_leave_government_funded_numeric,
           employ.furloughed_or_paid_leave_company_funded_numeric,
           employ.paid_leave_furloughed_numeric,
           employ.my_employment_status_has_not_changed_numeric
    )
}


## Employment change (baseline, EDGI)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(EDGI == TRUE) {
  #create numeric version of the highest education variable
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      employment_change_numeric =
        case_when(
          employ.reduction_in_hours_numeric == "1" ~ 1,
          employ.reduction_in_salary_numeric == "1" ~ 1,
          employ.became_unemployed_numeric == "1" ~ 1,
          employ.benefits_decreased_numeric == "1" ~ 1,
          employ.increased_hours_numeric == "1" ~ 2,
          employ.increased_salary_numeric == "1" ~ 3,
          employ.became_employed_numeric == "1" ~ 3,
          employ.benefits_increased_numeric  ==  "1" ~ 2,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 3,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 3,
          employ.paid_leave_furloughed_numeric == "1" ~ 3,
          employ.my_employment_status_has_not_changed_numeric == "1" ~ 4
        )
    )
  
  #recode the numeric version into a factor
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id %>%
    mutate(
      employment_change =
        recode_factor(
          employment_change_numeric,
          `1` = "Decreased employment",
          `2` = "Increased employment",
          `3` = "Furloughed",
          `4` = "Employment not changed")
    )
  edgi.coping.dem4.employ.raw.id %>%
    freq(employment_change)
}


##Check Employment change coding
if(EDGI == TRUE) {
  edgi.coping.dem4.employ.raw.id %>%
    select(employment_change,
           employment_change_numeric,
           employ.reduction_in_hours_numeric,
           employ.reduction_in_salary_numeric,
           employ.became_unemployed_numeric,
           employ.benefits_decreased_numeric,
           employ.increased_hours_numeric,
           employ.increased_salary_numeric,
           employ.became_employed_numeric,
           employ.benefits_increased_numeric,
           employ.furloughed_or_paid_leave_government_funded_numeric,
           employ.furloughed_or_paid_leave_company_funded_numeric,
           employ.paid_leave_furloughed_numeric,
           employ.my_employment_status_has_not_changed_numeric
    )
}

## Employment change (baseline, NBR)
#Proceed along qualifications fields. If field is not blank, take that as highest qual. If field is blank, check next field. If all fields are blank, NA
if(NBR == TRUE) {
  #create numeric version of the highest education variable
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      employment_change_numeric =
        case_when(
          employ.reduction_in_hours_numeric == "1" ~ 1,
          employ.reduction_in_salary_numeric == "1" ~ 1,
          employ.became_unemployed_numeric == "1" ~ 1,
          employ.benefits_decreased_numeric == "1" ~ 1,
          employ.increased_hours_numeric == "1" ~ 2,
          employ.increased_salary_numeric == "1" ~ 3,
          employ.became_employed_numeric == "1" ~ 3,
          employ.benefits_increased_numeric  ==  "1" ~ 2,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 3,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 3,
          employ.paid_leave_furloughed_numeric == "1" ~ 3,
          employ.my_employment_status_has_not_changed_numeric == "1" ~ 4
        )
    )
  
  #recode the numeric version into a factor
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id %>%
    mutate(
      employment_change =
        recode_factor(
          employment_change_numeric,
          `1` = "Decreased employment",
          `2` = "Increased employment",
          `3` = "Furloughed",
          `4` = "Employment not changed")
    )
  nbr.dem.employ.raw.id %>%
    freq(employment_change)
}


#Check Employment change coding
if(NBR == TRUE) {
  nbr.dem.employ.raw.id %>%
    select(employment_change,
           employment_change_numeric,
           employ.reduction_in_hours_numeric,
           employ.reduction_in_salary_numeric,
           employ.became_unemployed_numeric,
           employ.benefits_decreased_numeric,
           employ.increased_hours_numeric,
           employ.increased_salary_numeric,
           employ.became_employed_numeric,
           employ.benefits_increased_numeric,
           employ.furloughed_or_paid_leave_government_funded_numeric,
           employ.furloughed_or_paid_leave_company_funded_numeric,
           employ.paid_leave_furloughed_numeric,
           employ.my_employment_status_has_not_changed_numeric
    )
}

## Employment change (baseline, RAMP)
if(RAMP == TRUE) {
  #create numeric version of the highest education variable
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      employment_change_numeric =
        case_when(
          employ.reduction_in_hours_numeric == "1" ~ 1,
          employ.reduction_in_salary_numeric == "1" ~ 1,
          employ.became_unemployed_numeric == "1" ~ 1,
          employ.benefits_decreased_numeric == "1" ~ 1,
          employ.increased_hours_numeric == "1" ~ 2,
          employ.increased_salary_numeric == "1" ~ 3,
          employ.became_employed_numeric == "1" ~ 3,
          employ.benefits_increased_numeric  ==  "1" ~ 2,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 3,
          employ.furloughed_or_paid_leave_company_funded_numeric== "1" ~ 3,
          employ.paid_leave_furloughed_numeric == "1" ~ 3,
          employ.my_employment_status_has_not_changed_numeric == "1" ~ 4
        )
    )
  #recode the numeric version into a factor
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      employment_change =
        recode_factor(
          employment_change_numeric,
          `1` = "Decreased employment",
          `2` = "Increased employment",
          `3` = "Furloughed",
          `4` = "Employment not changed")
    )
  ramp.dem.employ.raw.id %>%
    freq(employment_change)
}

#Check Employment changen coding for RAMP
if(RAMP == TRUE) {
  ramp.dem.employ.raw.id %>%
    select(employment_change,
           employment_change_numeric,
           employ.reduction_in_hours_numeric,
           employ.reduction_in_salary_numeric,
           employ.became_unemployed_numeric,
           employ.benefits_decreased_numeric,
           employ.increased_hours_numeric,
           employ.increased_salary_numeric,
           employ.became_employed_numeric,
           employ.benefits_increased_numeric,
           employ.furloughed_or_paid_leave_government_funded_numeric,
           employ.furloughed_or_paid_leave_company_funded_numeric,
           employ.paid_leave_furloughed_numeric,
           employ.my_employment_status_has_not_changed_numeric
    )
}


## Key worker status GLAD
if(GLAD == TRUE) {
  #create numeric version of the variable
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == "0" ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
          employ.unemployed_numeric == "1" ~ 0,
          employ.stayathome_parent_or_carer_numeric == "1" ~ 0,
          employ.retired_numeric == "1" ~ 0,
          employ.became_unemployed_numeric == "0" ~ 0,
          employ.student_gcse_or_a_level_numeric == "1" ~ 0,
          employ.student_university_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 0,
          employ.paid_leave_furloughed_numeric == "1" ~ 0
        )
    )
  #recode the numeric version into a factor
  glad.coping.dem4.employ.raw.id  <- glad.coping.dem4.employ.raw.id  %>%
    mutate(
      key_worker =
        recode_factor(
          key_worker_numeric,
          `0` = "No key worker",
          `1` = "Key worker"
        )
    )
  glad.coping.dem4.employ.raw.id  %>%
    freq(key_worker)
}


## Employment status
if(GLAD == TRUE) {
  #create numeric version of the variable
  glad.coping.dem4.employ.raw.id <- glad.coping.dem4.employ.raw.id  %>%
    mutate(
      employed =
        case_when(
          employ.became_unemployed == "Became unemployed" ~ "Unemployed",
          
          employ.became_employed== "Became employed" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired") &
              (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.other == "Other") ~ "Unemployed"),
          
          employ.reduction_in_hours == "Reduction in hours" |
            employ.reduction_in_salary == "Reduction in salary" |
            employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
            employ.increased_hours == "Increased hours" |
            employ.increased_salary == "Increased salary" |
            employ.furloughed_or_paid_leave_government_funded == "Furloughed or paid leave (Government funded)" |
            employ.furloughed_or_paid_leave_company_funded == "Furloughed or paid leave (Company funded)" |
            employ.taking_unpaid_leave == "Taking unpaid leave" |
            employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired")  &
              (employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased") ~ "Unemployed"),
          
          (
            (employ.fulltime_employed   == "Full-time employed" |
               employ.parttime_employed == "Part-time employed" |
               employ.zerohours_contract == "Zero-hours contract" |
               employ.stayathome_parent_or_carer == "Stay-at-home parent or carer" |
               employ.selfemployed == "Self-employed" |
               employ.contract_or_freelance_work == "Contract or freelance work" |
               employ.small_business_owner == "Small business owner") &
              (
                employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                  employ.benefits_increased == "Benefits increased" |
                  employ.benefits_decreased == "Benefits decreased" |
                  employ.other == "Other") ~ "Employed"),
          
          (
            (employ.student_gcse_or_a_level == "Student (GCSE or A level)" |
               employ.student_university == "Student (University)"
            ) &
              (is.na(employ.my_employment_status_has_not_changed) |
                 employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.reduction_in_hours == "Reduction in hours" |
                 employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                 employ.increased_hours == "Increased hours" |
                 employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased" |
                 employ.other == "Other") ~ "Student"),
          
          
        )
    )
}
  
## Key worker status edgi
if(EDGI == TRUE) {
  #create numeric version of the variable
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == "0" ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
          employ.unemployed_numeric == "1" ~ 0,
          employ.stayathome_parent_or_carer_numeric == "1" ~ 0,
          employ.retired_numeric == "1" ~ 0,
          employ.became_unemployed_numeric == "0" ~ 0,
          employ.student_gcse_or_a_level_numeric == "1" ~ 0,
          employ.student_university_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 0,
          employ.paid_leave_furloughed_numeric == "1" ~ 0
        )
    )
  #recode the numeric version into a factor
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id  %>%
    mutate(
      key_worker =
        recode_factor(
          key_worker_numeric,
          `0` = "No key worker",
          `1` = "Key worker"
        )
    )
  edgi.coping.dem4.employ.raw.id  %>%
    freq(key_worker)
}

## Employment status
if(EDGI == TRUE) {
  #create factor version of the variable
  edgi.coping.dem4.employ.raw.id <- edgi.coping.dem4.employ.raw.id  %>%
    mutate(
      employed =
        case_when(
          employ.became_unemployed == "Became unemployed" ~ "Unemployed",
          
          employ.became_employed== "Became employed" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired") &
              (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.other == "Other") ~ "Unemployed"),
          
          employ.reduction_in_hours == "Reduction in hours" |
            employ.reduction_in_salary == "Reduction in salary" |
            employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
            employ.increased_hours == "Increased hours" |
            employ.increased_salary == "Increased salary" |
            employ.furloughed_or_paid_leave_government_funded == "Furloughed or paid leave (Government funded)" |
            employ.furloughed_or_paid_leave_company_funded == "Furloughed or paid leave (Company funded)" |
            employ.taking_unpaid_leave == "Taking unpaid leave" |
            employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired")  &
              (employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased") ~ "Unemployed"),
          
          (
            (employ.fulltime_employed   == "Full-time employed" |
               employ.parttime_employed == "Part-time employed" |
               employ.zerohours_contract == "Zero-hours contract" |
               employ.stayathome_parent_or_carer == "Stay-at-home parent or carer" |
               employ.selfemployed == "Self-employed" |
               employ.contract_or_freelance_work == "Contract or freelance work" |
               employ.small_business_owner == "Small business owner") &
              (
                employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                  employ.benefits_increased == "Benefits increased" |
                  employ.benefits_decreased == "Benefits decreased" |
                  employ.other == "Other") ~ "Employed"),
          
          (
            (employ.student_gcse_or_a_level == "Student (GCSE or A level)" |
               employ.student_university == "Student (University)"
            ) &
              (is.na(employ.my_employment_status_has_not_changed) |
                 employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.reduction_in_hours == "Reduction in hours" |
                 employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                 employ.increased_hours == "Increased hours" |
                 employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased" |
                 employ.other == "Other") ~ "Student"),
          
          
        )
    )
}
## Key worker status NBR
if(NBR == TRUE) {
  #create numeric version of the variable
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == "0" ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
          employ.unemployed_numeric == "1" ~ 0,
          employ.stayathome_parent_or_carer_numeric == "1" ~ 0,
          employ.retired_numeric == "1" ~ 0,
          employ.became_unemployed_numeric == "0" ~ 0,
          employ.student_gcse_or_a_level_numeric == "1" ~ 0,
          employ.student_university_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 0,
          employ.paid_leave_furloughed_numeric == "1" ~ 0
        )
    )
  #recode the numeric version into a factor
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id  %>%
    mutate(
      key_worker =
        recode_factor(
          key_worker_numeric,
          `0` = "No key worker",
          `1` = "Key worker"
        )
    )
  nbr.dem.employ.raw.id  %>%
    freq(key_worker)
}
## Employment status
if(NBR == TRUE) {
  #create factor version of the variable
  nbr.dem.employ.raw.id <- nbr.dem.employ.raw.id  %>%
    mutate(
      employed =
        case_when(
          employ.became_unemployed == "Became unemployed" ~ "Unemployed",
          
          employ.became_employed== "Became employed" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired") &
              (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.other == "Other") ~ "Unemployed"),
          
          employ.reduction_in_hours == "Reduction in hours" |
            employ.reduction_in_salary == "Reduction in salary" |
            employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
            employ.increased_hours == "Increased hours" |
            employ.increased_salary == "Increased salary" |
            employ.furloughed_or_paid_leave_government_funded == "Furloughed or paid leave (Government funded)" |
            employ.furloughed_or_paid_leave_company_funded == "Furloughed or paid leave (Company funded)" |
            employ.taking_unpaid_leave == "Taking unpaid leave" |
            employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired")  &
              (employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased") ~ "Unemployed"),
          
          (
            (employ.fulltime_employed   == "Full-time employed" |
               employ.parttime_employed == "Part-time employed" |
               employ.zerohours_contract == "Zero-hours contract" |
               employ.stayathome_parent_or_carer == "Stay-at-home parent or carer" |
               employ.selfemployed == "Self-employed" |
               employ.contract_or_freelance_work == "Contract or freelance work" |
               employ.small_business_owner == "Small business owner") &
              (
                employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                  employ.benefits_increased == "Benefits increased" |
                  employ.benefits_decreased == "Benefits decreased" |
                  employ.other == "Other") ~ "Employed"),
          
          (
            (employ.student_gcse_or_a_level == "Student (GCSE or A level)" |
               employ.student_university == "Student (University)"
            ) &
              (is.na(employ.my_employment_status_has_not_changed) |
                 employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.reduction_in_hours == "Reduction in hours" |
                 employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                 employ.increased_hours == "Increased hours" |
                 employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased" |
                 employ.other == "Other") ~ "Student"),
          
          
        )
    )
}
## Key worker status RAMP
if(RAMP == TRUE) {
  #create numeric version of the variable
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == "0" ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
          employ.unemployed_numeric == "1" ~ 0,
          employ.stayathome_parent_or_carer_numeric == "1" ~ 0,
          employ.retired_numeric == "1" ~ 0,
          employ.became_unemployed_numeric == "0" ~ 0,
          employ.student_gcse_or_a_level_numeric == "1" ~ 0,
          employ.student_university_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_government_funded_numeric == "1" ~ 0,
          employ.furloughed_or_paid_leave_company_funded_numeric == "1" ~ 0,
          employ.paid_leave_furloughed_numeric == "1" ~ 0
        )
    )
  #recode the numeric version into a factor
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id %>%
    mutate(
      key_worker =
        recode_factor(
          key_worker_numeric,
          `0` = "No key worker",
          `1` = "Key worker"
        )
    )
  ramp.dem.employ.raw.id %>%
    freq(key_worker)
}
## Employment status
if(RAMP == TRUE) {
  #create factor version of the variable
  ramp.dem.employ.raw.id <- ramp.dem.employ.raw.id  %>%
    mutate(
      employed =
        case_when(
          employ.became_unemployed == "Became unemployed" ~ "Unemployed",
          
          employ.became_employed== "Became employed" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired") &
              (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.other == "Other") ~ "Unemployed"),
          
          employ.reduction_in_hours == "Reduction in hours" |
            employ.reduction_in_salary == "Reduction in salary" |
            employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
            employ.increased_hours == "Increased hours" |
            employ.increased_salary == "Increased salary" |
            employ.furloughed_or_paid_leave_government_funded == "Furloughed or paid leave (Government funded)" |
            employ.furloughed_or_paid_leave_company_funded == "Furloughed or paid leave (Company funded)" |
           # employ.taking_unpaid_leave == "Taking unpaid leave" |
            employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)" ~ "Employed",
          
          (
            (employ.unemployed == "Unemployed" |
               employ.receiving_state_income == "Receiving state income" |
               employ.retired == "Retired")  &
              (employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased") ~ "Unemployed"),
          
          (
            (employ.fulltime_employed   == "Full-time employed" |
               employ.parttime_employed == "Part-time employed" |
               employ.zerohours_contract == "Zero-hours contract" |
               employ.stayathome_parent_or_carer == "Stay-at-home parent or carer" |
               employ.selfemployed == "Self-employed" |
               employ.contract_or_freelance_work == "Contract or freelance work" |
               employ.small_business_owner == "Small business owner") &
              (
                employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                  employ.benefits_increased == "Benefits increased" |
                  employ.benefits_decreased == "Benefits decreased" |
                  employ.other == "Other") ~ "Employed"),
          
          (
            (employ.student_gcse_or_a_level == "Student (GCSE or A level)" |
               employ.student_university == "Student (University)"
            ) &
              (is.na(employ.my_employment_status_has_not_changed) |
                 employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                 employ.reduction_in_hours == "Reduction in hours" |
                 employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                 employ.increased_hours == "Increased hours" |
                 employ.benefits_increased == "Benefits increased" |
                 employ.benefits_decreased == "Benefits decreased" |
                 employ.other == "Other") ~ "Student"),
          
          
        )
    )
}
