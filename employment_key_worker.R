## Key worker status GLAD
if(GLAD == TRUE) {
  # Create numeric version of the variable
  glad_coping_employment_id <- glad_coping_employment_id  %>%
    mutate(
      key_worker_numeric =
        case_when( 
          employ.government_work_key_workers_numeric == 0 ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
        )
    )
  
  # Recode the numeric version into a factor
  glad_coping_employment_id  <- glad_coping_employment_id  %>%
    mutate(
      key_worker =
        case_when(
          key_worker_numeric == 0 ~ "No key worker",
          key_worker_numeric == 1 ~ "Key worker"
        )
    )
  glad_coping_employment_id  %>%
    freq(key_worker)
  
  # NB: Only those who answer 'Full time employed', 'Part-time employed', Unemployed, Zero-hours, Stay at home parent, Self-employed, Contract/freelance work, Small business owner, or Recieving state income get asked the follow-up question.
  
  # Working out if still in paid employment at COPING baseline
  glad_coping_employment_id <- glad_coping_employment_id %>%
    mutate(baseline_paid_employment =
             case_when(
               
               # PAID EMPLOYMENT
               # Anyone who indicates reduction or increase in salary or being furloughed must be in paid employment
               employ.reduction_in_salary == "Reduction in salary" |
                 employ.increased_salary == "Increased salary" |
                 employ.furloughed_or_paid_leave_ == "Furloughed or paid leave (Government funded)" |
                 employ.furloughed_or_paid_leave_.1 == "Furloughed or paid leave (Company funded)" |
                 employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)"~ "Paid employment",
               
               # Anyone who indicates they were previously employed and... 
               ( (employ.fulltime_employed_numeric == "1"  |
                    employ.parttime_employed_numeric == "1"  |
                    employ.zerohours_contract_numeric == "1"|
                    employ.selfemployed_numeric == "1"|
                    employ.contract_or_freelance_work_numeric == "1" |
                    employ.small_business_owner_numeric == "1") &
                   
                   (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                      employ.reduction_in_hours == "Reduction in hours" |
                      employ.benefits_increased  ==  "Benefits increased" |
                      employ.benefits_decreased == "Benefits decreased" |
                      employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                      employ.increased_hours == "Increased hours" |
                      employ.other == "Other")) ~ "Paid employment",
               
               # Anyone who indicates they have become employed
               employ.became_employed == "Became employed" ~ "Paid employment",
               
               # NOT IN PAID EMPLOYMENT
               # Previously indicated not in paid employment and...
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  
                  (employ.my_employment_status_has_not_changed == "My employment status has not changed" |
                     employ.benefits_increased  ==  "Benefits increased" |
                     employ.benefits_decreased == "Benefits decreased" |
                     employ.change_in_duties_or_responsibilities == "Change in duties or responsibilities" |
                     employ.other == "Other")) ~ "Not in paid employment",
               
               # Stay at home parent/carer with reduced hours
               employ.stayathome_parent_or_carer_numeric == "1" &
                 (employ.reduction_in_hours_numeric == "1" |
                    employ.increased_hours_numeric == "1") ~ "Not in paid employment",
               
               # Taking unpaid leave
               employ.taking_unpaid_leave == "Taking unpaid leave" ~ "Not in paid employment",
               
               # Became unemployed
               employ.became_unemployed == "Became unemployed" ~ "Not in paid employment",
               
               # RETIRED
               # Previously indicated retired (anyone who put 'retired' didn't get follow-up questions)
               employ.retired == "Retired" ~ "Retired",
               
               # STUDENT (anyone who put 'student' didn't get follow-up questions)
               employ.student__numeric == "1" |
                 employ.student_.1_numeric == "1" ~ "Student",
               
               # TO COVER PEOPLE WHO DID NOT ANSWER FOLLOW-UP QUESTION
               
               # Anyone who indicates they were previously employed and didn't answer follow-up question 
               ((employ.fulltime_employed_numeric == "1"  |
                   employ.parttime_employed_numeric == "1"  |
                   employ.zerohours_contract_numeric == "1"|
                   employ.selfemployed_numeric == "1"|
                   employ.contract_or_freelance_work_numeric == "1" |
                   employ.small_business_owner_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Paid employment",
               
               
               # Previously indicated not in paid employment and didn't answer follow-up question 
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Not in paid employment",
               
               # TO COVER PERSON WHO DID ANSWER FOLLOW-UP QUESTION (but answer didn't really make sense)
               employ.receiving_state_income_numeric == "1" &
                 employ.increased_hours_numeric == "1" ~ "Not in paid employment"
               )
           
    )
  
  # Check
  glad_coping_employment_id %>%
    freq(baseline_paid_employment) 
  # NB: There is one person left who put "Not None of the above" for prepan employment but then is NA for all of them (i.e., didn't indicate which one) and then put "My employment status has not changed"
  
  
}


## Key worker status EDGI
if(EDGI == TRUE) {
  # Create numeric version of the variable
  edgi_coping_employment_id <- edgi_coping_employment_id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == 0 ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
        )
    )
  
  # Recode the numeric version into a factor
  edgi_coping_employment_id  <- edgi_coping_employment_id  %>%
    mutate(
      key_worker =
        case_when(
          key_worker_numeric == 0 ~ "No key worker",
          key_worker_numeric == 1 ~ "Key worker"
        )
    )
  edgi_coping_employment_id  %>%
    freq(key_worker)
  
  # NB: Only those who answer 'Full time employed', 'Part-time employed', Unemployed, Zero-hours, Stay at home parent, Self-employed, Contract/freelance work, Small business owner, or Recieving state income get asked the follow-up question.
  # Working out if still in paid employment at COPING baseline
  edgi_coping_employment_id <- edgi_coping_employment_id %>%
    mutate(baseline_paid_employment =
             case_when(
               
               # PAID EMPLOYMENT
               # Anyone who indicates reduction or increase in salary or furloughed must be in paid employment
               employ.reduction_in_salary_numeric == "1" |
                 employ.increased_salary_numeric == "1"  |
                 employ.furloughed_or_paid_leave_ == "Furloughed or paid leave (Government funded)" |
                 employ.furloughed_or_paid_leave_.1 == "Furloughed or paid leave (Company funded)" |
                 employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)"~ "Paid employment",
              
                # Anyone who indicates they were previously employed and... 
               ( ( employ.fulltime_employed_numeric == "1"  |
                     employ.parttime_employed_numeric == "1"  |
                     employ.zerohours_contract_numeric == "1"|
                     employ.selfemployed_numeric == "1"|
                     employ.contract_or_freelance_work_numeric == "1"|
                     employ.small_business_owner_numeric == "1") &
                   
                   (employ.my_employment_status_has_not_changed_numeric == "1" |
                      employ.reduction_in_hours_numeric == "1" |
                      employ.benefits_increased_numeric == "1" |
                      employ.benefits_decreased_numeric == "1" |
                      employ.change_in_duties_or_responsibilities_numeric == "1" |
                      employ.increased_hours_numeric == "1" |
                      employ.other_numeric == "1" )) ~ "Paid employment",
               
               # Anyone who indicates they have become employed
               employ.became_employed_numeric == "1" ~ "Paid employment",
               
               # NOT IN PAID EMPLOYMENT
               # Previously indicated not in paid employment and...
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  
                  (employ.my_employment_status_has_not_changed_numeric == "1" | 
                     employ.benefits_increased_numeric == "1" | 
                     employ.benefits_decreased_numeric == "1" | 
                     employ.change_in_duties_or_responsibilities_numeric == "1" | 
                     employ.other_numeric == "1" )) ~ "Not in paid employment",
               
               # Stay at home parent/carer with reduced hours
               employ.stayathome_parent_or_carer_numeric == "1" &
                 (employ.reduction_in_hours_numeric == "1" |
                    employ.increased_hours_numeric == "1") ~ "Not in paid employment",
               
               # Taking unpaid leave
               employ.taking_unpaid_leave_numeric == "1" ~ "Not in paid employment",
               
               # Became unemployed
               employ.became_unemployed_numeric == "1"  ~ "Not in paid employment",
               
               # RETIRED
               # Previously indicated retired (anyone who put 'retired' didn't get follow-up questions)
               employ.retired_numeric == "1"  ~ "Retired",
               
               # STUDENT (anyone who put 'student' didn't get follow-up questions)
               employ.student__numeric == "1" |
                 employ.student_.1_numeric == "1" ~ "Student",
               
               # TO COVER PEOPLE WHO DID NOT ANSWER FOLLOW-UP QUESTION
               
               # Anyone who indicates they were previously employed and didn't answer follow-up question  
               ((employ.fulltime_employed_numeric == "1"  |
                   employ.parttime_employed_numeric == "1"  |
                   employ.zerohours_contract_numeric == "1"|
                   employ.selfemployed_numeric == "1"|
                   employ.contract_or_freelance_work_numeric == "1" |
                   employ.small_business_owner_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Paid employment",
               
               
               # Previously indicated not in paid employment and didn't answer follow-up question 
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Not in paid employment"
               
             )
           
    )
  
  # Check
  edgi_coping_employment_id %>%
    freq(baseline_paid_employment)
  
}

## Key worker status NBR
if(NBR == TRUE) {
  # Create numeric version of the variable
  nbr_dem_employ_raw_id <- nbr_dem_employ_raw_id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == 0 ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
        )
    )
  
  # Recode the numeric version into a factor
  nbr_dem_employ_raw_id  <- nbr_dem_employ_raw_id  %>%
    mutate(
      key_worker =
        case_when(
          key_worker_numeric == 0 ~ "No key worker",
          key_worker_numeric == 1 ~ "Key worker"
        )
    )
  nbr_dem_employ_raw_id  %>%
    freq(key_worker)
  
  # NB: Only those who answer 'Full time employed', 'Part-time employed', Unemployed, Zero-hours, Stay at home parent, Self-employed, Contract/freelance work, Small business owner, or Recieving state income get asked the follow-up question.
  # Working out if still in paid employment at COPING baseline
  nbr_dem_employ_raw_id <- nbr_dem_employ_raw_id %>%
    mutate(baseline_paid_employment =
             case_when(
               
               # PAID EMPLOYMENT
               # Anyone who indicates reduction or increase in salary or furloughed must be in paid employment
               employ.reduction_in_salary_numeric == "1" |
                 employ.increased_salary_numeric == "1"  |
                 employ.furloughed_or_paid_leave_ == "Furloughed or paid leave (Government funded)" |
                 employ.furloughed_or_paid_leave_.1 == "Furloughed or paid leave (Company funded)" |
                 employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)"~ "Paid employment",
               
               # Anyone who indicates they were previously employed and... 
               ( ( employ.fulltime_employed_numeric == "1"  |
                     employ.parttime_employed_numeric == "1"  |
                     employ.zerohours_contract_numeric == "1"|
                     employ.selfemployed_numeric == "1" |
                     employ.contract_or_freelance_work_numeric == "1" |
                     employ.small_business_owner_numeric == "1") &
                   
                   (employ.my_employment_status_has_not_changed_numeric == "1" |
                      employ.reduction_in_hours_numeric == "1" |
                      employ.benefits_increased_numeric == "1" |
                      employ.benefits_decreased_numeric == "1" |
                      employ.change_in_duties_or_responsibilities_numeric == "1" |
                      employ.increased_hours_numeric == "1" |
                      employ.other_numeric == "1" )) ~ "Paid employment",
               
               # Anyone who indicates they have become employed
               employ.became_employed_numeric == "1" ~ "Paid employment",
               
               # NOT IN PAID EMPLOYMENT
               # Previously indicated not in paid employment and...
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  
                  (employ.my_employment_status_has_not_changed_numeric == "1" | 
                     employ.benefits_increased_numeric == "1" | 
                     employ.benefits_decreased_numeric == "1" | 
                     employ.change_in_duties_or_responsibilities_numeric == "1" | 
                     employ.other_numeric == "1" )) ~ "Not in paid employment",
               
               # Stay at home parent/carer with reduced hours
               employ.stayathome_parent_or_carer_numeric == "1" &
                 (employ.reduction_in_hours_numeric == "1" |
                    employ.increased_hours_numeric == "1") ~ "Not in paid employment",
               
               # Taking unpaid leave
               employ.taking_unpaid_leave_numeric == "1" ~ "Not in paid employment",
               
               # Became unemployed
               employ.became_unemployed_numeric == "1"  ~ "Not in paid employment",
               
               # RETIRED
               # Previously indicated retired (anyone who put 'retired' didn't get follow-up questions)
               employ.retired_numeric == "1"   ~ "Retired",
               
               # STUDENT (anyone who put 'student' didn't get follow-up questions)
               employ.student__numeric == "1" |
                 employ.student_.1_numeric == "1" ~ "Student",
               
               # TO COVER PEOPLE WHO DID NOT ANSWER FOLLOW-UP QUESTION
               
               # Anyone who indicates they were previously employed and didn't answer follow-up question 
               ((employ.fulltime_employed_numeric == "1"  |
                   employ.parttime_employed_numeric == "1"  |
                   employ.zerohours_contract_numeric == "1"|
                   employ.selfemployed_numeric == "1"|
                   employ.contract_or_freelance_work_numeric == "1" |
                   employ.small_business_owner_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Paid employment",
               
               
               # Previously indicated not in paid employment and didn't answer follow-up question 
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Not in paid employment",
               
               # TO COVER PERSON WHO DID ANSWER FOLLOW-UP QUESTION (but answer didn't really make sense)
               employ.receiving_state_income_numeric == "1" &
                 employ.increased_hours_numeric == "1" ~ "Not in paid employment"
               
               
             )
           
    )
  
  # Check
  nbr_dem_employ_raw_id %>%
    freq(baseline_paid_employment) # NB: Again, there is someone who did not answer first question about prepan employment status and then put "My employment status has not changed"
  
}

## Key worker status RAMP
if(RAMP == TRUE) {
  # Create numeric version of the variable
  ramp_dem_employ_raw_id <- ramp_dem_employ_raw_id  %>%
    mutate(
      key_worker_numeric =
        case_when(
          employ.government_work_key_workers_numeric == 0 ~ 0,
          employ.government_work_key_workers_numeric > 0 ~ 1,
        )
    )
  
  # Recode the numeric version into a factor
  ramp_dem_employ_raw_id  <- ramp_dem_employ_raw_id  %>%
    mutate(
      key_worker =
        case_when(
          key_worker_numeric == 0 ~ "No key worker",
          key_worker_numeric == 1 ~ "Key worker"
        )
    )
  ramp_dem_employ_raw_id  %>%
    freq(key_worker)
  
  # NB: Only those who answer 'Full time employed', 'Part-time employed', Unemployed, Zero-hours, Stay at home parent, Self-employed, Contract/freelance work, Small business owner, or Receiving state income get asked the follow-up question.
 
# Working out if still in paid employment at RAMP baseline
  ramp_dem_employ_raw_id <- ramp_dem_employ_raw_id %>%
    mutate(baseline_paid_employment =
             case_when(
               
               # PAID EMPLOYMENT
               # Anyone who indicates reduction or increase in salary or furloughed must be in paid employment
               employ.reduction_in_salary_numeric == "1" |
                 employ.increased_salary_numeric == "1" |
                 employ.furloughed_or_paid_leave_ == "Furloughed or paid leave (Government funded)" |
                 employ.furloughed_or_paid_leave_.1 == "Furloughed or paid leave (Company funded)" |
                 employ.paid_leave_furloughed == "Furloughed or paid leave (Government funded with company supplement)"~ "Paid employment",
               
               # Anyone who indicates they were previously employed and... 
               ( ( employ.fulltime_employed_numeric == "1"  |
                     employ.parttime_employed_numeric == "1"  |
                     employ.zerohours_contract_numeric == "1"|
                     employ.selfemployed_numeric == "1"|
                     employ.contract_or_freelance_work_numeric == "1" |
                     employ.small_business_owner_numeric == "1") &
                   
                   (employ.my_employment_status_has_not_changed_numeric == "1" |
                      employ.reduction_in_hours_numeric == "1" |
                      employ.benefits_increased_numeric == "1" |
                      employ.benefits_decreased_numeric == "1" |
                      employ.change_in_duties_or_responsibilities_numeric == "1" |
                      employ.increased_hours_numeric == "1" |
                      employ.other_numeric == "1" )) ~ "Paid employment",
               
               # Anyone who indicates they have become employed
               employ.became_employed_numeric == "1" ~ "Paid employment",
               
               # NOT IN PAID EMPLOYMENT
               # Previously indicated not in paid employment and...
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  
                  (employ.my_employment_status_has_not_changed_numeric == "1" | 
                     employ.benefits_increased_numeric == "1" | 
                     employ.benefits_decreased_numeric == "1" | 
                     employ.change_in_duties_or_responsibilities_numeric == "1" | 
                     employ.other_numeric == "1" )) ~ "Not in paid employment",
               
               # Stay at home parent/carer with reduced hours
               employ.stayathome_parent_or_carer_numeric == "1" &
                 (employ.reduction_in_hours_numeric == "1" |
                    employ.increased_hours_numeric == "1") ~ "Not in paid employment",
               
               # Taking unpaid leave
               # employ.taking_unpaid_leave_numeric == "1" ~ "Not in paid employment", # Not in RAMP
               
               # Became unemployed
               employ.became_unemployed_numeric == "1"  ~ "Not in paid employment",
               
               # RETIRED (anyone who put 'retired' didn't get follow-up questions)
               # Previously indicated retired 
               employ.retired_numeric == "1"   ~ "Retired",
               
               # STUDENT (anyone who put 'student' didn't get follow-up questions)
               employ.student__numeric == "1" |
                 employ.student_.1_numeric == "1" ~ "Student",
               
               # TO COVER PEOPLE WHO DID NOT ANSWER FOLLOW-UP QUESTION
               
               # Anyone who indicates they were previously employed and didn't answer follow-up question
               ((employ.fulltime_employed_numeric == "1"  |
                   employ.parttime_employed_numeric == "1"  |
                   employ.zerohours_contract_numeric == "1"|
                   employ.selfemployed_numeric == "1"|
                   employ.contract_or_freelance_work_numeric == "1" |
                   employ.small_business_owner_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Paid employment",
               
               
               # Previously indicated not in paid employment and didn't answer follow-up question
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1" |
                   employ.stayathome_parent_or_carer_numeric == "1") &
                  is.na(employ.my_employment_status_has_not_changed)) ~ "Not in paid employment",
               
               # TO COVER 2 PEOPLE WHO DID ANSWER FOLLOW-UP QUESTION (but answer didn't really make sense)
               ((employ.unemployed_numeric == "1" |
                   employ.receiving_state_income_numeric == "1") &
                  (employ.reduction_in_hours_numeric == "1" |
                     employ.increased_hours_numeric == "1")) ~ "Not in paid employment"
               
             )
           
    )
  
  # Check
  ramp_dem_employ_raw_id %>%
    freq(baseline_paid_employment)
  
}
