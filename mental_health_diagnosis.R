if(EDGI == TRUE) {
  # Mental health diagnosis
  ## EDGI 
  ### These individuals have either a self-reported diagnosis or an algorithm-derived diagnosis.
  
 # Merge all diagnostics data +++ need to add CIDI-D and CIDI-A
edgi_diagnostics <- dplyr::full_join(ed_algorithms_edgi,
                                       mhd_dat_id,
                                       by = c("ID",
                                              "sample"
                                       )
  )
  
  
edgi_diagnostics <- edgi_diagnostics %>%
    mutate(
      mental_health_diagnosis =
        case_when(
          # ++ Algorithm-derived depression
          # ++ Algorithm-derived anxiety
          
          # Algorithm-derived ED
          ed.DSM5_AN_binary_numeric == 1 |                                        
            ed.DSM5_AN_restricting_binary_numeric  == 1 |                                
            ed.DSM5_AN_binge_purge_binary_numeric  == 1 |                             
            ed.DSM5_BED_binary_numeric == 1 |                                           
            ed.DSM5_BN_binary_numeric == 1 |   
            
            # Self-reported psych disorder 
            mhd.mdd_numeric== 1 |                                                      
            mhd.perinatal_depression_numeric== 1 |                                     
            mhd.pmdd_numeric== 1 |                                                     
            mhd.bipolar_disorder_numeric== 1 |                                         
            mhd.gad_numeric== 1 |                                                      
            mhd.social_anxiety_numeric== 1 |                                           
            mhd.specific_phobia_numeric== 1 |                                          
            mhd.agoraphobia_numeric== 1 |                                              
            mhd.panic_disorder_numeric== 1 |                                           
            mhd.panic_attacks_numeric== 1 |                                            
            mhd.ptsd_numeric== 1 |                                                     
            mhd.ocd_numeric== 1 |                                                      
            mhd.bdd_numeric== 1 |                                                      
            mhd.other_ocd_numeric== 1 |                                                
            mhd.an_numeric == 1 |                                                       
            mhd.atypical_an_numeric== 1 |                                              
            mhd.bn_numeric== 1 |                                                       
            mhd.bed_numeric== 1 |                                                      
            mhd.schizophrenia_numeric== 1 |                                            
            mhd.schizoaffective_numeric== 1 |                                          
            mhd.psychosis_numeric== 1 |                                                
            mhd.personality_disorder_numeric== 1 |                                     
            mhd.asd_numeric == 1 |                                                      
            mhd.addadhd_numeric== 1 |                                                  
            mhd.other_numeric == 1 |                                                    
            mhd.personality_disorder_diagnosed_numeric== 1 |  
            mhd.suspected_eating_disorder_diagnosed_numeric == 1 | # Have you ever suspected that you may have an eating disorder, whether or not you were ever diagnosed?   
            mhd.eating_disorder_received_treatment_numeric == 1 | # Have you ever received treatment for an eating disorder?                   
            mhd.atypical_bn_numeric== 1 |                                              
            mhd.atypical_bed_numeric== 1 |                                             
            mhd.purging_disorder_numeric== 1 |                                         
            mhd.nighteating_syndrome_numeric== 1 |                                     
            mhd.pica_numeric== 1 |                                                     
            mhd.avoidantrestrictive_food_intake_disorder_numeric== 1 |                 
            mhd.rumination_disorder_numeric== 1 |                                      
            mhd.feeding_eating_disorder_numeric== 1 |                                  
            mhd.other_eating_disorder_numeric== 1 ~ "Psychiatric disorder",                                  
          
          # No psychiatric disorder
          (mhd.suspected_eating_disorder_diagnosed_numeric == 0 | # Either never suspected an ED (and wasn't shown list of EDs) or...
             mhd.none_eds_numeric == 1) & #...clicked none of the above when presented with list of EDs (may have endorsed previous question by mistake?)
            mhd.none_neuro_numeric == 1 &                                  
            mhd.none_dep_anx_ocd_ptsd_numeric== 1 ~ "No psychiatric disorder",                                   
          
           # Answered PNTA or Don't know at least once                                       
          mhd.dont_know_neuro_numeric == 1 |
            mhd.pnta_neuro_numeric == 1 |
           
            mhd.dont_know_eds_numeric== 1 |                                            
           mhd.pnta_eds_numeric == 1 |  
             
            mhd.dont_know_dep_anx_ocd_ptsd_numeric == 1 |                               
            mhd.pnta_dep_anx_ocd_ptsd_numeric== 1 ~ "PNTA or don't know"
        )
    )
  
  # Check
  edgi_diagnostics %>%
    freq(mental_health_diagnosis)
  
}

# Mental health diagnosis
## GLAD 
### These individuals have either a self-reported diagnosis or an algorithm-derived diagnosis.

if(GLAD == TRUE) {
  # Merge all diagnostics data +++ need to add CIDI-D and CIDI-A
  glad_mhd_and_ED100K <- dplyr::full_join(ed_algorithms_glad,
                                          mhd_dat_id,
                                          by = c("ID",
                                                 "sample"
                                          )
  )
  
glad_diagnostics <- dplyr::full_join(glad_mhd_and_ED100K,
                                                 glad_ed100k_selfreport,
                                                 by = c("ID",
                                                        "sample"
                                                 )
  )
  
  
  # Create numeric version of responses from ED100K (these are from the quali text responses)
  glad_diagnostics <- glad_diagnostics %>%
    mutate(
      ED100K_selfreported_ED =
        case_when(
          an.1.anorexia_nervosa_binge_purge == "Anorexia binge-eating/purging" |                    
            an.1.orthorexia == "Orthorexia" |                                      
            an.1.atypical_bulimia_nervosa == "Atypical bulimia nervosa" |                        
            an.1.anorexia_nervosa_athletica == "Anorexia athletica" |                      
            an.1.body_dysmorphic_disorder == "Body Dysmorphic disorder" |                        
            an.1.atypical_binge_eating_disorder == "Atypical BED" |                  
            an.1.diabulimia == "Diabulimia" |                                      
            an.1.EDNOS_or_OSFED == "OSFED/EDNOS" ~ 1                                
          
        )
    )
  
  glad_diagnostics <- glad_diagnostics %>%
    mutate(
      mental_health_diagnosis =
        case_when(
          # ++ Algorithm-derived depression
          # ++ Algorithm-derived anxiety
          
          # ED100K self-report diagnosis from GLAD sign-up (optional questionnaire, AN screener)                  
          an.1.diagnosed_suspected_eating_disorder_numeric == 1 |   
            an.1.eating_disorder_received_treatment_numeric == 1 |      
            an.1.anorexia_nervosa_numeric == 1 |                        
            an.1.atypical_anorexia_nervosa_numeric == 1 |               
            an.1.bulimia_nervosa_numeric == 1 |                         
            an.1.bingeeating_disorder_numeric == 1 |                    
            an.1.purging_disorder_numeric == 1 |                        
            an.1.night_eating_syndrome_numeric == 1 |                   
            an.1.pica_numeric == 1 |                                    
            an.1.avoidantrestrictive_food_intake_disorder_numeric == 1 |
            an.1.rumination_disorder_numeric == 1 |                     
            an.1.other_eating_disorder_numeric == 1 |                   
            ED100K_selfreported_ED == 1 |               
            
            # Algorithm-derived ED - GLAD OPTIONAL ED100K 
            ed.DSM5_AN_binary_numeric == 1 |                                        
            ed.DSM5_AN_restricting_binary_numeric  == 1 |                                
            ed.DSM5_AN_binge_purge_binary_numeric  == 1 |                             
            ed.DSM5_BED_binary_numeric == 1 |                                           
            ed.DSM5_BN_binary_numeric == 1 | 
            
            # Algorithm-derived ED - COPING
            ed.DSM5_AN_binary_numeric_cop == 1 |                                        
            ed.DSM5_AN_restricting_binary_numeric_cop  == 1 |                                
            ed.DSM5_AN_binge_purge_binary_numeric_cop  == 1 |                             
            ed.DSM5_BED_binary_numeric_cop == 1 |                                           
            ed.DSM5_BN_binary_numeric_cop == 1 | 
            
            # Self-reported psych disorder - GLAD SIGNUP MHD
            mhd.mdd_numeric== 1 |                                                      
            mhd.perinatal_depression_numeric== 1 |                                     
            mhd.pmdd_numeric== 1 |                                                     
            mhd.bipolar_disorder_numeric== 1 |                                         
            mhd.gad_numeric== 1 |                                                      
            mhd.social_anxiety_numeric== 1 |                                           
            mhd.specific_phobia_numeric== 1 |                                          
            mhd.agoraphobia_numeric== 1 |                                              
            mhd.panic_disorder_numeric== 1 |                                           
            mhd.panic_attacks_numeric== 1 |                                            
            mhd.ptsd_numeric== 1 |                                                     
            mhd.ocd_numeric== 1 |                                                      
            mhd.bdd_numeric== 1 |                                                      
            mhd.other_ocd_numeric== 1 |                                                
            mhd.an_numeric == 1 |                                                       
            mhd.atypical_an_numeric== 1 |                                              
            mhd.bn_numeric== 1 |                                                       
            mhd.bed_numeric== 1 |                                                      
            mhd.schizophrenia_numeric== 1 |                                            
            mhd.schizoaffective_numeric== 1 |                                          
            mhd.psychosis_numeric== 1 |                                                
            mhd.personality_disorder_numeric== 1 |                                     
            mhd.asd_numeric == 1 |                                                      
            mhd.addadhd_numeric== 1 |                                                  
            mhd.other_numeric == 1 |                                                    
            mhd.personality_disorder_diagnosed_numeric== 1 |  
            mhd.suspected_eating_disorder_diagnosed_numeric == 1 | # Have you ever suspected that you may have an eating disorder, whether or not you were ever diagnosed?   
            mhd.eating_disorder_received_treatment_numeric == 1 | # Have you ever received treatment for an eating disorder?                   
            mhd.atypical_bn_numeric== 1 |                                              
            mhd.atypical_bed_numeric== 1 |                                             
            mhd.purging_disorder_numeric== 1 |                                         
            mhd.nighteating_syndrome_numeric== 1 |                                     
            mhd.pica_numeric== 1 |                                                     
            mhd.avoidantrestrictive_food_intake_disorder_numeric== 1 |                 
            mhd.rumination_disorder_numeric== 1 |                                      
            mhd.feeding_eating_disorder_numeric== 1 |                                  
            mhd.other_eating_disorder_numeric== 1 |
            
            # Self-reported psych disorder - COPING MHD
            mhd.mdd_cop_numeric== 1 |                                                      
            mhd.perinatal_depression_cop_numeric== 1 |                                     
            mhd.pmdd_cop_numeric== 1 |                                                     
            mhd.bipolar_disorder_cop_numeric== 1 |                                         
            mhd.gad_cop_numeric== 1 |                                                      
            mhd.social_anxiety_cop_numeric== 1 |                                           
            mhd.specific_phobia_cop_numeric== 1 |                                          
            mhd.agoraphobia_cop_numeric== 1 |                                              
            mhd.panic_disorder_cop_numeric== 1 |                                           
            mhd.panic_attacks_cop_numeric== 1 |                                            
            mhd.ptsd_cop_numeric== 1 |                                                     
            mhd.ocd_cop_numeric== 1 |                                                      
            mhd.bdd_cop_numeric== 1 |                                                      
            mhd.other_ocd_cop_numeric== 1 |                                                
            mhd.an_cop_numeric == 1 |                                                       
            mhd.atypical_an_cop_numeric== 1 |                                              
            mhd.bn_cop_numeric== 1 |                                                       
            mhd.bed_cop_numeric== 1 |                                                      
            mhd.schizophrenia_cop_numeric== 1 |                                            
            #  mhd.schizoaffective_cop_numeric== 1 |     # Not in COPING                                     
            mhd.psychosis_cop_numeric== 1 |                                                
            mhd.personality_disorder_cop_numeric== 1 |                                     
            mhd.asd_cop_numeric == 1 |                                                      
            mhd.addadhd_cop_numeric== 1 |                                                  
            mhd.other_cop_numeric == 1 |                                                    
            mhd.personality_disorder_diagnosed_cop_numeric== 1 |  
            mhd.suspected_eating_disorder_diagnosed_cop_numeric == 1 | # Have you ever suspected that you may have an eating disorder, whether or not you were ever diagnosed?   
            mhd.eating_disorder_received_treatment_cop_numeric == 1 | # Have you ever received treatment for an eating disorder?                   
            mhd.atypical_bn_cop_numeric== 1 |                                              
            mhd.atypical_bed_cop_numeric== 1 |                                             
            mhd.purging_disorder_cop_numeric== 1 |                                         
            mhd.nighteating_syndrome_cop_numeric== 1 |                                     
            mhd.pica_cop_numeric== 1 |                                                     
            mhd.avoidantrestrictive_food_intake_disorder_cop_numeric== 1 |                 
            mhd.rumination_disorder_cop_numeric== 1 |                                      
            mhd.feeding_eating_disorder_cop_numeric== 1 |                                  
            mhd.other_eating_disorder_cop_numeric== 1 ~ "Psychiatric disorder",                                  
        
            # No psychiatric disorder 
          mhd.suspected_eating_disorder_diagnosed_numeric != 1 & # (NB: 'mhd.suspected_eating_disorder_diagnosed' was, for GLAD, in the optional ED100K so not everyone will have answered it. Therefore someone can be NA for this [as long as they are not 1] and that can count as not having an ED) 
            mhd.suspected_eating_disorder_diagnosed_cop_numeric == 0 &
            mhd.none_eds_cop_numeric == 1 &
            mhd.none_neuro_eds_numeric == 1 & 
            mhd.none_neuro_cop_numeric == 1 &
            mhd.none_dep_anx_ocd_ptsd_numeric == 1 &
            mhd.none_dep_anx_ocd_ptsd_cop_numeric == 1 ~ "No psychiatric disorder",                                   
          
          # Answered PNTA or Don't know at least once               
          an.1.dont_know_numeric == 1 |                               
            an.1.prefer_not_to_answer_numeric == 1 |    
            
          mhd.dont_know_eds_cop_numeric == 1 |
            mhd.dont_know_neuro_eds_numeric == 1 |
            mhd.dont_know_neuro_cop_numeric == 1 |
            mhd.dont_know_dep_anx_ocd_ptsd_numeric == 1 |
            mhd.dont_know_dep_anx_ocd_ptsd_cop_numeric == 1 ~ "No psychiatric disorder",                                   
          
          mhd.pnta_eds_cop_numeric == 1 |
            mhd.pnta_neuro_eds_numeric == 1 | 
            mhd.pnta_neuro_cop_numeric == 1 |
            mhd.pnta_dep_anx_ocd_ptsd_numeric == 1 |
            mhd.pnta_dep_anx_ocd_ptsd_cop_numeric == 1 ~ "PNTA or don't know"
        )
    )
  
  # Check
  glad_diagnostics %>%
    freq(mental_health_diagnosis)
  
}

# Mental health diagnosis
## NBR 
### These individuals have either a self-reported diagnosis or an algorithm-derived diagnosis.

if(NBR == TRUE) {
  # Merge all diagnostics data +++ need to add CIDI-D and CIDI-A
  nbr_diagnostics <- dplyr::full_join(mhd_dat_id,
                                      ed_algorithms_nbr,
                                      by = c("ID",
                                             "sample"
                                      )
  )
  
  
  nbr_diagnostics <- nbr_diagnostics %>%
    mutate(
      mental_health_diagnosis =
        case_when(
          # ++ Algorithm-derived depression
          # ++ Algorithm-derived anxiety
          
          # Algorithm-derived ED - COPING
          ed.DSM5_AN_binary_numeric_cop == 1 |                                        
            ed.DSM5_AN_restricting_binary_numeric_cop  == 1 |                                
            ed.DSM5_AN_binge_purge_binary_numeric_cop  == 1 |                             
            ed.DSM5_BED_binary_numeric_cop == 1 |                                           
            ed.DSM5_BN_binary_numeric_cop == 1 | 
            
            # Self-reported psych disorder - COPING MHD
            mhd.mdd_cop_numeric== 1 |                                                      
            mhd.perinatal_depression_cop_numeric== 1 |                                     
            mhd.pmdd_cop_numeric== 1 |                                                     
            mhd.bipolar_disorder_cop_numeric== 1 |                                         
            mhd.gad_cop_numeric== 1 |                                                      
            mhd.social_anxiety_cop_numeric== 1 |                                           
            mhd.specific_phobia_cop_numeric== 1 |                                          
            mhd.agoraphobia_cop_numeric== 1 |                                              
            mhd.panic_disorder_cop_numeric== 1 |                                           
            mhd.panic_attacks_cop_numeric== 1 |                                            
            mhd.ptsd_cop_numeric== 1 |                                                     
            mhd.ocd_cop_numeric== 1 |                                                      
            mhd.bdd_cop_numeric== 1 |                                                      
            mhd.other_ocd_cop_numeric== 1 |                                                
            mhd.an_cop_numeric == 1 |                                                       
            mhd.atypical_an_cop_numeric== 1 |                                              
            mhd.bn_cop_numeric== 1 |                                                       
            mhd.bed_cop_numeric== 1 |                                                      
            mhd.schizophrenia_cop_numeric== 1 |                                            
            # mhd.schizoaffective_cop_numeric== 1 |     # Not in COPING                                     
            mhd.psychosis_cop_numeric== 1 |                                                
            mhd.personality_disorder_cop_numeric== 1 |                                     
            mhd.asd_cop_numeric == 1 |                                                      
            mhd.addadhd_cop_numeric== 1 |                                                  
            mhd.other_cop_numeric == 1 |                                                    
            mhd.personality_disorder_diagnosed_cop_numeric== 1 |  
            mhd.suspected_eating_disorder_diagnosed_cop_numeric == 1 | # Have you ever suspected that you may have an eating disorder, whether or not you were ever diagnosed?   
            mhd.eating_disorder_received_treatment_cop_numeric == 1 | # Have you ever received treatment for an eating disorder?                   
            mhd.atypical_bn_cop_numeric== 1 |                                              
            mhd.atypical_bed_cop_numeric== 1 |                                             
            mhd.purging_disorder_cop_numeric== 1 |                                         
            mhd.nighteating_syndrome_cop_numeric== 1 |                                     
            mhd.pica_cop_numeric== 1 |                                                     
            mhd.avoidantrestrictive_food_intake_disorder_cop_numeric== 1 |                 
            mhd.rumination_disorder_cop_numeric== 1 |                                      
            mhd.feeding_eating_disorder_cop_numeric== 1 |                                  
            mhd.other_eating_disorder_cop_numeric== 1 ~ "Psychiatric disorder",                                  
          
          # No psychiatric disorder
          (mhd.suspected_eating_disorder_diagnosed_cop_numeric == 0 | # Either never suspected an ED (and wasn't shown list of EDs) or...
             mhd.none_eds_cop_numeric == 1) & #...clicked none of the above when presented with list of EDs (may have endorsed previous question by mistake?)
            
            mhd.none_neuro_cop_numeric == 1 &
            mhd.none_dep_anx_ocd_ptsd_cop_numeric == 1 ~ "No psychiatric disorder",                                   
          
          # Answered PNTA or Don't know at least once               
          mhd.dont_know_dep_anx_ocd_ptsd_cop_numeric  == 1 |
          mhd.pnta_dep_anx_ocd_ptsd_cop_numeric   == 1 |
          
          mhd.none_neuro_cop_numeric  == 1 |
          mhd.dont_know_neuro_cop_numeric  == 1 |
          mhd.pnta_neuro_cop_numeric == 1 | 
          mhd.dont_know_eds_cop_numeric == 1 |
          mhd.pnta_eds_cop_numeric == 1 ~ "PNTA or don't know"
        )
    )
  
  # Check
  nbr_diagnostics %>%
    freq(mental_health_diagnosis)
}

# Mental health diagnosis
## RAMP 
### These individuals have either a self-reported diagnosis or an algorithm-derived diagnosis.

if(RAMP == TRUE) {
  # Rename RAMP 
  ramp_diagnostics <- mhd_dat_id
  
  ramp_diagnostics <- ramp_diagnostics %>%
    mutate(
      mental_health_diagnosis =
        case_when(
          # ++ Algorithm-derived depression?
          # ++ Algorithm-derived anxiety?
          
          # Self-reported psych disorder - COPING MHD
           mhd.mdd_cop_numeric== 1 |                                                      
            mhd.perinatal_depression_cop_numeric== 1 |                                     
            mhd.pmdd_cop_numeric== 1 |                                                     
            mhd.bipolar_disorder_cop_numeric== 1 |                                         
            mhd.gad_cop_numeric== 1 |                                                      
            mhd.social_anxiety_cop_numeric== 1 |                                           
            mhd.specific_phobia_cop_numeric== 1 |                                          
            mhd.agoraphobia_cop_numeric== 1 |                                              
            mhd.panic_disorder_cop_numeric== 1 |                                           
            mhd.panic_attacks_cop_numeric== 1 |                                            
            mhd.ptsd_cop_numeric== 1 |                                                     
            mhd.ocd_cop_numeric== 1 |                                                      
            mhd.bdd_cop_numeric== 1 |                                                      
            mhd.other_ocd_cop_numeric== 1 |                                                
            mhd.an_cop_numeric == 1 |                                                       
            mhd.atypical_an_cop_numeric== 1 |                                              
            mhd.bn_cop_numeric== 1 |                                                       
            mhd.bed_cop_numeric== 1 |                                                      
            mhd.schizophrenia_cop_numeric== 1 |                                            
            #      mhd.schizoaffective_cop_numeric== 1 |     # Not in RAMP                                     
            mhd.psychosis_cop_numeric== 1 |                                                
            mhd.personality_disorder_cop_numeric== 1 |                                     
            mhd.asd_cop_numeric == 1 |                                                      
            mhd.addadhd_cop_numeric== 1 |                                                  
            mhd.other_cop_numeric == 1 |                                                    
            mhd.personality_disorder_diagnosed_cop_numeric== 1 |  
            mhd.suspected_eating_disorder_diagnosed_cop_numeric == 1 | # Have you ever suspected that you may have an eating disorder, whether or not you were ever diagnosed?   
            mhd.eating_disorder_received_treatment_cop_numeric == 1 | # Have you ever received treatment for an eating disorder?                   
            mhd.atypical_bn_cop_numeric== 1 |                                              
            mhd.atypical_bed_cop_numeric== 1 |                                             
            mhd.purging_disorder_cop_numeric== 1 |                                         
            mhd.nighteating_syndrome_cop_numeric== 1 |                                     
            mhd.pica_cop_numeric== 1 |                                                     
            mhd.avoidantrestrictive_food_intake_disorder_cop_numeric== 1 |                 
            mhd.rumination_disorder_cop_numeric== 1 |                                      
            mhd.feeding_eating_disorder_cop_numeric== 1 |                                  
            mhd.other_eating_disorder_cop_numeric== 1 ~ "Psychiatric disorder",                                  
          
          # No psychiatric disorder
          mhd.none_dep_anx_ocd_ptsd_cop_numeric == 1 &
            mhd.none_neuro_eds_cop_numeric == 1~ "No psychiatric disorder",                                   
          
      
          # Answered PNTA or Don't know at least once               
          mhd.dont_know_dep_anx_ocd_ptsd_cop_numeric == 1 |
          mhd.pnta_dep_anx_ocd_ptsd_cop_numeric == 1 |
          
          mhd.dont_know_neuro_eds_cop_numeric == 1 |
          mhd.pnta_neuro_eds_cop_numeric == 1 ~ "PNTA or don't know"
        )
    )
  
  
  # Check
  ramp_diagnostics %>%
    freq(mental_health_diagnosis)
  
}