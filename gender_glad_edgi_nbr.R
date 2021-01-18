#RAMP has the correct Gender categories so does not need editing.

if(GLAD == TRUE) {
  glad.coping.dem.raw.id <- glad.coping.dem.raw.id %>%
    mutate(
      gender_unc =
        recode_factor(gender_unc,
                      "Male" = "Male",
                      "Female" = "Female",
                      "Non-binary" = "Non-binary",
                      "self-define" = "Prefer to self-define"
        ))
}


# Gender categories synchronised - EDGI
#RAMP has the correct Gender categories so does not need editing.

if(EDGI == TRUE) {
  edgi.coping.dem.raw.id <- edgi.coping.dem.raw.id %>%
    mutate(
      gender_unc =
        recode_factor(gender_unc,
                      "Male" = "Male",
                      "Female" = "Female",
                      "Non-binary" = "Non-binary",
                      "self-define" = "Prefer to self-define"
        ))
}


# Gender categories synchronised - NBR
if(NBR == TRUE) {
  nbr.dem.raw.id <- nbr.dem.raw.id %>%
    mutate(
      gender_unc =
        recode_factor(gender_unc,
                      "Male" = "Male",
                      "Female" = "Female",
                      "Non-binary" = "Non-binary",
                      "Self-define" = "Prefer to self-define"
        ))
}
