#CREATING AGE variable (age_unc)
##NBR
###age from birthday

if(NBR == TRUE) {
  #add 1900 to birthyear
  nbr.dem.raw.id$birthday_year_numeric <- nbr.dem.raw.id$birthday_year_numeric + 1900
  
  #create dob as date format
  nbr.dem.raw.id$Birthdate <- as.Date(paste(nbr.dem.raw.id$birthday_year_numeric,
                                            nbr.dem.raw.id$birthday_month_numeric,
                                            nbr.dem.raw.id$birthday_day_numeric,
                                            sep = "-"))
  
  head(nbr.dem.raw.id$Birthdate)
  
  
  #Calculate dem.age(remove NAs as it throws an error)
  ages <- eeptools::age_calc(na.omit(nbr.dem.raw.id$Birthdate), enddate = Sys.Date(), units = "years")
  
  #Create dem.agecolumn
  nbr.dem.raw.id$age_unc<- NA
  
  #Populate with values (excluding NAs)
  nbr.dem.raw.id$age_unc[!is.na(nbr.dem.raw.id$Birthdate)] <- ages
  
  #Round age down
  nbr.dem.raw.id$age_unc <- floor(nbr.dem.raw.id$age_unc)
  
  #Visualise
  head(nbr.dem.raw.id$age_unc)
  head(nbr.dem.raw.id$Birthdate)
}
