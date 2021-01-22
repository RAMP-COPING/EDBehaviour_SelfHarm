###Functions (not in r)

#Add_numeric function
#Function used to convert character variables into numeric variables.
add_numeric <- function(dat, exclude = NULL) {
  dat_fct <- sjlabelled::as_label(dat)
  dat <- dat[!colnames(dat) %in% exclude]
  colnames(dat) <- paste(colnames(dat), "numeric", sep = "_")
  return(bind_cols(dat_fct, dat))
}

# Function to define a 'not in' operator as %!in%

'%!in%' <- function(x,y)!('%in%'(x,y))

