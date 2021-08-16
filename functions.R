###Functions (not in r)

#Add_numeric function
#Function used to convert character variables into numeric variables.
# add_numeric function
# Author: Yuhao Lin

add_numeric <- function(dat, exclude = NULL) {
  dat_fct <- sjlabelled::as_label(dat)
  dat <- dat[!colnames(dat) %in% exclude]
  colnames(dat) <- paste(colnames(dat), "numeric", sep = "_")
  return(bind_cols(dat_fct, dat))
}

add_numeric2 <- function(dat,exclude_numeric = "ID") {
  exclude <- sapply(dat, function(col) {
    any(is.na(attr(col, "labels")))
  })
  exclude_col <- colnames(dat)[exclude]
  dat_fct <- dat
  dat_fct[!colnames(dat_fct) %in% exclude_col] <- 
    sjlabelled::as_label(dat_fct[!colnames(dat_fct) %in% exclude_col])
  dat_fct_num <- dat[!colnames(dat) %in% c(exclude_col,exclude_numeric)]
  colnames(dat_fct_num) <- paste(colnames(dat_fct_num), "numeric", sep = "_")
  return(bind_cols(dat_fct, dat_fct_num))
} 

# Function to define a 'not in' operator as %!in%

'%!in%' <- function(x,y)!('%in%'(x,y))

#test<-data.frame(id=c(1,2,3), score1_numeric=c(1,10,3),score2_numeric=c(2,NA,4),score3_numeric=c(NA,NA,9))
#q<-test
sumscores <- function(q, item_order=NULL, na.rm=F, ignore.participant.na.num=1){
  
  # Create vector with scoring key: If the item is reversed coded use -1 ???
  
  q.col.names <- q %>%
    select(
      ends_with("numeric")) %>%
    colnames()
  
  q <- q %>% select(., q.col.names)
  # 	mutate(q_code_sum.score =
  #  rowSums(select(., q.col.names), na.rm = F)
  #  )
  
  num.na<-sum.na.row(q)
  q<-q[num.na<=ignore.participant.na.num,]
  
  return(custom_row_sum(rows = q, item_order = item_order, na.rm = na.rm))
  
}