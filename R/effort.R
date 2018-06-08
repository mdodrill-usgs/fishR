#' @title Correct effort for hoop data
#' @description This function can be applied to hoop net data to produce the correct effort.
#' @param x is the data.frame, ex. sample table with the correct columns names
#' @details Function needs columns: start_datetime, end_datetime, effort, total_hours and trip_id
#' @examples Need to add...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export


# Couple functions to clean up the effort data.
# the functions are set up to be "applied"
# cleans up any funky values in the effort
#   * exclude negative effort
#   * if my.eff == 0 return(NA)
#   * if the effort is in seconds convert to hours..
#   * See Bill's code !!! -- lots more of these... pg 23
# a lot of the efforts between 200 and 2050 are from AZGFD3 -- Not sure what to do with these?

effort = function(x){

# Calculates the effort in hours from the start and end times
time = function(x){
  eff.from.datetime = NA

  if(is.na(x[which(names(x) == "start_datetime")])){
    return(eff.from.datetime)
  }

  if(is.na(x[which(names(x) == "end_datetime")])){
    return(eff.from.datetime)
  }

  start = x[which(names(x) == "start_datetime")]
  end   = x[which(names(x) == "end_datetime")]

  eff.from.datetime = difftime(end, start, units = "hours")

  return(eff.from.datetime)
}

clean.eff = function(x){
  if(is.na(x)){
    return(x)
  }

  x2 = as.numeric(x)

  if(x2 <= 0){
    x2 = NA
    return(x2)
  }

  if(x2 <= .02){
    x2 = NA
    return(x2)
  }

  # these are likely a trammel net used as a seine. Efforts not valid
  if(x2 == 1000){
    x2 = NA
    return(x2)
  }

  if(x2 >= 200 & x2 <= 2050){
    x2 = x2 / 60     # devide these by 60 min.
    return(x2)
  }

  if(x2 > 2050){
    x2 = x2 / 60 / 60
  }
  x2
}


# run the "time" and "clean" functions --> calculates the effort

  my.eff = NA
  my.eff = time(x)

  if(is.na(my.eff)){

    if(!is.na(x[which(names(x) == "effort")])) {
      my.eff = x[which(names(x) == "effort")]
    }
  }

  col.idx = which(names(x) == "trip_id")

  # if trip == 20060713 use the total_hours
  if(x[col.idx] == "GC20060713"){
    my.eff = x[which(names(x) == "total_hours")]
  }

  my.eff = clean.eff(my.eff)
  return(my.eff)
}

# tester = apply(data5, 1, effort)
