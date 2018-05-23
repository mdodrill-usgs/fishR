#' @title Creates a SQLite database on the M drive
#'
#' @description Finds the Access database on the M drive and creates a new
#'   SQLite database, if the SQLite needs updating. The Function will not work
#'   unless you are connected to the M drive. Most of the time the user
#'   shouldn't have to use this as a stand alone function, but use the argument
#'   in \code{\link{connect_fish_db}}
#'
#' @examples Need to add...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export


update_db = function(){
  # require(RSQLite)
  # require(dplyr)

  db.dir.m = "M:/DATABASE/BIOLOGICAL/FISH/DATABASE_BACKUPS"

  if(dir.exists(path = db.dir.m) == FALSE){
    return(message("MD: Can't find the database!, Are you connected to the M drive?"))
  }

  all.files = list.files(path = db.dir.m, pattern = ".mdb")

  # find the most recent db on the M drive
  the.one = sort(all.files)[length(all.files)]
  the.one.date = substr(the.one, 30, nchar(the.one) - 4)

  db.path = paste(db.dir.m, the.one, sep = "/")

  #---------------------------------
  # check to see if this version of the db already exists as sqlite db
  db.dir.sq.m = "M:/DATABASE/BIOLOGICAL/FISH/DATABASE_R_SQLite"

  tmp = substr(list.files(path = db.dir.sq.m, pattern = ".sqlite3"), 7, 19)

  if(length(tmp) > 0){  # if it exists
    if(the.one.date == tmp){ # if this version matches the one on the M
      return(message("MD: Database is already up to date."))
    }
  }

  #-----------------------------------------------------------------------------#
  # the "AccessExporter" takes 3 arguments. 1) Path to the access database, 2) the
  # name of the table that you want, 3) path & name of the file to write (table to
  # export), this can be either a .csv or .txt

  # the first argument to system is the location & name of the "AccessExporter",
  # here "AccessExporter_v1.exe"

  # sample table
  file.name = paste(db.dir.sq.m, "/samp_table_", the.one.date, ".txt", sep = "")

  exporter.path = paste(db.dir.sq.m, "/Exporter/AccessExporter_v1.exe", sep = "")

  system(paste(exporter.path,
               db.path,
               "FISH_T_SAMPLE",
               file.name))
  #---------------------------------
  # specimen table
  file.name.2 = paste(db.dir.sq.m, "/spec_table_", the.one.date, ".txt", sep = "")

  system(paste(exporter.path,
               db.path,
               "FISH_T_SPECIMEN",
               file.name.2))

  #-----------------------------------------------------------------------------#
  # check if sqlite db is in the data folder and delete before making a new one

  if(length(list.files(path = db.dir.sq.m, pattern = ".sqlite3")) > 0){
    old.db = paste(db.dir.sq.m, list.files(path = db.dir.sq.m, pattern = ".sqlite3"), sep = "/")
    file.remove(old.db)
  }

  #---------------------------------
  # create the new db

  samp = read.table(file = file.name, header = T, sep = ",", stringsAsFactors = F)
  colnames(samp) = tolower(colnames(samp))

  spec = read.table(file = file.name.2, header = T, sep = ",", stringsAsFactors = F)
  colnames(spec) = tolower(colnames(spec))

  db.name = paste(db.dir.sq.m, "/my_db_", the.one.date, ".sqlite3", sep = "")

  # Could add some data formatting stuff here, before the tables are written to the db...
  samp$start_datetime = as.character(as.POSIXct(strptime(samp$start_datetime, "%m/%d/%Y %H:%M:%S")))
  samp$end_datetime = as.character(as.POSIXct(strptime(samp$end_datetime, "%m/%d/%Y %H:%M:%S")))

  # create a blank database
  my_db <- dplyr::src_sqlite(db.name, create = T)   # need to find a better way to do this as it's depricated

  # addes these tables to the "my_db"
  dplyr::copy_to(my_db, samp, temporary = FALSE)
  dplyr::copy_to(my_db, spec, temporary = FALSE)

  # remove the .txt files
  file.remove(file.name)
  file.remove(file.name.2)

  message("MD: Update Successful")
  return()
}
