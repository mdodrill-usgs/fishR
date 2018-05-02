#' @title A basic function
#' @description This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' @export
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}


# need to update, make this function run !!!!!!!!

update_db = function(){
  # require(RSQLite)
  # require(dplyr)

  db.dir = "M:/DATABASE/BIOLOGICAL/FISH/DATABASE_BACKUPS"

  if(dir.exists(path = db.dir) == FALSE){
    return(message("MD: Can't find the database!, Are you connected to the M drive?"))
  }

  all.files = list.files(path = db.dir, pattern = ".mdb")

  # find the most recent db on the M drive
  the.one = sort(all.files)[length(all.files)]
  the.one.date = substr(the.one, 30, nchar(the.one) - 4)

  db.path = paste(db.dir, the.one, sep = "/")

  #---------------------------------
  # check to see if this version of the db already exists as sqlite db
  tmp = substr(list.files(path = data.dir, pattern = ".sqlite3"), 7, 19)

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
  file.name = paste(data.dir, "/samp_table_", the.one.date, ".txt", sep = "")

  exporter.path = paste(str.dir, "/Exporter/AccessExporter_v1.exe", sep = "")

  system(paste(exporter.path,
               db.path,
               "FISH_T_SAMPLE",
               file.name))
  #---------------------------------
  # specimen table
  file.name.2 = paste(data.dir, "/spec_table_", the.one.date, ".txt", sep = "")

  system(paste(exporter.path,
               db.path,
               "FISH_T_SPECIMEN",
               file.name.2))

  #-----------------------------------------------------------------------------#
  # check if sqlite db is in the data folder and delete before making a new one

  if(length(list.files(path = data.dir, pattern = ".sqlite3")) > 0){
    old.db = paste(data.dir, list.files(path = data.dir, pattern = ".sqlite3"), sep = "/")
    file.remove(old.db)
  }

  #---------------------------------
  # create the new db

  samp = read.table(file = file.name, header = T, sep = ",")

  spec = read.table(file = file.name.2, header = T, sep = ",")

  db.name = paste(data.dir, "/my_db_", the.one.date, ".sqlite3", sep = "")

  # Could add some data formatting stuff here, before the tables are written to the db...

  # create a blank database
  my_db <- src_sqlite(db.name, create = T)

  # addes these tables to the "my_db"
  copy_to(my_db, samp, temporary = FALSE)
  copy_to(my_db, spec, temporary = FALSE)

  # remove the .txt files
  file.remove(file.name)
  file.remove(file.name.2)

  message("MD: Update Successful")
  return()
}
