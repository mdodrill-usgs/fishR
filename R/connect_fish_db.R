#' @title A basic function
#' @description This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' @export
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}

# need to update, make this function run !!!!!!!!

connect_fish_db = function(update.db = FALSE){
  # library(RSQLite)
  # library(dplyr)

  if(update.db == TRUE){
    update.db()
  }

  # path to sqlite db (there should only be one db in the folder)
  db.name = list.files(path = data.dir, pattern = ".sqlite3")
  my_path = paste(data.dir, db.name, sep = "/")

  # check to see if there's a db in the data folder, if not, then create one
  if(file.exists(my_path) == FALSE){
    message("MD: No sqlite database exists, so making a new one.")
    update.db()

    # need these to be updated
    db.name = list.files(path = data.dir, pattern = ".sqlite3")
    my_path = paste(data.dir, db.name, sep = "/")
  }

  # connect to db
  my_db = src_sqlite(path = my_path, create = FALSE)

 return(my_db)
}
