#' @title A basic function
#' @description This is a test function I wrote.
#' @param test Tests if the function is working. Defaults to TRUE.
#' @examples test()
#' @export
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}


connect_fish_db = function(update.db = FALSE){
  # Note: should redo this code, see: https://stackoverflow.com/questions/43620927/dplyr-auto-disconnecting-postgres-connection-issue

  # library(RSQLite)
  # library(dplyr)

  if(update.db == TRUE){
    update_db()
  }

  db.dir.loc <- paste0(find.package('fishR'), '/DataBase')
  db.dir.sq.m = "M:/DATABASE/BIOLOGICAL/FISH/DATABASE_R_SQLite"

  # if the dir doesn't exist, make one and copy the db into it
  if(dir.exists(path = db.dir.loc) == FALSE){
    message("MD: No sqlite database exists locally, so copying from M:")
    dir.create(db.dir.loc, showWarnings = FALSE)

    sq.name = list.files(path = db.dir.sq.m, pattern = ".sqlite3")
    sq.path.from = paste0(db.dir.sq.m, '/', sq.name)
    sq.path.to = paste0(db.dir.loc, '/', sq.name)

    file.copy(sq.path.from, sq.path.to)
  }

  sq.name = list.files(path = db.dir.loc, pattern = ".sqlite3")
  sq.path = paste0(db.dir.loc, '/', sq.name)

  # connect to db
  my_db = dplyr::src_sqlite(path = sq.path, create = FALSE)

 return(my_db)
}
