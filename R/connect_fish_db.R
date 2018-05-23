#' @title Conection to the fish database
#' @description This function connects to the fish database stored in SQLite so
#'   the user can access tables using database functions, mostly from dplyr.
#'   Setting update.db to TRUE will check the M: drive to see if a newer version
#'   is avaliable. The first time the function is run, a local copy of the fish
#'   database is created.
#' @param update.db will run the function 'update_db', which checks for a
#'   updated version of the database.
#' @examples Need to add...
#' @author Michael J. Dodrill, \email{mdodrill@usgs.gov}
#' @export


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
  # my_db = dplyr::src_sqlite(path = sq.path, create = FALSE)
  my_db = DBI::dbConnect(RSQLite::SQLite(), dbname = sq.path)

 return(my_db)
}
