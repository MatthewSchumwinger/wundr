##
## Begin Matthew Schumwinger's (mjs13) code
##


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This file contains Matthew Schumwinger's (mjs13) code.
#
# This document contains functions that interface with the CartoDB SQL API
# (http://docs.cartodb.com/cartodb-platform/sql-api/). This povides facilities
# to export Weather Underground PWS data to CartoDB's spatial database hosting
# and webmap publishing platform. A simple facilty to import SQL spatial table
# data from CartoDB into R is also provided.
# The primary functions include:
#
#  o getCDBtable
#  o r2cdb
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' fillblanks
#'
#' A helper function used to construct sanitized URLs in CartoDB fucntions.
#'
#' @param x A string.
#' @return A data frame.
#' @export
#' @examples
#' fillblanks("foo bar")
fillblanks <- function(x) {
  gsub(" ", '%20', x)
}

#' get_cdb_table
#'
#' Provides a simple facilty to the CartoDB SQL API
#' (\url{http://docs.cartodb.com/cartodb-platform/sql-api/}) to import SQL
#' spatial tabledata from CartoDB into an R data frame. NOTE: you will need to
#' use your account name and Authorization Key.
#'
#' @importFrom jsonlite fromJSON
#' @param table_name A PostGres-based table from CartoDB.
#' @param cdb_account A CartoDB account name.
#' @return A data frame.
#' @export
#' @examples
#' # test CartoDB connection by retrieving existing table ...
#' \dontrun{stations <- get_cdb_table("public.stations", your.cdb.account)}
get_cdb_table <- function(table_name, cdb_account) {
  sql_statement <- paste("select * from", table_name)
  cdb_url_base <- ".cartodb.com/api/v2/sql?q="
  jsonlite::fromJSON (paste0("https://", cdb_account, cdb_url_base,
                             fillblanks(sql_statement)))
}


#' r2cdb
#'
#' Povides facilities to export Weather Underground PWS data to CartoDB's
#' spatial database hosting and webmap publishing platform through its API
#' (\url{http://docs.cartodb.com/cartodb-platform/sql-api/}). NOTE: you will need
#' have an account and Authorization Key with CartoDB. You can create a free
#' account and get your key at \url{https://cartodb.com/signup}.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom RSQLite SQLite dbDataType
#' @param user_key A CartoDB user authorization key.
#' @param cdb_account A CartoDB account name.
#' @param PWS.Conditions A S4.class PWS.Conditions object of PWS locations and conditions attribute
#'   data.
#' @return NULL. Plus, printed information on status of CartoDB API and URL of
#'   exported map data on CartoDB service.
#' @export
#' @examples
#' # export PWS locations and conditions data to CartoDB
#' \dontrun{r2cdb(your.cdb.key, your.cdb.account, PWS.Conds.Chicago)}
r2cdb <- function(user_key, cdb_account, PWS.Conditions){
  # error test inputs
  if(class(PWS.Conditions) != "PWS.Conditions")
    stop("PWS.Conditions must be of class PWS.Conditions.")
  if(typeof(user_key)!="character")
    stop("user_key must be of type character.")
  if(typeof(cdb_account)!="character")
    stop("cdb_account must be of type character.")

  # helper: get SQL column types
  schema <- function(df){
    drv <- SQLite()
    sapply(df, function(x) dbDataType(drv, x))
  }

  # helper: columns with offending ".", throws API syntax error
  sanitize <- function(df){
    colnames(df)[1] <- "full_location"
    names(df) <- gsub("\\.", "_", names(df))
    df
  }

  # helper: enquote data strings for url export
  singleQuoter <- function(df){
    if (!is.numeric(df)) {
      df[] <- gsub("^|$", "\047", df[])
      df[] <- gsub("$", "\047", df[])
      df[] <- gsub("%", "percent", df[]) # replace "%" with "percent
    }
    df
  }

  # pick-off data from s4 object
  df <- PWS.Conditions@spatialPtDF@data

  tableName <- eval(substitute(quote(PWS.Conditions)))
  df.san <- sanitize(df)
  df.schema <- schema(df.san)
  cdb_url_base <- ".cartodb.com/api/v2/sql?q="
  cdb_url_view <- ".cartodb.com/tables/"

  # instantiate empty table with desired structure
  makeCdbTable <- function(user_key, cdb_account, df){
    colTypes <- sapply(seq_along(df.schema),
                       function(i) paste(names(df.schema)[[i]], df.schema[[i]]))
    columns <- paste(colTypes, collapse = ",")
    sql_create <- paste0("create table ", tableName," (", columns, ")")
    sql_register <- eval(substitute(paste0("select cdb_cartodbfytable('",
                                           tableName, "')")))
    cat(" Instantiating and registering new table with CartoDB ... ")
    jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                 fillblanks(sql_create),"&api_key=",user_key))
    Sys.sleep(5) # this may need to be adjusted
    jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                        fillblanks(sql_register),"&api_key=",user_key))
  }

  # populate empty table
  insertCdbTable <- function(user_key, cdb_account, df){
    c <- sapply(seq_along(df.schema), function(i) paste(names(df.schema)[[i]]))
    columns <- paste(c, collapse = ",")
    cat("Exporting ", nrow(df), " rows of data to table ", "'", tableName,
        "' ", sep = "")
    df[1:nrow(df),] <- lapply(df, singleQuoter)
    for (i in 1:nrow(df)){
      values <- paste(df[i, ], collapse = ",")
      coord <- paste(", ST_SetSRID(ST_Point(" , df$lon[i], ", ",
                     df$lat[i], "),4326))")
      sql_insert <- paste0("INSERT INTO ", tableName,
                           " (", columns, ",the_geom)",
                           " VALUES (", values, coord)
      jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                                fillblanks(sql_insert),"&api_key=",user_key))
      cat(".")
    }
    cat("Export complete.")
    cat("Link to your datasets (log in and click this first):",
                 paste0("https://", cdb_account,
                        ".cartodb.com/dashboard/datasets "),
                "Then, refresh your browser a few times (like 3 times)
                until you see the table",
                sep="\n")
    cat("Direct link to your new map:",
        paste0("https://", cdb_account, cdb_url_view, tolower(tableName),
               "/map "))
  }
  makeCdbTable(user_key, cdb_account, df)
  insertCdbTable(user_key, cdb_account, df)
}

#' matt_cdb_table dataset
#'
#' This contains Personal Weather Stations meta data exported to CartoDB with
#' r2cdb() and imported back to R as a dataframe.
#'
#' @examples
#' data(matt_cdb_table)
#' head(matt_cdb_table)
#'
#' @author wundr team
"matt_cdb_table"

##
## End Matthew Schumwinger's (mjs13) code
##
