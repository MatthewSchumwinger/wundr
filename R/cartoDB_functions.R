# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
fillblanks <- function(x){
  gsub(" ", '%20', x)
}

#' get_cdb_table
#'
#' ... yada yada
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
#' ... yada yada
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
#' \dontrun{r2cdb(your.cdb.key, your.cdb.account, PWS.Conditions)}

## export df to CartoDB
r2cdb <- function(user_key, cdb_account, PWS.Conditions){

  ## -- helpers ------
  # get SQL column types
  schema <- function(df){
    drv <- SQLite()
    sapply(df, function(x) dbDataType(drv, x))
  }

  # remove "display" columns with offending "." that throws API syntax error
  # TODO maybe swap for this? htmltools::htmlEscape
  sanitize <- function(df){
    colnames(df)[1] <- "full_location"
    names(df) <- gsub("\\.", "_", names(df))
    df
  }

  # enquote data strings for url export
  singleQuoter <- function(df){
    if (!is.numeric(df)) {
      df[] <- gsub("^|$", "\047", df[])
      df[] <- gsub("$", "\047", df[])
      df[] <- gsub("%", "percent", df[]) # replace "%" with "percent
    }
    df
  }
  ## -- end helpers --

  # pick-off data from s4 object
  df <- PWS.Conditions@spatialPtDF@data

  tableName <- eval(substitute(quote(PWS.Conditions)))
  df.san <- sanitize(df)
  df.schema <- schema(df.san)
  cdb_url_base <- ".cartodb.com/api/v2/sql?q="
  cdb_url_view <- ".cartodb.com/tables/"

  # instantiate empty table with desired structure
  makeCdbTable <- function(user_key, cdb_account, df){
    colTypes <- sapply(seq_along(df.schema), function(i) paste(names(df.schema)[[i]], df.schema[[i]]))
    columns <- paste(colTypes, collapse = ",")
    sql_create <- paste0("create table ", tableName," (", columns, ")")
    sql_register <- eval(substitute(paste0("select cdb_cartodbfytable('", tableName, "')")))
    cat(" Instantiating and registering new table with CartoDB ... ")
    jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                 fillblanks(sql_create),"&api_key=",user_key))
    Sys.sleep(5) # this may need to be adjusted
    jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                        fillblanks(sql_register),"&api_key=",user_key))
  }

  # populate empty table
  insertCdbTable <- function(user_key, cdb_account, df){
    # TODO insert real df data FIX url string
    # TODO tolower() in appropriate places
    c <- sapply(seq_along(df.schema), function(i) paste(names(df.schema)[[i]]))
    # columns <- paste(c[1:2], collapse = ",") # just first two columns to test
    columns <- paste(c, collapse = ",")
    cat("Exporting ", nrow(df), " rows of data to table ", "'", tableName, "' ", sep = "")
    df[1:nrow(df),] <- lapply(df, singleQuoter)
    for (i in 1:nrow(df)){
      # values <- paste("'foo'", "'bar'", sep = ",") # strings need to be quoted, numeric not so
      values <- paste(df[i, ], collapse = ",")
      coord <- paste(", ST_SetSRID(ST_Point(" , df$longitude[i], ", ", df$latitude[i], "),4326))")
      sql_insert <- paste0("INSERT INTO ", tableName, " (", columns, ",the_geom)",
                           " VALUES (", values, coord)
      jsonlite::fromJSON(paste0("https://", cdb_account, cdb_url_base,
                                fillblanks(sql_insert),"&api_key=",user_key))
      cat(".")
    }
    cat(" Export complete.")
    print(paste0("Link to your datasets (log in and click this first) --> ",
                 "https://", cdb_account, ".cartodb.com/dashboard/datasets "))
    print(paste0("Direct link to your new map --> ",
                 "https://", cdb_account, cdb_url_view, tolower(tableName), "/map "))
  }
  makeCdbTable(user_key, cdb_account, df)
  insertCdbTable(user_key, cdb_account, df)
}

## ---------- TODO -----------------------
# consider using dplyr::build_sql and/or httr::build_url
# or httr::modify_url  to improve functions
