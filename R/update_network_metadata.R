
#' Update SEBA metadata.
#'
#' Update metadata for SEBA network and upload to ADT database.
#' 
#' @export

sebaMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    metaFiles <- c("seba_aws_list.csv",
                   "seba_formatted_crds.csv",
                   "seba_parameters_table.csv")

    # ssh <- readRDS(file.path(dirAWS, "AWS_DATA", "AUTH", "adt.cred"))
    # session <- try(do.call(ssh::ssh_connect, ssh$cred), silent = TRUE)
    # if(inherits(session, "try-error")) stop("Unable to connect to ADT server\n")

    # for(ff in metaFiles){
    #     locFile <- file.path(dirAWS, "AWS_DATA", "CSV", ff)
    #     adtFile <- file.path(dirUP, "AWS_DATA", "CSV", ff)
    #     ssh::scp_upload(session, locFile, to = adtFile, verbose = FALSE)
    # }

    # ssh::ssh_exec_wait(session, command = c(
    #     # paste0('if [ ! -d ', dirADT, ' ] ; then mkdir -p ', dirADT, ' ; fi')
        
    #     ### run a script on ADT

    #     ####  (R script)
    #     ## read table seba_crds and seba_pars
    #     ## read file csv
    #     ## compare difference
    #     ## IF diff TRUE
    #     ## write to database seba_crds and seba_pars (overwrite = TRUE)
    #     ##
    #     ## awsCrds_csv <- read.table("seba_formatted_crds.csv")
    #     ## awsCrds_db <- DBI::dbReadTable(conn, "seba_crds")
    #      ## startdate_db <- awsCrds_db$startdate
    #      ## enddate_db <- awsCrds_db$enddate
    #     ## if diff awsCrds_csv[-(startdate, enddate)] and awsCrds_db[-(startdate, enddate)] is TRUE
    #      ## add startdate_db and enddate_db to awsCrds_csv
    #     ## DBI::dbWriteTable(conn, "seba_crds", awsCrds_csv, overwrite = TRUE, row.names = FALSE)
    #     ## awsPars_csv <- read.table("seba_parameters_table.csv")
    #     ## awsPars_db <- DBI::dbReadTable(conn, "seba_pars")
    #     ## if diff awsPars_csv and awsPars_db is TRUE
    #     ## DBI::dbWriteTable(conn, "seba_pars", awsPars_csv, overwrite = TRUE, row.names = FALSE)
    # ))

    cat("Not working yet!\n")
}

#' Update CAMPBELL metadata.
#'
#' Update metadata for CAMPBELL network and upload to ADT database.
#' 
#' @export

campbellMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    metaFiles <- c("campbell_aws_list.csv",
                   "campbell_formatted_crds.csv",
                   "campbell_parameters_table.csv")

    cat("Not working yet!\n")
}

#' Update SUTRON metadata.
#'
#' Update metadata for SUTRON network and upload to ADT database.
#' 
#' @export

sutronMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    metaFiles <- c("sutron_aws_list.csv",
                   "sutron_formatted_crds.csv",
                   "sutron_parameters_table.csv")

    cat("Not working yet!\n")
}

#' Update MICROSTEP metadata.
#'
#' Update metadata for MICROSTEP network and upload to ADT database.
#' 
#' @export

microstepMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    metaFiles <- c("microstep_aws_list.csv", 
                   "microstep_formatted_crds.csv",
                   "microstep_parameters_table.csv")

    cat("Not working yet!\n")
}

#' Update TAHMO metadata.
#'
#' Update metadata for TAHMO network and upload to ADT database.
#' 
#' @export

tahmoMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    # tahmo_aws_list.csv
    # tahmo_formatted_crds.csv
    # tahmo_parameters_table.csv

    cat("Not working yet!\n")
}

#' Update ADCON metadata.
#'
#' Update metadata for ADCON network and upload to ADT database.
#' 
#' @export

adconMetaUpdate <- function(){
    dirAWS <- "C:/kmd_adt"
    dirUP <- "/home/administrator"
    # adcon_formatted_crds.csv

    cat("Not working yet!\n")
}

