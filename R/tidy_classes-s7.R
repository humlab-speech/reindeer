
corpus <- S7::new_class(
  "corpus",
  parent = S7::new_S3_class("emuDBhandle"),
  properties = list(
    dbName = S7::class_character,
    basePath = S7::class_character,
    config = S7::class_any
  ),
  constructor = function(path, verbose = FALSE) {
    if (is.character(path)) {
      if (!dir.exists(path)) {
        cli::cli_abort("Database path {.path {path}} does not exist")
      }
      if (!stringr::str_ends(basename(path), "_emuDB")) {
        cli::cli_abort("Database directory should end with '_emuDB'")
      }
      handle <- emuR::load_emuDB(path, verbose = verbose)
    } else if ("emuDBhandle" %in% class(path)) {
      handle <- path
    } else {
      cli::cli_abort("Invalid input: expected path or emuDBhandle")
    }
    
    S7::new_object(
      handle,
      dbName = handle$dbName,
      basePath = handle$basePath,
      config = emuR:::load_DBconfig(handle)
    )
  },
  validator = function(self) {
    if (!dir.exists(self@basePath)) {
      "Database path must exist"
    } else if (is.null(self@dbName) || nchar(self@dbName) == 0) {
      "Database name must be specified"
    }
  }
)

segments <- S7::new_class("segments",
                      S7::class_data.frame,
                      properties = list(
                        basePath = S7::class_character
                      ),
                      validator=function(self){
                        mandatoryNames <- c("labels", "start", "end", "db_uuid", "session", "bundle", "start_item_id",
                                            "end_item_id", "level", "attribute", "start_item_seq_idx", "end_item_seq_idx",
                                            "type", "sample_start", "sample_end", "sample_rate")
                        if(is.null(self@basePath) ){
                          "@basePath is not set"
                        }else if(!dir.exists(self@basePath)){
                          "@basePath must point to an existing database directory"
                        }else if(! all(mandatoryNames %in% names(self) )){
                          "Not all mandatory columns are present in the data.frame and is not a valid segments class"
                        }
                      },
                      constructor=function(x,basePath,fileExtension=NULL){
                        if(is.null(fileExtension)){
                          emuDBhandle <- emuR::load_emuDB(basePath)
                        }
                        emuR:::load_DBconfig(emuDBhandle)$mediafileExtension
                        x[,"listOfFiles"] <- file.path(.inside_of$basePath,
                                                       paste0(x[["session"]],emuR:::session.suffix),
                                                       paste0(x[["bundle"]],emuR:::bundle.dir.suffix),
                                                       paste(x[["bundle"]],fileExtension,sep="."))
                        S7::new_object(x,basePath=basePath)

                      })


bundles <- S7::new_class("bundles",
                     S7::class_data.frame,
                     properties = list(
                       basePath = S7::class_character
                     ),
                     validator=function(self){
                       mandatoryNames <- c("session","name")
                       if(is.null(self@basePath) ){
                         "@basePath is not set"
                       }else if(!dir.exists(self@basePath)){
                         "@basePath must point to an existing database directory"
                       }else if(! all(mandatoryNames %in% names(self) )){
                         "Not all mandatory columns are present in the data.frame and is not a valid segments class"
                       }
                     },
                     constructor=function(x,basePath){
                       x[,"listOfFiles"] <- file.path(basePath,
                                                      x[["session"]],
                                                      x[["bundle"]])
                       S7::new_object(x,basePath=basePath)

                     })

session <-S7:: new_class("session",
                     S7::class_character,
                     properties = list(
                       bundle_files = S7::class_character
                     ),
                     constructor = function(...){
                       fl <- as.list(...)
                       for(f in fl){
                         fileInf <- file.info(f)

                         if(file.info(f)[["isdir"]]){

                         }
                       }
                       S7::new_object(fl,bundle_files)
                     }
)

import <- S7::new_generic("inside",c("x","y"))



sdb <- S7::new_class("sdb",
                 parent=S7::new_S3_class("emuDBhandle",
                                     constructor= function(.data=character()){
                                       reindeer::load_emuDB(.data,verbose = FALSE)
                                     },
                                     validator=function(self){
                                       if (!is.character(self@basePath)) {
                                         "Underlying data must be a character"
                                       }

                                     }),
                 properties=list(
                   dbName=S7::class_character,
                   basePath=S7::class_character
                 )
                )

