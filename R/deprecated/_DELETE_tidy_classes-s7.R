# ============================================================================
# DEPRECATED FILE - MARKED FOR DELETION
# ============================================================================
# 
# This file is no longer used by the reindeer package and will be removed.
# See R/deprecated/README.md for replacement files.
#
# DO NOT USE FUNCTIONS FROM THIS FILE
# 
# Date marked: 2025-10-16
# ============================================================================

# NOTE: corpus class is now defined in reindeer-corpus.R
# This conflicting definition has been removed to avoid duplicates
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

