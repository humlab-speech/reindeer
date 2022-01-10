

#' Extract part of a mediafiles based on a segment list
#'
#' This function enables the user to extract parts of speech recordings based on a segment list (resulting from a call of [query]).
#' The extracted parts will be named by the sample it was cut from, the first sample, last sample extracted, as well as the sample rate of the original media file. The extracted signal file will be placed in sub-folders according session and / or bundle if the user specifies it. If `create.session.subdir=FALSE,create.bundle.subdir=FALSE`, all extracted signal portions will be placed together in the output directory.
#'
#'
#'
#' @param emuDBhandle The database handle.
#' @param seglist A segment list (resulting from a call of [query]) which should be used as a cut list.
#' @param output.directory The directory where all extracted parts of the signal, and sub-folders if required, will be placed.
#' @param create.session.subdir Boolean; Should bundles belonging to different sessions be kept separate?
#' @param create.bundle.subdir Boolean; Should signal files belonging to different bundles be kept separate?
#'
#' @export
#'
#' @examples
#' library(reindeer)
#' reindeer:::unlink_emuRDemoDir()
#' reindeer:::create_ae_db() -> emuDBhandle
#' query(emuDBhandle,"Phonetic = p") -> psl
#' output.directory <- file.path(tempdir(),"reindeeR_extract")
#' unlink(output.directory,recursive=TRUE)
#' extract_samples(emuDBhandle,psl,output.directory = output.directory,create.session.subdir=TRUE,create.bundle.subdir=TRUE)
#' print(list.files(path=output.directory))
#'
extract_samples <- function(emuDBhandle,seglist, output.directory,create.session.subdir=TRUE,create.bundle.subdir=TRUE){
  dbConfig <- emuR:::load_DBconfig(emuDBhandle)
  mediafileExtension <- dbConfig$mediafileExtension
  seglist$absolute_file_path <- file.path(emuDBhandle$basePath,
                                          paste0(seglist$session,emuR:::session.suffix),
                                          paste0(seglist$bundle,emuR:::bundle.dir.suffix),
                                          paste(seglist$bundle,mediafileExtension,sep="."))
  for(r in 1:nrow(seglist)){
    out.dir <- ifelse(create.session.subdir,
                  file.path(output.directory, seglist[[r,"session"]]),
                  output.directory)

    out.dir <- ifelse(create.bundle.subdir,
                      file.path(out.dir, seglist[[r,"bundle"]]),
                      out.dir)

    dir.create(out.dir,recursive=TRUE)
    currSSF <- wrassp::read.AsspDataObj(fname=seglist[[r,"absolute_file_path"]],
                                        begin=seglist[[r,"sample_start"]],
                                        end=seglist[[r,"sample_end"]],
                                        samples = TRUE)
    out.file <- file.path(out.dir,
                          paste0(seglist[[r,"bundle"]],"_",seglist[[r,"sample_start"]],"_",seglist[[r,"sample_end"]],"_",seglist[[r,"sample_rate"]],
                                 ".",mediafileExtension))
    wrassp::write.AsspDataObj(file = out.file, dobj = currSSF)
    rm(currSSF)
  }
}


### For interactive testing
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db() -> emuDBhandle
# query(emuDBhandle,"Phonetic = p") -> psl
# output.directory <- file.path(tempdir(),"reindeeR_extract")
# extract_samples(emuDBhandle,psl,output.directory = output.directory,create.session.subdir=TRUE,create.bundle.subdir=TRUE)
#
# print(list.files(path=output.directory,recursive=TRUE))
