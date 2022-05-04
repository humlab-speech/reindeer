

#' Extract part of a mediafiles based on a segment list
#'
#' This function enables the user to extract parts of speech recordings based on
#' a segment list (resulting from a call of [query]). The extracted parts will
#' be named by the segment label, the first sample, last sample extracted, as
#' well as the sample rate of the original media file. The user can also append
#' metadata fields to the file file name following the segment label. The
#' extracted signal file will be placed in sub-folders according session and /
#' or bundle if the user specifies it. If
#' `create.session.subdir=FALSE,create.bundle.subdir=FALSE`, all extracted
#' signal portions will be placed together in the output directory.
#'
#' Optionally, the user may obfuscate the bundle name by MD5 hashing. This
#' process makes the origin of the recording not possible to deduce from the
#' name.
#'
#'
#'
#' @param emuDBhandle The database handle.
#' @param seglist A segment list (resulting from a call of [query]) which should
#'   be used as a cut list.
#' @param output.directory The directory where all extracted parts of the
#'   signal, and sub-folders if required, will be placed.
#' @param include.labels Boolean; Include the label of a segment in the output
#'   file name?
#' @param include.metadata.fields Names of metadata fields whos values should be
#'   included in the file name. If a boolean (TRUE) all metadata fields will be
#'   encoded in the name of the output file.
#' @param create.session.subdir Boolean; Should bundles belonging to different
#'   sessions be kept separate?
#' @param create.bundle.subdir Boolean; Should signal files belonging to
#'   different bundles be kept separate?
#' @param encode.name boolean; Should the bundle name be obfuscated in the
#'   output using md5 hashing?
#' @param field.separator The field separator string to use when constructing
#'   the output file name.
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
extract_samples <- function(emuDBhandle,seglist, output.directory,include.labels=FALSE,include.metadata.fields=FALSE,create.session.subdir=TRUE,create.bundle.subdir=TRUE,encode.name=FALSE,field.separator="_"){
  dbConfig <- emuR:::load_DBconfig(emuDBhandle)
  mediafileExtension <- dbConfig$mediafileExtension
  seglist$absolute_file_path <- file.path(emuDBhandle$basePath,
                                          paste0(seglist$session,emuR:::session.suffix),
                                          paste0(seglist$bundle,emuR:::bundle.dir.suffix),
                                          paste(seglist$bundle,mediafileExtension,sep="."))


  if(  length(include.metadata.fields) > 1 || (is.logical(include.metadata.fields) & include.metadata.fields) ){
    md <- get_metadata(emuDBhandle = emuDBhandle)

    if(is.logical(include.metadata.fields) && include.metadata.fields){
      include.metadata.fields <- setdiff(names(md),c("session","bundle"))
    }else{
      if(length(include.metadata.fields) > 0 ){
        if(! all(include.metadata.fields %in% names(md))){
          stop("Please make sure that all fields named in 'include.metadata.fields' are metadata fields in the database.")
        }
      }
    }



    mdsel <- cbind(md[,c("session","bundle")], md[,include.metadata.fields])
    seglist <- seglist %>%
      dplyr::left_join(mdsel,by=c("session","bundle")) %>%
      tidyr::unite(labels, all_of(c("labels",include.metadata.fields)),sep=field.separator)

  }

  if(encode.name){
    seglist %>%
      mutate(bundle=openssl::md5(bundle)) -> seglist
  }

  for(r in 1:nrow(seglist)){
    out.dir <- ifelse(create.session.subdir,
                  file.path(output.directory, seglist[[r,"session"]]),
                  output.directory)

    out.dir <- ifelse(create.bundle.subdir,
                      file.path(out.dir, seglist[[r,"bundle"]]),
                      out.dir)

    dir.create(out.dir,recursive=TRUE,showWarnings = FALSE)
    currSSF <- wrassp::read.AsspDataObj(fname=seglist[[r,"absolute_file_path"]],
                                        begin=seglist[[r,"sample_start"]],
                                        end=seglist[[r,"sample_end"]],
                                        samples = TRUE)

    labelPart <- ifelse(include.labels,
                        paste0(field.separator,
                               stringr::str_trim(seglist[[r,"labels"]],side="both"),
                               field.separator),
                        field.separator)



    out.file <- file.path(out.dir,
                          paste0(seglist[[r,"bundle"]],
                                 labelPart,
                                 seglist[[r,"sample_start"]],field.separator,seglist[[r,"sample_end"]],field.separator,seglist[[r,"sample_rate"]],
                                 ".",mediafileExtension))
    wrassp::write.AsspDataObj(file = out.file, dobj = currSSF)
    rm(currSSF)
  }
}


### For interactive testing
 # library(reindeer)
 # reindeer:::unlink_emuRDemoDir()
 # reindeer:::create_ae_db() -> emuDBhandle
 # reindeer:::add_dummy_metadata(emuDBhandle)
 # query(emuDBhandle,"Phonetic = p") -> psl
 # output.directory <- file.path(tempdir(),"reindeeR_extract")
 # unlink(output.directory,recursive = TRUE)
#  extract_samples(emuDBhandle,psl,output.directory = output.directory,create.session.subdir=FALSE,create.bundle.subdir=TRUE,include.labels = TRUE,include.metadata.fields = c("Gender","Age"))
#  extract_samples(emuDBhandle,psl,output.directory = output.directory,create.session.subdir=FALSE,create.bundle.subdir=TRUE,include.labels = TRUE,include.metadata.fields =TRUE,field.separator="..")
#  extract_samples(emuDBhandle,psl,output.directory = output.directory,create.session.subdir=FALSE,create.bundle.subdir=TRUE,include.labels = TRUE,include.metadata.fields = c("Gender","Age"),encode.name = TRUE)
#
# print(list.files(path=output.directory,recursive=TRUE))
