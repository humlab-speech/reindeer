#context("Code extending the signal handling abilities of emuR")

library(reindeer)
library(superassp)

#Set up the base test database
reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db() -> emuDBhandle
reindeer:::make_dummy_metafiles(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
#Clean up possibly conflicting tracks
reindeer::remove_ssffTrackDefinition(emuDBhandle,name="fm",deleteFiles = TRUE)
reindeer::remove_ssffTrackDefinition(emuDBhandle,name="dft",deleteFiles = TRUE)


test_that("Check that we can get default signal processing parameters",{
  data("DSPP")
  message(nrow(DSPP))
  reindeer::get_parameters() -> dsp

  testthat::expect_equal(nrow(DSPP),nrow(dsp))
  testthat::expect_gte(nrow(DSPP),60)

})

reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db() -> emuDBhandle
reindeer:::make_dummy_metafiles(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])


nonSSFFFunctions <-c("rfcana","afdiff","affilter")

for(fun in setdiff(names(wrassp::wrasspOutputInfos),nonSSFFFunctions)){
  test_that(paste0("Signals can be created using the wrassp::",fun," function"),{
      ext <- wrassp::wrasspOutputInfos[[fun]][["ext"]]
      tracks <- setdiff(wrassp::wrasspOutputInfos[[fun]][["tracks"]],"arf|lar|lpc|rfc")
      outputType <- wrassp::wrasspOutputInfos[[fun]][["outputType"]]
      if(outputType == "SSFF"){
        for(tr in tracks){

          if(tr == ""){
            tr <- paste0(ext,tr)
          }
          add_trackDefinition(emuDBhandle,name=tr,columnName = tr,fileExtension = ext,onTheFlyFunctionName = fun)
          fe <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$fileExtension

          expect_true(ext %in% fe)

          flfu <- reindeer::list_files(emuDBhandle,ext)
          expect_equal(nrow(flfu),nrow(fl))
          #Cleaup
          emuR::remove_ssffTrackDefinition(emuDBhandle,name=tr, deleteFiles=TRUE)
        }
      }
   })

}

praatfuns <- c("praat_formant_burg","praat_sauce","praat_intensity","praat_moments")

for(currFun in praatfuns){
  test_that(paste0("Check that add_trackDefintion() can create signals using the ",currFun," Praat function, and attach all tracks"),{
    currExt <- superassp::get_extension(currFun)
    tr <- superassp::get_definedtracks(currFun)
    firstTrack <- head(tr,1)
    attachTracks <- tail(tr,-1) #Everything but the first track


    unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])

    #Make the file
    reindeer::add_trackDefinition(emuDBhandle,name=firstTrack,columnName = firstTrack,fileExtension = currExt,onTheFlyFunctionName = currFun)
    fe <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$fileExtension

    expect_true(currExt %in% fe)
    #Attach track definitions to the file
    for(currTr in attachTracks){
      reindeer::add_trackDefinition(emuDBhandle,name=currTr,columnName = currTr,fileExtension = currExt)
      cn <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$columnName
      expect_true(currTr %in% cn)
      reindeer::remove_ssffTrackDefinition(emuDBhandle,name=currTr)
    }
    #Cleanup the file too
    reindeer::remove_ssffTrackDefinition(emuDBhandle,name=firstTrack,deleteFiles = TRUE)
  })
}
