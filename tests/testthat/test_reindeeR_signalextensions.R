#context("Code extending the signal handling abilities of emuR")

library(reindeer)
library(superassp)

reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db() -> emuDBhandle
reindeer:::make_dummy_metafiles(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])

#add_trackDefinition(emuDBhandle,"fm","fm","fm",onTheFlyFunctionName = "praat_formant_burg",onTheFlyOptLogFilePath = "/Users/frkkan96/Desktop/test")


test_that("Check that we can get default signal processing parameters",{
  data("DSPP")
  message(nrow(DSPP))
  reindeer::get_parameters() -> dsp

  testthat::expect_equal(nrow(DSPP),nrow(dsp))
  testthat::expect_gte(nrow(DSPP),60)

})

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
        }
      }
   })

}


test_that("Check that add_trackDefintion() can apply Praat functions",{

  reindeer::add_trackDefinition(emuDBhandle,name="pfm",columnName = "fm",fileExtension = "pfms",onTheFlyFunctionName = "praat_formant_burg")

  testthat::expect_true("pfm" %in% list_ssffTrackDefinitions(emuDBhandle)$name )

  reindeer::add_trackDefinition(emuDBhandle,name="Hc",columnName = "Hc",fileExtension = "psa",onTheFlyFunctionName = "praat_sauce")

  for(tr in setdiff(superassp::get_definedtracks("praat_sauce"),"Hc")){
    reindeer::add_trackDefinition(emuDBhandle,name=tr,columnName = tr,fileExtension = "psa")
  }
  #Check that we now have a track for each defined output of praat_sauce
  testthat::expect_true(all(superassp::get_definedtracks("praat_sauce") %in% list_ssffTrackDefinitions(emuDBhandle)$name ))


})
