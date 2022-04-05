#context("Code extending the signal handling abilities of emuR")
library(testthat)
library(reindeer)
library(superassp)
library(dplyr,verbose = FALSE)

#Set up the base test database
reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
reindeer:::add_dummy_metadata(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
#Clean up possibly conflicting tracks
reindeer::remove_ssffTrackDefinition(emuDBhandle,name="fm",deleteFiles = TRUE)
reindeer::remove_ssffTrackDefinition(emuDBhandle,name="dft",deleteFiles = TRUE)

nonSSFFFunctions <-c("rfcana","afdiff","affilter")

test_that("Check that we can get default signal processing parameters",{
  data("DSPP")
  #message(nrow(DSPP))
  reindeer:::dspp_metadataParameters() -> dspp

  testthat::expect_equal(nrow(DSPP),nrow(dspp))
  testthat::expect_gte(nrow(DSPP),60)

})

for(fun in setdiff(names(wrassp::wrasspOutputInfos),nonSSFFFunctions)){
  test_that(paste0("[",fun,"] - Check that metadata_parameters handles missing metadata files correctly"),{
    reindeer:::unlink_emuRDemoDir()
    reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
    reindeer:::add_dummy_metadata(emuDBhandle)
    fl = emuR::list_files(emuDBhandle,"wav")
    unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])
     md <- reindeer:::match_parameters(emuDBhandle,"forest")

     testthat::expect_equal(nrow(md), nrow(fl))

  })
}



reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
reindeer:::add_dummy_metadata(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])

for(fun in setdiff(names(wrassp::wrasspOutputInfos),nonSSFFFunctions)){
  test_that(paste0("Signals can be created using the wrassp::",fun," function"),{
      ext <- superassp::get_extension(fun)
      tracks <- setdiff(superassp::get_definedtracks(fun),c("arf|lar|lpc|rfc",""))
      outputType <- superassp::get_outputType(fun)
      if(outputType == "SSFF"){
        for(tr in tracks){

          reindeer::add_trackDefinition(emuDBhandle,
                                        name=tr,
                                        columnName = tr,
                                        fileExtension = ext,
                                        onTheFlyFunctionName = fun,verbose=FALSE)
          #,onTheFlyOptLogFilePath = "~/Desktop/temp/"

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

#Set up the base test database
reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
reindeer:::add_dummy_metadata(emuDBhandle)
reindeer::remove_ssffTrackDefinition(emuDBhandle,"fm",deleteFiles = TRUE)

praatfuns <- c("praat_formant_burg","praat_sauce","praat_intensity","praat_moments")

for(currFun in praatfuns){
  test_that(paste0("Check that add_trackDefintion() can create signals using the ",currFun," Praat function, and attach all tracks"),{
    currExt <- superassp::get_extension(currFun)
    tr <- superassp::get_definedtracks(currFun)
    firstTrack <- head(tr,1)
    attachTracks <- tail(tr,-1) #Everything but the first track


    unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])

    #Make the file
    reindeer::add_trackDefinition(emuDBhandle,name=firstTrack,columnName = firstTrack,fileExtension = currExt,onTheFlyFunctionName = currFun,verbose=FALSE)
    fe <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$fileExtension

    expect_true(currExt %in% fe)
    #Attach track definitions to the file
    for(currTr in attachTracks){
      reindeer::add_trackDefinition(emuDBhandle,name=currTr,columnName = currTr,fileExtension = currExt,verbose=FALSE)
      cn <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$columnName
      expect_true(currTr %in% cn)
      reindeer::remove_ssffTrackDefinition(emuDBhandle,name=currTr)
    }
    #Cleanup the file too
    reindeer::remove_ssffTrackDefinition(emuDBhandle,name=firstTrack,deleteFiles = TRUE)
  })
}


test_that("Cut points and npoints are handled properly",{
  expect_equal(getSamples(512,522,cut=0.4,3),c(514,516))
  expect_equal(getSamples(512,522,cut=0.4,2),c(515,516))
  expect_equal(getSamples(512,525,cut=0.4,3),c(516,518))
  expect_equal(getSamples(512,525,cut=0.4,2),c(517,518))
  #Check boundaries
  expect_equal(getSamples(1,525,cut=0,npoints=3),c(1,3))
  expect_equal(getSamples(1,525,cut=1,npoints=3),c(524,526))
  expect_equal(getSamples(1,525,cut=1,npoints=3,endOfTrack = 525),c(523,525))
})


test_that("Check that cut and npoints arguments result in the equivalent subsetting of read data",{
  signalfile <- file.path("..","signalfiles","prolonged_a","a1.wav")
  expect_equal(readTrackData(signalfile,cut=0,npoints = 5),readTrackData(signalfile)[1:5,] )
  readTrackData(signalfile) -> ref

  expect_equal(readTrackData(signalfile,cut=0.5,npoints = 5),ref[(nrow(ref)/2-2):(nrow(ref)/2+2), ])

  expect_equal(readTrackData(signalfile,cut=0.25,npoints = 5),ref[(round(nrow(ref)/4)-2):(round(nrow(ref)/4)+2), ])

  expect_equal(readTrackData(signalfile,cut=1,npoints = 5),tail(ref,n=5))
})

for( kind in c("","no ")){
  test_that(paste0("We can add missing signal track files when ",kind,"metadata are present"),{
    reindeer:::unlink_emuRDemoDir()
    reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
    if(kind == ""){
      reindeer:::add_dummy_metadata(emuDBhandle)
    }
    reindeer::remove_ssffTrackDefinition(emuDBhandle,name="fm",deleteFiles = TRUE)
    reindeer::remove_ssffTrackDefinition(emuDBhandle,name="dft",deleteFiles = TRUE)
    #Check that we can make a track using defaults
    reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",onTheFlyFunctionName = "forest",verbose = FALSE)

    expect_true(!is.null(list_ssffTrackDefinitions(emuDBhandle)))
    reindeer::remove_ssffTrackDefinition(emuDBhandle, name="FORMANTS",deleteFiles =TRUE)
    expect_null(list_ssffTrackDefinitions(emuDBhandle))
    #Check that we can make a track using specified inputs
    reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",columnName = "fm",fileExtension = "for",onTheFlyFunctionName = "forest",verbose = FALSE)
    before <- list_files(emuDBhandle = emuDBhandle,fileExtension = "for")$absolute_file_path
    toRemove <-  before[1:2]
    unlink(x=toRemove,recursive = FALSE)
    after <- list_files(emuDBhandle = emuDBhandle,fileExtension = "for")$absolute_file_path

    assertthat::assert_that(length(before) == 7,length(toRemove)== 2, length(after) == 5)
    #
    expect_error({
      reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",fileExtension = "fms",onTheFlyFunctionName = "forest",verbose = FALSE)}
      ,regexp= "A track .* is already defined.* file extension .*")

    unlink(x=toRemove,recursive = FALSE)
    expect_error(
      reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",columnName = "fms",onTheFlyFunctionName = "forest",verbose = FALSE),
      regexp= "A track .* is already defined.* column name .* "
    )

    reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",onTheFlyFunctionName = "forest",verbose = FALSE)
    reinitialized <- list_files(emuDBhandle = emuDBhandle,fileExtension = "for")$absolute_file_path
    expect_equal(sort(before),sort(reinitialized))

    # Now regenerate another way
    unlink(x=toRemove,recursive = FALSE)
    reindeer::add_trackDefinition(emuDBhandle = emuDBhandle,name="FORMANTS",onTheFlyFunctionName = "forest",verbose = FALSE)
    reinitialized <- list_files(emuDBhandle = emuDBhandle,fileExtension = "for")$absolute_file_path
    expect_equal(sort(before),sort(reinitialized))
  })
}

