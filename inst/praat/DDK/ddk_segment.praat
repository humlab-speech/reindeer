form Automatic segmentation of DDK sequences
	sentence SoundDirectory ../../../tests/signalfiles/DDK
	real BeginTime 0.0
	real EndTime 0.0
	real Time_step 0.005
	real Minimum_pitch 100.0
	real Silence_threshold_(dB) -9.0
	real Minimum_silent_interval_duration_(s) 0.05
	real	Minimum_sounding_interval_duration_(s) 0.025
	text Consonant_label C
	text Vowel_label V
	#text Sequence_label DDK
	real Sequence_silence_threshold -25.0
	real Sequence_minimum_duration 0.100
	word WindowShape Gaussian1
	real RelativeWidth 0.001
	word Spectrogram_window_shape Gaussian
	real Spectrogram_resolution 40.0
	text Mediafile_extension wav
	sentence TableOut ../../../tests/signalfiles/DDK/ddk.Table
endform

fileList = Create Strings as file list: "fileList", soundDirectory$ + "/*." + mediafile_extension$
noFiles = Get number of strings

for currFile from 1 to noFiles
	select fileList
 	currSound$ = Get string: currFile
	segment$ = replace$ (currSound$, ".wav", "", 1)
	soundFile$ = soundDirectory$ + "/" + currSound$

	if fileReadable(soundFile$)
		sound = Read from file... 'soundFile$'
	else
		exitScript("Could not read file 'soundFile$'")
	endif

	currint = To Intensity: minimum_pitch, 0, "yes"

	sil = To TextGrid (silences): silence_threshold, minimum_silent_interval_duration, minimum_sounding_interval_duration, consonant_label$, vowel_label$
	Insert interval tier: 1, "DDK Syllables"
	Set tier name: 2, "DDK Segments"

	select currint
	ddkSequence = To TextGrid (silences): sequence_silence_threshold, 0.1, sequence_minimum_duration, "", "DDK"
	ddkTab = Down to Table: "no", 6, "yes", "no"
	ddkStart = Get value: 1, "tmin"
	ddkEnd = Get value: 1, "tmax"
	select sil
	nocheck Insert boundary... 1 'ddkStart'
	nocheck Insert boundary... 2 'ddkStart'
	nocheck Insert boundary... 1 'ddkEnd'
	nocheck Insert boundary... 2 'ddkEnd'

	noSegs = Get number of intervals... 2

	for i from 1 to noSegs

		lab$ = Get label of interval... 2 'i'
		endTime = Get end time of interval... 2 'i'
		startTime = Get start time of interval... 2 'i'
		if lab$ == vowel_label$
			nocheck Insert boundary... 1 'endTime'
		elsif lab$ == consonant_label$
			nocheck Insert boundary... 2 'startTime'
		endif 
	endfor

	noSylls = Get number of intervals... 2

	for i from 1 to noSylls
	#	Set interval text... 2 'i' 'markedLab$'
		select sil
		tim = Get end time of interval: 2, 'i'
		nocheck Insert boundary: 2, 'tim'
	endfor


	currOutTab = Down to Table: "no", 20, "yes","no"
	Append column: "segment"
	noRows = Get number of rows

	for r from 1 to noRows
		Set string value: r, "segment", segment$
		curr = Get value: r, "tmin"
		new = curr + beginTime
		Set numeric value: r, "tmin", new
		curr = Get value: r, "tmax"
		new = curr + beginTime
		Set numeric value: r, "tmax", new
	endfor

	#Now make sure we have an output
	if currFile == 1
		outTab = currOutTab
		#removeObject: currOutTab
	else
		select outTab 
		plus currOutTab
		newOutTab = Append
		outTab = newOutTab
	endif
endfor

#removeObject: currint
#plus currint
#Remove
selectObject: outTab
Save as semicolon-separated file: tableOut$


