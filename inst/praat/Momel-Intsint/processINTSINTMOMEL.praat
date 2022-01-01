form INTSINT/MOMEL processing of a single file
	sentence Input_Directory /Users/frkkan96/Desktop/INT/
	#sentence Momel_parameters 30 60 750 1.04 20 5 0.05
	integer Window_length_(ms) 30
	integer Minimum_f0_(Hz) 60
	integer Maximum_f0_(Hz) 750
	real Pitch_span 1.5 (=normal, 2.5=expressive speech)
	real Maximum_error 1.04
	integer Reduced_window_length_(ms) 20
	integer Minimal_distance_(ms) 20
	real Minimal_frequency_ratio 0.05
	sentence Output_file /Users/frkkan96/Desktop/INT/MOMELINTSINT.csv
endform

    
#      fprintf(outf,"\nusage: \tMtargets [options] win1 lo hi maxerr win2 mind minr < [F0 values]\n\n");
#      fprintf(outf,"win1:\t\tcible window length\n");
#      fprintf(outf,"lo:\t\tF0 threshold\n");
#      fprintf(outf,"hi:\t\tF0 ceiling\n");
#      fprintf(outf,"maxerr:\t\tmaximum error\n");
#      fprintf(outf,"win2:\t\treduc window length\n");
#      fprintf(outf,"mind:\t\tminimal distance\n");
#      fprintf(outf,"minr:\t\tminimal frequency ratio\n");


soundLst = Create Strings as file list: "soundList", "'input_Directory$'/*.wav"
noSounds = Get number of strings

bundleLst = Replace all: ".wav", "", 1, "literals"
Rename: "bundleLst"

for f from 1 to noSounds
	select soundLst
	currFileName$ = Get string: f
	select bundleLst
	currBundle$ = Get string: f


	currSound = Read from file: "'input_Directory$'/'currFileName$'"

	runScript: "./plugin_momel-intsint/analysis/automatic_min_max_f0.praat", pitch_span
	currPitch = selected("Pitch")

	runScript: "./plugin_momel-intsint/analysis/momel_single_file.praat", "'window_length' 'minimum_f0' 'maximum_f0' 'maximum_error' 'reduced_window_length' 'minimal_distance' 'minimal_frequency_ratio'" 
	currPT = selected("PitchTier")
	runScript: "./plugin_momel-intsint/analysis/code_with_intsint.praat"
	momIntTG = selected("TextGrid")

	tgTable = Down to Table: "no", 20, "yes", "no"
	Append column: "file"
	noAnchors = Get number of rows

	for r from 1 to noAnchors
		select tgTable
		Set string value: r, "file", currBundle$
	endfor

	if f == 1
		outTab = tgTable
	else
		selectObject: outTab 
		plusObject: tgTable
		outTab = Append
	endif
endfor

selectObject: outTab
Save as semicolon-separated file: "'output_file$'"

