beginPause: "computations pharyngealisation"
comment: "Where are your sound files and TextGrids?"
sentence: "directory1", "D:\ArticlesResearch\GitHub_scripts\Praat\pharyFULL"
comment: "Results File"
sentence: "output1", "Results"
clicked = endPause: "OK", 1



if directory1$ = ""
	directory1$ = chooseDirectory$("Select your directory of sound files and TextGrids")
endif

createDirectory: "spectra"
createDirectory: "images"


Create Strings as file list: "fileList", "'directory1$'\*.wav"
nbFiles = Get number of strings

appendFileLine: "'output1$'.xls", "File name", tab$, "language", tab$, "speaker", tab$, 
... "context", tab$, "consonant", tab$, "vowel", tab$, "Duration", tab$, 
... "startPointFrame", tab$, "midPointFrame", tab$, "endPointFrame", tab$, "f1Start", tab$, 
... "f2Start", tab$, "f3Start", tab$, "f1Mid", tab$, "f2Mid", tab$, "f3Mid", tab$, "bw1Start", tab$,
... "bw2Start", tab$, "bw3Start", tab$, "bw1Mid", tab$, "bw2Mid", tab$, "bw3Mid", tab$, 
... "f0Start", tab$, "f0Mid", tab$, "intensityStart", tab$, "intensityMid", tab$, 
... "h1mnh2Onset", tab$, "h1mnh2OnsetNorm", tab$, "h1mna1Onset", tab$, "h1mna1OnsetNorm", tab$, 
... "h1mna2Onset", tab$, "h1mna2OnsetNorm", tab$, "h1mna3Onset", tab$, "h1mna3OnsetNorm", tab$, 
... "a1mna2Onset", tab$, "a1mna3Onset", tab$, "a1mna3OnsetNorm", tab$, "a2mna3Onset", tab$, 
... "a2mna3OnsetNorm", tab$, "h1mnh2Mid", tab$, "h1mnh2MidNorm", tab$, "h1mna1Mid", tab$, 
... "h1mna1MidNorm", tab$, "h1mna2Mid", tab$, "h1mna2MidNorm", tab$, "h1mna3Mid", tab$, 
... "h1mna3MidNorm", tab$, "a1mna2Mid", tab$, "a1mna3Mid", tab$, "a1mna3MidNorm", tab$, 
... "a2mna3Mid", tab$, "a2mna3MidNorm", tab$, "h1Onset", tab$, "h1HzOnset", tab$, 
... "h1OnsetNorm", tab$, "h2Onset", tab$, "h2HzOnset", tab$, "h2OnsetNorm", tab$, "a1Onset", tab$, 
... "a1HzOnset", tab$, "a2Onset", tab$, "a2HzOnset", tab$, "a3Onset", tab$, "a3HzOnset", tab$, 
... "a3OnsetNorm", tab$, "h1Mid", tab$, "h1HzMid", tab$, "h1MidNorm", tab$, "h2Mid", tab$, 
... "h2HzMid", tab$, "h2MidNorm", tab$, "a1Mid", tab$, "a1HzMid", tab$, "a2Mid", tab$, 
... "a2HzMid", tab$, "a3Mid", tab$, "a3HzMid", tab$, "a3MidNorm"


for i from 1 to nbFiles
	select Strings fileList
	nameFile$ = Get string: i
	

	language$ = mid$ ("'nameFile$'", 1, 2)
	speaker$ = mid$ ("'nameFile$'", 6, 2)

	first_carac$ = mid$ ("'nameFile$'", 9, 1)
	second_carac$ = mid$ ("'nameFile$'", 10, 1)
	if first_carac$ = "d" and second_carac$ <> "d"
		consonant$ = "d"
	elsif first_carac$ = "d" and second_carac$ = "d"
		consonant$ = "dd"
	elsif first_carac$ = "m"
		consonant$ = "dd"
	endif

	Read from file: "'directory1$'/'nameFile$'"
	name$ = selected$("Sound")	
	Read from file: "'directory1$'/'name$'.TextGrid"
	selectObject: "Sound 'name$'"
	#compute f0, two-passes
	noprogress To Pitch (ac): 0.005, 75, 15, "yes", 0.03, 0.45, 0.01, 0.35, 0.14, 600
	q1 = Get quantile: 0, 0, 0.25, "Hertz"
	q3 = Get quantile: 0, 0, 0.75, "Hertz"
	minPitch = q1*0.75
	maxPitch = q3*1.5
	Remove
	selectObject: "Sound 'name$'"
	noprogress To Pitch (ac): 0.005, minPitch, 15, "yes", 0.03, 0.45, 0.01, 0.35, 0.14, maxPitch
	To PointProcess
	meanPeriod = Get mean period: 0, 0, 0.0001, 0.02, 1.3

	selectObject: "Sound 'name$'"
	To Intensity: minPitch, 0.005, "yes"

	selectObject: "Sound 'name$'"
	To Spectrogram: 0.005, 5000, 0.002, 20, "Gaussian"

	selectObject: "Sound 'name$'"
	Resample: 10000, 50
	soundResampled = selected ("Sound")

	selectObject: "TextGrid 'name$'"
	Convert to Unicode
	nb_interval = Get number of intervals: 3
	finishing_time = Get finishing time


	for j from 1 to nb_interval

		selectObject: "TextGrid 'name$'"
		label$ = Get label of interval: 3, j
		label2$ = Get label of interval: 1, j
		if label$ <> ""

			end = Get end point: 3, j
			start = Get starting point: 3, j
			duration = (end - start)
			duration_ms = duration*1000
			mid = start+(duration/2)
			startCut = start - 0.05
			endCut = end + 0.05

			midBeforeFrame = mid - (meanPeriod/2)
			midAfterFrame = mid + (meanPeriod/2)
			startAfterFrame = start + meanPeriod
			endBeforeFrame = end - meanPeriod

			startSpectrumBefore = start
			startSpectrumAfter = start+0.04
			midSpectrumBefore = mid-0.02
			midSpectrumAfter = mid+0.02

			selectObject: "Intensity 'name$'"
			maxTimeIntensityStart = Get time of maximum: start, startAfterFrame, "Parabolic"
			maxTimeIntensityMid = Get time of maximum: midBeforeFrame, midAfterFrame, "Parabolic"
			maxTimeIntensityEnd = Get time of maximum: endBeforeFrame, end, "Parabolic"
		
			selectObject: "Sound 'name$'"
			soundPart = Extract part: startCut, endCut, "rectangular", 1, "yes"

			formant = To Formant (burg): 0.005, 5, 5000, 0.025, 50
			nbFormants = Get minimum number of formants
			if nbFormants >= 4
				formantTrack = Track: 4, 500, 1500, 2500, 3500, 4500, 1, 1, 1
			elsif nbFormants = 3
				formantTrack = Track: 3, 500, 1500, 2500, 3500, 4500, 1, 1, 1
			else
				formantTrack = Track: 2, 500, 1500, 2500, 3500, 4500, 1, 1, 1
			endif


			select formantTrack
			fnStart = Get frame number from time: maxTimeIntensityStart
			fnMid = Get frame number from time: maxTimeIntensityMid
			fnEnd = Get frame number from time: maxTimeIntensityEnd
			fnStartR = round(fnStart)
			fnMidR = round(fnMid)
			fnEndR = round(fnEnd)
		
			startPointFrame = Get time from frame number: fnStartR
			midPointFrame = Get time from frame number: fnMidR
			endPointFrame = Get time from frame number: fnEndR


			f1Start = Get value at time: 1, startPointFrame, "Hertz", "Linear"
				if f1Start = undefined
					f1Start = 1234
				elsif f1Start > 5000
					f1Start = 4999
				endif
			f2Start = Get value at time: 2, startPointFrame, "Hertz", "Linear"
				if f2Start = undefined
					f2Start = 1234
				elsif f2Start > 5000
					f2Start = 4999
				endif
			f3Start = Get value at time: 3, startPointFrame, "Hertz", "Linear"
				if f3Start = undefined
					f3Start = 1234
				elsif f3Start > 5000
					f3Start = 4999
				endif

			f1Mid = Get value at time: 1, midPointFrame, "Hertz", "Linear"
				if f1Mid = undefined
					f1Mid = 1234
				elsif f1Mid > 5000
					f1Mid = 4999
				endif
			f2Mid = Get value at time: 2, midPointFrame, "Hertz", "Linear"
				if f2Mid = undefined
					f2Mid = 1234
				elsif f2Mid > 5000
					f2Mid = 4999
				endif
			f3Mid = Get value at time: 3, midPointFrame, "Hertz", "Linear"
				if f3Mid = undefined
					f3Mid = 1234
				elsif f3Mid > 5000
					f3Mid = 4999
				endif

			bw1Start = Get bandwidth at time: 1, startPointFrame, "Hertz", "Linear"
				if bw1Start = undefined
					bw1Start = 1234
				endif
			bw2Start = Get bandwidth at time: 2, startPointFrame, "Hertz", "Linear"
				if bw2Start = undefined
					bw2Start = 1234
				endif
			bw3Start = Get bandwidth at time: 3, startPointFrame, "Hertz", "Linear"
				if bw3Start = undefined
					bw3Start = 1234
				endif
			bw1StartEst = round (80 + ((120*f1Start)/5000))
			bw1StartDiff = abs(bw1Start - bw1StartEst)
			bw1StartDiffRn = round (bw1StartDiff)
				if bw1Start <> bw1StartEst and bw1StartDiffRn > 20
					bw1StartNorm = bw1StartEst
				else
					bw1StartNorm = bw1Start
				endif
			bw2StartEst = round (80 + ((120*f2Start)/5000))
			bw2StartDiff = abs(bw2Start - bw2StartEst)
			bw2StartDiffRn = round (bw2StartDiff)
				if bw2Start <> bw2StartEst and bw2StartDiffRn > 20
					bw2StartNorm = bw2StartEst
				else
					bw2StartNorm = bw2Start
				endif
			bw3StartEst = round (80 + ((120*f3Start)/5000))
			bw3StartDiff = abs(bw3Start - bw3StartEst)
			bw3StartDiffRn = round (bw3StartDiff)
				if bw3Start <> bw3StartEst and bw3StartDiffRn > 20
					bw3StartNorm = bw3StartEst
				else
					bw3StartNorm = bw3Start
				endif

			bw1Mid = Get bandwidth at time: 1, midPointFrame, "Hertz", "Linear"
				if bw1Mid = undefined
					bw1Mid = 1234
				endif
			bw2Mid = Get bandwidth at time: 2, midPointFrame, "Hertz", "Linear"
				if bw2Mid = undefined
					bw2Mid = 1234
				endif
			bw3Mid = Get bandwidth at time: 3, midPointFrame, "Hertz", "Linear"
				if bw3Mid = undefined
					bw3Mid = 1234
				endif
			bw1MidEst = round (80 + ((120*f1Mid)/5000))
			bw1MidDiff = abs(bw1Mid - bw1MidEst)
			bw1MidDiffRn = round (bw1MidDiff)
				if bw1Mid <> bw1MidEst and bw1MidDiffRn > 20
					bw1MidNorm = bw1MidEst
				else
					bw1MidNorm = bw1Mid
					endif
			bw2MidEst = round (80 + ((120*f2Mid)/5000))
			bw2MidDiff = abs(bw2Mid - bw2MidEst)
			bw2MidDiffRn = round (bw2MidDiff)
				if bw2Mid <> bw2MidEst and bw2MidDiffRn > 20
					bw2MidNorm = bw2MidEst
				else
					bw2MidNorm = bw2Mid
				endif
			bw3MidEst = round (80 + ((120*f3Mid)/5000))
			bw3MidDiff = abs(bw3Mid - bw3MidEst)
			bw3MidDiffRn = round (bw3MidDiff)
				if bw3Mid <> bw3MidEst and bw3MidDiffRn > 20
					bw3MidNorm = bw3MidEst
				else
					bw3MidNorm = bw3Mid
				endif


			selectObject: "Pitch 'name$'"
			f0Start = Get value at time: startPointFrame, "Hertz", "Linear"
				if f0Start = undefined
					f0Start = 1234
				endif
			f0Mid = Get value at time: midPointFrame, "Hertz", "Linear"
				if f0Mid = undefined
					f0Mid = 1234
				endif
			f0End = Get value at time: endPointFrame, "Hertz", "Linear"
				if f0End = undefined
					f0End = 1234
				endif

			selectObject: "Intensity 'name$'"
			intensityStart = Get value at time: startPointFrame, "Cubic"
			intensityMid = Get value at time: midPointFrame, "Cubic"
			intensityEnd = Get value at time: endPointFrame, "Cubic"

			if duration_ms > 120

			#########
			# Onset	#
			#########

			########################################################
			# Computation of the difference in harmonics amplitude #
			#   with normalisation based on Iseli and Alwan 2004   #
			#   H1 and H2 are obtained from the Inverse filtered   #
			#       sound and the normalisation is based on it     #
			########################################################

			#########################################################################################################################################################################################################################################################################################################################################################################################################################################
			# normalisation formulae from Iseli and Alwan:		 																																														#
			# 10*LOG10((((EXPonential(-PI()*(Bi/SamplingFrequency)))^2)+(1-(2*(EXPonential(-PI()*(Bi/SamplingFrequency)))*(COS(2*PI()*(Fi/SF))))))^2/((((EXPonential(-PI()*(Bi/SamplingFrequency)))^2)+1-(2*(EXPonential(-PI()*(Bi/SamplingFrequency)))*(COS((2*PI()*(f0/SF))+2*PI()*(Fi/SF)))))*((((EXPonential(-PI()*(Bi/SamplingFrequency)))^2)+1-(2*(EXPonential(-PI()*(Bi/SamplingFrequency)))*(COS(((2*PI()*(f0/SF))-2*PI()*(Fi/SF))))))))	#
			# 10*log10((((exp(-pi*(91/44100)))^2)+(1-(2*(exp(-pi*(91/44100)))*(cos(2*pi*(473/44100))))))^2/((((exp(-pi*(91/44100)))^2)+(1-(2*(exp(-pi*(91/44100)))*(cos((2*pi*(237/44100))+(2*pi*(473/44100)))))))*(((exp(-pi*(91/44100)))^2)+(1-(2*(exp(-pi*(91/44100)))*(cos((2*pi*(237/44100))-(2*pi*(473/44100)))))))))																#
			#########################################################################################################################################################################################################################################################################################################################################################################################################################################

				select soundResampled
				sampfreq = Get sampling frequency


				#########
				# Onset	#
				#########
				select soundResampled
				Extract part: startSpectrumBefore, startSpectrumAfter, "Kaiser2", 1, "yes"
				soundSlice = selected("Sound")
				select soundSlice
				To Spectrum: "yes"
				spectrumFilteredOnset = selected("Spectrum")
				Filter (pass Hann band): 0, 5000, 100
				Formula... if x >= 50 then self*x else self fi
				select spectrumFilteredOnset
				To Ltas (1-to-1)
					ltasFilteredOnset = selected("Ltas")

				if f0Start = undefined
					h1Onset = 1234
					h1HzOnset = 1234
					h1OnsetNorm = 1234
					h2Onset = 1234
					h2HzOnset = 1234
					h2OnsetNorm = 1234
					a1Onset = 1234
					a1HzOnset = 1234
					a2Onset = 1234
					a2HzOnset = 1234
					a3Onset = 1234
					a3HzOnset = 1234
					a3OnsetNorm = 1234
					h1mnh2Onset = 1234
					h1mnh2OnsetNorm = 1234
						h1mna1Onset = 1234
					h1mna1OnsetNorm = 1234
						h1mna2Onset = 1234
					h1mna2OnsetNorm = 1234
						h1mna3Onset = 1234
					h1mna3OnsetNorm = 1234
					a1mna2Onset = 1234
					a1mna3Onset = 1234
					a1mna3OnsetNorm = 1234
					a2mna3Onset = 1234
					a2mna3OnsetNorm = 1234

					select spectrumFilteredOnset
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset_Und" + ".Spectrum"
					
				else
					f0Start10 = f0Start / 10
					select ltasFilteredOnset
						lowerbh1start = f0Start - f0Start10
						upperbh1start = f0Start + f0Start10
						lowerbh2start = (f0Start * 2) - f0Start10
						upperbh2start = (f0Start * 2) + f0Start10
						h1Onset = Get maximum: lowerbh1start, upperbh1start, "Parabolic"
						if h1Onset = undefined
							h1Onset = 1234
						endif
					h1HzOnset = Get frequency of maximum: lowerbh1start, upperbh1start, "Parabolic"
						if h1HzOnset = undefined
							h1HzOnset = 1234
						elsif h1HzOnset > 5000
							h1HzOnset = 4999
						endif
						h2Onset = Get maximum: lowerbh2start, upperbh2start, "Parabolic"
						if h2Onset = undefined
							h2Onset = 1234
						endif
					h2HzOnset = Get frequency of maximum: lowerbh2start, upperbh2start, "Parabolic"
						if h2HzOnset = undefined
							h2HzOnset = 1234
						elsif h2HzOnset > 5000
							h2HzOnset = 4999
						endif

						h1mnh2Onset = h1Onset - h2Onset
					h1f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					h1f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					h2f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					h2f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					h1OnsetNorm = h1Onset - (h1f1OnsetNorm + h1f2OnsetNorm)
					h2OnsetNorm = h2Onset - (h2f1OnsetNorm + h2f2OnsetNorm)

					select ltasFilteredOnset
					lowerba1start = f1Start - (bw1StartNorm/2)
						upperba1start = f1Start + (bw1StartNorm/2)
						lowerba2start = f2Start - (bw2StartNorm/2)
						upperba2start = f2Start + (bw2StartNorm/2)
						lowerba3start = f3Start - (bw3StartNorm/2)
						upperba3start = f3Start + (bw3StartNorm/2)
						a1Onset = Get maximum: lowerba1start, upperba1start, "Parabolic"
						if a1Onset = undefined
							a1Onset = 1234
						endif
							
					a1HzOnset = Get frequency of maximum: lowerba1start, upperba1start, "Parabolic"
						if a1HzOnset = undefined
							a1HzOnset = 1234
						elsif a1HzOnset >= 5000
							a1HzOnset = 4999
						endif
						a2Onset = Get maximum: lowerba2start, upperba2start, "Parabolic"
						if a2Onset = undefined
							a2Onset = 1234
						endif
					a2HzOnset = Get frequency of maximum: lowerba2start, upperba2start, "Parabolic"
						if a2HzOnset = undefined
							a2HzOnset = 1234
						elsif a2HzOnset >= 5000
							a2HzOnset = 4999
						endif
						a3Onset = Get maximum: lowerba3start, upperba3start, "Parabolic"
						if a3Onset = undefined
							a3Onset = 1234
						endif
					a3HzOnset = Get frequency of maximum: lowerba3start, upperba3start, "Parabolic"
						if a3HzOnset = undefined
							a3HzOnset = 1234
						elsif a3HzOnset >= 5000
							a3HzOnset = 4999
						endif

					a3f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					a3f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					a3f3OnsetNorm = 10*log10((((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos(2*pi*(f3Start/sampfreq))))))^2/((((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f3Start/sampfreq)))))))*(((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f3Start/sampfreq)))))))))

					a3OnsetNorm = a3Onset - (a3f1OnsetNorm+a3f2OnsetNorm+a3f3OnsetNorm)

						h1mnh2Onset = h1Onset - h2Onset
					h1mnh2OnsetNorm = h1OnsetNorm - h2OnsetNorm
						h1mna1Onset = h1Onset - a1Onset
					h1mna1OnsetNorm = h1OnsetNorm - a1Onset
						h1mna2Onset = h1Onset - a2Onset
					h1mna2OnsetNorm = h1OnsetNorm - a2Onset
						h1mna3Onset = h1Onset - a3Onset
					h1mna3OnsetNorm = h1OnsetNorm - a3OnsetNorm
					a1mna2Onset = a1Onset - a2Onset
					a1mna3Onset = a1Onset - a3Onset
					a1mna3OnsetNorm = a1Onset - a3OnsetNorm
					a2mna3Onset = a2Onset - a3Onset
					a2mna3OnsetNorm = a2Onset - a3OnsetNorm

					select spectrumFilteredOnset
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset" + ".Spectrum"

					#################################################################
					# Here we draw a spectrogram of a particular sound with the 	#
					# corresponding TextGrid and the formant tracking overlaid.	#
					#################################################################

					Erase all
						Font size: 14
						display_from = start - 0.05
							if display_from < 0
									display_from = 0
							endif
						display_until = end + 0.05
							if display_until > finishing_time
									display_until = finishing_time
							endif
						selectObject: "Spectrogram 'name$'"
						Viewport: 0, 7, 0, 4
						Paint: display_from, display_until, 0, 5000, 100, "yes", 50, 6, 0, "no"
						select formantTrack
						Yellow
						Speckle: display_from, display_until, 5000, 30, "no"
						Marks left every: 1, 500, "yes", "yes", "yes"  
						Viewport: 0, 7, 0, 8
						selectObject: "TextGrid 'name$'"
						Black
						Draw: display_from, display_until, "no", "yes", "yes"
						One mark bottom: startPointFrame, "yes", "yes", "yes", ""
						Viewport: 0, 7, 0, 0.75
						
						Text top: "yes", "F1Start: 'f1Start:2' --- F2Start: 'f2Start:2' --- F3Start: 'f3Start:2'"
						Viewport: 0, 7, 0.25, 1
						Text top: "yes", "a1Start: 'a1HzOnset:2' --- a2Start: 'a2HzOnset:2' --- a3Start: 'a3HzOnset:2'"


					#########################################################################
					#      then we draw the spectrum of that same portion with LTAS and LPC #
					#      overlaid with indication of where things where measured!		#
					#########################################################################

					select spectrumFilteredOnset
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"
					select ltasFilteredOnset
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "no", "bars"
						Marks bottom every: 1, 500, "yes", "yes", "no"
						Marks bottom every: 1, 250, "no", "no", "yes"
						Line width: 1
						Text top: "yes", "Spectrum [40 ms], Ltas(1-to-1) [40 ms]"
					Black
					select spectrumFilteredOnset
						LPC smoothing: 5, 50
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"

					Viewport: 0, 7, 8, 12
						Font size: 14
						Line width: 3
					Red
						One mark top: h1HzOnset, "no", "yes", "yes", "H1"
						One mark top: h2HzOnset, "no", "yes", "yes", "H2"
						One mark top: a1HzOnset, "no", "yes", "yes", "A1"
					One mark top: a2HzOnset, "no", "yes", "yes", "A2"
						One mark top: a3HzOnset, "no", "yes", "yes", "A3"
					Line width: 4
					Blue
					One mark top: f1Start, "no", "yes", "yes", "F1"
					One mark top: f2Start, "no", "yes", "yes", "F2"
						One mark top: f3Start, "no", "yes", "yes", "F3"

					Select outer viewport: 0, 7, 0, 12		
					Save as 300-dpi PNG file: directory1$ + "/images" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset" + ".png"

				endif

				select spectrumFilteredOnset
				plus ltasFilteredOnset
				plus soundSlice
				Remove



				############
				# Midpoint #
				############

				select soundResampled
				Extract part:  midSpectrumBefore, midSpectrumAfter, "Kaiser2", 1, "yes"
				soundSlice = selected("Sound")
				select soundSlice
				To Spectrum: "yes"
				spectrumFilteredMid = selected("Spectrum")
				select spectrumFilteredMid
				Filter (pass Hann band): 0, 5000, 100
				Formula: "if x >= 50 then self*x else self fi"
				select spectrumFilteredMid
				To Ltas (1-to-1)
					ltasFilteredMid = selected ("Ltas")


				if f0Mid = undefined
					h1Mid = 1234
					h1HzMid = 1234
					h1MidNorm = 1234
					h2Mid = 1234
					h2HzMid = 1234
					h2MidNorm = 1234
					a1Mid = 1234
					a1HzMid = 1234
					a2Mid = 1234
					a2HzMid = 1234
					a3Mid = 1234
					a3HzMid = 1234
					a3MidNorm = 1234
					h1mnh2Mid = 1234
					h1mnh2MidNorm = 1234
						h1mna1Mid = 1234
					h1mna1MidNorm = 1234
						h1mna2Mid = 1234
					h1mna2MidNorm = 1234
						h1mna3Mid = 1234
					h1mna3MidNorm = 1234
					a1mna2Mid = 1234
					a1mna3Mid = 1234
					a1mna3MidNorm = 1234
					a2mna3Mid = 1234
					a2mna3MidNorm = 1234

					select spectrumFilteredMid
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Mid_Und" + ".spectrum"

				else
					f0Mid10 = f0Mid / 10
						select ltasFilteredMid
						lowerbh1Mid = f0Mid - f0Mid10
						upperbh1Mid = f0Mid + f0Mid10
						lowerbh2Mid = (f0Mid * 2) - f0Mid10
						upperbh2Mid = (f0Mid * 2) + f0Mid10
						h1Mid = Get maximum: lowerbh1Mid, upperbh1Mid, "Parabolic"
						if h1Mid = undefined
							h1Mid = 1234
						endif

					h1HzMid = Get frequency of maximum: lowerbh1Mid, upperbh1Mid, "Parabolic"
						if h1HzMid = undefined
							h1HzMid = 1234
						elsif h1HzMid > 5000
							h1HzMid = 4999
						endif

						h2Mid = Get maximum: lowerbh2Mid, upperbh2Mid, "Parabolic"
						if h2Mid = undefined
							h2Mid = 1234
						endif

					h2HzMid = Get frequency of maximum: lowerbh2Mid, upperbh2Mid, "Parabolic"
						if h2HzMid = undefined
							h2HzMid = 1234
						elsif h2HzMid > 5000
							h2HzMid = 4999
						endif

						h1mnh2Mid = h1Mid - h2Mid
					h1f1MidNorm = 10*log10((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos(2*pi*(f1Mid/sampfreq))))))^2/((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*(f0Mid/sampfreq))+(2*pi*(f1Mid/sampfreq)))))))*(((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*(f0Mid/sampfreq))-(2*pi*(f1Mid/sampfreq)))))))))
					h1f2MidNorm = 10*log10((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos(2*pi*(f2Mid/sampfreq))))))^2/((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*(f0Mid/sampfreq))+(2*pi*(f2Mid/sampfreq)))))))*(((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*(f0Mid/sampfreq))-(2*pi*(f2Mid/sampfreq)))))))))
					h2f1MidNorm = 10*log10((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos(2*pi*(f1Mid/sampfreq))))))^2/((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*((2*f0Mid)/sampfreq))+(2*pi*(f1Mid/sampfreq)))))))*(((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*((2*f0Mid)/sampfreq))-(2*pi*(f1Mid/sampfreq)))))))))
					h2f2MidNorm = 10*log10((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos(2*pi*(f2Mid/sampfreq))))))^2/((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*((2*f0Mid)/sampfreq))+(2*pi*(f2Mid/sampfreq)))))))*(((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*((2*f0Mid)/sampfreq))-(2*pi*(f2Mid/sampfreq)))))))))
					h1MidNorm = h1Mid - (h1f1MidNorm+h1f2MidNorm)
					h2MidNorm = h2Mid - (h2f1MidNorm+h2f2MidNorm)

					select ltasFilteredMid
					lowerba1Mid = f1Mid - (bw1MidNorm/2)
						upperba1Mid = f1Mid + (bw1MidNorm/2)
						lowerba2Mid = f2Mid - (bw2MidNorm/2)
						upperba2Mid = f2Mid + (bw2MidNorm/2)
						lowerba3Mid = f3Mid - (bw3MidNorm/2)
						upperba3Mid = f3Mid + (bw3MidNorm/2)
						a1Mid = Get maximum: lowerba1Mid, upperba1Mid, "Parabolic"
						if a1Mid = undefined
							a1Mid = 1234
						endif
					a1HzMid = Get frequency of maximum: lowerba1Mid, upperba1Mid, "Parabolic"
						if a1HzMid = undefined
							a1HzMid = 1234
						elsif a1HzMid > 5000
							a1HzMid = 4999
						endif
						a2Mid = Get maximum: lowerba2Mid, upperba2Mid, "Parabolic"
						if a2Mid = undefined
							a2Mid = 1234
						endif
					a2HzMid = Get frequency of maximum: lowerba2Mid, upperba2Mid, "Parabolic"
						if a2HzMid = undefined
							a2HzMid = 1234
						elsif a2HzMid > 5000
							a2HzMid = 4999
						endif
						a3Mid = Get maximum: lowerba3Mid, upperba3Mid, "Parabolic"
						if a3Mid = undefined
							a3Mid = 1234
						endif
					a3HzMid = Get frequency of maximum: lowerba3Mid, upperba3Mid, "Parabolic"
						if a3HzMid = undefined
							a3HzMid = 1234
						elsif a3HzMid > 5000
							a3HzMid = 4999
						endif

					a3f1MidNorm = 10*log10((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos(2*pi*(f1Mid/sampfreq))))))^2/((((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))+(2*pi*(f1Mid/sampfreq)))))))*(((exp(-pi*(bw1MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))-(2*pi*(f1Mid/sampfreq)))))))))
					a3f2MidNorm = 10*log10((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos(2*pi*(f2Mid/sampfreq))))))^2/((((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))+(2*pi*(f2Mid/sampfreq)))))))*(((exp(-pi*(bw2MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))-(2*pi*(f2Mid/sampfreq)))))))))
					a3f3MidNorm = 10*log10((((exp(-pi*(bw3MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3MidNorm/sampfreq)))*(cos(2*pi*(f3Mid/sampfreq))))))^2/((((exp(-pi*(bw3MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))+(2*pi*(f3Mid/sampfreq)))))))*(((exp(-pi*(bw3MidNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3MidNorm/sampfreq)))*(cos((2*pi*(f3Mid/sampfreq))-(2*pi*(f3Mid/sampfreq)))))))))
					a3MidNorm = a3Mid - (a3f1MidNorm+a3f2MidNorm+a3f3MidNorm)

						h1mnh2Mid = h1Mid - h2Mid
					h1mnh2MidNorm = h1MidNorm - h2MidNorm
						h1mna1Mid = h1Mid - a1Mid
					h1mna1MidNorm = h1MidNorm - a1Mid
						h1mna2Mid = h1Mid - a2Mid
					h1mna2MidNorm = h1MidNorm - a2Mid
						h1mna3Mid = h1Mid - a3Mid
					h1mna3MidNorm = h1MidNorm - a3MidNorm
					a1mna2Mid = a1Mid - a2Mid
					a1mna3Mid = a1Mid - a3Mid
					a1mna3MidNorm = a1Mid - a3MidNorm
					a2mna3Mid = a2Mid - a3Mid
					a2mna3MidNorm = a2Mid - a3MidNorm

					select spectrumFilteredMid
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Mid" + ".Spectrum"

					#################################################################
					# Here we draw a spectrogram of a particular sound with the 	#
					# corresponding TextGrid and the formant tracking overlaid.	#
					#################################################################

					Erase all
						Font size: 14
						display_from = start - 0.05
							if display_from < 0
									display_from = 0
							endif
						display_until = end + 0.05
							if display_until > finishing_time
									display_until = finishing_time
							endif
						selectObject: "Spectrogram 'name$'"
						Viewport: 0, 7, 0, 4
						Paint: display_from, display_until, 0, 5000, 100, "yes", 50, 6, 0, "no"
						select formantTrack
						Yellow
						Speckle: display_from, display_until, 5000, 30, "no"
						Marks left every: 1, 500, "yes", "yes", "yes"
						Viewport: 0, 7, 0, 8
						selectObject: "TextGrid 'name$'"
						Black
						Draw: display_from, display_until, "no", "yes", "yes"
						One mark bottom: midPointFrame, "yes", "yes", "yes", ""
						Viewport: 0, 7, 0, 0.75
							
						Text top: "yes", "F1Mid: 'f1Mid:2' --- F2Mid: 'f2Mid:2' --- F3Mid: 'f3Mid:2'"
						Viewport: 0, 7, 0.25, 1
						Text top: "yes", "a1Mid: 'a1HzMid:2' --- a2Mid: 'a2HzMid:2' --- a3Mid: 'a3HzMid:2'"


					#########################################################################
					#      then we draw the spectrum of that same portion with LTAS and LPC #
					#      overlaid with indication of where things where measured!		#
					#########################################################################

					select spectrumFilteredMid
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"
					select ltasFilteredMid
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "no", "bars"
						Marks bottom every: 1, 500, "yes", "yes", "no"
						Marks bottom every: 1, 250, "no", "no", "yes"
						Line width: 1
						Text top: "yes", "Spectrum [40 ms], Ltas(1-to-1) [40 ms]"
					Black
					select spectrumFilteredMid
						LPC smoothing: 5, 50
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"

					Viewport: 0, 7, 8, 12
						Font size: 14
						Line width: 3
					Red
						One mark top: h1HzMid, "no", "yes", "yes", "H1"
						One mark top: h2HzMid, "no", "yes", "yes", "H2"
						One mark top: a1HzMid, "no", "yes", "yes", "A1"
					One mark top: a2HzMid, "no", "yes", "yes", "A2"
						One mark top: a3HzMid, "no", "yes", "yes", "A3"
					Line width: 4
					Blue
					One mark top: f1Mid, "no", "yes", "yes", "F1"
					One mark top: f2Mid, "no", "yes", "yes", "F2"
						One mark top: f3Mid, "no", "yes", "yes", "F3"



					Select outer viewport: 0, 7, 0, 12
					Save as 300-dpi PNG file: directory1$ + "/images" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Mid" + ".png"

				endif

				select spectrumFilteredMid
				plus ltasFilteredMid
				plus soundSlice
				Remove

			elsif duration_ms >= 40 and duration_ms <=120

				select soundResampled
				sampfreq = Get sampling frequency

			#########################
			# Duration >= 40 <= 120	#
			#########################

				#########
				# Onset	#
				#########
				select soundResampled
				Extract part:  startSpectrumBefore, startSpectrumAfter, "Kaiser2", 1, "yes"
				soundSlice = selected ("Sound")
				select soundSlice
				To Spectrum: "yes"
				spectrumFilteredOnset = selected ("Spectrum")
				select spectrumFilteredOnset
				Filter (pass Hann band): 0, 5000, 100
				Formula: if x >= 50 then self*x else self fi
				select spectrumFilteredOnset
				To Ltas (1-to-1)
					ltasFilteredOnset = selected ("Ltas")

				if f0Start = undefined
					h1Onset = 1234
					h1HzOnset = 1234
					h1OnsetNorm = 1234
					h2Onset = 1234
					h2HzOnset = 1234
					h2OnsetNorm = 1234
					a1Onset = 1234
					a1HzOnset = 1234
					a2Onset = 1234
					a2HzOnset = 1234
					a3Onset = 1234
					a3HzOnset = 1234
					a3OnsetNorm = 1234
					h1mnh2Onset = 1234
					h1mnh2OnsetNorm = 1234
						h1mna1Onset = 1234
					h1mna1OnsetNorm = 1234
						h1mna2Onset = 1234
					h1mna2OnsetNorm = 1234
						h1mna3Onset = 1234
					h1mna3OnsetNorm = 1234
					a1mna2Onset = 1234
					a1mna3Onset = 1234
					a1mna3OnsetNorm = 1234
					a2mna3Onset = 1234
					a2mna3OnsetNorm = -1234

					select spectrumFilteredOnset
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset_Und" + ".Spectrum"

				else
					f0Start10 = f0Start / 10
					select ltasFilteredOnset
						lowerbh1start = f0Start - f0Start10
						upperbh1start = f0Start + f0Start10
						lowerbh2start = (f0Start * 2) - f0Start10
						upperbh2start = (f0Start * 2) + f0Start10
						h1Onset = Get maximum: lowerbh1start, upperbh1start, "Parabolic"
						if h1Onset = undefined
							h1Onset = 1234
						endif
					h1HzOnset = Get frequency of maximum: lowerbh1start, upperbh1start, "Parabolic"
						if h1HzOnset = undefined
							h1HzOnset = 1234
						elsif h1HzOnset > 5000
							h1HzOnset = 4999
						endif
						h2Onset = Get maximum: lowerbh2start, upperbh2start, "Parabolic"
						if h2Onset = undefined
							h2Onset = 1234
						endif
					h2HzOnset = Get frequency of maximum: lowerbh2start, upperbh2start, "Parabolic"
						if h2HzOnset = undefined
							h2HzOnset = 1234
						elsif h2HzOnset >= 5000
							h2HzOnset = 4999
						endif

						h1mnh2Onset = h1Onset - h2Onset
					h1f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					h1f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f0Start/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					h2f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					h2f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*((2*f0Start)/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					h1OnsetNorm = h1Onset - (h1f1OnsetNorm+h1f2OnsetNorm)
					h2OnsetNorm = h2Onset - (h2f1OnsetNorm+h2f2OnsetNorm)

					select ltasFilteredOnset
					lowerba1start = f1Start - (bw1StartNorm/2)
						upperba1start = f1Start + (bw1StartNorm/2)
						lowerba2start = f2Start - (bw2StartNorm/2)
						upperba2start = f2Start + (bw2StartNorm/2)
						lowerba3start = f3Start - (bw3StartNorm/2)
						upperba3start = f3Start + (bw3StartNorm/2)
						a1Onset = Get maximum: lowerba1start, upperba1start, "Parabolic"
						if a1Onset = undefined
							a1Onset = 1234
						endif
					a1HzOnset = Get frequency of maximum: lowerba1start, upperba1start, "Parabolic"
						if a1HzOnset = undefined
							a1HzOnset = 1234
						elsif a1HzOnset > 5000
							a1HzOnset = 4999
						endif
						a2Onset = Get maximum: lowerba2start, upperba2start, "Parabolic"
						if a2Onset = undefined
							a2Onset = 1234
						endif
					a2HzOnset = Get frequency of maximum: lowerba2start, upperba2start, "Parabolic"
						if a2HzOnset = undefined
							a2HzOnset = 1234
						elsif a2HzOnset > 5000
							a2HzOnset = 4999
						endif
						a3Onset = Get maximum: lowerba3start, upperba3start, "Parabolic"
						if a3Onset = undefined
							a3Onset = 1234
						endif
					a3HzOnset = Get frequency of maximum: lowerba3start, upperba3start, "Parabolic"
						if a3HzOnset = undefined
							a3HzOnset = 1234
						elsif a3HzOnset > 5000
							a3HzOnset = 4999
						endif

					a3f1OnsetNorm = 10*log10((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos(2*pi*(f1Start/sampfreq))))))^2/((((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f1Start/sampfreq)))))))*(((exp(-pi*(bw1StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw1StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f1Start/sampfreq)))))))))
					a3f2OnsetNorm = 10*log10((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos(2*pi*(f2Start/sampfreq))))))^2/((((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f2Start/sampfreq)))))))*(((exp(-pi*(bw2StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw2StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f2Start/sampfreq)))))))))
					a3f3OnsetNorm = 10*log10((((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos(2*pi*(f3Start/sampfreq))))))^2/((((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))+(2*pi*(f3Start/sampfreq)))))))*(((exp(-pi*(bw3StartNorm/sampfreq)))^2)+(1-(2*(exp(-pi*(bw3StartNorm/sampfreq)))*(cos((2*pi*(f3Start/sampfreq))-(2*pi*(f3Start/sampfreq)))))))))
	
					a3OnsetNorm = a3Onset - (a3f1OnsetNorm+a3f2OnsetNorm+a3f3OnsetNorm)

						h1mnh2Onset = h1Onset - h2Onset
					h1mnh2OnsetNorm = h1OnsetNorm - h2OnsetNorm
						h1mna1Onset = h1Onset - a1Onset
					h1mna1OnsetNorm = h1OnsetNorm - a1Onset
						h1mna2Onset = h1Onset - a2Onset
					h1mna2OnsetNorm = h1OnsetNorm - a2Onset
						h1mna3Onset = h1Onset - a3Onset
					h1mna3OnsetNorm = h1OnsetNorm - a3OnsetNorm
					a1mna2Onset = a1Onset - a2Onset
					a1mna3Onset = a1Onset - a3Onset
					a1mna3OnsetNorm = a1Onset - a3OnsetNorm
					a2mna3Onset = a2Onset - a3Onset
					a2mna3OnsetNorm = a2Onset - a3OnsetNorm

					select spectrumFilteredOnset
					Write to binary file: directory1$ + "/spectra" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset" + ".Spectrum"

					#################################################################
					# Here we draw a spectrogram of a particular sound with the 	#
					# corresponding TextGrid and the formant tracking overlaid.	#
					#################################################################

					Erase all
						Font size: 14
						display_from = start - 0.05
							if display_from < 0
									display_from = 0
							endif
						display_until = end + 0.05
							if display_until > finishing_time
									display_until = finishing_time
							endif
						selectObject: "Spectrogram 'name$'"
						Viewport: 0, 7, 0, 4
						Paint, display_from, display_until, 0, 5000, 100, "yes", 50, 6, 0, "no"
						select 'formantTrack'
						Yellow
						Speckle: display_from, display_until, 5000, 30, "no"
						Marks left every: 1, 500, "yes", "yes", "yes"  
						Viewport: 0, 7, 0, 8
						selectObject: "TextGrid 'name$'"
						Black
						Draw: display_from, display_until, "no", "yes", "yes"
					One mark bottom: startPointFrame, "yes", "yes", "yes", ""
					Viewport: 0, 7, 0, 0.75
						
					Text top: "yes", "F1Start: 'f1Start:2' --- F2Start: 'f2Start:2' --- F3Start: 'f3Start:2'"
					Viewport: 0, 7, 0.25, 1
					Text top: "yes", "a1Start: 'a1HzOnset:2' --- a2Start: 'a2HzOnset:2' --- a3Start: 'a3HzOnset:2'"


					#########################################################################
					#      then we draw the spectrum of that same portion with LTAS and LPC #
					#      overlaid with indication of where things where measured!		#
					#########################################################################

					select spectrumFilteredOnset
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"
					select ltasFilteredOnset
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "no", "bars"
						Marks bottom every: 1, 500, "yes", "yes", "no"
						Marks bottom every: 1, 250, "no", "no", "yes"
						Line width: 1
						Text top: "yes", "Spectrum [40 ms], Ltas(1-to-1) [40 ms]"
					Black
					select spectrumFilteredOnset
						LPC smoothing: 5, 50
						Viewport: 0, 7, 8, 12
						Draw: 0, 5000, 0, 140, "yes"
						
					Viewport: 0, 7, 8, 12
						Font size: 14
						Line width: 3
					Red
						One mark top: h1HzOnset, "no", "yes", "yes", "H1"
						One mark top: h2HzOnset, "no", "yes", "yes", "H2"
						One mark top: a1HzOnset, "no", "yes", "yes", "A1"
					One mark top: a2HzOnset, "no", "yes", "yes", "A2"
						One mark top: a3HzOnset, "no", "yes", "yes", "A3"
					Line width: 4
					Blue
					One mark top: f1Start, "no", "yes", "yes", "F1"
					One mark top: f2Start, "no", "yes", "yes", "F2"
						One mark top: f3Start, "no", "yes", "yes", "F3"

					Select outer viewport: 0, 7, 0, 12
					Save as 300-dpi PNG file: directory1$ + "/images" + "/" +  "'name$'_'i'_'j'_'consonant$'_'label2$'_Onset" + ".png"

				endif

				select spectrumFilteredOnset
				plus ltasFilteredOnset
				plus soundSlice
				Remove

				h1Mid = 1234
				h1HzMid = 1234
				h1MidNorm = 1234
				h2Mid = 1234
				h2HzMid = 1234
				h2MidNorm = 1234
				a1Mid = 1234
				a1HzMid = 1234
				a2Mid = 1234
				a2HzMid = 1234
				a3Mid = 1234
				a3HzMid = 1234
				a3MidNorm = 1234
				h1mnh2Mid = 1234
				h1mnh2MidNorm = 1234
					h1mna1Mid = 1234
				h1mna1MidNorm = 1234
					h1mna2Mid = 1234
				h1mna2MidNorm = 1234
					h1mna3Mid = 1234
				h1mna3MidNorm = 1234
				a1mna2Mid = 1234
				a1mna3Mid = 1234
				a1mna3MidNorm = 1234
				a2mna3Mid = 1234
				a2mna3MidNorm = 1234

			else

				h1Onset = 1234
				h1HzOnset = 1234
				h1OnsetNorm = 1234
				h2Onset = 1234
				h2HzOnset = 1234
				h2OnsetNorm = 1234
				a1Onset = 1234
				a1HzOnset = 1234
				a2Onset = 1234
				a2HzOnset = 1234
				a3Onset = 1234
				a3HzOnset = 1234
				a3OnsetNorm = 1234
				h1mnh2Onset = 1234
				h1mnh2OnsetNorm = 1234
					h1mna1Onset = 1234
				h1mna1OnsetNorm = 1234
					h1mna2Onset = 1234
				h1mna2OnsetNorm = 1234
					h1mna3Onset = 1234
				h1mna3OnsetNorm = 1234
				a1mna2Onset = 1234
				a1mna3Onset = 1234
				a1mna3OnsetNorm = 1234
				a2mna3Onset = 1234
				a2mna3OnsetNorm = 1234
				h1Mid = 1234
				h1HzMid = 1234
				h1MidNorm = 1234
				h2Mid = 1234
				h2HzMid = 1234
				h2MidNorm = 1234
				a1Mid = 1234
				a1HzMid = 1234
				a2Mid = 1234
				a2HzMid = 1234
				a3Mid = 1234
				a3HzMid = 1234
				a3MidNorm = 1234
				h1mnh2Mid = 1234
				h1mnh2MidNorm = 1234
					h1mna1Mid = 1234
				h1mna1MidNorm = 1234
					h1mna2Mid = 1234
				h1mna2MidNorm = 1234
					h1mna3Mid = 1234
				h1mna3MidNorm = 1234
				a1mna2Mid = 1234
				a1mna3Mid = 1234
				a1mna3MidNorm = 1234
				a2mna3Mid = 1234
				a2mna3MidNorm = 1234
			endif

			appendFileLine: "'output1$'.xls", nameFile$, tab$, language$, tab$, speaker$, tab$, 
			... label2$, tab$, consonant$, tab$, label$, tab$, duration_ms, tab$, 
			... startPointFrame, tab$, midPointFrame, tab$, endPointFrame, tab$, f1Start, tab$, 
			... f2Start, tab$, f3Start, tab$, f1Mid, tab$, f2Mid, tab$, f3Mid, tab$, bw1Start, tab$,
			... bw2Start, tab$, bw3Start, tab$, bw1Mid, tab$, bw2Mid, tab$, bw3Mid, tab$, 
			... f0Start, tab$, f0Mid, tab$, intensityStart, tab$, intensityMid, tab$, 
			... h1mnh2Onset, tab$, h1mnh2OnsetNorm, tab$, h1mna1Onset, tab$, h1mna1OnsetNorm, tab$, 
			... h1mna2Onset, tab$, h1mna2OnsetNorm, tab$, h1mna3Onset, tab$, h1mna3OnsetNorm, tab$, 
			... a1mna2Onset, tab$, a1mna3Onset, tab$, a1mna3OnsetNorm, tab$, a2mna3Onset, tab$, 
			... a2mna3OnsetNorm, tab$, h1mnh2Mid, tab$, h1mnh2MidNorm, tab$, h1mna1Mid, tab$, 
			... h1mna1MidNorm, tab$, h1mna2Mid, tab$, h1mna2MidNorm, tab$, h1mna3Mid, tab$, 
			... h1mna3MidNorm, tab$, a1mna2Mid, tab$, a1mna3Mid, tab$, a1mna3MidNorm, tab$, 
			... a2mna3Mid, tab$, a2mna3MidNorm, tab$, h1Onset, tab$, h1HzOnset, tab$, 
			... h1OnsetNorm, tab$, h2Onset, tab$, h2HzOnset, tab$, h2OnsetNorm, tab$, a1Onset, tab$, 
			... a1HzOnset, tab$, a2Onset, tab$, a2HzOnset, tab$, a3Onset, tab$, a3HzOnset, tab$, 
			... a3OnsetNorm, tab$, h1Mid, tab$, h1HzMid, tab$, h1MidNorm, tab$, h2Mid, tab$, 
			... h2HzMid, tab$, h2MidNorm, tab$, a1Mid, tab$, a1HzMid, tab$, a2Mid, tab$, 
			... a2HzMid, tab$, a3Mid, tab$, a3HzMid, tab$, a3MidNorm


			endif
		endfor
select all
minus Strings fileList
Remove
endfor

echo Finished! Check the file 'output1$'.xls located in the 'directory1$'
