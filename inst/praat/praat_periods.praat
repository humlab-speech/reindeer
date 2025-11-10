form Compute
sentence SoundFile /Users/frkkan96/Desktop/kaa_yw_pb.wav
real BeginTime 0.0
real EndTime 0.0
real Time_step 0.005
real Minimum_f0 75.0
real Maximum_f0 600.0
word WindowShape Gaussian1
word Interpolation cubic
real RelativeWidth 1.0
sentence TrackOut /Users/frkkan96/Desktop/kaa_yw_pb.per
endform


if fileReadable(soundFile$)
	sound = Read from file... 'soundFile$'
else
	exitScript("Could not read file 'soundFile$'")
endif

selectObject: sound
dur = Get total duration
sr = Get sampling frequency

# Check that start or end times should be condidered, and that they are within
# ok limits.
if  ( beginTime > 0.0 or endTime > 0.0 ) and (beginTime >= 0.0 and endTime <= dur)

	selectObject: sound
	# Preserve times so that start end end record may be computed later
	soundPart = Extract part: beginTime, endTime, windowShape$, relativeWidth, 1
	selectObject: sound
	Remove
	sound = soundPart
endif

outTab = Create Table with column names: "outTable", 0, "Time Intensity"

selectObject: sound
noprogress To Intensity: minimum_f0, 0.0, 1
int = selected ("Intensity")
selectObject: sound
noprogress To PointProcess (periodic, peaks): minimum_f0, maximum_f0, 1, 0
pp = selected ("PointProcess")

noPoints = Get number of points

for point from 1 to noPoints
	select pp
	currTime = Get time from index: point
	select int
	currInt = Get value at time: currTime, interpolation$

	selectObject: outTab
	Append row
	row = Get number of rows
	Set numeric value: row, "Time", currTime
	Set numeric value: row, "Intensity", currInt


endfor

select outTab
Save as comma-separated file: trackOut$
Remove
