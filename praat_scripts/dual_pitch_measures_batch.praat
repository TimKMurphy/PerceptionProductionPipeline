form Dual measures
	sentence MainPipelineDirectory C:\Users\anarc\Documents\PerceptionProductionStudy\data_pipeline
	sentence ParticipantID 2b3a0
endform
Erase all
clearinfo

mac = 0 
inspect = 0 
silenceThr = 0.03
voicingThr = 0.45
octave = 0.01
octavejump = 0.35
voiceunvoiced = 0.14
pitchmax = 400
#F0 smoothing bandwidth (Hz).
smooth = 10




####### Settings
## create directory paths
os_slash$ = "\"
if mac
	os_slash$="/"
endif

outputCSV$= "'inDirAudio$''ParticipantID$'_mean_pitch.csv"

inDirAudio$ = "'MainPipelineDirectory$''os_slash$'raw'os_slash$''ParticipantID$''os_slash$'"
outDirPitchObject$ ="'MainPipelineDirectory$''os_slash$'output'os_slash$'pitch_objects'os_slash$'"
outDirPitchTier$ = "'MainPipelineDirectory$''os_slash$'output'os_slash$'pitch_tiers'os_slash$'"
#outDirIntensityTier$ ="'MainPipelineDirectory$''os_slash$'output'os_slash$'intensity_tiers'os_slash$'"
outDirMeanPitch$ ="'MainPipelineDirectory$''os_slash$'output'os_slash$'mean_pitch'os_slash$'"


## list files
Create Strings as file list: "soundFileObj",  "'inDirAudio$'*.weba"
number_of_files = Get number of strings
for i from 1 to number_of_files
	selectObject: "Strings soundFileObj"
	current_file$ = Get string: 'i'
	name_prefix$ = current_file$ - ".weba"

## create intensity tiers
	Read from file: "'inDirAudio$''current_file$'"
#		To Intensity: 40, 0.001, "yes"
#		Down to IntensityTier
#		Save as short text file: "'outDirIntensityTier$''name_prefix$'.IntensityTier"
#		Remove
#		selectObject: "Intensity 'name_prefix$'"
#		Remove

## create pitch object and tier (manually inspect files if selected)
## create csv file
outputCSV$= "'outDirMeanPitch$''ParticipantID$'_mean_pitch.csv"
writeFileLine: outputCSV$, "filename, f01, f02, f03, f04, f05, tmin, tmax"
if inspect = 1
	selectObject: "Sound 'name_prefix$'"
		View & Edit
		To Pitch (ac): 0.001, 40, 15, "yes", silenceThr,
           ...voicingThr, octave, octavejump, voiceunvoiced, pitchmax
		View & Edit
		pause Confirm
elsif inspect = 0
	selectObject: "Sound 'name_prefix$'"
		To Pitch (ac): 0.001, 40, 15, "yes", silenceThr,
           ...voicingThr, octave, octavejump, voiceunvoiced, pitchmax
endif
		Save as short text file: "'outDirPitchObject$''name_prefix$'.Pitch"
		To PointProcess
		To TextGrid (vuv)... 0.02 0.01
		Save as short text file: "'outDirPitchObject$''name_prefix$'.TEXTGRID"
		tmin = Get start time of interval: 1, 2
		tmax = Get end time of interval: 1, 2
		#Smooth: smooth
	selectObject: "Pitch 'name_prefix$'"
		Down to PitchTier
		mean$ = Get mean (curve)... tmin tmax
		one$ = Get value at index... 1
		two$ = Get value at index... 2
		three$ = Get value at index... 3
		four$ = Get value at index... 4
		five$ = Get value at index... 5
		appendInfoLine: outputCSV$, name_prefix$, "," , one$, "," , two$, "," , three$, "," , four$, "," , five$, "," , tmin, ",", tmax
		Save as short text file: "'outDirPitchTier$''name_prefix$'.PitchTier"
		
## finish and clear  
  select all
    minusObject: "Strings soundFileObj"
    Remove

appendFile: outputCSV$, info$ ()
endfor
select all
Remove
