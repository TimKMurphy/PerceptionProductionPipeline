# TITLE:	Praat's Periodic Power (PPP)
# INPUT:	Audio files (preferably at least 44.1kHz sample-rate and 16bit PCM).
# OUTPUT:	Pitch objects, pitch tiers and intensity tiers.
# NOTES:	This script creates the corresponding Praat objects (see "output") for 
#		files in the audio directory ("input"). The extracted parameters in 
#		pitch objects and intensity tiers are used in the R code to yield 
#		periodic energy curves. The pitch tier object is used for standard 
#		F0 information, which can be tweaked in fields of this script's form, 
#		and can be manually corrected when the "inspect" option is switched on.
#IMPORTANT:	Before running the script, make sure to replace the 'xxx' in the 
#		directories with your relevant directory string (either in the script 
#		or in the prompted form). Also, make sure that your audio files are in 
#		the "Audio" directory.
# AUTHORS:	Aviad Albert and Francesco Cangemi {a.albert / fcangemi} @uni-koeln.de
# Further modifications by Tim Murphy, 2020

####### Input form
form Input parameters
        comment Replace "xxx" with your diretories info. Note your platform's syntax:
        comment PC directories are often "C:\...\"; Mac directories are often "/Users/.../".
        comment Do not forget the final slash!
	comment Using mac? (check if yes)
	boolean mac 0 
	sentence MainPipelineDirectory C:\Users\anarc\Documents\PerceptionProductionStudy\data_pipeline
	sentence ParticipantID 2b3a0
        comment Manually inspect F0 for corrections?
        boolean inspect 0 
        comment F0 path finder settings (adjustable).
        real silenceThr 0.03
        real voicingThr 0.45
        real octave 0.01
        real octavejump 0.35
        real voiceunvoiced 0.14
        integer pitchmax 400
        comment F0 smoothing bandwidth (Hz).
        integer smooth 10
endform
Erase all

clearinfo

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
outDirIntensityTier$ ="'MainPipelineDirectory$''os_slash$'output'os_slash$'intensity_tiers'os_slash$'"
outDirMeanPitch$ ="'MainPipelineDirectory$''os_slash$'output'os_slash$'mean_pitch'os_slash$'"


## list files
Create Strings as file list: "soundFileObj", "'inDirAudio$'*.weba"
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
writeFileLine: outputCSV$, "filename, meanF0, initialF0, tmin, tmax"
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
		initial$ = Get value at index... 1
		appendInfoLine: outputCSV$, name_prefix$, "," , mean$, "," , initial$, ",", tmin, ",", tmax
		Save as short text file: "'outDirPitchTier$''name_prefix$'.PitchTier"
		
## finish and clear  
  select all
    minusObject: "Strings soundFileObj"
    Remove

appendFile: outputCSV$, info$ ()
endfor
select all
Remove
