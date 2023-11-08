
————————————————————————————————————————Data-————————————————————————————————
File: et2_for_drew (included as .csv and .RData and as .dta)

Contains processed data for each subject and trial. Does not include information about fixations, only the outcome of each trial.

N = 39
Choice trials with no item fixations for more than 40 ms at the beginning or end of the trial are excluded

subject 	= subject ID
trial		= trial number for subject
leftrating 	= rating for food item presented on the left
rightrating	= rating for food item presented on the right
rt		= reaction time for the trial (milliseconds)
choice		= item the subject selected on this trial, 1 for left, 0 for right
correct		= 1 if they pick the item with the higher rating, 0 otherwise (0 always if ratings are equal)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Files: fixations_final (included as .csv, and .dta) 

These two datasets are essentially identical.

Contains processed data for each subject and each trial of the experiment. Each line represents a new fixation.

N = 39
Choice trials with no item fixations for more than 40 ms at the beginning or end of the trial are excluded. Blank fixations are already accounted for as specified in the paper.

subject			= subject ID
trial			= trial number for subject
fix_num			= fixation number for trial
event_duration		= duration of the current fixation (milliseconds)
leftrating		= rating for food item presented on the left
rightrating		= rating for food item presented on the right
rt			= reaction time for the trial given by the software (milliseconds)
choice			= item the subject selected on this trial, 1 for left, 0 for right
roi 			= dummy variable for if current fixation is on left or right item (1 = left, 2 = right)
computed_rt		= total fixation duration (i.e. reaction time) for the trial computed by summing the event_durations for the trial, including blank fixations (milliseconds)
num_fixation		= total number of fixations in that trial
rev_fix_num		= fixation number for trial, numbering backwards
temp			= for a subject’s nth fixation, the average fixation duration of the nth fixation over all the subject’s trials
corr_fix_duration 	= corrected durations for 1st fixations with duration less than 150 ms by changing them to the average of the subject’s 1st fixations



—————————————————————————Experimental Task-————————————————————————————————
Stimulus set folder: stimuli krajbich nat neu 2009 food images
