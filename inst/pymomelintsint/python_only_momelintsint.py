# momelintsint.py

from pathlib import Path
import parselmouth
import math
import numpy
import platform
import subprocess




# A Python Parselmouth reimplementation of 
#Praat script automatic_min_max_fo.praat
#
#version:   2016-11-21
#author     Daniel Hirst
#email: daniel.hirst@lpl-aix.fr
#
#purpose: calculate fo using two passes with automatic estimation of optimal max and min
#       min fo is 0.75 * 1st quartile of fo distribution
#       max fo is min fo * max pitch span
#       values rounded down/up to nearest 10.
# Version History
# [2016-11-21]      added  variable pitch-span to control range
# [2011-02-16]      max_pitch raised to 2.5*q75 to allow for expressive speech (was 1.5*q75)
# [2008-07-11]
# 
# Reimplementation by 
#   Fredrik Nylén 

def intsint_linear (x):
    return pow(2,x)

def intsint_octave(x):
    return math.log(x)/math.log(2)

def intsint_round(x):
    return int(x + 0.5)

def getSound(file,beginTime=0.0 ,endTime=0.0 ):
    name = Path(file).stem
    if (beginTime <= 0.0 and endTime <= 0.0):
        sound = parselmouth.Sound(file)
    else:
        beginTime = max(beginTime,0.0) 
        endTime = max(endTime,0.0)
        ls = parselmouth.praat.call("Open long sound file","~/Desktop/input/test.wav")
        sound = parselmouth.praat.call(ls,"Extract part",beginTime,endTime)
    parselmouth.praat.call(sound,"Rename",name)
    return sound


def automatic_min_max_fo(sound,maximum_pitch_span=1.5, minimum_fo=60, maximum_fo=750): 
    firstPass = sound.to_pitch(0.01,minimum_fo,maximum_fo)
    q25 = parselmouth.praat.call(firstPass, "Get quantile", 0, 0 , 0.25, "Hertz")
    min_fo = math.floor(q25 * 0.75/10)*10
    max_fo = math.ceil((min_fo * pow(2,maximum_pitch_span))/10)*10
    secondPass = sound.to_pitch(0.01,min_fo,max_fo)
    return secondPass



def momel(pitchobj,window_length=30, minimum_fo=60, maximum_fo=750, maximum_error=1.04,reduced_window_length=20, minimal_distance=5, minimal_frequency_ratio=0.05):
    #momel_parameters=["30","60","750","1.04","20","5","0.05"]):
    momel_parameters=[window_length, minimum_fo, maximum_fo, maximum_error,reduced_window_length, minimal_distance, minimal_frequency_ratio]
    momel_parameters_str = ["%.2f" % x for x in momel_parameters]
    if platform.system() == "Darwin":
        momel_exec = "./momel_osx_intel"
    else:
        momel_exec = "./momel_linux_intel"
    pitchvalues = parselmouth.praat.call(pitchobj,"List values in all frames","Hertz")
    numpy.nan_to_num(pitchvalues,nan=0.0,copy=False)
    pitchvalues_str = "\n".join(pitchvalues.astype(str))
    process = subprocess.Popen([momel_exec] + momel_parameters_str, stdout=subprocess.PIPE, stdin=subprocess.PIPE,text=True)
    stdout, stderr = process.communicate(input=pitchvalues_str)
    momel_pitch_pairs = stdout.split("\n")
    momel_pitch_values = [tuple(x.split(" ")) for x in momel_pitch_pairs if x != '']
    momelPitchTier = parselmouth.praat.call("Create PitchTier",pitchobj.name,0.0,float(pit.duration))
    for x in momel_pitch_values:
        # In the Praat plugin code, this is done in the perl command, but he logic is moved here since it makes more sense
        # and is more convenient to check at insert time than later.
        pitchValue = min(max(float(x[1]),float(minimum_fo)), float(maximum_fo)) 
        timeValue = float(x[0])/1000
        parselmouth.praat.call(momelPitchTier,"Add point", timeValue, pitchValue)
    return (momelPitchTier,momel_pitch_values)

def code_with_intsint(pitchTierObj, min_fo=60,max_fo=600,momel_tier="Momel",intsint_tier="Intsint",min_pause=0.5,min_range=0.5, max_range=2.5, step_range=0.1, mean_shift=50, step_shift=1, higher=0.5, lower=0.5, up=0.25,down=0.25):
    big_number=9999
    epsilon_time = 0.0001
    nPoints = parselmouth.praat.call(pitchTierObj,"Get number of points")
    mean_fo = parselmouth.praat.call(pitchTierObj,"Get mean (points)",0.0,0.0)
    linear_mean_fo = intsint_round(intsint_linear(mean_fo))
    min_mean = linear_mean_fo - mean_shift
    max_mean = linear_mean_fo + mean_shift
    min_ss_error = big_number
    last_estimate = 0
    tout = [(parselmouth.praat.call(pitchTierObj,"Get time from index",i),parselmouth.praat.call(pitchTierObj,"Get value at index",i)) for i in range(1,nPoints)]
    t, fo = zip(*tout)
    t = list(t) # The perl code converted times to second, but Praat already works in / returns seconds
    fo = list(fo)
    intsint = [None] * nPoints
    best_range = None
    best_mid = None
    best_intsint = None 
    best_estimate = [0] * nPoints
    for r in [x / 10.0 for x in range(int(min_range*10), int(max_range*10), int(step_range*10))]:
        for lm in [x / 10.0 for x in range(int(min_mean*10), int(max_mean*10), int(step_shift*10))]:
            mid = intsint_octave(lm)
            intsint, min_ss_error, last_estimate, best_range, best_mid, best_intsint, best_estimate = optimise(mid, r, t, fo , intsint, big_number,min_pause, min_ss_error, last_estimate, best_range, best_mid, best_intsint, best_estimate)
    mid = best_mid
    key = intsint_linear(mid)
    bottom = mid - best_range / 2
    top = mid + best_range / 2

    for i in range(nPoints):
        currfo = intsint_round(intsint_linear(fo[i]) )
        currestimate = intsint_round(intsint_linear(float(best_estimate[i])) )
        print(f"{t[i]} {best_intsint[i]} {currfo} {currestimate}")


def optimise(mid, r, t, fo , intsint, big_number,min_pause,min_ss_error, last_estimate, best_range, best_mid, best_intsint,best_estimate):
    
    list_tones = ["M", "S", "T",  "B", "H", "L", "U", "D"];
    higher = 0.5
    lower = 0.5
    up = 0.25
    down = 0.25
    top = mid + r / 2
    bottom = mid - r / 2
    firstfo = fo[0]
    nval = len(fo)
    estimates = [None] * nval
    
    if top - firstfo < abs(firstfo - mid):
        intsint[0] = "T"
    elif firstfo - bottom < abs(firstfo - mid):
        intsint[0] = "B"
    else:
        intsint[0] = "M"

    est = estimate(intsint[0], last_estimate,mid, top, bottom, higher, up, lower, down)
    estimates[0] = est
    error = abs(est - firstfo)
    ss_error = error * error
    last_estimate = est

    for i in range(nval):
        target = fo[i]

        # efter paus välj från [MTB]
        if t[i] - t[i - 1] > min_pause:
            if top - target < abs(target - mid):
                intsint[i] = "T"
            elif target - bottom < abs(target - mid):
                intsint[i] = "B"
            else:
                intsint[i] = "M"

        # någon annanstans någon ton utom M
        else:
            min_difference = big_number
            best_tone = ""
            for tone in list_tones:
                if tone != "M":
                    estimates[i] = estimate(tone, last_estimate,mid, top, bottom, higher, up, lower, down)
                    difference = abs(target - estimates[i])
                    if difference < min_difference:
                        min_difference = difference
                        best_tone = tone

            intsint[i] = best_tone

        estimates[i] = estimate(intsint[i], last_estimate,mid, top, bottom, higher, up, lower, down)
        error = abs(estimates[i] - fo[i])
        ss_error += error * error
        last_estimate = estimates[i]

    if ss_error < min_ss_error:
        min_ss_error = ss_error
        best_range = r
        best_mid = mid
        best_intsint = intsint
        best_estimate = estimates
    return (intsint, min_ss_error, last_estimate, best_range, best_mid, best_intsint, best_estimate)


def estimate(tone, last_target,mid, top, bottom, higher, up, lower, down):

    if tone == "M":
        estimate = mid
    elif tone == "S":
        estimate = last_target
    elif tone == "T":
        estimate = top
    elif tone == "H":
        estimate = last_target + (top - last_target) * higher
    elif tone == "U":
        estimate = last_target + (top - last_target) * up
    elif tone == "B":
        estimate = bottom
    elif tone == "L":
        estimate = last_target - (last_target - bottom) * lower
    elif tone == "D":
        estimate = last_target - (last_target - bottom) * down

    return estimate




snd = getSound("/Users/frkkan96/Documents/forskning/Parkinson/dysprosody_assessed_by_machines/Data/pyannote/MI/403_1585574_P34_mon_186_96_e509114fcf92715f0a2eaf32aca9dcd6/403_1585574_P34_mon_186_96_e509114fcf92715f0a2eaf32aca9dcd6.wav ")
pit = automatic_min_max_fo(snd)
po, pv = momel(pit)
co = code_with_intsint(po)
