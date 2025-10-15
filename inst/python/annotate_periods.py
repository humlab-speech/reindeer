"""
annotate_periods.py

Python/Parselmouth implementation of period detection annotation.
Replaces the Praat script praat_periods.praat

Original Praat script: inst/praat/praat_periods.praat
Author: Fredrik Karlsson (conversion to Python)
"""

import parselmouth
from parselmouth.praat import call
import pandas as pd
import numpy as np
from pathlib import Path


def annotate_periods_single(
    sound_file,
    begin_time=0.0,
    end_time=0.0,
    time_step=0.005,
    minimum_f0=75.0,
    maximum_f0=600.0,
    window_shape="Gaussian1",
    interpolation="cubic",
    relative_width=1.0
):
    """
    Extract periods from a sound file and return timing/intensity information
    
    Parameters
    ----------
    sound_file : str
        Path to sound file
    begin_time : float
        Start time in seconds (0.0 = start of file)
    end_time : float
        End time in seconds (0.0 = end of file)
    time_step : float
        Time step for analysis (default: 0.005 s = 5 ms)
    minimum_f0 : float
        Minimum fundamental frequency (Hz)
    maximum_f0 : float
        Maximum fundamental frequency (Hz)
    window_shape : str
        Window shape for extraction ("Gaussian1", "rectangular", etc.)
    interpolation : str
        Interpolation method ("nearest", "linear", "cubic", "sinc70", "sinc700")
    relative_width : float
        Relative width of extraction window
        
    Returns
    -------
    pd.DataFrame
        DataFrame with columns: time (ms), intensity (dB)
    """
    
    # Load sound
    if not Path(sound_file).exists():
        raise FileNotFoundError(f"Sound file not found: {sound_file}")
    
    sound = parselmouth.Sound(sound_file)
    
    # Get duration and sample rate
    dur = sound.duration
    sr = sound.sampling_frequency
    
    # Handle time window extraction
    if (begin_time > 0.0 or end_time > 0.0) and (begin_time >= 0.0 and end_time <= dur):
        # Extract part of sound
        sound = call(sound, "Extract part", begin_time, end_time, 
                    window_shape, relative_width, True)
    
    # Convert to intensity
    intensity = call(sound, "To Intensity", minimum_f0, 0.0, True)
    
    # Extract periodic peaks
    point_process = call(sound, "To PointProcess (periodic, peaks)", 
                        minimum_f0, maximum_f0, True, False)
    
    # Get number of points
    n_points = call(point_process, "Get number of points")
    
    # Build output table
    times = []
    intensities = []
    
    for i in range(1, n_points + 1):
        curr_time = call(point_process, "Get time from index", i)
        
        # Get intensity at this time with specified interpolation
        try:
            curr_int = call(intensity, "Get value at time", curr_time, interpolation)
        except Exception:
            curr_int = np.nan
        
        times.append(curr_time * 1000)  # Convert to ms
        intensities.append(curr_int)
    
    # Create output dataframe
    result = pd.DataFrame({
        'time': times,
        'intensity': intensities
    })
    
    return result


def annotate_periods_batch(
    sound_files,
    session_names,
    bundle_names,
    begin_time=0.0,
    end_time=0.0,
    time_step=0.005,
    minimum_f0=75.0,
    maximum_f0=600.0,
    window_shape="Gaussian1",
    interpolation="cubic",
    relative_width=1.0
):
    """
    Process multiple sound files for period annotation
    
    Parameters
    ----------
    sound_files : list
        List of sound file paths
    session_names : list
        List of session names corresponding to each file
    bundle_names : list
        List of bundle names corresponding to each file
    (other parameters as in annotate_periods_single)
    
    Returns
    -------
    pd.DataFrame
        Combined DataFrame with session, bundle, time, intensity columns
    """
    
    all_results = []
    
    for sound_file, session, bundle in zip(sound_files, session_names, bundle_names):
        try:
            result = annotate_periods_single(
                sound_file=sound_file,
                begin_time=begin_time,
                end_time=end_time,
                time_step=time_step,
                minimum_f0=minimum_f0,
                maximum_f0=maximum_f0,
                window_shape=window_shape,
                interpolation=interpolation,
                relative_width=relative_width
            )
            
            # Add session and bundle info
            result['session'] = session
            result['bundle'] = bundle
            
            all_results.append(result)
            
        except Exception as e:
            print(f"Error processing {sound_file}: {e}")
            continue
    
    # Combine all results
    if all_results:
        combined = pd.concat(all_results, ignore_index=True)
        # Reorder columns
        combined = combined[['session', 'bundle', 'time', 'intensity']]
        return combined
    else:
        return pd.DataFrame(columns=['session', 'bundle', 'time', 'intensity'])


if __name__ == "__main__":
    # Test code
    import sys
    if len(sys.argv) > 1:
        result = annotate_periods_single(sys.argv[1])
        print(result)
