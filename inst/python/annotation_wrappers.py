"""
annotation_wrappers.py

Python/Parselmouth implementations of automatic annotation functions for Reindeer.
This module provides memory-only implementations that return pandas DataFrames
for integration with the R transcription system.

All functions are designed to be thread-safe for parallel processing.
"""

import parselmouth
from parselmouth.praat import call
import pandas as pd
import numpy as np
from pathlib import Path
import sys
import os

# Add pymomelintsint to path if needed
pymomel_path = Path(__file__).parent.parent / "pymomelintsint"
if pymomel_path.exists():
    sys.path.insert(0, str(pymomel_path))

# Lazy import of momelintsint to avoid import-time execution issues
HAS_MOMEL = False
_momel_module = None

def _get_momel_module():
    """Lazy loader for momelintsint module"""
    global _momel_module, HAS_MOMEL
    if _momel_module is None:
        try:
            import momelintsint
            _momel_module = momelintsint
            HAS_MOMEL = True
        except Exception as e:
            print(f"Warning: momelintsint module not available: {e}")
            HAS_MOMEL = False
    return _momel_module


# ==============================================================================
# PERIOD ANNOTATION
# ==============================================================================

def annotate_periods_single(
    sound_file,
    begin_time=0.0,
    end_time=0.0,
    minimum_f0=75.0,
    maximum_f0=600.0,
    window_shape="Gaussian1",
    interpolation="cubic",
    relative_width=1.0
):
    """
    Extract periods from a sound file using Parselmouth.
    
    Equivalent to praat_periods.praat
    
    Parameters
    ----------
    sound_file : str
        Path to sound file
    begin_time : float
        Start time in seconds (0.0 = entire file)
    end_time : float
        End time in seconds (0.0 = entire file)
    minimum_f0 : float
        Minimum F0 for period detection (Hz)
    maximum_f0 : float
        Maximum F0 for period detection (Hz)
    window_shape : str
        Window shape ("Gaussian1", "rectangular", etc.)
    interpolation : str
        Intensity interpolation method ("nearest", "linear", "cubic", "sinc70", "sinc700")
    relative_width : float
        Relative width of extraction window
        
    Returns
    -------
    pd.DataFrame
        DataFrame with columns: start_time (ms), end_time (ms), label
        where label = intensity value as string, end_time = start_time (EVENT)
    """
    
    # Load sound
    sound = parselmouth.Sound(sound_file)
    dur = sound.duration
    
    # Handle time window extraction
    if (begin_time > 0.0 or end_time > 0.0) and (begin_time >= 0.0 and end_time <= dur):
        sound = call(sound, "Extract part", begin_time, end_time, 
                    window_shape, relative_width, True)
    
    # Convert to intensity
    intensity = call(sound, "To Intensity", minimum_f0, 0.0, True)
    
    # Extract periodic peaks
    point_process = call(sound, "To PointProcess (periodic, peaks)", 
                        minimum_f0, maximum_f0, True, False)
    
    # Get number of points
    n_points = call(point_process, "Get number of points")
    
    # Build output
    results = []
    for i in range(1, n_points + 1):
        curr_time = call(point_process, "Get time from index", i)
        
        try:
            curr_int = call(intensity, "Get value at time", curr_time, interpolation)
            label = f"{curr_int:.2f}" if not np.isnan(curr_int) else ""
        except Exception:
            label = ""
        
        time_ms = curr_time * 1000
        results.append({
            'start_time': time_ms,
            'end_time': time_ms,  # EVENT type - start and end are same
            'label': label
        })
    
    return pd.DataFrame(results)


# ==============================================================================
# MOMEL/INTSINT ANNOTATION
# ==============================================================================

def annotate_intsint_momel_single(
    sound_file,
    window_length=30,
    minimum_f0=60,
    maximum_f0=750,
    pitch_span=1.5,
    maximum_error=1.04,
    reduced_window_length=20,
    minimal_distance=20,
    minimal_frequency_ratio=0.05
):
    """
    Annotate INTSINT tones using MOMEL/INTSINT via Parselmouth.
    
    Equivalent to processINTSINTMOMEL.praat
    
    Parameters
    ----------
    sound_file : str
        Path to sound file
    window_length : int
        Analysis window length (ms)
    minimum_f0 : int
        Minimum F0 (Hz)
    maximum_f0 : int
        Maximum F0 (Hz)
    pitch_span : float
        Pitch span in octaves (1.5 = normal, 2.5 = expressive)
    maximum_error : float
        Maximum error for MOMEL
    reduced_window_length : int
        Reduced analysis window (ms)
    minimal_distance : int
        Minimal distance between targets (ms)
    minimal_frequency_ratio : float
        Minimal frequency ratio
        
    Returns
    -------
    pd.DataFrame
        DataFrame with columns: start_time (ms), end_time (ms), label
        where label = INTSINT tone (T, M, B, H, U, S, D, L)
    """
    
    momel_mod = _get_momel_module()
    if momel_mod is None:
        raise RuntimeError("MOMEL/INTSINT module not available")
    
    # Load sound
    sound = parselmouth.Sound(sound_file)
    
    # Automatic F0 range estimation
    pitch, min_f0, max_f0 = momel_mod.automatic_min_max_fo(
        sound, 
        maximum_pitch_span=pitch_span,
        minimum_fo=minimum_f0,
        maximum_fo=maximum_f0
    )
    
    # Run MOMEL
    momel_pitch_tier, momel_values = momel_mod.momel(
        pitch,
        window_length=window_length,
        minimum_fo=min_f0,
        maximum_fo=max_f0,
        maximum_error=maximum_error,
        reduced_window_length=reduced_window_length,
        minimal_distance=minimal_distance,
        minimal_frequency_ratio=minimal_frequency_ratio
    )
    
    # Code with INTSINT
    textgrid, nvalues, pmean, orlow, orhigh, orstep, omlow, omhigh, omstep, prange, key = momel_mod.code_with_intsint(
        sound, momel_pitch_tier
    )
    
    # Extract INTSINT tier (tier 2)
    intsint_tier = call(textgrid, "Extract tier", 2)
    n_points = call(intsint_tier, "Get number of points")
    
    results = []
    for i in range(1, n_points + 1):
        time = call(intsint_tier, "Get time from index", i) * 1000  # Convert to ms
        label = call(intsint_tier, "Get label of point", i)
        
        results.append({
            'start_time': time,
            'end_time': time,  # EVENT type
            'label': label
        })
    
    return pd.DataFrame(results)


# ==============================================================================
# BATCH PROCESSING WRAPPERS
# ==============================================================================

def process_annotation_batch(
    annotation_func,
    sound_files,
    session_names,
    bundle_names,
    **kwargs
):
    """
    Process multiple files with an annotation function.
    
    Thread-safe for parallel processing - each file is processed independently.
    
    Parameters
    ----------
    annotation_func : callable
        Single-file annotation function (e.g., annotate_periods_single)
    sound_files : list
        List of sound file paths
    session_names : list
        Corresponding session names
    bundle_names : list
        Corresponding bundle names
    **kwargs
        Additional arguments passed to annotation_func
        
    Returns
    -------
    pd.DataFrame
        Combined results with session and bundle columns added
    """
    
    all_results = []
    
    for sound_file, session, bundle in zip(sound_files, session_names, bundle_names):
        try:
            result = annotation_func(sound_file, **kwargs)
            result['session'] = session
            result['bundle'] = bundle
            all_results.append(result)
        except Exception as e:
            print(f"Error processing {sound_file}: {e}", file=sys.stderr)
            continue
    
    if all_results:
        combined = pd.concat(all_results, ignore_index=True)
        # Reorder columns
        cols = ['session', 'bundle', 'start_time', 'end_time', 'label']
        return combined[cols]
    else:
        return pd.DataFrame(columns=['session', 'bundle', 'start_time', 'end_time', 'label'])


# ==============================================================================
# CONVENIENCE FUNCTIONS FOR R INTEGRATION
# ==============================================================================

def periods_batch(sound_files, session_names, bundle_names, **kwargs):
    """Batch process period annotations"""
    return process_annotation_batch(
        annotate_periods_single,
        sound_files,
        session_names,
        bundle_names,
        **kwargs
    )


def intsint_momel_batch(sound_files, session_names, bundle_names, **kwargs):
    """Batch process INTSINT/MOMEL annotations"""
    return process_annotation_batch(
        annotate_intsint_momel_single,
        sound_files,
        session_names,
        bundle_names,
        **kwargs
    )


# ==============================================================================
# THREAD SAFETY NOTES
# ==============================================================================
"""
Thread Safety Analysis:

1. Parselmouth objects are created independently for each file
2. No shared state between function calls
3. Each sound file is processed in isolation
4. NumPy/Pandas operations are thread-safe for read-only data

Parallel processing is SAFE when:
- Each call processes a different sound file
- No shared Parselmouth objects across threads
- Results are collected after all threads complete

Recommended usage:
- Use multiprocessing (not threading) for true parallelism in Python
- Or call from R using parallel::mclapply or future::future_map
- Parselmouth's internal state is per-object, not global
"""


if __name__ == "__main__":
    # Basic tests
    import sys
    if len(sys.argv) > 1:
        test_file = sys.argv[1]
        
        print("Testing period annotation...")
        periods = annotate_periods_single(test_file)
        print(periods.head())
        
        if HAS_MOMEL:
            print("\nTesting INTSINT/MOMEL annotation...")
            intsint = annotate_intsint_momel_single(test_file)
            print(intsint.head())
