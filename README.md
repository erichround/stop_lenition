# Acoustic analysis of stop phonemes

This repository contains R code for automated acoustic analysis of stop phonemes, including lenited variants, as described in:

Ennever, Thomas, Felicity Meakins, & Erich R. Round. (2017). A replicable acoustic measure of lenition and the nature of variability in Gurindji stops. Laboratory Phonology: Journal of the Association for Laboratory Phonology, 8(1), 20. DOI: http://doi.org/10.5334/labphon.18

Please cite that work if you use the code.

# Functionality

Obtain replicable measures of:
- stop duration
- overall change in intensity
- peak absolute intensity velocity (i.e., first derivative of intensity wrt time)
- in user-specified spectral bands

Inputs:
- Audio recordings in .WAV format
- Stop tokens, each annotated by a single point in a Praat TextGrid

Outputs:
- CSV file of measures
- TextGrid augmented with a tier delimiting the stops

See the [wiki](https://github.com/erichround/stop_lenition/wiki) for details.

# Updates

## 18 July 2022

A bug was identified which could cause an R error "Error in identify_events(velocity_data) : object 'i_peak_location' not found", which would halt the script. Note, it could not introduce any errors into the restuls per se, so it will not have affected any research results. The bug could arise in the unlucky case that the usual intensity landmarks could not be found for the very first stop analysed, i.e., the first stop in the first textGrid.
