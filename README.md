# Acoustic analysis of stop phonemes

This repository contains R code for acoustic analysis of stop phonemes, including lenited variants, as described in:

Thomas Ennever, Felicity Meakins & Erich R. Round (submitted) An acoustic measure of lenition, and Gurindji stops

# Functionality

Obtain replicable measures of:
- stop duration
- overall change in intensity
- intensity vecolity (i.e., 1st derivative of intensity wrt time)
- in user-specified spectral bands

Inputs:
- Audio recordings in .WAV format
- Stop tokens, each annotated by a single point in an Praat TextGrid

Outputs:
- CSV file of measures
- TextGrid augmented with a tier delimiting the stops

See the wiki for details
