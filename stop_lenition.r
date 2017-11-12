
###########################################
# Analysis of lenition using intensity data
###########################################
# By Erich Round
# Created 2015
# Last updated 2017-11-13

  library(stringr)
  library(dplyr)
  library(stats)
  library(tidyr)

################################
### Default parameter values ###
################################

# Change the values between the braces in order to alter
# the default settings

default_smoothing_param = function() { 0.7 }
default_threshold_ratio = function() { 0.6 }
default_intensity_analysis_window = function() { 0.01 }

# This is the one band suggested for analysing stops:
default_band_floors = function() { 400 }
default_band_ceilings = function() { 1200 }

# This is the full set of bands compared in the Gurindji paper:
# default_band_floors = function() { c(0,0,300,400,400,600,1000,1200,3200) }
# default_band_ceilings = function() { c(300,400,1000,1000,1200,1400,3200,3200,10000) }

# This is the directory in which Praat is installed. Set the default
# to NULL in order to have the script choose an operating-system-sensitive
# default; or override it to suit your own computer set-up
default_Praat_path = function() { NULL }


######################################################
######################################################
### Execute this command to process & analyse the data
######################################################
######################################################

analyse_stops = function(overwrite = FALSE, skip_bandpassing = FALSE) {
  # This gets you analysis that's arranged "bandwise", i.e., it's been run
  # for each segment token in each of your frequency bands.
  #
  # By default the spar parameter for cubic splines is set at 0.7
  # the intensity velocity threshold ration at 0.6, as used in
  # our Gurindji paper.

  if (!skip_bandpassing) {
  	cat("Extracting phoneme metadata from Praat origin TextGrids\n")
  	assemble_phoneme_metadata_from_Praat_origin_TextGrids()

  	cat("Bandpassing sound data\n")
  	extract_band_intesities(overwrite = overwrite)
  }

	cat("Assembling raw intensity data\n")
	raw_intensity_data <- assemble_raw_intensity_data()

	cat("Calculating intensity velocity\n")
	velocity_data <- add_velocity_data(raw_intensity_data)

	cat("Locating events\n")
	events_data <- identify_events(velocity_data)

	cat("Saving complete data to RData file\n")
	save(raw_intensity_data, events_data, file = "Intensity_event_data.RData")

	cat("Exporting Praat TextGrids showing demarcated stops\n")
	export_praat_textgrids(raw_intensity_data)

	cat("Saving summary data to CSV file\n")
	export_summary_to_csv(events_data)

	cat("Reporting on stops which could not be demarcated\n")
	report_failures_to_demarcate(events_data)
}

######################################################
######################################################



##################################
###### Mid-level functions #######
##################################

### Data compilation ###

assemble_phoneme_metadata_from_Praat_origin_TextGrids = function(
  file_names = NULL,
  output_metadata_file = "stop_origins.csv") {

	if (is.null(file_names)) { file_names <- origin_files_list() }
  file_name_bases <- file_names %>% str_replace("_origins.TextGrid", "")
  file_paths <- file.path(sound_files_path(), file_names)
  nfiles <- length(file_names)
  output_df <- data.frame(
    label = character(0),  
    # phoneme = character(0), 
    # environment = character(0), 
    sound_file = character(0), 
    origin = numeric(0))

  for (f in 1:nfiles) {
    raw_lines <- read.table(file_paths[f], sep = "=", fill = TRUE, row.names = NULL, skip = 15)[,2]
    nlines <- length(raw_lines)
    origins <- raw_lines[seq(from = 1, by = 3, length.out = nlines / 3)]
    labels <- str_replace(raw_lines[seq(from = 2, by = 3, length.out = nlines / 3)], "^ ", "")
    output_df <- rbind(output_df, data.frame(
      label = labels,  
      # phoneme = str_replace(labels, "_.*$", ""),
      # environment = str_replace(labels, "^[^_]+_([^_]+)_.*$", "\\1"), 
      sound_file = file_name_bases[f], 
      origin = origins))
  }
  write.csv(output_df, file = output_metadata_file, row.names = FALSE)
}

extract_band_intesities = function(
  file_names = NULL,
  band_floors = NULL,
  band_ceilings = NULL,
  intensity_analysis_window = NULL,
  overwrite = FALSE,
  Praat_path = NULL) {

  # Using vectors of floor and ceiling cutoffs, bandpasses the .wav files
  # and then creates intensity files for them; the bandpassed .wav files are not kept

  if (is.null(band_floors)) { band_floors <- default_band_floors() }
  if (is.null(band_ceilings)) { band_ceilings <- default_band_ceilings() }
  if (is.null(intensity_analysis_window)) { intensity_analysis_window <- default_intensity_analysis_window() }

  band_names <- get_band_names(band_floors,band_ceilings)
  nbands <- length(band_names)

	if (is.null(file_names)) { file_names <- sound_files_list() }
  file_name_bases <- file_names %>% str_replace("\\.wav$|\\.WAV$", "")
  nfiles <- length(file_name_bases)

  # Create Praat script files
  create_script_files()

  # Do bandpassing of soundfiles
  smoothing <- 50
  intensity_pitch_floor <- 3.2 / intensity_analysis_window
  temporary_file <- file.path(getwd(), "temp_bandpassed.wav")
  if (str_detect(temporary_file, " ")) { 
  	stop("Please ensure that the names of the working directory and all its parents contain no spaces.") 
  }

  for (b in 1:nbands) {
    cat("  Extracting band:", band_names[b], "\n")

    for (f in 1:nfiles) {
	    outfile <- file.path(sound_files_path(), str_c(file_name_bases[f], "_intensity_", band_names[b], ".txt"))
	    if (file.exists(outfile) & !overwrite) { 
	    	cat("   ", outfile, "already exists.\n    If you want to overwrite it, set overwrite = TRUE.\n")
	    	next
	    } 

      # bandpass your file using a given pass band, save to temporary_file (overwriting any existing temporary_file)
      praat_filter(
      	floor = band_floors[b],
      	ceiling = band_ceilings[b],
      	smoothing = smoothing,
      	infile = file.path(sound_files_path(), str_c(file_name_bases[f], ".wav")),
      	outfile = temporary_file,
      	overwrite = TRUE,
      	Praat_path = Praat_path
      	)

      # create an Intensity object from the bandpassed file
      praat_toIntensity(
      	pitch_floor = intensity_pitch_floor,
      	infile = temporary_file,
      	outfile = outfile,
      	Praat_path = Praat_path,
      	overwrite = overwrite
      	)
    }
  }

  # Clean up
  if(file.exists(temporary_file)) { invisible(file.remove(temporary_file)) }
  remove_script_files()
}

assemble_raw_intensity_data = function(
  metadata_file = "stop_origins.csv",
  band_floors = NULL,
  band_ceilings = NULL,
  # how far from the 'origin' time point of each stop to get data: 
  relative_start_time = -0.2, 
  relative_end_time = +0.2) {

  # This augments the data from stops_origins.csv with an intensity time series pulled out of the intensity files created
  # by BandpassAndIntensity, also a series giving the time points (start of seg=0) and relative intensity (start of seg=0)
  # It then add columns with an estimate of intensity using cubic splines, and an estimate of intensity velocity

  if (is.null(band_floors)) { band_floors <- default_band_floors() }
  if (is.null(band_ceilings)) { band_ceilings <- default_band_ceilings() }

  band_names <- get_band_names(band_floors, band_ceilings)
  nbands <- length(band_names)

  phoneme_data <- unique(read.csv(metadata_file))
  nphonemes <- nrow(phoneme_data)
  nseries <- nphonemes * nbands

  # Check that left time margin is okay
  short_margins <- filter(phoneme_data, origin < -relative_start_time)$sound_file
  if (length(short_margins) > 0) {
      msg <- str_c(
        "Sound files need to have at least ", 
        -relative_start_time, 
        "s before the first 'origin'.\n",
        "This is not the case for these files:\n",
        str_c(str_c(short_margins, ".wav"), collapse = "\n"))
      warning(msg)
      stop("Sound file(s) with origin too early.")
    }

  # Replicate the data n time_points for n bands, and add a Band column
  bandwise_data <- phoneme_data[rep(1:nphonemes, each = nbands),]
  bandwise_data$band <- rep(band_names, nphonemes)
  bandwise_data$intensity_file <- file.path(sound_files_path(), str_c(bandwise_data$sound_file, "_intensity_", bandwise_data$band, ".txt"))
  bandwise_data$raw_intensity_series <- 0
  bandwise_data$raw_times <- 0

  # Load all Praat intensity data into one vector; intensity_file_df keeps track of where each file's
  # data starts in that vector (as its 'offset')
  intensity_files <- unique(bandwise_data$intensity_file)
  file_check <- file.exists(intensity_files)
  if (any(!file_check)) { 
  	stop_message <- str_c("Missing intensity files,", 
  		"possibly never created due to missing sound files:",
  		intensity_files[!file_check], sep = " ")
  	stop( stop_message )
  }
  nfiles <- length(intensity_files) 

  cat("  Reading intensity data from", nfiles, "files.\n") 

  intensity_file_df <- data.frame(intensity_file = intensity_files, offset = rep(0, nfiles))
  all_intensity_data <- numeric(0)
  for (i in 1:nfiles) {
    all_intensity_data <- c(all_intensity_data, read.table(intensity_files[i], sep = "=", skip = 15)[,2])
    if (i < nfiles) { intensity_file_df$offset[i + 1] <- length(all_intensity_data) }
  }

  cat("  Assembling intensity series for", nseries, "stops-in-frequency-bands.\n")

  # Fill the bandwise_data df with data
  time_step <- read.table(intensity_files[1], sep="=", skip=6, nrow=1)[,2]
  for (i in 1:nseries) { 
    offset_steps <- intensity_file_df$offset[which(intensity_file_df$intensity_file == bandwise_data$intensity_file[i])]
    origin_time <- bandwise_data$origin[i]
    time_step_sequence <- (((origin_time + relative_start_time) %/% time_step):((origin_time + relative_end_time) %/% time_step))
    print(time_step_sequence[1])
    print(origin_time)
    print(relative_start_time)
    bandwise_data$raw_intensity_series[i] <- list(all_intensity_data[time_step_sequence + offset_steps])
    bandwise_data$raw_times[i] <- list(time_step_sequence * time_step)
  }

  return(bandwise_data)
}

add_velocity_data = function(raw_intensity_data, smoothing_param = NULL) {
  # To the dataframe produced by assemble_raw_intensity_data, add columns with (1) a cubic spline smoothing of the relative_intensity_series
  # data, and (2) its first derivative

  if (is.null(smoothing_param)) { smoothing_param <- default_smoothing_param() }
	bandwise_data <- raw_intensity_data

  nseries <- nrow(bandwise_data)
  bandwise_data$smoothed_intensity_series  <- 0
  bandwise_data$intensity_velocity_series  <- 0  

  for (i in 1:(nseries)) { 
    raw_intensity_series <- bandwise_data$raw_intensity_series[i] %>% unlist
    raw_times <- bandwise_data$raw_times[i] %>% unlist
    smooth   <- smooth.spline(x = raw_times, y = raw_intensity_series, spar = smoothing_param)
    new_time_vector <- seq(min(raw_times), max(raw_times), by = 0.001)
    bandwise_data$smoothed_intensity_series[i]  <- list(predict(smooth, x = new_time_vector, deriv = 0)$y)
    bandwise_data$intensity_velocity_series[i]  <- list(predict(smooth, x = new_time_vector, deriv = 1)$y)
    bandwise_data$smoothed_times[i] <- list(new_time_vector)
    bandwise_data$smoothed_origin_step[i] <- max(which(new_time_vector < bandwise_data$origin[i]))
  }
  return(bandwise_data)
}

identify_events = function(velocity_data, threshold_ratio = NULL) {
  # Takes the output from add_velocity_data and analyses the 
  # for intensity velocity time series, to locate extrema and thresholds of (0.6 * extremum)
  # and adds this info to the data frame
  # This is the dataset used to argue which of the bands offers for the best basis for defining the
  # boundaries of the phoneme.

  if (is.null(threshold_ratio)) { threshold_ratio <- default_threshold_ratio() }
  bandwise_data <- velocity_data
  nseries <- nrow(bandwise_data)

  for (i in 1:nseries) { 
    velocities  <- unlist(bandwise_data$intensity_velocity_series[i])
    intensities <- unlist(bandwise_data$smoothed_intensity_series[i])
    time_points <- unlist(bandwise_data$smoothed_times[i])
    npoints <- length(time_points)
    origin_step <- bandwise_data$smoothed_origin_step[i]

    # Locate an intensity pit to the right (getting NA if none is found)
    i_pit_step  <- locate_extremum(series = intensities, start_step = origin_step, extremum = "minimum", direction = "right")
    
    # If none is found, then velocity pit & peak are also NA ...
    if (is.na(i_pit_step)) {
      closure_v_extreme_location <- NA
      release_v_extreme_location <- NA
    } else {
      # However, if you find one, then locate a velocity pit to its left and a velocity peak to its right
      closure_v_extreme_location  <- locate_extremum(series = velocities, start_step = i_pit_step, extremum = "minimum", direction = "left")
      release_v_extreme_location <- locate_extremum(series = velocities, start_step = i_pit_step, extremum = "maximum", direction = "right")
      # Also look for an intensity peak to its right
      i_peak_location <- locate_extremum(series = intensities, start_step = i_pit_step, extremum = "maximum", direction = "right")
    }

    # If a release_v_extreme was located, grab its details; if not, set them to NA
    if (!is.na(release_v_extreme_location)) {
      release_v_extreme <- velocities[release_v_extreme_location]
      t_release_v_extreme <- time_points[release_v_extreme_location]
      release_start <- locate_rising_threshold(
        series = velocities, peak_location = release_v_extreme_location, 
        peak_amplitude = release_v_extreme, threshold_ratio = threshold_ratio)
    } else {
      release_start <- NA
      release_v_extreme <- NA
    }

    # If a closure_v_extreme was located, grab its details; if not, set them to NA
    if (!is.na(closure_v_extreme_location)) {
      closure_v_extreme <- velocities[closure_v_extreme_location]
      t_closure_v_extreme <- time_points[closure_v_extreme_location]
      closure_start <- locate_falling_threshold(
        series = velocities, pit_location = closure_v_extreme_location, 
        pit_amplitude = closure_v_extreme, threshold_ratio = threshold_ratio)
    } else {
      closure_start <- NA
      closure_v_extreme <- NA
    }

    # If a i_peak was located, grab its details; if not, set them to NA
    if (!is.na(i_peak_location)) {
      i_peak <- intensities[i_peak_location]
      t_i_peak <- time_points[i_peak_location]
    } else {
      i_peak <- NA
    }

    if(!is.na(closure_start) & !is.na(release_start)) {
      # Redefine time series to just those within the phoneme
      intensities <- intensities[closure_start:release_start]
      time_points <- time_points[closure_start:release_start]
      npoints <- release_start - closure_start + 1
      i_pit <- min(intensities)

      bandwise_data$t_init[i] <- time_points[1]
      bandwise_data$duration[i] <- time_points[npoints] - time_points[1]
      bandwise_data$i_init[i] <- intensities[1]
      bandwise_data$delta_i[i] <- i_pit - intensities[1]    
      bandwise_data$i_pit_lag[i] <- time_points[min(which(intensities == i_pit))] - time_points[1]
      bandwise_data$closure_v_extreme[i] <- closure_v_extreme
      bandwise_data$release_v_extreme[i] <- release_v_extreme
      bandwise_data$closure_v_extreme_lag[i] <- t_closure_v_extreme - time_points[1]
      bandwise_data$closure_start[i] <- closure_start
      bandwise_data$release_start[i] <- release_start
      bandwise_data$acceleration_quotient[i] <- bandwise_data$closure_v_extreme_lag[i] / bandwise_data$duration[i]
      if(!is.na(i_peak_location)) {
        bandwise_data$i_peak_amplitude[i] <- i_peak - intensities[npoints]
        bandwise_data$i_peak_lag[i] <- t_i_peak - time_points[npoints]
      } else {
        bandwise_data$i_peak_amplitude[i] <- NA
        bandwise_data$i_peak_lag[i] <- NA
      }
    } else {
      bandwise_data$t_init[i] <- NA
      bandwise_data$duration[i] <- NA
      bandwise_data$i_init[i] <- NA
      bandwise_data$delta_i[i] <- NA
      bandwise_data$i_pit_lag[i] <- NA
      bandwise_data$closure_v_extreme[i] <- closure_v_extreme
      bandwise_data$release_v_extreme[i] <- release_v_extreme
      bandwise_data$closure_v_extreme_lag[i] <- NA
      bandwise_data$i_peak_amplitude[i] <- NA
      bandwise_data$i_peak_lag[i] <- NA  
      bandwise_data$closure_start[i] <- NA
      bandwise_data$release_start[i] <- NA
      bandwise_data$acceleration_quotient[i] <- NA
    }

  }
  bandwise_data$intensity_file <- NULL
  bandwise_data$raw_intensity_series <- NULL
  bandwise_data$raw_times <- NULL
  bandwise_data$smoothed_origin_step <- NULL
  return(bandwise_data)
}

### Data export ###

export_praat_textgrids = function (raw_intensity_data, 
  band_name = "Band400_1200",
  smoothing_params = NULL,
  thresholds = NULL,
  intensity_analysis_window = NULL,
  path = NULL,
  filename_suffix = "_demarcated",
  points_tier_names = NA) {

	if (is.null(path)) { path <- sound_files_path() }
	if (is.null(smoothing_params)) { smoothing_params <- default_smoothing_param() }
	if (is.null(thresholds)) { thresholds <- default_threshold_ratio() }
  if (is.null(intensity_analysis_window)) { intensity_analysis_window <- default_intensity_analysis_window() }

  sound_files <- unique(raw_intensity_data$sound_file)
  nfiles <- length(sound_files)

  nsparams <- length(smoothing_params)
  nthresholds <- length(thresholds) 
  ntiers <- nsparams * nthresholds

  if (is.na(points_tier_names)) { npoints_tiers <- 0 } else { npoints_tiers <- length(points_tier_names) }

  praat_time_adjustment <- intensity_analysis_window * 1.5   # in order for points to line up correctly in Praat

  for (sp in 1:nsparams) {
    splines_data <- add_velocity_data(raw_intensity_data, smoothing_param = smoothing_params[sp])
  
    for (th in 1:nthresholds) {
      tiernum <- (sp - 1) * nthresholds + th

      bandwise_data <- splines_data %>%
        identify_events(threshold_ratio = thresholds[th])

      for (f in 1:nfiles) {
        outfile_name <- file.path(path, str_c(sound_files[f], filename_suffix, ".TextGrid"))
        interval_data <- bandwise_data %>% 
          filter(band == band_name, sound_file == sound_files[f], !is.na(duration)) %>%
          select(label, t_init, duration) %>%
          mutate(t_fin = t_init + duration) %>%
          select(-duration) %>%
          arrange(t_init)
        nphonemes <- nrow(interval_data)
        interval_data$previous_fin <- c(-praat_time_adjustment, interval_data$t_fin[-nphonemes])

        if (tiernum == 1) {
          # Initialise textgrid file and its header lines
          sink(outfile_name)
            cat("File type = \"ooTextFile\"\n")
            cat("Object class = \"TextGrid\"\n\n")
            cat("xmin = 0\n")
            cat("xmax =", max(interval_data$t_fin) + 5, "\n")
            cat("tiers? <exists>\n")
            cat("size =", ntiers + 1 + npoints_tiers, "\n")
            cat("item []:\n")
          sink()
        }

        # Header lines for a tier
        sink(outfile_name, append = T)
          cat("    item [", tiernum, "]:\n", sep = "")
          cat("        class = \"IntervalTier\"\n") 
          cat("        name = \"sm", smoothing_params[sp], " th", thresholds[th], "\"\n", sep="")
          cat("        xmin = 0\n")
          cat("        xmax =", max(interval_data$t_fin) + 5, "\n")
          cat("        intervals: size =", nphonemes * 2 + 1, "\n") 
        sink()
        
        # Blank interval in the space before the stop phoneme,
        # then an interval for the stop, annotated with its label
        for (p in 1:nphonemes) {
          sink(outfile_name, append = T)
            cat("        intervals [", p * 2 - 1 , "]:\n", sep = "")
            cat("            xmin =", interval_data$previous_fin[p] + praat_time_adjustment, "\n")
            cat("            xmax =", interval_data$t_init[p] + praat_time_adjustment, "\n")
            cat("            text = \"\"\n")
            cat("        intervals [", p * 2 , "]:\n", sep = "")
            cat("            xmin =", interval_data$t_init[p] + praat_time_adjustment, "\n")
            cat("            xmax =", interval_data$t_fin[p] + praat_time_adjustment, "\n")
            cat("            text = \"", as.character(interval_data$label[p]), "\"\n", sep="")
          sink()
        }

        # Final blank interval
        sink(outfile_name, append = T)
          cat("        intervals [", nphonemes * 2 + 1 , "]:\n", sep = "")
          cat("            xmin =", interval_data$t_fin[nphonemes] + praat_time_adjustment, "\n")
          cat("            xmax =", interval_data$t_fin[nphonemes] + 5, "\n")
          cat("            text = \"\"\n")
        sink()
      }
    }
  }

  # Add origins tier
    for (f in 1:nfiles) {
      outfile_name <- file.path(path, str_c(sound_files[f], filename_suffix, ".TextGrid"))
	    point_data <- bandwise_data %>% 
	      filter(band == band_name, sound_file == sound_files[f]) %>%
	      select(label, origin) %>%
	      arrange(origin)
      sink(outfile_name, append = T)      
      cat("    item [", ntiers + 1, "]:\n", sep = "")
      cat("        class = \"TextTier\"\n") 
      cat("        name = \"Origins\"\n", sep="")
      cat("        xmin = 0\n")
      cat("        xmax =", max(interval_data$t_fin) + 5, "\n")
      cat("        points: size =", nphonemes, "\n")
	    for (p in 1:nphonemes) {
	      cat("        points [", p, "]:\n", sep = "")
	      cat("            number =", point_data$origin[p], "\n")
	      cat("            mark = \"", as.character(point_data$label[p]), "\"\n", sep="")
	    }
      sink()
    }  

  # Add empty points tiers if required
  if(npoints_tiers > 0) {
    for (f in 1:nfiles) {
      outfile_name <- file.path(path, str_c(sound_files[f], filename_suffix, ".TextGrid"))
      sink(outfile_name, append = T)      
      for (t in 1:npoints_tiers) {
        cat("    item [", ntiers + 1 + t, "]:\n", sep = "")
        cat("        class = \"TextTier\"\n") 
        cat("        name = \"", points_tier_names[t], "\"\n", sep="")
        cat("        xmin = 0\n")
        cat("        xmax =", max(interval_data$t_fin) + 5, "\n")
        cat("        points: size = 0\n")
      }
      sink()
    }
  }
}

export_summary_to_csv = function(events_data) {
	events_data %>%
		rename(v_peak = closure_v_extreme,
			v_peak_lag = closure_v_extreme_lag) %>%
		select(label, sound_file, band,
			duration, v_peak, delta_i,
			t_init, i_init, i_pit_lag, 
			v_peak_lag, origin) %>%
		write.csv(file = "Intensity_events_summary.csv")
}

### Data analysis ###

report_failures_to_demarcate = function(events_data = NULL, show_pits_release_v_extremes = FALSE) {
  # Shows how many segments could not be demarcated automatically in each
  # frequency band, due to a failure to find either an intensity pit or peak
  #
  # You may want to look at this info by environment, in which case run, for example:
  # bandwise_segment_data %>% filter(environment == "i") %>% report_failures_to_demarcate()

  bandwise_data <- events_data
  if (is.null(bandwise_data)) { bandwise_data <- load_saved_events_data() }
  bands <- unique(bandwise_data$band)
  nbands <- length(bands)
  for (b in 1:nbands) {
    cat("  ", bands[b], ":\t", sep = "")
    cat("failures to demarcate:", nrow(bandwise_data %>% filter(band == bands[b], is.na(duration))),"\t")
    if (show_pits_release_v_extremes) {
      cat("peak not found:", nrow(bandwise_data %>% filter(band == bands[b], is.na(release_v_extreme))),"\t")
      cat("pit not found:", nrow(bandwise_data %>% filter(band == bands[b], is.na(closure_v_extreme))))
    }
    cat("\n")
  }
}


##################################
##### Lower level functions ######
##################################

get_band_names =function(band_floors, band_ceilings) {
  return (str_replace_all(str_replace_all(str_c("Band", band_floors, "_", band_ceilings), "0000", "0k"), "000", "k"))
}

locate_extremum = function(series, start_step, extremum = "maximum", direction = "right", window_length = 10) {
  # Given a time series vector, and starting step in it, expressed as the ith element in the vector,
  # look to the right/left of the start_step, finding the nearest semi-local extremum (see locate_next_maximum
  # for more info). Return the position of that extreme element.

  nsteps <- length(series)

  # Perform the operation by flipping the series as needed, and calling locate_next_maximum
  if (extremum == "minimum") { series <- (series * -1) }
  if (direction == "left")   { series <- rev(series); start_step <- 1 + nsteps - start_step }
  
  extremum_step <- locate_next_maximum(series, start_step, window_length)

  # If needed, unflip the answer that was got
  if (direction == "left")   { extremum_step <- 1 + nsteps - extremum_step }
  return(extremum_step)
}

locate_next_maximum = function(series, start_step, window_length) {
  # Given a time series vector, and starting step in it, expressed as the ith element in the vector,
  # look to the right of the start_step, finding the nearest semi-local maximum: i.e., find a local maximum
  # first, but check if there are more extreme values in the next window_length, and if so, chase them.
  # Return the index, j, of that jth element.

  last_step <- length(series)

  # If at any time below, the algorithm settles of a peak that's lower than the original start_step,
  # that's a failure, and it'll need to return NA. So, keep this threshold and test all subsequent
  # results against it.

  threshold <- series[start_step]

  # If the series is decreasing instead of increasing at the start_step, then: look in the next window_length
  # to see if that changes ...

  if (series[start_step] >= series[start_step + 1]) {

    window_start <- start_step
    window_end   <- min(c(window_start + window_length, last_step))
    increasing_within_window <- which(
      series[window_start:(window_end - 1)] < 
      series[(window_start + 1):(window_end)]
      )

    # If not, return NA
    if (length(increasing_within_window) == 0) { return(NA) }

    # Otherwise, adjust the start_step to be at the first increasing step
    start_step <- min(increasing_within_window) + (start_step - 1)
  }

  # Find the next local maximum. If none exists, return NA.

  elements_after_maximum <- which(
    series[start_step:(last_step - 1)] >= 
    series[(start_step + 1):last_step]
    )
  if (length(elements_after_maximum) == 0) { 
    return(NA) 
  } else {
    localmax_step <- min(elements_after_maximum) + (start_step - 1)
  }

  # Check in the window beyond that local maximum. If no higher point is found, return the position
  # of the local maximum, provided its higher

  window_start <- localmax_step
  window_end   <- min(c(window_start + window_length, last_step))
  if (series[window_start] >= max(series[window_start:window_end])) {
    max_step <- window_start
  } else {
    
    # Otherwise:
    repeat{

      # find the highest element in the search window to the right of the local maximum ...
      max_steps_in_window <- which(
        series[window_start:window_end] == 
        max(series[window_start:window_end])
        )
      first_max_step_in_window <- min(max_steps_in_window) + (window_start - 1)

      # and if the highest point is anywhere other that at the very end of the window, 
      # return its location, ...    
      if (first_max_step_in_window < window_end) {
        max_step <- first_max_step_in_window
        break
      }

      # but if not, advanced the window by one window_length
      window_start <- window_start + window_length
      window_end   <- min(c(window_start + window_length, last_step))

      # If the window has overrun the end of the series, return NA, otherwise, repeat
      # the above actions.
      if (window_start >= last_step) { 
        max_step <- NA
        break
      }
    }
  }
  if (is.na(max_step)) { return(NA) }
  if (series[max_step] > threshold) { return(max_step) } else { return(NA) }
}

locate_falling_threshold = function(series, pit_location, pit_amplitude, threshold_ratio) {
  # Locates the last occasion before the pit_location, at which a falling series crossed a threshold ratio
  # of the pit amplitude
  threshold_value <- pit_amplitude * threshold_ratio
  steps_within_threshold <- which(series[1:pit_location] > threshold_value)
  if (length(steps_within_threshold) == 0) { return(NA) } else { return(max(steps_within_threshold)) }
}

locate_rising_threshold = function(series, peak_location, peak_amplitude, threshold_ratio) {
  # Locates the last occasion before the peak_location, at which a rising series crossed a threshold ratio
  # of the pit amplitude
  threshold_value <- peak_amplitude * threshold_ratio
  steps_within_threshold <- which(series[1:peak_location] < threshold_value)
  if (length(steps_within_threshold) == 0) { return(NA) } else { return(max(steps_within_threshold)) }
}

sound_files_path = function() { file.path(getwd(), "sound_data") }

sound_files_list = function() {
	all_files <- list.files(sound_files_path())
	sound_files <- all_files[str_detect(all_files, ".wav$|.WAV")]
	if (any(str_detect(sound_files, " "))) { stop( "Please ensure the sound file names contain no spaces.")}
	return(sound_files)
}

origin_files_list = function() {
	all_files <- list.files(sound_files_path())
	origin_files <- all_files[str_detect(all_files, "_origins.TextGrid")]
	return(origin_files)
}

load_saved_events_data = function() {
  load("Intensity_event_data.RData")
  return(events_data)
}

load_saved_raw_data = function() {
  load("Intensity_event_data.RData")
  return(raw_intensity_data)
}

################################
#### Interfacing with Praat ####
################################

praat_filter = function(floor, ceiling, smoothing, infile, outfile, overwrite = FALSE, Praat_path = NULL) {
	# Calls Praat and executes a "Filter (pass Hann band)..." command
	if (file.exists(outfile) & !overwrite) { 
		stop_message <- str_c(outfile, " already exists. Set overwrite = TRUE to overwrite it.")
		stop(stop_message)
	}
	user_OS <- get_user_OS()
	Praat_path <- confirm_praat_path(Praat_path, user_OS)
	script_path <- file.path(getwd(), "filter_script.praat")

	command_string <- str_c(sep=" ", 
		Praat_path, 
		"--run", 
		script_path, 
		floor, 
		ceiling, 
		smoothing, 
		infile, 
		outfile)
	
	if( user_OS == "Windows" ) { shell(cmd = command_string, intern = FALSE) }
	else { system(command = command_string, intern = FALSE) }
}

praat_toIntensity = function(pitch_floor, infile, outfile, overwrite = FALSE, Praat_path = NULL) {
	# Calls Praat and executes a "To Intensity..." command
	user_OS <- get_user_OS()
	Praat_path <- confirm_praat_path(Praat_path, user_OS)
	script_path <- file.path(getwd(), "intensity_script.praat")

	command_string <- str_c(sep=" ", 
		Praat_path, 
		"--run", 
		script_path, 
		pitch_floor, 
		infile, 
		outfile)

	if( user_OS == "Windows" ) { shell(cmd = command_string, intern = FALSE) }
	else { system(command = command_string, intern = FALSE) }
}

confirm_praat_path = function(Praat_path = NULL, user_OS) {
	# Work out where Praat is; return errors if not found
	if (is.null(Praat_path)) { Praat_path <- default_Praat_path() }
	if (is.null(Praat_path)) {
		if(user_OS == "Mac") { Praat_path <- "/Applications/Praat.app/Contents/MacOS/Praat" }
		else if (user_OS == "Windows") { Praat_path <- file.path("C:", "Program Files", "Praat.exe")}
		else if (user_OS == "Linux") { Praat_path <- "/usr/bin/praat" }
		if (!file.exists(Praat_path)) { 
			stop_message <- str_c("Did not find Praat in default location: ",
				Praat_path, "  Please specify the correct directory.")
			stop(stop_message)
		}
	} else if (!file.exists(Praat_path)) { 
		stop_message <- str_c("Did not find Praat in specified location: ", Praat_path)
		stop(stop_message)
	}
	return(Praat_path)
}

get_user_OS = function() {
	OSType <- .Platform$OS.type # "windows" for Windows, "unix" for Linux or Mac
	SystemName <- Sys.info()["sysname"] # "Windows", "Linux", or "Darwin" (=Mac)
	if (OSType == "windows") { return("Windows") } 
	if (SystemName == "Darwin") { return("Mac") }
	if (SystemName == "Linux") { return("Linux") }
}

create_script_files = function() {
	write(file = "filter_script.praat", 
		str_c(sep = "\n",
			"form band_pass",
			"\treal floor",
			"\treal ceiling",
			"\treal smoothing",
			"\tsentence infile",
			"\tsentence outfile",
			"endform",
			"do ( \"Read from file...\", infile$ )",
			"do ( \"Filter (pass Hann band)...\", floor, ceiling, smoothing )",
			"do ( \"Save as text file...\", outfile$ )"
			))
	write(file = "intensity_script.praat", 
		str_c(sep = "\n",
			"form calculate_intensity",
			"\treal pitch_floor",
			"\tsentence infile",
			"\tsentence outfile",
			"endform",
			"do ( \"Read from file...\", infile$ )",
			"do ( \"To Intensity...\", pitch_floor, 0, \"yes\" )",
			"do ( \"Save as text file...\", outfile$ )"
			))
}

remove_script_files = function() {
	if(file.exists("filter_script.praat")) { invisible(file.remove("filter_script.praat"))}
	if(file.exists("intensity_script.praat")) { invisible(file.remove("intensity_script.praat"))}	
}