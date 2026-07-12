# ============================================================
# INVENTORY AND CLASSIFY PHD PHOTOS + VIDEOS BEFORE MOVIE BUILD
# ============================================================
#
# Purpose
#   1. Recursively inventory images and videos.
#   2. Read embedded metadata with ExifTool via exifr.
#   3. Detect dates/times written in filenames or dated folders.
#   4. Compare filename/path dates with metadata dates.
#   5. Flag conflicts, missing dates, possible camera-clock shifts,
#      unreadable videos, short-video candidates, and likely photo bursts.
#   6. Produce a manual-review template that can later drive the movie script.
#
# This script DOES NOT rename, move, edit, or delete any source media.
# It only writes diagnostic CSV/TXT files to outputs/_movie_inventory.
# ============================================================

# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "exifr",
  "av",
  "dplyr",
  "stringr",
  "purrr",
  "readr",
  "tibble"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(exifr)
library(av)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tibble)

# ------------------------------------------------------------
# 1. USER SETTINGS
# ------------------------------------------------------------

# Read-only source archive.
root_directory <-
  "C:/Users/HP/Desktop/Shared data/PhD_photos/Pastoralism in Lebanon"

# All generated files from the movie workflow are kept outside the archive.
output_root_directory <-
  "C:/Users/HP/Desktop/Shared data/PhD_photos/outputs"

output_directory <- file.path(
  output_root_directory,
  "_movie_inventory"
)

# Treat unzoned camera/filename times as local Lebanon time.
# This does not alter the files; it only gives R a consistent timezone.
capture_timezone <- "Asia/Beirut"

# Date-comparison thresholds
agreement_minutes <- 10
minor_time_difference_hours <- 2
clock_shift_tolerance_minutes <- 5
possible_clock_shift_hours <- c(1, 2, 3, 4, 12, 24)

# Likely photo-sequence settings
burst_gap_seconds <- 15
burst_minimum_photos <- 3

# Video categories used only for diagnosis/planning
mini_video_max_seconds <- 15
short_video_max_seconds <- 60

# Set FALSE if probing every video with av/FFmpeg is too slow.
probe_videos_with_av <- TRUE

image_extensions <- c(
  "jpg", "jpeg", "png", "tif", "tiff", "heic", "heif",
  "webp", "bmp", "gif", "dng", "nef", "cr2", "cr3", "arw", "rw2"
)

video_extensions <- c(
  "mp4", "mov", "m4v", "avi", "mkv", "wmv", "flv", "webm",
  "3gp", "3g2", "mts", "m2ts", "mpg", "mpeg", "vob", "ts"
)

# Known generated movie outputs should not be treated as source footage.
excluded_file_names <- c(
  "PhD_photo_movie.mp4"
)

# ------------------------------------------------------------
# 2. BASIC CHECKS AND OUTPUT FOLDER
# ------------------------------------------------------------

if (!dir.exists(root_directory)) {
  stop(
    "The source archive does not exist:\n",
    root_directory,
    "\nCheck the path in Section 1."
  )
}

dir.create(
  output_root_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

normalize_for_join <- function(x) {
  normalizePath(
    x,
    winslash = "/",
    mustWork = FALSE
  )
}

root_normalized <- normalize_for_join(root_directory)
output_normalized <- normalize_for_join(output_directory)

make_relative_path <- function(full_path, root_path) {
  full_path <- normalize_for_join(full_path)
  prefix <- paste0(root_path, "/")

  ifelse(
    startsWith(full_path, prefix),
    substring(full_path, nchar(prefix) + 1L),
    full_path
  )
}

# ------------------------------------------------------------
# 3. FIND ALL MEDIA FILES
# ------------------------------------------------------------

all_files <- list.files(
  root_directory,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE,
  all.files = TRUE,
  no.. = TRUE
)

all_files_normalized <- normalize_for_join(all_files)

# Exclude the diagnostic output directory itself.
all_files <- all_files[
  !startsWith(
    all_files_normalized,
    paste0(output_normalized, "/")
  )
]

extensions <- tolower(tools::file_ext(all_files))
media_type <- ifelse(
  extensions %in% image_extensions,
  "image",
  ifelse(extensions %in% video_extensions, "video", NA_character_)
)

keep_media <- !is.na(media_type) &
  !(basename(all_files) %in% excluded_file_names)

media_files <- all_files[keep_media]
media_type <- media_type[keep_media]
extensions <- extensions[keep_media]

if (length(media_files) == 0) {
  stop("No supported image or video files were found.")
}

message(
  "Found ",
  sum(media_type == "image"),
  " images and ",
  sum(media_type == "video"),
  " videos."
)

file_details <- file.info(media_files)

file_inventory <- tibble(
  source_file = normalize_for_join(media_files),
  relative_path = make_relative_path(media_files, root_normalized),
  file_name = basename(media_files),
  file_stem = tools::file_path_sans_ext(basename(media_files)),
  parent_folder = basename(dirname(media_files)),
  extension = extensions,
  media_type = media_type,
  file_size_bytes = as.numeric(file_details$size),
  file_modified_time = as.POSIXct(
    file_details$mtime,
    tz = capture_timezone
  ),
  file_created_time = as.POSIXct(
    file_details$ctime,
    tz = capture_timezone
  )
)

# ------------------------------------------------------------
# 4. READ EMBEDDED METADATA WITH EXIFTOOL
# ------------------------------------------------------------

metadata_tags <- c(
  "SourceFile",
  "FileType",
  "MIMEType",
  "DateTimeOriginal",
  "SubSecDateTimeOriginal",
  "CreateDate",
  "MediaCreateDate",
  "TrackCreateDate",
  "CreationDate",
  "ModifyDate",
  "FileModifyDate",
  "Make",
  "Model",
  "Software",
  "ImageWidth",
  "ImageHeight",
  "Orientation",
  "Rotation",
  "Duration",
  "VideoFrameRate",
  "AvgBitrate",
  "AudioChannels",
  "GPSLatitude",
  "GPSLongitude"
)

metadata <- tryCatch(
  exifr::read_exif(
    media_files,
    tags = metadata_tags,
    args = "-n",
    quiet = TRUE
  ),
  error = function(e) {
    warning(
      "ExifTool metadata extraction failed. The inventory will continue ",
      "with filesystem and filename information only.\n",
      conditionMessage(e)
    )

    tibble(SourceFile = media_files)
  }
)

if (!"SourceFile" %in% names(metadata)) {
  metadata$SourceFile <- media_files[seq_len(min(nrow(metadata), length(media_files)))]
}

metadata <- metadata %>%
  mutate(
    source_file = normalize_for_join(SourceFile)
  ) %>%
  select(-any_of("SourceFile")) %>%
  distinct(source_file, .keep_all = TRUE)

inventory <- file_inventory %>%
  left_join(metadata, by = "source_file")

safe_column <- function(data, column_name) {
  if (column_name %in% names(data)) {
    as.character(data[[column_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

# ------------------------------------------------------------
# 5. DATE/TIME PARSING HELPERS
# ------------------------------------------------------------

parse_embedded_datetime_one <- function(x, tz) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(NA_real_)
  }

  x <- trimws(as.character(x))

  # Keep the first conventional date-time sequence and ignore subseconds/
  # timezone suffixes for this diagnostic comparison.
  hit <- stringr::str_extract(
    x,
    "[12][0-9]{3}[:/-][0-9]{2}[:/-][0-9]{2}[ T][0-9]{2}[:.][0-9]{2}[:.][0-9]{2}"
  )

  if (is.na(hit)) {
    return(NA_real_)
  }

  hit <- sub(
    "^([0-9]{4})[:/-]([0-9]{2})[:/-]([0-9]{2})",
    "\\1-\\2-\\3",
    hit
  )
  hit <- gsub("T", " ", hit, fixed = TRUE)

  # Standardize time separators after the date.
  date_part <- substr(hit, 1, 10)
  time_part <- substr(hit, 12, 19)
  time_part <- gsub("\\.", ":", time_part)
  normalized <- paste(date_part, time_part)

  parsed <- suppressWarnings(
    as.POSIXct(
      normalized,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  )

  as.numeric(parsed)
}

parse_embedded_datetime <- function(x, tz) {
  parsed_numeric <- vapply(
    x,
    parse_embedded_datetime_one,
    numeric(1),
    tz = tz
  )

  as.POSIXct(
    parsed_numeric,
    origin = "1970-01-01",
    tz = tz
  )
}

valid_datetime_numeric <- function(
  year,
  month,
  day,
  hour = 12,
  minute = 0,
  second = 0,
  tz
) {
  candidate <- sprintf(
    "%04d-%02d-%02d %02d:%02d:%02d",
    as.integer(year),
    as.integer(month),
    as.integer(day),
    as.integer(hour),
    as.integer(minute),
    as.integer(second)
  )

  parsed <- suppressWarnings(
    as.POSIXct(
      candidate,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  )

  # as.POSIXct may normalize impossible dates on some systems. Confirm that
  # the formatted result still equals the requested components.
  if (
    is.na(parsed) ||
    format(parsed, "%Y-%m-%d %H:%M:%S", tz = tz) != candidate
  ) {
    return(NA_real_)
  }

  as.numeric(parsed)
}

extract_datetime_from_text_one <- function(text, tz) {
  empty_result <- list(
    datetime_numeric = NA_real_,
    date_character = NA_character_,
    precision = NA_character_,
    pattern = NA_character_,
    matched_text = NA_character_
  )

  if (length(text) == 0 || is.na(text) || !nzchar(trimws(text))) {
    return(empty_result)
  }

  text <- as.character(text)

  # Pattern A: YYYY-MM-DD, YYYY_MM_DD, YYYY.MM.DD,
  # optionally followed by HH-MM-SS / HH.MM.SS / HH:MM:SS.
  match_a <- stringr::str_match(
    text,
    paste0(
      "(20[0-9]{2})[-_. ]([0-9]{1,2})[-_. ]([0-9]{1,2})",
      "(?:[^0-9]{0,8}([0-9]{1,2})[:._-]([0-9]{2})",
      "(?:[:._-]([0-9]{2}))?)?"
    )
  )

  if (!is.na(match_a[1, 1])) {
    has_time <- !is.na(match_a[1, 5])
    hour <- if (has_time) as.integer(match_a[1, 5]) else 12L
    minute <- if (has_time) as.integer(match_a[1, 6]) else 0L
    second <- if (has_time && !is.na(match_a[1, 7])) {
      as.integer(match_a[1, 7])
    } else {
      0L
    }

    parsed <- valid_datetime_numeric(
      match_a[1, 2],
      match_a[1, 3],
      match_a[1, 4],
      hour,
      minute,
      second,
      tz
    )

    if (!is.na(parsed)) {
      parsed_time <- as.POSIXct(parsed, origin = "1970-01-01", tz = tz)
      return(list(
        datetime_numeric = parsed,
        date_character = format(parsed_time, "%Y-%m-%d", tz = tz),
        precision = if (has_time) "datetime" else "date",
        pattern = "Y-M-D",
        matched_text = match_a[1, 1]
      ))
    }
  }

  # Pattern B: YYYYMMDD, optionally followed by HHMMSS.
  # Covers IMG_20240516_132405 and VID-20240516-WA0001.
  match_b <- stringr::str_match(
    text,
    paste0(
      "(20[0-9]{2})(0[1-9]|1[0-2])([0-2][0-9]|3[01])",
      "(?:[^0-9]{0,4}([01][0-9]|2[0-3])([0-5][0-9])([0-5][0-9]))?"
    )
  )

  if (!is.na(match_b[1, 1])) {
    has_time <- !is.na(match_b[1, 5])
    hour <- if (has_time) as.integer(match_b[1, 5]) else 12L
    minute <- if (has_time) as.integer(match_b[1, 6]) else 0L
    second <- if (has_time) as.integer(match_b[1, 7]) else 0L

    parsed <- valid_datetime_numeric(
      match_b[1, 2],
      match_b[1, 3],
      match_b[1, 4],
      hour,
      minute,
      second,
      tz
    )

    if (!is.na(parsed)) {
      parsed_time <- as.POSIXct(parsed, origin = "1970-01-01", tz = tz)
      return(list(
        datetime_numeric = parsed,
        date_character = format(parsed_time, "%Y-%m-%d", tz = tz),
        precision = if (has_time) "datetime" else "date",
        pattern = "YYYYMMDD",
        matched_text = match_b[1, 1]
      ))
    }
  }

  # Pattern C: DD-MM-YYYY / DD_MM_YYYY / DD.MM.YYYY,
  # optionally followed by a time. This assumes day-month-year.
  match_c <- stringr::str_match(
    text,
    paste0(
      "([0-9]{1,2})[-_. ]([0-9]{1,2})[-_. ](20[0-9]{2})",
      "(?:[^0-9]{0,8}([0-9]{1,2})[:._-]([0-9]{2})",
      "(?:[:._-]([0-9]{2}))?)?"
    )
  )

  if (!is.na(match_c[1, 1])) {
    has_time <- !is.na(match_c[1, 5])
    hour <- if (has_time) as.integer(match_c[1, 5]) else 12L
    minute <- if (has_time) as.integer(match_c[1, 6]) else 0L
    second <- if (has_time && !is.na(match_c[1, 7])) {
      as.integer(match_c[1, 7])
    } else {
      0L
    }

    parsed <- valid_datetime_numeric(
      match_c[1, 4],
      match_c[1, 3],
      match_c[1, 2],
      hour,
      minute,
      second,
      tz
    )

    if (!is.na(parsed)) {
      parsed_time <- as.POSIXct(parsed, origin = "1970-01-01", tz = tz)
      return(list(
        datetime_numeric = parsed,
        date_character = format(parsed_time, "%Y-%m-%d", tz = tz),
        precision = if (has_time) "datetime" else "date",
        pattern = "D-M-Y",
        matched_text = match_c[1, 1]
      ))
    }
  }

  empty_result
}

extract_datetime_from_text <- function(x, tz) {
  parsed <- lapply(
    x,
    extract_datetime_from_text_one,
    tz = tz
  )

  tibble(
    parsed_datetime = as.POSIXct(
      vapply(parsed, `[[`, numeric(1), "datetime_numeric"),
      origin = "1970-01-01",
      tz = tz
    ),
    parsed_date = vapply(parsed, `[[`, character(1), "date_character"),
    parsed_precision = vapply(parsed, `[[`, character(1), "precision"),
    parsed_pattern = vapply(parsed, `[[`, character(1), "pattern"),
    parsed_match = vapply(parsed, `[[`, character(1), "matched_text")
  )
}

# ------------------------------------------------------------
# 6. CHOOSE THE BEST EMBEDDED METADATA DATE
# ------------------------------------------------------------

metadata_date_priority <- c(
  "SubSecDateTimeOriginal",
  "DateTimeOriginal",
  "CreateDate",
  "MediaCreateDate",
  "TrackCreateDate",
  "CreationDate",
  "ModifyDate",
  "FileModifyDate"
)

metadata_capture_time <- as.POSIXct(
  rep(NA_real_, nrow(inventory)),
  origin = "1970-01-01",
  tz = capture_timezone
)
metadata_capture_source <- rep(NA_character_, nrow(inventory))
metadata_capture_raw <- rep(NA_character_, nrow(inventory))

for (column_name in metadata_date_priority) {
  raw_values <- safe_column(inventory, column_name)
  parsed_values <- parse_embedded_datetime(raw_values, capture_timezone)

  use_rows <- is.na(metadata_capture_time) & !is.na(parsed_values)

  metadata_capture_time[use_rows] <- parsed_values[use_rows]
  metadata_capture_source[use_rows] <- column_name
  metadata_capture_raw[use_rows] <- raw_values[use_rows]
}

inventory$metadata_capture_time <- metadata_capture_time
inventory$metadata_capture_source <- metadata_capture_source
inventory$metadata_capture_raw <- metadata_capture_raw

# ------------------------------------------------------------
# 7. EXTRACT DATES FROM FILENAMES; USE DATED FOLDERS AS BACKUP
# ------------------------------------------------------------

filename_dates <- extract_datetime_from_text(
  inventory$file_stem,
  capture_timezone
) %>%
  rename_with(~ paste0("filename_", .x))

path_dates <- extract_datetime_from_text(
  inventory$relative_path,
  capture_timezone
) %>%
  rename_with(~ paste0("path_", .x))

inventory <- bind_cols(
  inventory,
  filename_dates,
  path_dates
)

use_filename <- !is.na(inventory$filename_parsed_datetime)

name_time_numeric <- ifelse(
  use_filename,
  as.numeric(inventory$filename_parsed_datetime),
  as.numeric(inventory$path_parsed_datetime)
)

inventory$name_capture_time <- as.POSIXct(
  name_time_numeric,
  origin = "1970-01-01",
  tz = capture_timezone
)

inventory$name_capture_date <- ifelse(
  use_filename,
  inventory$filename_parsed_date,
  inventory$path_parsed_date
)

inventory$name_capture_precision <- ifelse(
  use_filename,
  inventory$filename_parsed_precision,
  inventory$path_parsed_precision
)

inventory$name_capture_source <- ifelse(
  use_filename,
  "filename",
  ifelse(!is.na(inventory$path_parsed_datetime), "folder_or_path", NA_character_)
)

inventory$name_capture_pattern <- ifelse(
  use_filename,
  inventory$filename_parsed_pattern,
  inventory$path_parsed_pattern
)

inventory$name_capture_match <- ifelse(
  use_filename,
  inventory$filename_parsed_match,
  inventory$path_parsed_match
)

# ------------------------------------------------------------
# 8. COMPARE EMBEDDED AND NAME/PATH DATES
# ------------------------------------------------------------

n_files <- nrow(inventory)
comparison_class <- rep(NA_character_, n_files)
difference_seconds <- rep(NA_real_, n_files)
difference_days <- rep(NA_integer_, n_files)
needs_manual_review <- rep(FALSE, n_files)
review_reason <- rep(NA_character_, n_files)
provisional_time_numeric <- rep(NA_real_, n_files)
provisional_time_source <- rep(NA_character_, n_files)

combine_name_date_with_metadata_clock <- function(name_date, metadata_time, tz) {
  if (is.na(name_date) || is.na(metadata_time)) {
    return(NA_real_)
  }

  candidate <- paste(
    name_date,
    format(metadata_time, "%H:%M:%S", tz = tz)
  )

  as.numeric(
    as.POSIXct(
      candidate,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  )
}

for (i in seq_len(n_files)) {
  metadata_time <- inventory$metadata_capture_time[i]
  name_time <- inventory$name_capture_time[i]
  name_date <- inventory$name_capture_date[i]
  name_precision <- inventory$name_capture_precision[i]

  has_metadata <- !is.na(metadata_time)
  has_name <- !is.na(name_time)

  if (has_metadata && has_name) {
    metadata_date <- format(metadata_time, "%Y-%m-%d", tz = capture_timezone)
    same_date <- identical(metadata_date, name_date)

    difference_days[i] <- as.integer(
      as.Date(name_date) - as.Date(metadata_date)
    )

    if (identical(name_precision, "date")) {
      if (same_date) {
        comparison_class[i] <- "AGREE_SAME_DATE_FILENAME_HAS_NO_TIME"
        provisional_time_numeric[i] <- as.numeric(metadata_time)
        provisional_time_source[i] <- "metadata_time_on_agreed_date"
      } else {
        comparison_class[i] <- "CONFLICT_DIFFERENT_DATE_FILENAME_DATE_ONLY"
        needs_manual_review[i] <- TRUE
        review_reason[i] <- "Filename/folder date differs from embedded metadata date"
        provisional_time_numeric[i] <-
          combine_name_date_with_metadata_clock(
            name_date,
            metadata_time,
            capture_timezone
          )
        provisional_time_source[i] <-
          "filename_date_plus_metadata_clock_PROVISIONAL"
      }
    } else {
      difference_seconds[i] <- as.numeric(
        difftime(
          name_time,
          metadata_time,
          units = "secs"
        )
      )

      absolute_minutes <- abs(difference_seconds[i]) / 60
      absolute_hours <- abs(difference_seconds[i]) / 3600
      nearest_hour <- round(absolute_hours)
      near_integer_hour <-
        abs(absolute_hours - nearest_hour) <=
        clock_shift_tolerance_minutes / 60

      if (absolute_minutes <= agreement_minutes) {
        comparison_class[i] <- "AGREE_WITHIN_TOLERANCE"
        provisional_time_numeric[i] <- as.numeric(metadata_time)
        provisional_time_source[i] <- "metadata"
      } else if (
        same_date &&
        absolute_hours <= minor_time_difference_hours
      ) {
        comparison_class[i] <- "MINOR_TIME_DIFFERENCE_SAME_DATE"
        provisional_time_numeric[i] <- as.numeric(name_time)
        provisional_time_source[i] <- "filename_datetime"
      } else if (
        near_integer_hour &&
        nearest_hour %in% possible_clock_shift_hours
      ) {
        comparison_class[i] <- "POSSIBLE_CAMERA_CLOCK_OR_TIMEZONE_SHIFT"
        needs_manual_review[i] <- TRUE
        review_reason[i] <- paste0(
          "Times differ by approximately ",
          nearest_hour,
          " hour(s)"
        )
        provisional_time_numeric[i] <- as.numeric(name_time)
        provisional_time_source[i] <- "filename_datetime_PROVISIONAL"
      } else if (same_date) {
        comparison_class[i] <- "CONFLICT_TIME_SAME_DATE"
        needs_manual_review[i] <- TRUE
        review_reason[i] <- "Filename time and metadata time differ substantially"
        provisional_time_numeric[i] <- as.numeric(name_time)
        provisional_time_source[i] <- "filename_datetime_PROVISIONAL"
      } else {
        comparison_class[i] <- "CONFLICT_DIFFERENT_DATE"
        needs_manual_review[i] <- TRUE
        review_reason[i] <- "Filename/folder date differs from embedded metadata date"
        provisional_time_numeric[i] <- as.numeric(name_time)
        provisional_time_source[i] <- "filename_datetime_PROVISIONAL"
      }
    }
  } else if (has_name) {
    if (identical(name_precision, "date")) {
      comparison_class[i] <- "FILENAME_OR_FOLDER_DATE_ONLY"
    } else {
      comparison_class[i] <- "FILENAME_OR_FOLDER_DATETIME_ONLY"
    }

    provisional_time_numeric[i] <- as.numeric(name_time)
    provisional_time_source[i] <- inventory$name_capture_source[i]
  } else if (has_metadata) {
    comparison_class[i] <- "METADATA_ONLY"
    provisional_time_numeric[i] <- as.numeric(metadata_time)
    provisional_time_source[i] <- "metadata"
  } else {
    comparison_class[i] <- "NO_USABLE_CAPTURE_DATE"
    needs_manual_review[i] <- TRUE
    review_reason[i] <- "No usable date found in metadata, filename, or folder path"

    # Filesystem modification time is retained only as a last-resort provisional
    # ordering field; it is not treated as a true capture date.
    provisional_time_numeric[i] <- as.numeric(inventory$file_modified_time[i])
    provisional_time_source[i] <- "filesystem_modified_time_LAST_RESORT"
  }
}

inventory$date_comparison_class <- comparison_class
inventory$name_minus_metadata_seconds <- difference_seconds
inventory$name_minus_metadata_days <- difference_days
inventory$needs_manual_review <- needs_manual_review
inventory$review_reason <- review_reason
inventory$provisional_capture_time <- as.POSIXct(
  provisional_time_numeric,
  origin = "1970-01-01",
  tz = capture_timezone
)
inventory$provisional_capture_source <- provisional_time_source

inventory$date_reliability <- case_when(
  inventory$date_comparison_class %in% c(
    "AGREE_WITHIN_TOLERANCE",
    "AGREE_SAME_DATE_FILENAME_HAS_NO_TIME"
  ) ~ "HIGH",
  inventory$date_comparison_class %in% c(
    "MINOR_TIME_DIFFERENCE_SAME_DATE",
    "FILENAME_OR_FOLDER_DATE_ONLY",
    "FILENAME_OR_FOLDER_DATETIME_ONLY",
    "METADATA_ONLY"
  ) ~ "MEDIUM",
  TRUE ~ "REVIEW"
)

# ------------------------------------------------------------
# 9. OPTIONAL AV/FFMPEG VIDEO PROBE
# ------------------------------------------------------------

first_value <- function(data, column_name, default = NA) {
  if (
    is.null(data) ||
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    !column_name %in% names(data)
  ) {
    return(default)
  }

  data[[column_name]][1]
}

probe_one_video <- function(file) {
  info <- tryCatch(
    av::av_media_info(file),
    error = function(e) e
  )

  if (inherits(info, "error")) {
    return(tibble(
      source_file = normalize_for_join(file),
      av_readable = FALSE,
      av_probe_error = conditionMessage(info),
      av_duration_seconds = NA_real_,
      av_width = NA_real_,
      av_height = NA_real_,
      av_framerate = NA_real_,
      av_video_codec = NA_character_,
      av_has_audio = NA,
      av_audio_codec = NA_character_
    ))
  }

  video_data <- info$video
  audio_data <- info$audio

  tibble(
    source_file = normalize_for_join(file),
    av_readable = TRUE,
    av_probe_error = NA_character_,
    av_duration_seconds = suppressWarnings(as.numeric(info$duration[1])),
    av_width = suppressWarnings(as.numeric(first_value(video_data, "width"))),
    av_height = suppressWarnings(as.numeric(first_value(video_data, "height"))),
    av_framerate = suppressWarnings(as.numeric(first_value(video_data, "framerate"))),
    av_video_codec = as.character(first_value(video_data, "codec", NA_character_)),
    av_has_audio = !is.null(audio_data) &&
      is.data.frame(audio_data) &&
      nrow(audio_data) > 0,
    av_audio_codec = as.character(first_value(audio_data, "codec", NA_character_))
  )
}

video_files <- inventory$source_file[inventory$media_type == "video"]

if (probe_videos_with_av && length(video_files) > 0) {
  message("Probing ", length(video_files), " videos with av/FFmpeg...")

  video_probe <- purrr::map_dfr(
    video_files,
    probe_one_video
  )

  inventory <- inventory %>%
    left_join(video_probe, by = "source_file")
} else {
  inventory <- inventory %>%
    mutate(
      av_readable = NA,
      av_probe_error = NA_character_,
      av_duration_seconds = NA_real_,
      av_width = NA_real_,
      av_height = NA_real_,
      av_framerate = NA_real_,
      av_video_codec = NA_character_,
      av_has_audio = NA,
      av_audio_codec = NA_character_
    )
}

embedded_duration <- suppressWarnings(
  as.numeric(safe_column(inventory, "Duration"))
)

inventory$video_duration_seconds <- dplyr::coalesce(
  inventory$av_duration_seconds,
  embedded_duration
)

inventory$movie_media_treatment <- case_when(
  inventory$media_type == "image" ~ "PHOTO_FRAME",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) &
    inventory$video_duration_seconds <= mini_video_max_seconds ~
    "MINI_VIDEO_CANDIDATE",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) &
    inventory$video_duration_seconds <= short_video_max_seconds ~
    "SHORT_VIDEO_CANDIDATE",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) ~
    "LONG_VIDEO_REVIEW_OR_TRIM",
  inventory$media_type == "video" ~ "VIDEO_DURATION_UNKNOWN",
  TRUE ~ NA_character_
)

# A failed av probe is a separate technical review issue.
failed_video_probe <- inventory$media_type == "video" &
  inventory$av_readable %in% FALSE

inventory$needs_manual_review[failed_video_probe] <- TRUE
inventory$review_reason[failed_video_probe] <- ifelse(
  is.na(inventory$review_reason[failed_video_probe]),
  "Video could not be read by av/FFmpeg",
  paste(
    inventory$review_reason[failed_video_probe],
    "Video could not be read by av/FFmpeg",
    sep = "; "
  )
)

# ------------------------------------------------------------
# 10. IDENTIFY LIKELY RAPID PHOTO SEQUENCES
# ------------------------------------------------------------

model_values <- safe_column(inventory, "Model")

inventory$sequence_device_key <- ifelse(
  !is.na(model_values) & nzchar(trimws(model_values)),
  model_values,
  inventory$parent_folder
)

sequence_candidates <- inventory %>%
  filter(
    media_type == "image",
    !is.na(provisional_capture_time)
  ) %>%
  mutate(
    provisional_capture_day = as.Date(
      provisional_capture_time,
      tz = capture_timezone
    )
  ) %>%
  arrange(
    sequence_device_key,
    provisional_capture_day,
    provisional_capture_time,
    source_file
  ) %>%
  group_by(
    sequence_device_key,
    provisional_capture_day
  ) %>%
  mutate(
    gap_from_previous_seconds = as.numeric(
      difftime(
        provisional_capture_time,
        lag(provisional_capture_time),
        units = "secs"
      )
    ),
    sequence_break = is.na(gap_from_previous_seconds) |
      gap_from_previous_seconds > burst_gap_seconds,
    sequence_number_within_day = cumsum(sequence_break)
  ) %>%
  group_by(
    sequence_device_key,
    provisional_capture_day,
    sequence_number_within_day
  ) %>%
  mutate(
    sequence_photo_count = n(),
    likely_rapid_sequence = sequence_photo_count >= burst_minimum_photos,
    likely_sequence_id = ifelse(
      likely_rapid_sequence,
      paste0(
        format(provisional_capture_day, "%Y%m%d"),
        "_",
        sprintf("%04d", cur_group_id())
      ),
      NA_character_
    )
  ) %>%
  ungroup() %>%
  select(
    source_file,
    gap_from_previous_seconds,
    sequence_photo_count,
    likely_rapid_sequence,
    likely_sequence_id
  )

inventory <- inventory %>%
  left_join(sequence_candidates, by = "source_file") %>%
  mutate(
    likely_rapid_sequence = dplyr::coalesce(
      likely_rapid_sequence,
      FALSE
    )
  )

# ------------------------------------------------------------
# 11. CLEAN AND ORDER THE MAIN INVENTORY
# ------------------------------------------------------------

inventory <- inventory %>%
  arrange(
    provisional_capture_time,
    media_type,
    relative_path
  ) %>%
  mutate(
    inventory_order = row_number(),
    file_size_mb = round(file_size_bytes / 1024^2, 3),
    provisional_capture_day = as.Date(
      provisional_capture_time,
      tz = capture_timezone
    )
  ) %>%
  relocate(
    inventory_order,
    media_type,
    movie_media_treatment,
    relative_path,
    file_name,
    extension,
    file_size_mb,
    metadata_capture_time,
    metadata_capture_source,
    name_capture_time,
    name_capture_precision,
    name_capture_source,
    date_comparison_class,
    date_reliability,
    needs_manual_review,
    review_reason,
    provisional_capture_time,
    provisional_capture_source,
    likely_rapid_sequence,
    likely_sequence_id,
    sequence_photo_count,
    video_duration_seconds,
    av_readable
  )

# ------------------------------------------------------------
# 12. WRITE OUTPUTS
# ------------------------------------------------------------

main_inventory_file <- file.path(
  output_directory,
  "01_all_media_inventory.csv"
)

review_file <- file.path(
  output_directory,
  "02_date_conflicts_and_manual_review.csv"
)

video_file <- file.path(
  output_directory,
  "03_video_inventory.csv"
)

sequence_file <- file.path(
  output_directory,
  "04_likely_photo_sequences.csv"
)

manual_template_file <- file.path(
  output_directory,
  "05_manual_movie_decisions_template.csv"
)

classification_summary_file <- file.path(
  output_directory,
  "06_summary_by_date_classification.csv"
)

extension_summary_file <- file.path(
  output_directory,
  "07_summary_by_extension.csv"
)

summary_text_file <- file.path(
  output_directory,
  "00_READ_ME_SUMMARY.txt"
)

readr::write_csv(inventory, main_inventory_file, na = "")

manual_review <- inventory %>%
  filter(needs_manual_review) %>%
  select(
    inventory_order,
    media_type,
    movie_media_treatment,
    relative_path,
    metadata_capture_time,
    metadata_capture_source,
    metadata_capture_raw,
    name_capture_time,
    name_capture_precision,
    name_capture_source,
    name_capture_match,
    date_comparison_class,
    name_minus_metadata_seconds,
    name_minus_metadata_days,
    provisional_capture_time,
    provisional_capture_source,
    review_reason,
    av_readable,
    av_probe_error,
    video_duration_seconds
  )

readr::write_csv(manual_review, review_file, na = "")

video_inventory <- inventory %>%
  filter(media_type == "video") %>%
  select(
    inventory_order,
    relative_path,
    movie_media_treatment,
    video_duration_seconds,
    av_readable,
    av_probe_error,
    av_width,
    av_height,
    av_framerate,
    av_video_codec,
    av_has_audio,
    av_audio_codec,
    metadata_capture_time,
    name_capture_time,
    date_comparison_class,
    needs_manual_review,
    provisional_capture_time
  )

readr::write_csv(video_inventory, video_file, na = "")

likely_sequences <- inventory %>%
  filter(likely_rapid_sequence) %>%
  select(
    likely_sequence_id,
    sequence_photo_count,
    inventory_order,
    provisional_capture_time,
    gap_from_previous_seconds,
    relative_path,
    date_comparison_class,
    needs_manual_review
  ) %>%
  arrange(
    likely_sequence_id,
    provisional_capture_time,
    relative_path
  )

readr::write_csv(likely_sequences, sequence_file, na = "")

manual_decisions_template <- inventory %>%
  transmute(
    inventory_order,
    source_file,
    relative_path,
    media_type,
    current_movie_treatment = movie_media_treatment,
    provisional_capture_time,
    date_comparison_class,
    needs_manual_review,
    include_in_movie = "YES",
    manual_capture_datetime = "",
    manual_movie_treatment = "",
    video_start_seconds = "",
    video_end_seconds = "",
    keep_video_audio = "NO",
    hold_photo_seconds = "",
    manual_notes = ""
  )

readr::write_csv(
  manual_decisions_template,
  manual_template_file,
  na = ""
)

classification_summary <- inventory %>%
  count(
    date_comparison_class,
    date_reliability,
    needs_manual_review,
    sort = TRUE,
    name = "file_count"
  )

readr::write_csv(
  classification_summary,
  classification_summary_file,
  na = ""
)

extension_summary <- inventory %>%
  count(
    media_type,
    extension,
    sort = TRUE,
    name = "file_count"
  )

readr::write_csv(
  extension_summary,
  extension_summary_file,
  na = ""
)

summary_lines <- c(
  "PHD MEDIA INVENTORY SUMMARY",
  "===========================",
  "",
  paste0("Scanned root: ", root_directory),
  paste0("Output folder: ", output_directory),
  paste0("Capture timezone used for comparison: ", capture_timezone),
  "",
  paste0("Total media files: ", nrow(inventory)),
  paste0("Images: ", sum(inventory$media_type == "image")),
  paste0("Videos: ", sum(inventory$media_type == "video")),
  paste0(
    "Files requiring manual review: ",
    sum(inventory$needs_manual_review, na.rm = TRUE)
  ),
  paste0(
    "Mini-video candidates (<= ",
    mini_video_max_seconds,
    " s): ",
    sum(
      inventory$movie_media_treatment == "MINI_VIDEO_CANDIDATE",
      na.rm = TRUE
    )
  ),
  paste0(
    "Short-video candidates (> ",
    mini_video_max_seconds,
    " and <= ",
    short_video_max_seconds,
    " s): ",
    sum(
      inventory$movie_media_treatment == "SHORT_VIDEO_CANDIDATE",
      na.rm = TRUE
    )
  ),
  paste0(
    "Long videos needing review/trim: ",
    sum(
      inventory$movie_media_treatment == "LONG_VIDEO_REVIEW_OR_TRIM",
      na.rm = TRUE
    )
  ),
  paste0(
    "Videos unreadable by av/FFmpeg: ",
    sum(
      inventory$media_type == "video" & inventory$av_readable %in% FALSE,
      na.rm = TRUE
    )
  ),
  paste0(
    "Images assigned to likely rapid sequences: ",
    sum(inventory$likely_rapid_sequence, na.rm = TRUE)
  ),
  paste0(
    "Likely rapid photo sequences: ",
    dplyr::n_distinct(
      inventory$likely_sequence_id[!is.na(inventory$likely_sequence_id)]
    )
  ),
  "",
  "DATE CLASSIFICATION COUNTS",
  "--------------------------",
  paste0(
    classification_summary$date_comparison_class,
    ": ",
    classification_summary$file_count
  ),
  "",
  "IMPORTANT",
  "---------",
  paste0(
    "The script does not alter source files. Review ",
    basename(review_file),
    " first."
  ),
  paste0(
    "Use ",
    basename(manual_template_file),
    " to record final dates, exclusions, photo holds, and video trims."
  ),
  "The later movie-building script can read that completed template."
)

writeLines(
  summary_lines,
  summary_text_file,
  useBytes = TRUE
)

message("\nInventory completed.")
message("Open this summary first:")
message(summary_text_file)
message("\nMain inventory:")
message(main_inventory_file)
message("\nManual-review table:")
message(review_file)
message("\nMovie-decision template for the later script:")
message(manual_template_file)
