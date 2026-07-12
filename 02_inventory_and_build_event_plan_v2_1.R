# ============================================================
# PHD MEDIA INVENTORY V2.1 + EVENT-LEVEL MOVIE PLAN
# ============================================================
#
# This second-pass diagnostic script:
#   * inventories photographs, animated GIFs, and videos;
#   * does NOT treat FileModifyDate as a capture date;
#   * tests all meaningful embedded date fields for plausibility;
#   * treats explicit filename date-times as authoritative for phone videos;
#   * automatically resolves the common MP4 UTC/local-time offset;
#   * never combines a filename date with a download/modification clock time;
#   * detects animated GIF + companion-still pairs;
#   * separates photographic sequences into BURST_MOTION,
#     RAPID_SERIES, and FIELD_PROGRESSION;
#   * produces an event-level plan for the later mixed-media movie script.
#
# The script NEVER renames, moves, edits, or deletes source media.
# It writes only to the _movie_inventory_v2 folder.
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

root_directory <-
  "C:/Users/HP/Desktop/Shared data/PhD_photos/Pastoralism in Lebanon"

output_directory <- file.path(root_directory, "_movie_inventory_v2_1")

capture_timezone <- "Asia/Beirut"

# Used only to reject obviously broken/reset camera dates.
# Adjust the earliest date if older archival material is later added.
earliest_plausible_capture_date <- as.Date("2010-01-01")
latest_plausible_capture_date <- Sys.Date() + 1

# Filename-versus-metadata comparisons.
agreement_minutes <- 10
integer_hour_tolerance_minutes <- 6
auto_timezone_shift_hours <- c(1, 2, 3, 4)

# Sequence detection.
sequence_group_gap_seconds <- 15
sequence_minimum_photos <- 3
burst_motion_max_gap_seconds <- 3
rapid_series_max_gap_seconds <- 5

# Video planning categories.
mini_video_max_seconds <- 15
short_video_max_seconds <- 60
probe_videos_with_av <- TRUE

image_extensions <- c(
  "jpg", "jpeg", "png", "tif", "tiff", "heic", "heif",
  "webp", "bmp", "gif", "dng", "nef", "cr2", "cr3", "arw", "rw2"
)

video_extensions <- c(
  "mp4", "mov", "m4v", "avi", "mkv", "wmv", "flv", "webm",
  "3gp", "3g2", "mts", "m2ts", "mpg", "mpeg", "vob", "ts"
)

excluded_file_names <- c(
  "PhD_photo_movie.mp4"
)

# ------------------------------------------------------------
# 2. BASIC HELPERS
# ------------------------------------------------------------

if (!dir.exists(root_directory)) {
  stop(
    "The root directory does not exist:\n",
    root_directory,
    "\nCheck root_directory in Section 1."
  )
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

normalize_for_join <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
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

safe_column <- function(data, column_name) {
  if (column_name %in% names(data)) {
    as.character(data[[column_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

append_note <- function(existing, addition) {
  ifelse(
    is.na(existing) | !nzchar(existing),
    addition,
    paste(existing, addition, sep = "; ")
  )
}

format_local_time <- function(x, tz) {
  ifelse(
    is.na(x),
    NA_character_,
    format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
  )
}

# ------------------------------------------------------------
# 3. FIND MEDIA FILES
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

# Exclude both diagnostic-output folders.
exclude_output <- startsWith(
  all_files_normalized,
  paste0(output_normalized, "/")
) | grepl("/(_movie_inventory|_movie_inventory_v2)/", all_files_normalized)

all_files <- all_files[!exclude_output]
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
  source_directory = normalize_for_join(dirname(media_files)),
  extension = extensions,
  media_type = media_type,
  file_size_bytes = as.numeric(file_details$size),
  file_modified_time = as.POSIXct(file_details$mtime, tz = capture_timezone),
  file_created_time = as.POSIXct(file_details$ctime, tz = capture_timezone)
)

# ------------------------------------------------------------
# 4. READ EMBEDDED METADATA
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
  "FrameCount",
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
      "ExifTool metadata extraction failed. Continuing with filename and ",
      "filesystem information only.\n",
      conditionMessage(e)
    )
    tibble(SourceFile = media_files)
  }
)

if (!"SourceFile" %in% names(metadata)) {
  metadata$SourceFile <- media_files[seq_len(min(nrow(metadata), length(media_files)))]
}

metadata <- metadata %>%
  mutate(source_file = normalize_for_join(SourceFile)) %>%
  select(-any_of("SourceFile")) %>%
  distinct(source_file, .keep_all = TRUE)

inventory <- file_inventory %>%
  left_join(metadata, by = "source_file")

# ------------------------------------------------------------
# 5. DATE/TIME PARSERS
# ------------------------------------------------------------

parse_embedded_datetime_one <- function(x, tz) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(NA_real_)
  }

  x <- trimws(as.character(x))

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

  date_part <- substr(hit, 1, 10)
  time_part <- gsub("\\.", ":", substr(hit, 12, 19))
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
  numeric_values <- vapply(
    x,
    parse_embedded_datetime_one,
    numeric(1),
    tz = tz
  )

  as.POSIXct(numeric_values, origin = "1970-01-01", tz = tz)
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

  # A. YYYY-MM-DD or YYYY_MM_DD, optionally followed by time.
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
      match_a[1, 2], match_a[1, 3], match_a[1, 4],
      hour, minute, second, tz
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

  # B. YYYYMMDD, optionally followed by HHMMSS.
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
      match_b[1, 2], match_b[1, 3], match_b[1, 4],
      hour, minute, second, tz
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

  # C. DD-MM-YYYY, optionally followed by time.
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
      match_c[1, 4], match_c[1, 3], match_c[1, 2],
      hour, minute, second, tz
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
  parsed <- lapply(x, extract_datetime_from_text_one, tz = tz)

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

is_plausible_capture_time <- function(x) {
  date_x <- as.Date(x, tz = capture_timezone)
  !is.na(date_x) &
    date_x >= earliest_plausible_capture_date &
    date_x <= latest_plausible_capture_date
}

# ------------------------------------------------------------
# 6. PARSE ALL MEANINGFUL EMBEDDED DATE FIELDS
# ------------------------------------------------------------

metadata_date_fields <- c(
  "SubSecDateTimeOriginal",
  "DateTimeOriginal",
  "MediaCreateDate",
  "TrackCreateDate",
  "CreateDate",
  "CreationDate"
)

for (field_name in metadata_date_fields) {
  parsed_name <- paste0(field_name, "_parsed")
  plausible_name <- paste0(field_name, "_plausible")

  inventory[[parsed_name]] <- parse_embedded_datetime(
    safe_column(inventory, field_name),
    capture_timezone
  )

  inventory[[plausible_name]] <- is_plausible_capture_time(
    inventory[[parsed_name]]
  )
}

# ModifyDate and FileModifyDate are retained only as filesystem/editing evidence.
# Neither is used as a capture date.

# A preliminary filename parse is used here only to choose among competing
# embedded video dates. For example, it lets VID_YYYYMMDD_HHMMSS prefer the
# embedded field closest to that explicit filename rather than a later export
# date. The full filename/path fields are created in Section 7.
pre_filename_dates <- extract_datetime_from_text(
  inventory$file_stem,
  capture_timezone
)

inventory$pre_filename_datetime <- pre_filename_dates$parsed_datetime
inventory$pre_filename_date <- pre_filename_dates$parsed_date
inventory$pre_filename_precision <- pre_filename_dates$parsed_precision

image_metadata_priority <- c(
  "SubSecDateTimeOriginal",
  "DateTimeOriginal",
  "CreateDate",
  "CreationDate"
)

video_metadata_priority <- c(
  "SubSecDateTimeOriginal",
  "DateTimeOriginal",
  "MediaCreateDate",
  "TrackCreateDate",
  "CreateDate",
  "CreationDate"
)

choose_best_metadata_for_row <- function(i) {
  priorities <- if (inventory$media_type[i] == "video") {
    video_metadata_priority
  } else {
    image_metadata_priority
  }

  candidate_fields <- character(0)
  candidate_times <- numeric(0)

  for (field_name in priorities) {
    parsed_value <- inventory[[paste0(field_name, "_parsed")]][i]
    plausible_value <- inventory[[paste0(field_name, "_plausible")]][i]

    if (!is.na(plausible_value) && plausible_value && !is.na(parsed_value)) {
      candidate_fields <- c(candidate_fields, field_name)
      candidate_times <- c(candidate_times, as.numeric(parsed_value))
    }
  }

  if (length(candidate_fields) == 0) {
    return(list(
      time_numeric = NA_real_,
      source = NA_character_,
      raw = NA_character_
    ))
  }

  chosen_index <- 1L
  filename_time <- inventory$pre_filename_datetime[i]
  filename_date <- inventory$pre_filename_date[i]
  filename_precision <- inventory$pre_filename_precision[i]

  # When the filename supplies a date, choose the embedded field closest to
  # it. This resolves phone-video CreateDate versus later MediaCreateDate
  # disagreements without hard-coding one metadata field for every camera.
  if (!is.na(filename_time)) {
    candidate_posix <- as.POSIXct(
      candidate_times,
      origin = "1970-01-01",
      tz = capture_timezone
    )

    if (identical(filename_precision, "date")) {
      scores <- abs(
        as.numeric(
          as.Date(candidate_posix, tz = capture_timezone) -
            as.Date(filename_date)
        )
      )
    } else {
      scores <- abs(candidate_times - as.numeric(filename_time))
    }

    chosen_index <- which.min(scores)
  }

  chosen_field <- candidate_fields[chosen_index]

  list(
    time_numeric = candidate_times[chosen_index],
    source = chosen_field,
    raw = safe_column(inventory, chosen_field)[i]
  )
}

metadata_choice <- lapply(seq_len(nrow(inventory)), choose_best_metadata_for_row)

inventory$metadata_capture_time <- as.POSIXct(
  vapply(metadata_choice, `[[`, numeric(1), "time_numeric"),
  origin = "1970-01-01",
  tz = capture_timezone
)
inventory$metadata_capture_source <- vapply(
  metadata_choice,
  `[[`,
  character(1),
  "source"
)
inventory$metadata_capture_raw <- vapply(
  metadata_choice,
  `[[`,
  character(1),
  "raw"
)

# Record broken/reset metadata fields separately instead of allowing them to
# control chronology.
implausible_field_text <- character(nrow(inventory))
metadata_internal_spread_hours <- rep(NA_real_, nrow(inventory))
metadata_internal_conflict <- rep(FALSE, nrow(inventory))

for (i in seq_len(nrow(inventory))) {
  bad_fields <- character(0)
  plausible_times <- numeric(0)

  for (field_name in metadata_date_fields) {
    parsed_value <- inventory[[paste0(field_name, "_parsed")]][i]
    plausible_value <- inventory[[paste0(field_name, "_plausible")]][i]
    raw_value <- safe_column(inventory, field_name)[i]

    if (!is.na(parsed_value)) {
      if (isTRUE(plausible_value)) {
        plausible_times <- c(plausible_times, as.numeric(parsed_value))
      } else if (!is.na(raw_value) && nzchar(raw_value)) {
        bad_fields <- c(bad_fields, paste0(field_name, "=", raw_value))
      }
    }
  }

  if (length(bad_fields) > 0) {
    implausible_field_text[i] <- paste(bad_fields, collapse = " | ")
  } else {
    implausible_field_text[i] <- NA_character_
  }

  if (length(plausible_times) >= 2) {
    spread_hours <- diff(range(plausible_times)) / 3600
    metadata_internal_spread_hours[i] <- spread_hours

    nearest_hour <- round(spread_hours)
    looks_like_timezone_shift <-
      nearest_hour %in% auto_timezone_shift_hours &&
      abs(spread_hours - nearest_hour) <= integer_hour_tolerance_minutes / 60

    metadata_internal_conflict[i] <- spread_hours > 24 && !looks_like_timezone_shift

    # Do not require manual review when the filename independently
    # corroborates the embedded candidate selected above. This is common for
    # phone videos that also contain a later export/transcode date.
    chosen_time <- inventory$metadata_capture_time[i]
    filename_time <- inventory$pre_filename_datetime[i]
    filename_date <- inventory$pre_filename_date[i]
    filename_precision <- inventory$pre_filename_precision[i]

    filename_corroborates_choice <- FALSE

    if (!is.na(chosen_time) && !is.na(filename_time)) {
      if (identical(filename_precision, "date")) {
        filename_corroborates_choice <- identical(
          format(chosen_time, "%Y-%m-%d", tz = capture_timezone),
          filename_date
        )
      } else {
        chosen_difference_hours <- abs(
          as.numeric(difftime(filename_time, chosen_time, units = "hours"))
        )
        chosen_nearest_hour <- round(chosen_difference_hours)

        filename_corroborates_choice <-
          chosen_difference_hours <= agreement_minutes / 60 ||
          (
            chosen_nearest_hour %in% auto_timezone_shift_hours &&
            abs(chosen_difference_hours - chosen_nearest_hour) <=
              integer_hour_tolerance_minutes / 60
          )
      }
    }

    if (filename_corroborates_choice) {
      metadata_internal_conflict[i] <- FALSE
    }
  }
}

inventory$implausible_metadata_fields <- implausible_field_text
inventory$metadata_internal_spread_hours <- metadata_internal_spread_hours
inventory$metadata_internal_conflict <- metadata_internal_conflict

# ------------------------------------------------------------
# 7. EXTRACT DATES FROM FILENAMES AND PATHS
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

inventory <- bind_cols(inventory, filename_dates, path_dates)

use_filename <- !is.na(inventory$filename_parsed_datetime)

inventory$name_capture_time <- as.POSIXct(
  ifelse(
    use_filename,
    as.numeric(inventory$filename_parsed_datetime),
    as.numeric(inventory$path_parsed_datetime)
  ),
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

inventory$name_capture_match <- ifelse(
  use_filename,
  inventory$filename_parsed_match,
  inventory$path_parsed_match
)

# Sequence numbers help order date-only WhatsApp exports without inventing
# clock times.
wa_match <- stringr::str_match(
  inventory$file_stem,
  stringr::regex("WA([0-9]{3,6})", ignore_case = TRUE)
)

numeric_suffix_match <- stringr::str_match(
  inventory$file_stem,
  "(?:^|[_-])([0-9]{3,7})$"
)

inventory$filename_sequence_number <- suppressWarnings(
  as.integer(
    ifelse(
      !is.na(wa_match[, 2]),
      wa_match[, 2],
      numeric_suffix_match[, 2]
    )
  )
)

inventory$filename_looks_like_whatsapp <- grepl(
  "(?:WhatsApp|[-_]WA[0-9]+)",
  inventory$file_name,
  ignore.case = TRUE
)

# ------------------------------------------------------------
# 8. RESOLVE THE EFFECTIVE CAPTURE TIME
# ------------------------------------------------------------

n_files <- nrow(inventory)
resolution_class <- rep(NA_character_, n_files)
date_reliability <- rep(NA_character_, n_files)
needs_manual_date_review <- rep(FALSE, n_files)
review_reason <- rep(NA_character_, n_files)
effective_time_numeric <- rep(NA_real_, n_files)
effective_source <- rep(NA_character_, n_files)
effective_precision <- rep(NA_character_, n_files)
name_minus_metadata_seconds <- rep(NA_real_, n_files)
auto_timezone_corrected <- rep(FALSE, n_files)

for (i in seq_len(n_files)) {
  metadata_time <- inventory$metadata_capture_time[i]
  metadata_source <- inventory$metadata_capture_source[i]
  name_time <- inventory$name_capture_time[i]
  name_date <- inventory$name_capture_date[i]
  name_precision <- inventory$name_capture_precision[i]
  name_source <- inventory$name_capture_source[i]

  has_metadata <- !is.na(metadata_time)
  has_name <- !is.na(name_time)
  explicit_filename_datetime <-
    has_name &&
    identical(name_source, "filename") &&
    identical(name_precision, "datetime")

  substantive_metadata <-
    has_metadata &&
    metadata_source %in% c(
      "SubSecDateTimeOriginal",
      "DateTimeOriginal",
      "MediaCreateDate",
      "TrackCreateDate",
      "CreateDate",
      "CreationDate"
    )

  if (has_metadata && has_name) {
    metadata_date <- format(metadata_time, "%Y-%m-%d", tz = capture_timezone)
    same_date <- identical(metadata_date, name_date)

    if (identical(name_precision, "date")) {
      if (same_date) {
        resolution_class[i] <- "AGREED_DATE_METADATA_SUPPLIES_TIME"
        effective_time_numeric[i] <- as.numeric(metadata_time)
        effective_source[i] <- paste0("agreed_filename_date_plus_", metadata_source)
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "HIGH"
      } else if (!substantive_metadata || identical(metadata_source, "ModifyDate")) {
        resolution_class[i] <- "FILENAME_DATE_OVERRIDES_LOW_TRUST_METADATA"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- paste0(name_source, "_date_noon_synthetic")
        effective_precision[i] <- "date_only_synthetic_time"
        date_reliability[i] <- "MEDIUM"
      } else if (inventory$filename_looks_like_whatsapp[i]) {
        resolution_class[i] <- "WHATSAPP_FILENAME_DATE_USED_METADATA_CONFLICT_REVIEW"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- "whatsapp_filename_date_noon_synthetic"
        effective_precision[i] <- "date_only_synthetic_time"
        date_reliability[i] <- "REVIEW"
        needs_manual_date_review[i] <- TRUE
        review_reason[i] <- "Date-only WhatsApp filename conflicts with substantive embedded capture date"
      } else {
        resolution_class[i] <- "DATE_ONLY_NAME_CONFLICTS_WITH_METADATA"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- paste0(name_source, "_date_noon_synthetic_PROVISIONAL")
        effective_precision[i] <- "date_only_synthetic_time"
        date_reliability[i] <- "REVIEW"
        needs_manual_date_review[i] <- TRUE
        review_reason[i] <- "Filename/folder date conflicts with substantive embedded capture date"
      }
    } else {
      difference_seconds <- as.numeric(
        difftime(name_time, metadata_time, units = "secs")
      )
      name_minus_metadata_seconds[i] <- difference_seconds
      absolute_minutes <- abs(difference_seconds) / 60
      absolute_hours <- abs(difference_seconds) / 3600
      nearest_hour <- round(absolute_hours)
      near_integer_hour <-
        nearest_hour %in% auto_timezone_shift_hours &&
        abs(absolute_hours - nearest_hour) <=
          integer_hour_tolerance_minutes / 60

      # Explicitly dated phone videos use local time in their filenames while
      # MP4 container creation times commonly store UTC. Use the filename and
      # auto-resolve the offset.
      if (
        inventory$media_type[i] == "video" &&
        explicit_filename_datetime &&
        (absolute_minutes <= agreement_minutes || near_integer_hour)
      ) {
        if (absolute_minutes <= agreement_minutes) {
          resolution_class[i] <- "VIDEO_FILENAME_AND_METADATA_AGREE"
        } else {
          resolution_class[i] <- "VIDEO_FILENAME_LOCAL_TIME_METADATA_UTC_AUTO_RESOLVED"
          auto_timezone_corrected[i] <- TRUE
        }

        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- "explicit_video_filename_datetime"
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "HIGH"
      } else if (absolute_minutes <= agreement_minutes) {
        resolution_class[i] <- "FILENAME_AND_METADATA_AGREE"
        effective_time_numeric[i] <- as.numeric(metadata_time)
        effective_source[i] <- metadata_source
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "HIGH"
      } else if (
        explicit_filename_datetime &&
        near_integer_hour &&
        same_date
      ) {
        resolution_class[i] <- "FILENAME_DATETIME_CLOCK_SHIFT_AUTO_RESOLVED"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- "explicit_filename_datetime"
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "HIGH"
        auto_timezone_corrected[i] <- TRUE
      } else if (
        explicit_filename_datetime &&
        same_date &&
        absolute_hours <= 2
      ) {
        resolution_class[i] <- "FILENAME_DATETIME_USED_MINOR_SAME_DAY_DIFFERENCE"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- "explicit_filename_datetime"
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "MEDIUM"
      } else if (
        explicit_filename_datetime &&
        (!substantive_metadata || identical(metadata_source, "ModifyDate"))
      ) {
        resolution_class[i] <- "FILENAME_DATETIME_OVERRIDES_LOW_TRUST_METADATA"
        effective_time_numeric[i] <- as.numeric(name_time)
        effective_source[i] <- "explicit_filename_datetime"
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "HIGH"
      } else {
        resolution_class[i] <- "GENUINE_FILENAME_METADATA_CONFLICT"
        effective_time_numeric[i] <- if (explicit_filename_datetime) {
          as.numeric(name_time)
        } else {
          as.numeric(metadata_time)
        }
        effective_source[i] <- if (explicit_filename_datetime) {
          "explicit_filename_datetime_PROVISIONAL"
        } else {
          paste0(metadata_source, "_PROVISIONAL")
        }
        effective_precision[i] <- "datetime"
        date_reliability[i] <- "REVIEW"
        needs_manual_date_review[i] <- TRUE
        review_reason[i] <- "Filename/path date-time conflicts materially with substantive embedded metadata"
      }
    }
  } else if (has_name) {
    if (identical(name_precision, "datetime")) {
      resolution_class[i] <- "FILENAME_OR_FOLDER_DATETIME_ONLY"
      effective_precision[i] <- "datetime"
      date_reliability[i] <- ifelse(
        identical(name_source, "filename"),
        "HIGH",
        "MEDIUM"
      )
      effective_source[i] <- paste0(name_source, "_datetime")
    } else {
      resolution_class[i] <- "FILENAME_OR_FOLDER_DATE_ONLY"
      effective_precision[i] <- "date_only_synthetic_time"
      date_reliability[i] <- "MEDIUM"
      effective_source[i] <- paste0(name_source, "_date_noon_synthetic")
    }

    effective_time_numeric[i] <- as.numeric(name_time)
  } else if (has_metadata) {
    resolution_class[i] <- "METADATA_ONLY"
    effective_time_numeric[i] <- as.numeric(metadata_time)
    effective_source[i] <- metadata_source
    effective_precision[i] <- "datetime"
    date_reliability[i] <- ifelse(
      metadata_source %in% c("SubSecDateTimeOriginal", "DateTimeOriginal"),
      "HIGH",
      "MEDIUM"
    )
  } else {
    resolution_class[i] <- "NO_USABLE_CAPTURE_DATE"
    effective_time_numeric[i] <- as.numeric(inventory$file_modified_time[i])
    effective_source[i] <- "filesystem_modified_time_ORDERING_ONLY"
    effective_precision[i] <- "filesystem_fallback_not_capture_time"
    date_reliability[i] <- "REVIEW"
    needs_manual_date_review[i] <- TRUE
    review_reason[i] <- "No plausible capture date in metadata, filename, or folder path"
  }

  if (isTRUE(inventory$metadata_internal_conflict[i])) {
    needs_manual_date_review[i] <- TRUE
    date_reliability[i] <- "REVIEW"
    review_reason[i] <- append_note(
      review_reason[i],
      "Plausible embedded metadata fields disagree by more than 24 hours"
    )
  }
}

inventory$date_resolution_class <- resolution_class
inventory$date_reliability <- date_reliability
inventory$needs_manual_date_review <- needs_manual_date_review
inventory$date_review_reason <- review_reason
inventory$effective_capture_time <- as.POSIXct(
  effective_time_numeric,
  origin = "1970-01-01",
  tz = capture_timezone
)
inventory$effective_capture_source <- effective_source
inventory$effective_capture_precision <- effective_precision
inventory$name_minus_metadata_seconds <- name_minus_metadata_seconds
inventory$auto_timezone_corrected <- auto_timezone_corrected

# ------------------------------------------------------------
# 8B. APPLY VERIFIED DATE OVERRIDES
# ------------------------------------------------------------
#
# These two clips contain conflicting embedded fields. Their CreateDate values
# form a coherent July 14 sequence (MAQ04564 followed by MAQ04565), whereas the
# MediaCreateDate values appear to describe later processing/export on July 20.
# Keep explicit overrides here so the decision is transparent and reproducible.

manual_date_overrides <- tibble::tribble(
  ~relative_path, ~override_capture_time_local, ~override_note,
  "MAQ04564.MP4", "2017-07-14 16:02:51",
  "Use CreateDate; coherent with MAQ filename numbering and adjacent clip chronology",
  "MAQ04565.MP4", "2017-07-14 16:14:29",
  "Use CreateDate; coherent with MAQ filename numbering and adjacent clip chronology"
)

inventory$manual_date_override_applied <- FALSE
inventory$manual_date_override_note <- NA_character_

for (j in seq_len(nrow(manual_date_overrides))) {
  override_row <- inventory$relative_path == manual_date_overrides$relative_path[j]

  if (any(override_row)) {
    override_time <- as.POSIXct(
      manual_date_overrides$override_capture_time_local[j],
      format = "%Y-%m-%d %H:%M:%S",
      tz = capture_timezone
    )

    inventory$effective_capture_time[override_row] <- override_time
    inventory$effective_capture_source[override_row] <-
      "verified_manual_override_CreateDate"
    inventory$effective_capture_precision[override_row] <- "datetime"
    inventory$date_resolution_class[override_row] <-
      "VERIFIED_MANUAL_DATE_OVERRIDE"
    inventory$date_reliability[override_row] <- "HIGH"
    inventory$needs_manual_date_review[override_row] <- FALSE
    inventory$date_review_reason[override_row] <- NA_character_
    inventory$manual_date_override_applied[override_row] <- TRUE
    inventory$manual_date_override_note[override_row] <-
      manual_date_overrides$override_note[j]
  }
}

# ------------------------------------------------------------
# 9. PAIR ANIMATED GIFS WITH COMPANION STILLS
# ------------------------------------------------------------

inventory$gif_pair_base_stem <- ifelse(
  inventory$extension == "gif",
  sub("-ANIMATION$", "", inventory$file_stem, ignore.case = TRUE),
  inventory$file_stem
)

inventory$gif_pair_key <- paste0(
  tolower(inventory$source_directory),
  "/",
  tolower(inventory$gif_pair_base_stem)
)

static_image_extensions <- c(
  "jpg", "jpeg", "png", "tif", "tiff", "heic", "heif", "webp", "bmp"
)

static_lookup <- inventory %>%
  filter(extension %in% static_image_extensions) %>%
  arrange(
    desc(date_reliability == "HIGH"),
    file_name
  ) %>%
  distinct(gif_pair_key, .keep_all = TRUE) %>%
  select(
    gif_pair_key,
    companion_still_source_file = source_file,
    companion_still_relative_path = relative_path,
    companion_still_capture_time = effective_capture_time,
    companion_still_capture_source = effective_capture_source,
    companion_still_date_reliability = date_reliability
  )

inventory <- inventory %>%
  left_join(static_lookup, by = "gif_pair_key") %>%
  mutate(
    is_animated_gif = extension == "gif",
    has_companion_still = is_animated_gif & !is.na(companion_still_source_file)
  )

# Companion stills also need a reverse marker.
gif_keys_with_companion <- unique(
  inventory$gif_pair_key[inventory$is_animated_gif & inventory$has_companion_still]
)

inventory$is_companion_still_for_gif <-
  inventory$extension %in% static_image_extensions &
  inventory$gif_pair_key %in% gif_keys_with_companion

# If a GIF has no trustworthy own date, inherit the exact time from its still.
gif_inherit_rows <- inventory$is_animated_gif &
  inventory$has_companion_still &
  (
    inventory$date_reliability == "REVIEW" |
    inventory$effective_capture_precision != "datetime"
  )

inventory$effective_capture_time[gif_inherit_rows] <-
  inventory$companion_still_capture_time[gif_inherit_rows]
inventory$effective_capture_source[gif_inherit_rows] <-
  "companion_still"
inventory$effective_capture_precision[gif_inherit_rows] <-
  "datetime"
inventory$date_resolution_class[gif_inherit_rows] <-
  "ANIMATED_GIF_DATE_INHERITED_FROM_COMPANION_STILL"
inventory$date_reliability[gif_inherit_rows] <-
  inventory$companion_still_date_reliability[gif_inherit_rows]
inventory$needs_manual_date_review[gif_inherit_rows] <- FALSE
inventory$date_review_reason[gif_inherit_rows] <- NA_character_

# ------------------------------------------------------------
# 10. PROBE VIDEOS WITH AV/FFMPEG
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
  info <- tryCatch(av::av_media_info(file), error = function(e) e)

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
  video_probe <- purrr::map_dfr(video_files, probe_one_video)
  inventory <- inventory %>% left_join(video_probe, by = "source_file")
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

embedded_duration <- suppressWarnings(as.numeric(safe_column(inventory, "Duration")))

inventory$video_duration_seconds <- dplyr::coalesce(
  inventory$av_duration_seconds,
  embedded_duration
)

inventory$initial_media_treatment <- case_when(
  inventory$is_animated_gif ~ "ANIMATED_GIF",
  inventory$is_companion_still_for_gif ~ "STILL_COMPANION_TO_ANIMATED_GIF",
  inventory$media_type == "image" ~ "STATIC_PHOTO",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) &
    inventory$video_duration_seconds <= mini_video_max_seconds ~
    "MINI_VIDEO_INCLUDE_CANDIDATE",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) &
    inventory$video_duration_seconds <= short_video_max_seconds ~
    "SHORT_VIDEO_REVIEW",
  inventory$media_type == "video" &
    !is.na(inventory$video_duration_seconds) ~
    "LONG_VIDEO_TRIM_REQUIRED",
  inventory$media_type == "video" ~ "VIDEO_DURATION_UNKNOWN",
  TRUE ~ NA_character_
)

failed_video_probe <- inventory$media_type == "video" &
  inventory$av_readable %in% FALSE

inventory$needs_manual_date_review[failed_video_probe] <- TRUE
inventory$date_review_reason[failed_video_probe] <- append_note(
  inventory$date_review_reason[failed_video_probe],
  "Video could not be decoded by av/FFmpeg"
)

# ------------------------------------------------------------
# 11. CLASSIFY RAPID PHOTOGRAPHIC SEQUENCES
# ------------------------------------------------------------

model_values <- safe_column(inventory, "Model")

inventory$sequence_device_key <- ifelse(
  !is.na(model_values) & nzchar(trimws(model_values)),
  model_values,
  inventory$parent_folder
)

# Only exact/estimated date-times can establish motion. Date-only WhatsApp
# exports remain ordered sets, not inferred motion sequences.
sequence_candidates <- inventory %>%
  filter(
    media_type == "image",
    !is_animated_gif,
    !is_companion_still_for_gif,
    effective_capture_precision == "datetime",
    !is.na(effective_capture_time)
  ) %>%
  mutate(
    effective_capture_day = as.Date(
      effective_capture_time,
      tz = capture_timezone
    )
  ) %>%
  arrange(
    sequence_device_key,
    effective_capture_day,
    effective_capture_time,
    filename_sequence_number,
    source_file
  ) %>%
  group_by(sequence_device_key, effective_capture_day) %>%
  mutate(
    gap_from_previous_seconds = as.numeric(
      difftime(
        effective_capture_time,
        lag(effective_capture_time),
        units = "secs"
      )
    ),
    broad_sequence_break = is.na(gap_from_previous_seconds) |
      gap_from_previous_seconds > sequence_group_gap_seconds,
    broad_sequence_number = cumsum(broad_sequence_break),
    # Critical: the gap that starts a new group belongs to the preceding
    # inter-sequence interval, not to the new sequence itself.
    gap_within_sequence_seconds = if_else(
      broad_sequence_break,
      NA_real_,
      gap_from_previous_seconds
    )
  ) %>%
  group_by(
    sequence_device_key,
    effective_capture_day,
    broad_sequence_number
  ) %>%
  mutate(
    sequence_photo_count = n(),
    sequence_duration_seconds = as.numeric(
      difftime(
        max(effective_capture_time),
        min(effective_capture_time),
        units = "secs"
      )
    ),
    sequence_max_internal_gap_seconds = ifelse(
      n() > 1,
      max(gap_within_sequence_seconds, na.rm = TRUE),
      NA_real_
    ),
    sequence_median_internal_gap_seconds = ifelse(
      n() > 1,
      median(gap_within_sequence_seconds, na.rm = TRUE),
      NA_real_
    ),
    qualifies_as_sequence = sequence_photo_count >= sequence_minimum_photos,
    sequence_class = case_when(
      !qualifies_as_sequence ~ NA_character_,
      sequence_max_internal_gap_seconds <= burst_motion_max_gap_seconds ~
        "BURST_MOTION",
      sequence_max_internal_gap_seconds <= rapid_series_max_gap_seconds ~
        "RAPID_SERIES",
      TRUE ~ "FIELD_PROGRESSION"
    )
  ) %>%
  ungroup()

sequence_headers <- sequence_candidates %>%
  filter(qualifies_as_sequence) %>%
  distinct(
    sequence_device_key,
    effective_capture_day,
    broad_sequence_number,
    sequence_class,
    sequence_photo_count,
    sequence_duration_seconds,
    sequence_max_internal_gap_seconds,
    sequence_median_internal_gap_seconds
  ) %>%
  arrange(
    effective_capture_day,
    sequence_device_key,
    broad_sequence_number
  ) %>%
  mutate(
    sequence_id = paste0(
      "SEQ_",
      format(effective_capture_day, "%Y%m%d"),
      "_",
      sprintf("%04d", row_number())
    ),
    suggested_sequence_fps = case_when(
      sequence_class == "BURST_MOTION" ~ 12,
      sequence_class == "RAPID_SERIES" ~ 8,
      sequence_class == "FIELD_PROGRESSION" ~ 4,
      TRUE ~ NA_real_
    )
  )

# Defensive checks: every within-sequence gap must be no larger than the
# grouping threshold and cannot exceed the sequence's total duration.
invalid_sequence_gap <- sequence_headers %>%
  filter(
    sequence_max_internal_gap_seconds > sequence_group_gap_seconds |
      (
        sequence_duration_seconds > 0 &
          sequence_max_internal_gap_seconds > sequence_duration_seconds
      )
  )

if (nrow(invalid_sequence_gap) > 0) {
  stop(
    "Internal sequence validation failed for ",
    nrow(invalid_sequence_gap),
    " sequence(s). No event plan was written."
  )
}

sequence_members <- sequence_candidates %>%
  filter(qualifies_as_sequence) %>%
  left_join(
    sequence_headers,
    by = c(
      "sequence_device_key",
      "effective_capture_day",
      "broad_sequence_number",
      "sequence_class",
      "sequence_photo_count",
      "sequence_duration_seconds",
      "sequence_max_internal_gap_seconds",
      "sequence_median_internal_gap_seconds"
    )
  ) %>%
  select(
    source_file,
    sequence_id,
    sequence_class,
    suggested_sequence_fps,
    gap_from_previous_seconds,
    gap_within_sequence_seconds,
    sequence_photo_count,
    sequence_duration_seconds,
    sequence_max_internal_gap_seconds,
    sequence_median_internal_gap_seconds
  )

inventory <- inventory %>%
  left_join(sequence_members, by = "source_file")

# ------------------------------------------------------------
# 12. CREATE EVENT MEMBERSHIP
# ------------------------------------------------------------

inventory <- inventory %>%
  mutate(
    gif_event_key = ifelse(
      is_animated_gif | is_companion_still_for_gif,
      paste0("GIFPAIR::", gif_pair_key),
      NA_character_
    ),
    event_key = case_when(
      !is.na(sequence_id) ~ paste0("SEQUENCE::", sequence_id),
      !is.na(gif_event_key) ~ gif_event_key,
      media_type == "video" ~ paste0("VIDEO::", source_file),
      TRUE ~ paste0("PHOTO::", source_file)
    )
  )

# Sort files before assigning compact event IDs.
inventory <- inventory %>%
  arrange(
    effective_capture_time,
    filename_sequence_number,
    relative_path
  ) %>%
  mutate(
    inventory_order = row_number(),
    file_size_mb = round(file_size_bytes / 1024^2, 3),
    effective_capture_day = as.Date(
      effective_capture_time,
      tz = capture_timezone
    ),
    effective_capture_time_local = format_local_time(
      effective_capture_time,
      capture_timezone
    ),
    metadata_capture_time_local = format_local_time(
      metadata_capture_time,
      capture_timezone
    ),
    name_capture_time_local = format_local_time(
      name_capture_time,
      capture_timezone
    )
  )

event_id_lookup <- inventory %>%
  group_by(event_key) %>%
  summarise(
    event_capture_time = min(effective_capture_time, na.rm = TRUE),
    first_inventory_order = min(inventory_order),
    .groups = "drop"
  ) %>%
  arrange(event_capture_time, first_inventory_order, event_key) %>%
  mutate(event_id = paste0("EVT_", sprintf("%05d", row_number())))

inventory <- inventory %>%
  left_join(event_id_lookup %>% select(event_key, event_id), by = "event_key")

# ------------------------------------------------------------
# 13. BUILD EVENT-LEVEL MOVIE PLAN
# ------------------------------------------------------------

event_plan <- inventory %>%
  group_by(event_id, event_key) %>%
  summarise(
    event_capture_time = min(effective_capture_time, na.rm = TRUE),
    event_capture_time_local = format_local_time(
      min(effective_capture_time, na.rm = TRUE),
      capture_timezone
    ),
    member_count = n(),
    first_relative_path = relative_path[which.min(inventory_order)],
    last_relative_path = relative_path[which.max(inventory_order)],
    contains_video = any(media_type == "video"),
    contains_animated_gif = any(is_animated_gif),
    contains_companion_still = any(is_companion_still_for_gif),
    sequence_class = dplyr::first(na.omit(sequence_class), default = NA_character_),
    sequence_fps = dplyr::first(
      na.omit(suggested_sequence_fps),
      default = NA_real_
    ),
    source_sequence_duration_seconds = dplyr::first(
      na.omit(sequence_duration_seconds),
      default = NA_real_
    ),
    video_duration_seconds = dplyr::first(
      na.omit(video_duration_seconds),
      default = NA_real_
    ),
    any_audio = any(av_has_audio %in% TRUE),
    date_reliability = case_when(
      any(date_reliability == "REVIEW") ~ "REVIEW",
      any(date_reliability == "MEDIUM") ~ "MEDIUM",
      TRUE ~ "HIGH"
    ),
    needs_manual_date_review = any(needs_manual_date_review),
    date_review_reason = paste(
      unique(na.omit(date_review_reason)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    event_type = case_when(
      contains_video ~ "VIDEO",
      contains_animated_gif ~ "ANIMATED_GIF_EVENT",
      !is.na(sequence_class) ~ sequence_class,
      TRUE ~ "SINGLE_PHOTO"
    ),
    suggested_movie_treatment = case_when(
      event_type == "BURST_MOTION" ~ "PLAY_IMAGE_SEQUENCE_FAST",
      event_type == "RAPID_SERIES" ~ "PLAY_IMAGE_SEQUENCE_MEDIUM_FAST",
      event_type == "FIELD_PROGRESSION" ~ "PLAY_IMAGE_SEQUENCE_MODERATELY",
      event_type == "ANIMATED_GIF_EVENT" ~
        "USE_ANIMATED_GIF_AND_SUPPRESS_DUPLICATE_STILL",
      event_type == "VIDEO" &
        !is.na(video_duration_seconds) &
        video_duration_seconds <= mini_video_max_seconds ~
        "INCLUDE_FULL_VIDEO_CANDIDATE",
      event_type == "VIDEO" &
        !is.na(video_duration_seconds) &
        video_duration_seconds <= short_video_max_seconds ~
        "REVIEW_AND_OPTIONALLY_TRIM_VIDEO",
      event_type == "VIDEO" ~ "SELECT_SHORT_EXCERPT_FROM_VIDEO",
      TRUE ~ "HOLD_SINGLE_PHOTO"
    ),
    suggested_photo_hold_seconds = ifelse(
      event_type == "SINGLE_PHOTO",
      0.8,
      NA_real_
    ),
    suggested_keep_audio = ifelse(
      event_type == "VIDEO" & any_audio,
      "REVIEW",
      "NO"
    ),
    editorial_review_required = case_when(
      needs_manual_date_review ~ TRUE,
      event_type == "VIDEO" ~ TRUE,
      event_type == "ANIMATED_GIF_EVENT" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  arrange(event_capture_time, event_id)

# Event-members table makes every event transparent.
event_members <- inventory %>%
  select(
    event_id,
    inventory_order,
    effective_capture_time,
    effective_capture_time_local,
    filename_sequence_number,
    relative_path,
    media_type,
    extension,
    initial_media_treatment,
    sequence_id,
    sequence_class,
    gap_from_previous_seconds,
    is_animated_gif,
    is_companion_still_for_gif,
    needs_manual_date_review
  ) %>%
  arrange(event_id, effective_capture_time, filename_sequence_number, relative_path)

# ------------------------------------------------------------
# 14. WRITE OUTPUTS
# ------------------------------------------------------------

summary_file <- file.path(output_directory, "00_READ_ME_SUMMARY_V2_1.txt")
main_inventory_file <- file.path(output_directory, "01_all_media_inventory_v2_1.csv")
manual_review_file <- file.path(output_directory, "02_genuine_date_review_v2_1.csv")
video_file <- file.path(output_directory, "03_video_inventory_v2_1.csv")
sequence_file <- file.path(output_directory, "04_sequence_summary_v2_1.csv")
event_plan_file <- file.path(output_directory, "05_event_level_movie_plan_v2_1.csv")
event_members_file <- file.path(output_directory, "06_event_members_v2_1.csv")
manual_event_file <- file.path(output_directory, "07_manual_event_decisions_template_v2_1.csv")
date_summary_file <- file.path(output_directory, "08_date_resolution_summary_v2_1.csv")
event_summary_file <- file.path(output_directory, "09_event_type_summary_v2_1.csv")

inventory_output <- inventory %>%
  relocate(
    inventory_order,
    event_id,
    media_type,
    initial_media_treatment,
    relative_path,
    file_name,
    extension,
    file_size_mb,
    effective_capture_time_local,
    effective_capture_source,
    effective_capture_precision,
    date_resolution_class,
    date_reliability,
    needs_manual_date_review,
    date_review_reason,
    metadata_capture_time_local,
    metadata_capture_source,
    name_capture_time_local,
    name_capture_precision,
    name_capture_source,
    auto_timezone_corrected,
    implausible_metadata_fields,
    metadata_internal_spread_hours,
    metadata_internal_conflict,
    sequence_id,
    sequence_class,
    sequence_photo_count,
    video_duration_seconds,
    av_readable,
    is_animated_gif,
    is_companion_still_for_gif,
    companion_still_relative_path
  )

readr::write_csv(inventory_output, main_inventory_file, na = "")

manual_review <- inventory_output %>%
  filter(needs_manual_date_review) %>%
  select(
    inventory_order,
    event_id,
    media_type,
    relative_path,
    date_resolution_class,
    effective_capture_time_local,
    effective_capture_source,
    metadata_capture_time_local,
    metadata_capture_source,
    metadata_capture_raw,
    name_capture_time_local,
    name_capture_precision,
    name_capture_source,
    name_capture_match,
    name_minus_metadata_seconds,
    implausible_metadata_fields,
    metadata_internal_spread_hours,
    date_review_reason
  )

readr::write_csv(manual_review, manual_review_file, na = "")

video_inventory <- inventory_output %>%
  filter(media_type == "video") %>%
  select(
    inventory_order,
    event_id,
    relative_path,
    initial_media_treatment,
    video_duration_seconds,
    av_readable,
    av_probe_error,
    av_width,
    av_height,
    av_framerate,
    av_video_codec,
    av_has_audio,
    av_audio_codec,
    effective_capture_time_local,
    effective_capture_source,
    date_resolution_class,
    date_reliability,
    needs_manual_date_review,
    implausible_metadata_fields
  )

readr::write_csv(video_inventory, video_file, na = "")

sequence_summary <- sequence_headers %>%
  select(
    sequence_id,
    effective_capture_day,
    sequence_device_key,
    sequence_class,
    sequence_photo_count,
    sequence_duration_seconds,
    sequence_max_internal_gap_seconds,
    sequence_median_internal_gap_seconds,
    suggested_sequence_fps
  )

readr::write_csv(sequence_summary, sequence_file, na = "")
readr::write_csv(event_plan, event_plan_file, na = "")
readr::write_csv(event_members, event_members_file, na = "")

manual_event_template <- event_plan %>%
  transmute(
    event_id,
    event_capture_time_local,
    event_type,
    member_count,
    first_relative_path,
    suggested_movie_treatment,
    date_reliability,
    needs_manual_date_review,
    editorial_review_required,
    include_event = "YES",
    manual_capture_datetime_local = "",
    manual_treatment = "",
    manual_sequence_fps = "",
    photo_hold_seconds = "",
    video_start_seconds = "",
    video_end_seconds = "",
    keep_audio = "NO",
    manual_notes = ""
  )

readr::write_csv(manual_event_template, manual_event_file, na = "")

date_resolution_summary <- inventory %>%
  count(
    date_resolution_class,
    date_reliability,
    needs_manual_date_review,
    sort = TRUE,
    name = "file_count"
  )

readr::write_csv(date_resolution_summary, date_summary_file, na = "")

event_type_summary <- event_plan %>%
  count(
    event_type,
    suggested_movie_treatment,
    editorial_review_required,
    sort = TRUE,
    name = "event_count"
  )

readr::write_csv(event_type_summary, event_summary_file, na = "")

summary_lines <- c(
  "PHD MEDIA INVENTORY V2.1 + EVENT PLAN",
  "===================================",
  "",
  paste0("Scanned root: ", root_directory),
  paste0("Output folder: ", output_directory),
  paste0("Capture timezone: ", capture_timezone),
  paste0(
    "Plausible capture-date range: ",
    earliest_plausible_capture_date,
    " to ",
    latest_plausible_capture_date
  ),
  "",
  paste0("Total media files: ", nrow(inventory)),
  paste0("Images: ", sum(inventory$media_type == "image")),
  paste0("Videos: ", sum(inventory$media_type == "video")),
  paste0("Animated GIFs: ", sum(inventory$is_animated_gif)),
  paste0(
    "Animated GIFs paired with companion stills: ",
    sum(inventory$is_animated_gif & inventory$has_companion_still)
  ),
  paste0(
    "Files requiring genuine date review: ",
    sum(inventory$needs_manual_date_review, na.rm = TRUE)
  ),
  paste0(
    "Verified manual date overrides applied: ",
    sum(inventory$manual_date_override_applied, na.rm = TRUE)
  ),
  paste0(
    "UTC/local clock offsets auto-resolved: ",
    sum(inventory$auto_timezone_corrected, na.rm = TRUE)
  ),
  paste0(
    "Files with implausible embedded date fields auto-bypassed: ",
    sum(!is.na(inventory$implausible_metadata_fields))
  ),
  "",
  paste0("Total movie events: ", nrow(event_plan)),
  paste0(
    "Single-photo events: ",
    sum(event_plan$event_type == "SINGLE_PHOTO")
  ),
  paste0(
    "Burst-motion events: ",
    sum(event_plan$event_type == "BURST_MOTION")
  ),
  paste0(
    "Rapid-series events: ",
    sum(event_plan$event_type == "RAPID_SERIES")
  ),
  paste0(
    "Field-progression events: ",
    sum(event_plan$event_type == "FIELD_PROGRESSION")
  ),
  paste0(
    "Animated-GIF events: ",
    sum(event_plan$event_type == "ANIMATED_GIF_EVENT")
  ),
  paste0(
    "Video events: ",
    sum(event_plan$event_type == "VIDEO")
  ),
  "",
  "NEXT FILES TO OPEN",
  "------------------",
  paste0("1. ", basename(manual_review_file), " -- only unresolved date cases"),
  paste0("2. ", basename(event_plan_file), " -- one row per movie event"),
  paste0("3. ", basename(video_file), " -- video durations and technical status"),
  paste0("4. ", basename(manual_event_file), " -- later editorial decisions"),
  "",
  "IMPORTANT",
  "---------",
  "Do not edit the old file-level manual template.",
  "Use the V2.1 event-level template only after the V2.1 outputs have been reviewed.",
  "This script does not alter any source photograph or video."
)

writeLines(summary_lines, summary_file, useBytes = TRUE)

message("\nV2.1 inventory and event plan completed.")
message("Open this summary first:")
message(summary_file)
message("\nThen inspect:")
message(manual_review_file)
message(event_plan_file)
message(video_file)
