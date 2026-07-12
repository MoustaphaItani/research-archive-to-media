# ============================================================
# BUILD A FLEXIBLE-YEAR MIXED-MEDIA PILOT MOVIE
# ============================================================
#
# Purpose
# -------
# This script creates a chronological pilot movie from the corrected V2.1
# event inventory. It can render all available years, one specific year,
# several selected years, or an inclusive range of years.
#
# It includes:
#   * single photographs;
#   * burst-motion, rapid-series, and field-progression photo events;
#   * animated GIFs (while suppressing their duplicate companion stills);
#   * short excerpts from videos;
#   * an automatically inserted title card whenever a new year begins.
#
# Source media are never altered. All output is written to a selection-specific
# pilot folder. Audio is intentionally omitted from this rough-cut version.
#
# EXPANSION-SAFE V3.1 ADDITIONS
#   * event clips are cached by event ID + source signature;
#   * changed events cannot accidentally reuse stale clips;
#   * the previous final movie, logs, timeline, and chapter CSVs are snapshotted
#     before an ALL-years rebuild.
# ============================================================

# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "av",
  "magick",
  "readr",
  "dplyr",
  "stringr",
  "purrr",
  "tibble"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(av)
library(magick)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
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

inventory_directory <- file.path(
  output_root_directory,
  "_movie_inventory_v2_1"
)

dir.create(
  output_root_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# YEAR SWITCH
# -----------
# Choose exactly one of the following styles:
#
#   year_selection <- "ALL"          # every available year
#   year_selection <- 2017L          # one specific year
#   year_selection <- c(2017L, 2020L, 2024L)  # selected years
#   year_selection <- 2017:2020      # inclusive range of years
#
# Character ranges such as "2017-2020" and "2017:2020" also work.
year_selection <- "ALL"

capture_timezone <- "Asia/Beirut"

# A 720p pilot is faster and lighter than a final 1080p render.
output_width <- 1280L
output_height <- 720L
output_fps <- 24L

# Pacing for photographs.
single_photo_hold_seconds <- 0.20
burst_motion_fps <- 15
rapid_series_fps <- 10
field_progression_fps <- 6

# GIF handling.
gif_default_duration_seconds <- 2.5
gif_max_duration_seconds <- 4.0

# Video handling for the silent rough cut.
mini_video_max_seconds <- 15
short_video_max_seconds <- 60
short_video_excerpt_seconds <- 5
long_video_excerpt_seconds <- 8
video_sampling_fps <- output_fps

# This rough cut is deliberately silent. Audio can be curated later.
keep_audio <- FALSE

# Duration of each automatically inserted year title card.
year_title_card_seconds <- 2

# Keep event clips after the final movie is assembled. This is useful for
# diagnosing individual events. Set FALSE later to save disk space.
keep_intermediate_event_clips <- TRUE

# When TRUE, the script rebuilds event clips even if they already exist.
rebuild_existing_event_clips <- FALSE

# Snapshot the previous ALL-years build and editor records before replacement.
# This does not copy the large event-clip cache.
snapshot_previous_all_build <- TRUE

# Optional emergency limit for testing. Keep Inf for the complete selection.
maximum_events <- Inf

# ------------------------------------------------------------
# 2. INPUT AND OUTPUT PATHS
# ------------------------------------------------------------

event_plan_file <- file.path(
  inventory_directory,
  "05_event_level_movie_plan_v2_1.csv"
)

event_members_file <- file.path(
  inventory_directory,
  "06_event_members_v2_1.csv"
)

required_files <- c(event_plan_file, event_members_file)
missing_inputs <- required_files[!file.exists(required_files)]

if (length(missing_inputs) > 0) {
  stop(
    "Required V2.1 inventory output is missing:\n",
    paste(missing_inputs, collapse = "\n"),
    "\nRun the V2.1 inventory script first."
  )
}

# ------------------------------------------------------------
# 3. READ THE CORRECTED EVENT INVENTORY
# ------------------------------------------------------------

event_plan <- readr::read_csv(
  event_plan_file,
  show_col_types = FALSE,
  na = c("", "NA")
)

event_members <- readr::read_csv(
  event_members_file,
  show_col_types = FALSE,
  na = c("", "NA")
)

required_plan_columns <- c(
  "event_id",
  "event_capture_time_local",
  "event_type",
  "event_source_signature",
  "member_count",
  "first_relative_path",
  "video_duration_seconds",
  "needs_manual_date_review"
)

required_member_columns <- c(
  "event_id",
  "inventory_order",
  "effective_capture_time_local",
  "filename_sequence_number",
  "relative_path",
  "media_type",
  "extension",
  "is_animated_gif",
  "is_companion_still_for_gif",
  "needs_manual_date_review"
)

missing_plan_columns <- setdiff(required_plan_columns, names(event_plan))
missing_member_columns <- setdiff(required_member_columns, names(event_members))

if (length(missing_plan_columns) > 0) {
  stop(
    "The event-plan file is missing required columns: ",
    paste(missing_plan_columns, collapse = ", ")
  )
}

if (length(missing_member_columns) > 0) {
  stop(
    "The event-members file is missing required columns: ",
    paste(missing_member_columns, collapse = ", ")
  )
}

parse_local_datetime <- function(x) {
  as.POSIXct(
    x,
    format = "%Y-%m-%d %H:%M:%S",
    tz = capture_timezone
  )
}

event_plan <- event_plan %>%
  mutate(
    event_capture_time_parsed = parse_local_datetime(event_capture_time_local),
    event_year = as.integer(
      format(event_capture_time_parsed, "%Y", tz = capture_timezone)
    )
  )

available_years <- event_plan %>%
  filter(!needs_manual_date_review, !is.na(event_year)) %>%
  distinct(event_year) %>%
  arrange(event_year) %>%
  pull(event_year)

if (length(available_years) == 0) {
  stop("The event inventory contains no eligible dated years.")
}

selection_is_all <-
  is.character(year_selection) &&
  length(year_selection) == 1 &&
  identical(toupper(trimws(year_selection)), "ALL")

resolve_year_selection <- function(selection, available) {
  if (
    is.character(selection) &&
    length(selection) == 1 &&
    identical(toupper(trimws(selection)), "ALL")
  ) {
    return(available)
  }

  requested <- NULL

  if (is.numeric(selection)) {
    requested <- as.integer(selection)
  } else if (is.character(selection)) {
    cleaned <- trimws(selection)

    if (length(cleaned) == 1 && grepl("^[0-9]{4}[[:space:]]*[:-][[:space:]]*[0-9]{4}$", cleaned)) {
      bounds <- as.integer(
        strsplit(gsub("[[:space:]]", "", cleaned), "[:-]")[[1]]
      )
      requested <- seq(min(bounds), max(bounds))
    } else {
      pieces <- unlist(strsplit(cleaned, "[,; ]+"))
      requested <- suppressWarnings(as.integer(pieces[nzchar(pieces)]))
    }
  }

  if (is.null(requested) || length(requested) == 0 || any(is.na(requested))) {
    stop(
      "Invalid year_selection. Use \"ALL\", one year such as 2017L, ",
      "a vector such as c(2017L, 2020L), or a range such as 2017:2020."
    )
  }

  requested <- sort(unique(requested))
  unavailable <- setdiff(requested, available)

  if (length(unavailable) > 0) {
    warning(
      "These requested years contain no eligible events and will be skipped: ",
      paste(unavailable, collapse = ", ")
    )
  }

  selected <- intersect(requested, available)

  if (length(selected) == 0) {
    stop(
      "None of the requested years occur in the eligible event inventory. ",
      "Available years are: ",
      paste(available, collapse = ", ")
    )
  }

  selected
}

years_to_render <- resolve_year_selection(year_selection, available_years)

is_contiguous_selection <-
  length(years_to_render) > 1 &&
  identical(years_to_render, seq(min(years_to_render), max(years_to_render)))

selection_slug <- if (selection_is_all) {
  "ALL"
} else if (length(years_to_render) == 1) {
  as.character(years_to_render)
} else if (is_contiguous_selection) {
  paste0(min(years_to_render), "_to_", max(years_to_render))
} else {
  paste(years_to_render, collapse = "_")
}

selection_display <- if (selection_is_all) {
  paste0(
    "ALL available years (",
    min(years_to_render),
    "-",
    max(years_to_render),
    ")"
  )
} else if (length(years_to_render) == 1) {
  as.character(years_to_render)
} else if (is_contiguous_selection) {
  paste0(min(years_to_render), "-", max(years_to_render))
} else {
  paste(years_to_render, collapse = ", ")
}

output_directory <- file.path(
  output_root_directory,
  paste0("_movie_pilot_", selection_slug)
)

output_video <- file.path(
  output_directory,
  paste0(
    "Pastoralism_in_Lebanon_",
    selection_slug,
    "_PILOT.mp4"
  )
)

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

event_clip_directory <- file.path(output_directory, "event_clips")
working_directory <- file.path(output_directory, "working_files")
log_directory <- file.path(output_directory, "logs")

dir.create(event_clip_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(working_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(log_directory, recursive = TRUE, showWarnings = FALSE)

# Preserve the previous public-facing build and annotation records before an
# ALL-years rebuild. The event clip directory remains in place as a cache.
if (
  selection_is_all &&
  snapshot_previous_all_build &&
  file.exists(output_video)
) {
  snapshot_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  snapshot_directory <- file.path(
    output_root_directory,
    "_movie_archive_versions",
    paste0("ALL_", snapshot_stamp)
  )

  dir.create(snapshot_directory, recursive = TRUE, showWarnings = FALSE)

  files_to_snapshot <- c(
    output_video,
    file.path(log_directory, "event_render_log.csv"),
    file.path(output_root_directory, "_movie_editor", "timeline_index.csv"),
    file.path(output_root_directory, "_movie_editor", "event_source_links.csv"),
    file.path(output_root_directory, "_movie_editor", "chapter_annotations.csv"),
    file.path(output_root_directory, "_movie_editor", "chapter_event_links.csv"),
    file.path(output_root_directory, "_movie_editor", "chapter_source_links.csv")
  )

  files_to_snapshot <- files_to_snapshot[file.exists(files_to_snapshot)]

  if (length(files_to_snapshot) > 0) {
    file.copy(
      files_to_snapshot,
      file.path(snapshot_directory, basename(files_to_snapshot)),
      overwrite = TRUE
    )
  }

  message("Previous ALL-years build snapshot: ", snapshot_directory)
}

event_plan <- event_plan %>%
  filter(
    event_year %in% years_to_render,
    !needs_manual_date_review
  ) %>%
  arrange(event_capture_time_parsed, event_id)

if (is.finite(maximum_events)) {
  event_plan <- event_plan %>% slice_head(n = as.integer(maximum_events))
}

if (nrow(event_plan) == 0) {
  stop("No eligible events were found for the selected year(s).")
}

event_members <- event_members %>%
  filter(event_id %in% event_plan$event_id) %>%
  mutate(
    effective_capture_time_parsed = parse_local_datetime(
      effective_capture_time_local
    ),
    full_path = file.path(root_directory, relative_path)
  )

missing_media <- event_members %>%
  filter(!file.exists(full_path)) %>%
  mutate(
    path_looks_generated = grepl(
      paste0(
        "(^|/)",
        "(",
        "_movie_inventory[^/]*",
        "|_movie_pilot_[^/]*",
        "|_movie_editor",
        "|_movie_archive_versions",
        ")",
        "(/|$)"
      ),
      gsub("\\\\", "/", relative_path),
      ignore.case = TRUE
    )
  )

if (nrow(missing_media) > 0) {
  missing_media_file <- file.path(
    log_directory,
    "missing_source_media.csv"
  )

  readr::write_csv(
    missing_media,
    missing_media_file,
    na = ""
  )

  message("")
  message("MISSING MEDIA DETECTED")
  message("----------------------")

  for (i in seq_len(nrow(missing_media))) {
    message(
      i,
      ". event_id = ",
      missing_media$event_id[i],
      " | relative_path = ",
      missing_media$relative_path[i],
      " | expected = ",
      missing_media$full_path[i]
    )
  }

  if (any(missing_media$path_looks_generated)) {
    stop(
      nrow(missing_media),
      " missing row(s) were found. At least one path belongs to a generated ",
      "_movie_* folder rather than the original source archive. ",
      "Rerun 02_inventory_and_build_event_plan_v3_1_EXPANSION_SAFE.R, ",
      "then rerun this script. Diagnostic file: ",
      missing_media_file
    )
  }

  stop(
    nrow(missing_media),
    " genuine source-media file(s) are missing or were moved. ",
    "See: ",
    missing_media_file
  )
}

message(
  "Preparing ",
  nrow(event_plan),
  " events for ",
  selection_display,
  "."
)

# ------------------------------------------------------------
# 4. IMAGE HELPERS
# ------------------------------------------------------------

canvas_geometry <- paste0(output_width, "x", output_height)

standardize_magick_image <- function(image_object) {
  image_object %>%
    magick::image_orient() %>%
    magick::image_resize(canvas_geometry) %>%
    magick::image_extent(
      geometry = canvas_geometry,
      gravity = "center",
      color = "black"
    ) %>%
    magick::image_convert(colorspace = "sRGB")
}

standardize_image_file <- function(input_file, output_file) {
  image_object <- magick::image_read(input_file)

  if (length(image_object) > 1) {
    image_object <- image_object[1]
  }

  image_object <- standardize_magick_image(image_object)

  magick::image_write(
    image_object,
    path = output_file,
    format = "jpeg",
    quality = 88
  )

  normalizePath(output_file, winslash = "/", mustWork = TRUE)
}

write_magick_frame <- function(image_object, output_file) {
  image_object <- standardize_magick_image(image_object)

  magick::image_write(
    image_object,
    path = output_file,
    format = "jpeg",
    quality = 88
  )

  normalizePath(output_file, winslash = "/", mustWork = TRUE)
}

make_title_card <- function(year_value, output_file) {
  card <- magick::image_blank(
    width = output_width,
    height = output_height,
    color = "black"
  ) %>%
    magick::image_annotate(
      text = as.character(year_value),
      size = round(output_height * 0.13),
      gravity = "center",
      color = "white"
    ) %>%
    magick::image_annotate(
      text = "Pastoral fieldwork in Lebanon",
      size = round(output_height * 0.038),
      gravity = "south",
      color = "white",
      location = "+0+110"
    )

  magick::image_write(card, output_file, format = "jpeg", quality = 92)
  normalizePath(output_file, winslash = "/", mustWork = TRUE)
}

encode_still_frames <- function(frame_files, output_file, input_fps) {
  av::av_encode_video(
    input = frame_files,
    output = output_file,
    framerate = input_fps,
    vfilter = paste0("fps=", output_fps, ",format=yuv420p"),
    codec = "libx264",
    verbose = FALSE
  )

  normalizePath(output_file, winslash = "/", mustWork = TRUE)
}

# ------------------------------------------------------------
# 5. EVENT RENDERERS
# ------------------------------------------------------------

render_single_photo_event <- function(event_row, members, event_work_dir, clip_file) {
  source_file <- members$full_path[1]
  frame_file <- file.path(event_work_dir, "single_photo.jpg")
  frame_file <- standardize_image_file(source_file, frame_file)

  frame_count <- max(
    1L,
    as.integer(round(single_photo_hold_seconds * output_fps))
  )

  encode_still_frames(
    frame_files = rep(frame_file, frame_count),
    output_file = clip_file,
    input_fps = output_fps
  )
}

render_sequence_event <- function(event_row, members, event_work_dir, clip_file) {
  members <- members %>%
    arrange(
      effective_capture_time_parsed,
      filename_sequence_number,
      inventory_order,
      relative_path
    )

  sequence_input_fps <- switch(
    event_row$event_type,
    "BURST_MOTION" = burst_motion_fps,
    "RAPID_SERIES" = rapid_series_fps,
    "FIELD_PROGRESSION" = field_progression_fps,
    field_progression_fps
  )

  frame_files <- character(nrow(members))

  for (i in seq_len(nrow(members))) {
    frame_file <- file.path(
      event_work_dir,
      sprintf("sequence_%05d.jpg", i)
    )

    frame_files[i] <- standardize_image_file(
      members$full_path[i],
      frame_file
    )
  }

  encode_still_frames(
    frame_files = frame_files,
    output_file = clip_file,
    input_fps = sequence_input_fps
  )
}

render_gif_event <- function(event_row, members, event_work_dir, clip_file) {
  gif_rows <- members %>%
    filter(is_animated_gif %in% TRUE | tolower(extension) == "gif")

  if (nrow(gif_rows) == 0) {
    stop("No animated GIF member was found for ", event_row$event_id)
  }

  gif_frames <- magick::image_read(gif_rows$full_path[1]) %>%
    magick::image_coalesce()

  gif_info <- magick::image_info(gif_frames)
  n_gif_frames <- length(gif_frames)

  if (n_gif_frames < 1) {
    stop("The GIF contains no readable frames: ", gif_rows$full_path[1])
  }

  if (
    "delay" %in% names(gif_info) &&
      any(!is.na(gif_info$delay) & gif_info$delay > 0)
  ) {
    frame_delays <- ifelse(
      is.na(gif_info$delay) | gif_info$delay <= 0,
      1 / output_fps,
      gif_info$delay / 100
    )
  } else {
    frame_delays <- rep(
      gif_default_duration_seconds / n_gif_frames,
      n_gif_frames
    )
  }

  repeat_counts <- pmax(
    1L,
    as.integer(round(frame_delays * output_fps))
  )

  expanded_indices <- rep(seq_len(n_gif_frames), repeat_counts)
  maximum_output_frames <- as.integer(
    round(gif_max_duration_seconds * output_fps)
  )

  if (length(expanded_indices) > maximum_output_frames) {
    selected_positions <- unique(
      round(seq(1, length(expanded_indices), length.out = maximum_output_frames))
    )
    expanded_indices <- expanded_indices[selected_positions]
  }

  unique_indices <- unique(expanded_indices)
  frame_lookup <- character(n_gif_frames)

  for (frame_index in unique_indices) {
    frame_file <- file.path(
      event_work_dir,
      sprintf("gif_%05d.jpg", frame_index)
    )

    frame_lookup[frame_index] <- write_magick_frame(
      gif_frames[frame_index],
      frame_file
    )
  }

  output_frames <- frame_lookup[expanded_indices]

  encode_still_frames(
    frame_files = output_frames,
    output_file = clip_file,
    input_fps = output_fps
  )
}

choose_video_excerpt <- function(duration_seconds) {
  if (is.na(duration_seconds) || duration_seconds <= 0) {
    return(c(start = 0, end = NA_real_))
  }

  if (duration_seconds <= mini_video_max_seconds) {
    return(c(start = 0, end = duration_seconds))
  }

  excerpt_length <- if (
    duration_seconds <= short_video_max_seconds
  ) {
    short_video_excerpt_seconds
  } else {
    long_video_excerpt_seconds
  }

  excerpt_length <- min(excerpt_length, duration_seconds)
  start_time <- max(0, (duration_seconds - excerpt_length) / 2)
  end_time <- min(duration_seconds, start_time + excerpt_length)

  c(start = start_time, end = end_time)
}

render_video_event <- function(event_row, members, event_work_dir, clip_file) {
  video_rows <- members %>% filter(media_type == "video")

  if (nrow(video_rows) == 0) {
    stop("No video member was found for ", event_row$event_id)
  }

  video_file <- video_rows$full_path[1]
  duration_seconds <- suppressWarnings(
    as.numeric(event_row$video_duration_seconds)
  )

  excerpt <- choose_video_excerpt(duration_seconds)

  trim_text <- if (is.na(excerpt["end"])) {
    NULL
  } else {
    sprintf("%.3f:%.3f", excerpt["start"], excerpt["end"])
  }

  extracted_directory <- file.path(event_work_dir, "video_frames_raw")
  standardized_directory <- file.path(event_work_dir, "video_frames_standardized")

  dir.create(extracted_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(standardized_directory, recursive = TRUE, showWarnings = FALSE)

  extracted_frames <- av::av_video_images(
    video = video_file,
    destdir = extracted_directory,
    format = "jpg",
    fps = video_sampling_fps,
    trim = trim_text
  )

  if (length(extracted_frames) == 0) {
    stop("No frames were extracted from video: ", video_file)
  }

  standardized_frames <- character(length(extracted_frames))

  for (i in seq_along(extracted_frames)) {
    output_frame <- file.path(
      standardized_directory,
      sprintf("video_%06d.jpg", i)
    )

    standardized_frames[i] <- standardize_image_file(
      extracted_frames[i],
      output_frame
    )
  }

  # Audio is intentionally omitted. The visual stream is encoded at a common
  # frame rate so that all event clips can be concatenated safely.
  encode_still_frames(
    frame_files = standardized_frames,
    output_file = clip_file,
    input_fps = output_fps
  )
}

render_event <- function(event_row, members) {
  event_id <- event_row$event_id
  event_signature <- substr(
    as.character(event_row$event_source_signature),
    1,
    16
  )

  if (is.na(event_signature) || !nzchar(event_signature)) {
    stop("Missing event_source_signature for ", event_id)
  }

  clip_file <- file.path(
    event_clip_directory,
    paste0(event_id, "__", event_signature, ".mp4")
  )

  if (file.exists(clip_file) && !rebuild_existing_event_clips) {
    return(normalizePath(clip_file, winslash = "/", mustWork = TRUE))
  }

  event_work_dir <- file.path(working_directory, event_id)

  if (dir.exists(event_work_dir)) {
    unlink(event_work_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(event_work_dir, recursive = TRUE, showWarnings = FALSE)

  rendered_clip <- switch(
    event_row$event_type,
    "SINGLE_PHOTO" = render_single_photo_event(
      event_row, members, event_work_dir, clip_file
    ),
    "BURST_MOTION" = render_sequence_event(
      event_row, members, event_work_dir, clip_file
    ),
    "RAPID_SERIES" = render_sequence_event(
      event_row, members, event_work_dir, clip_file
    ),
    "FIELD_PROGRESSION" = render_sequence_event(
      event_row, members, event_work_dir, clip_file
    ),
    "ANIMATED_GIF_EVENT" = render_gif_event(
      event_row, members, event_work_dir, clip_file
    ),
    "VIDEO" = render_video_event(
      event_row, members, event_work_dir, clip_file
    ),
    stop("Unsupported event type: ", event_row$event_type)
  )

  unlink(event_work_dir, recursive = TRUE, force = TRUE)
  rendered_clip
}

# ------------------------------------------------------------
# 6. YEAR TITLE-CARD HELPER
# ------------------------------------------------------------

create_year_title_clip <- function(year_value) {
  title_card_image <- file.path(
    working_directory,
    sprintf("title_%d.jpg", year_value)
  )

  title_card_clip <- file.path(
    event_clip_directory,
    sprintf("00000_TITLE_%d.mp4", year_value)
  )

  if (!file.exists(title_card_clip) || rebuild_existing_event_clips) {
    title_card_image <- make_title_card(year_value, title_card_image)

    title_frame_count <- max(
      1L,
      as.integer(round(year_title_card_seconds * output_fps))
    )

    encode_still_frames(
      frame_files = rep(title_card_image, title_frame_count),
      output_file = title_card_clip,
      input_fps = output_fps
    )
  }

  normalizePath(title_card_clip, winslash = "/", mustWork = TRUE)
}

# ------------------------------------------------------------
# 7. RENDER EACH EVENT
# ------------------------------------------------------------

render_log <- vector("list", nrow(event_plan))
rendered_event_clips <- character(0)

for (i in seq_len(nrow(event_plan))) {
  event_row <- event_plan[i, , drop = FALSE]
  members <- event_members %>% filter(event_id == event_row$event_id)

  message(
    "Rendering event ",
    i,
    " of ",
    nrow(event_plan),
    ": ",
    event_row$event_id,
    " [",
    event_row$event_type,
    "]"
  )

  result <- tryCatch(
    {
      rendered_clip <- render_event(event_row, members)

      list(
        success = TRUE,
        rendered_clip = rendered_clip,
        error_message = NA_character_
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        rendered_clip = NA_character_,
        error_message = conditionMessage(e)
      )
    }
  )

  render_log[[i]] <- tibble(
    render_order = i,
    event_id = event_row$event_id,
    event_source_signature = event_row$event_source_signature,
    event_id_status = if ("event_id_status" %in% names(event_row)) {
      event_row$event_id_status
    } else {
      NA_character_
    },
    event_capture_time_local = event_row$event_capture_time_local,
    event_year = event_row$event_year,
    event_type = event_row$event_type,
    first_relative_path = event_row$first_relative_path,
    success = result$success,
    rendered_clip = result$rendered_clip,
    error_message = result$error_message
  )

  if (isTRUE(result$success)) {
    rendered_event_clips <- c(rendered_event_clips, result$rendered_clip)
  }

  if (i %% 25 == 0 || i == nrow(event_plan)) {
    partial_log <- bind_rows(render_log[seq_len(i)])

    readr::write_csv(
      partial_log,
      file.path(log_directory, "event_render_log.csv"),
      na = ""
    )
  }
}

render_log <- bind_rows(render_log)

readr::write_csv(
  render_log,
  file.path(log_directory, "event_render_log.csv"),
  na = ""
)

failed_events <- render_log %>% filter(!success)

if (nrow(failed_events) > 0) {
  readr::write_csv(
    failed_events,
    file.path(log_directory, "failed_events.csv"),
    na = ""
  )

  warning(
    nrow(failed_events),
    " event(s) failed and were skipped. See logs/failed_events.csv."
  )
}

if (length(rendered_event_clips) == 0) {
  stop("No event clips were rendered successfully.")
}

# ------------------------------------------------------------
# 8. CONCATENATE THE PILOT MOVIE
# ------------------------------------------------------------

successful_events <- render_log %>%
  filter(success) %>%
  arrange(render_order)

clips_to_combine <- character(0)
previous_year <- NA_integer_

for (i in seq_len(nrow(successful_events))) {
  current_year <- as.integer(successful_events$event_year[i])

  if (is.na(previous_year) || current_year != previous_year) {
    clips_to_combine <- c(
      clips_to_combine,
      create_year_title_clip(current_year)
    )
    previous_year <- current_year
  }

  clips_to_combine <- c(
    clips_to_combine,
    successful_events$rendered_clip[i]
  )
}

message(
  "Combining ",
  length(clips_to_combine),
  " clips into the selected pilot..."
)

av::av_encode_video(
  input = clips_to_combine,
  output = output_video,
  framerate = output_fps,
  vfilter = "format=yuv420p",
  codec = "libx264",
  verbose = TRUE
)

# ------------------------------------------------------------
# 9. FINAL REPORT
# ------------------------------------------------------------

pilot_info <- av::av_media_info(output_video)

summary_lines <- c(
  paste0("MIXED-MEDIA PILOT SUMMARY: ", selection_display),
  paste0(rep("=", nchar(paste0("MIXED-MEDIA PILOT SUMMARY: ", selection_display))), collapse = ""),
  "",
  paste0("Output: ", output_video),
  paste0("Year selection requested: ", paste(year_selection, collapse = ", ")),
  paste0("Years actually rendered: ", paste(sort(unique(successful_events$event_year)), collapse = ", ")),
  paste0("Events requested: ", nrow(event_plan)),
  paste0("Events rendered successfully: ", sum(render_log$success)),
  paste0(
    "Signature-addressed event clips used: ",
    dplyr::n_distinct(render_log$rendered_clip[render_log$success])
  ),
  paste0("Events skipped after errors: ", sum(!render_log$success)),
  paste0("Output width: ", output_width),
  paste0("Output height: ", output_height),
  paste0("Output fps: ", output_fps),
  paste0("Audio included: ", keep_audio),
  paste0("Year title-card duration: ", year_title_card_seconds, " s"),
  paste0(
    "Approximate final duration, seconds: ",
    round(as.numeric(pilot_info$duration[1]), 2)
  ),
  "",
  "PACING USED",
  "-----------",
  paste0("Single photograph hold: ", single_photo_hold_seconds, " s"),
  paste0("Burst-motion input speed: ", burst_motion_fps, " fps"),
  paste0("Rapid-series input speed: ", rapid_series_fps, " fps"),
  paste0("Field-progression input speed: ", field_progression_fps, " fps"),
  paste0("Short-video excerpt: ", short_video_excerpt_seconds, " s"),
  paste0("Long-video excerpt: ", long_video_excerpt_seconds, " s"),
  "",
  "NEXT REVIEW QUESTIONS",
  "---------------------",
  "1. Are isolated photographs too fast or too slow?",
  "2. Do the three sequence classes feel meaningfully different?",
  "3. Do portrait images look acceptable on a black canvas?",
  "4. Are the centre excerpts from videos informative?",
  "5. Which videos deserve original audio in the later edit?"
)

summary_output <- file.path(
  output_directory,
  "00_PILOT_RENDER_SUMMARY.txt"
)

writeLines(summary_lines, summary_output, useBytes = TRUE)

if (!keep_intermediate_event_clips) {
  unlink(event_clip_directory, recursive = TRUE, force = TRUE)
}

message("\nPilot movie completed for ", selection_display, ":")
message(output_video)
message("\nRender summary:")
message(summary_output)
