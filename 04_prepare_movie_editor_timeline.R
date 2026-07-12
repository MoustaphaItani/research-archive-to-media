# ============================================================
# 04. PREPARE THE PHD MOVIE EDITOR TIMELINE
# ============================================================
#
# Run this once after successfully rendering the ALL-years pilot.
#
# It creates an exact, reusable timeline index linking:
#   full-movie time -> rendered event clip -> event ID -> original files
#
# Source media and rendered clips are never altered.
# ============================================================

packages <- c("av", "readr", "dplyr", "tibble")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(av)
library(readr)
library(dplyr)
library(tibble)

# ------------------------------------------------------------
# 1. USER SETTINGS
# ------------------------------------------------------------

root_directory <-
  "C:/Users/HP/Desktop/Shared data/PhD_photos/Pastoralism in Lebanon"

inventory_directory <- file.path(
  root_directory,
  "_movie_inventory_v2_1"
)

pilot_directory <- file.path(
  root_directory,
  "_movie_pilot_ALL"
)

pilot_movie <- file.path(
  pilot_directory,
  "Pastoralism_in_Lebanon_ALL_PILOT.mp4"
)

render_log_file <- file.path(
  pilot_directory,
  "logs",
  "event_render_log.csv"
)

event_plan_file <- file.path(
  inventory_directory,
  "05_event_level_movie_plan_v2_1.csv"
)

event_members_file <- file.path(
  inventory_directory,
  "06_event_members_v2_1.csv"
)

editor_directory <- file.path(
  root_directory,
  "_movie_editor"
)

timeline_index_file <- file.path(
  editor_directory,
  "timeline_index.csv"
)

duration_cache_file <- file.path(
  editor_directory,
  "clip_duration_cache.csv"
)

event_source_links_file <- file.path(
  editor_directory,
  "event_source_links.csv"
)

summary_file <- file.path(
  editor_directory,
  "00_EDITOR_PREPARATION_SUMMARY.txt"
)

# Keep TRUE unless you deliberately want to probe every clip again.
reuse_duration_cache <- TRUE

# If the concatenated movie duration differs slightly from the sum of its
# component clips, scale the timeline positions to the real movie duration.
apply_global_drift_correction <- TRUE

# ------------------------------------------------------------
# 2. CHECK INPUTS
# ------------------------------------------------------------

required_files <- c(
  pilot_movie,
  render_log_file,
  event_plan_file,
  event_members_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Required input file(s) are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

dir.create(editor_directory, recursive = TRUE, showWarnings = FALSE)

render_log <- readr::read_csv(
  render_log_file,
  show_col_types = FALSE,
  na = c("", "NA")
)

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

successful_events <- render_log %>%
  filter(success %in% TRUE) %>%
  arrange(render_order)

if (nrow(successful_events) == 0) {
  stop("The render log contains no successful events.")
}

missing_rendered_clips <- successful_events %>%
  filter(is.na(rendered_clip) | !file.exists(rendered_clip))

if (nrow(missing_rendered_clips) > 0) {
  readr::write_csv(
    missing_rendered_clips,
    file.path(editor_directory, "missing_rendered_clips.csv"),
    na = ""
  )

  stop(
    nrow(missing_rendered_clips),
    " rendered event clip(s) are missing. See missing_rendered_clips.csv."
  )
}

# ------------------------------------------------------------
# 3. REUSABLE DURATION CACHE
# ------------------------------------------------------------

file_signature <- function(path) {
  details <- file.info(path)

  tibble(
    clip_path = normalizePath(path, winslash = "/", mustWork = TRUE),
    file_size_bytes = as.numeric(details$size),
    file_modified_numeric = as.numeric(details$mtime)
  )
}

probe_duration <- function(path) {
  info <- av::av_media_info(path)
  as.numeric(info$duration[1])
}

clip_paths <- unique(successful_events$rendered_clip)

title_years <- successful_events %>%
  distinct(event_year) %>%
  arrange(event_year) %>%
  pull(event_year)

title_paths <- file.path(
  pilot_directory,
  "event_clips",
  sprintf("00000_TITLE_%d.mp4", title_years)
)

existing_title_paths <- title_paths[file.exists(title_paths)]
all_component_paths <- unique(c(clip_paths, existing_title_paths))

current_signatures <- bind_rows(
  lapply(all_component_paths, file_signature)
)

if (reuse_duration_cache && file.exists(duration_cache_file)) {
  cached <- readr::read_csv(
    duration_cache_file,
    show_col_types = FALSE,
    na = c("", "NA")
  )
} else {
  cached <- tibble(
    clip_path = character(),
    file_size_bytes = numeric(),
    file_modified_numeric = numeric(),
    duration_seconds = numeric()
  )
}

duration_table <- current_signatures %>%
  left_join(
    cached,
    by = c(
      "clip_path",
      "file_size_bytes",
      "file_modified_numeric"
    )
  )

needs_probe <- is.na(duration_table$duration_seconds)

if (any(needs_probe)) {
  paths_to_probe <- duration_table$clip_path[needs_probe]

  message(
    "Probing ",
    length(paths_to_probe),
    " clip duration(s). This is cached for later runs."
  )

  probed <- numeric(length(paths_to_probe))

  for (i in seq_along(paths_to_probe)) {
    message(
      "Duration ",
      i,
      " of ",
      length(paths_to_probe),
      ": ",
      basename(paths_to_probe[i])
    )

    probed[i] <- tryCatch(
      probe_duration(paths_to_probe[i]),
      error = function(e) {
        warning(
          "Could not read duration for ",
          paths_to_probe[i],
          ": ",
          conditionMessage(e)
        )
        NA_real_
      }
    )
  }

  duration_table$duration_seconds[needs_probe] <- probed
}

if (any(is.na(duration_table$duration_seconds))) {
  failed <- duration_table %>%
    filter(is.na(duration_seconds))

  readr::write_csv(
    failed,
    file.path(editor_directory, "duration_probe_failures.csv"),
    na = ""
  )

  stop(
    nrow(failed),
    " clip duration(s) could not be determined. ",
    "See duration_probe_failures.csv."
  )
}

readr::write_csv(
  duration_table,
  duration_cache_file,
  na = ""
)

duration_lookup <- setNames(
  duration_table$duration_seconds,
  duration_table$clip_path
)

normalized_event_paths <- normalizePath(
  successful_events$rendered_clip,
  winslash = "/",
  mustWork = TRUE
)

successful_events$clip_duration_seconds <-
  unname(duration_lookup[normalized_event_paths])

# ------------------------------------------------------------
# 4. BUILD THE FULL-MOVIE TIMELINE
# ------------------------------------------------------------

timeline_rows <- list()
timeline_counter <- 0L
cursor_seconds <- 0
previous_year <- NA_integer_

for (i in seq_len(nrow(successful_events))) {
  event <- successful_events[i, , drop = FALSE]
  current_year <- as.integer(event$event_year)

  if (is.na(previous_year) || current_year != previous_year) {
    title_path <- file.path(
      pilot_directory,
      "event_clips",
      sprintf("00000_TITLE_%d.mp4", current_year)
    )

    if (!file.exists(title_path)) {
      stop("Missing title-card clip: ", title_path)
    }

    title_path_normalized <- normalizePath(
      title_path,
      winslash = "/",
      mustWork = TRUE
    )

    title_duration <- unname(duration_lookup[title_path_normalized])

    timeline_counter <- timeline_counter + 1L

    timeline_rows[[timeline_counter]] <- tibble(
      timeline_item_order = timeline_counter,
      item_type = "YEAR_TITLE_CARD",
      item_id = paste0("TITLE_", current_year),
      event_id = NA_character_,
      event_year = current_year,
      event_type = "YEAR_TITLE_CARD",
      start_seconds_raw = cursor_seconds,
      end_seconds_raw = cursor_seconds + title_duration,
      duration_seconds_raw = title_duration,
      rendered_clip = title_path_normalized,
      first_relative_path = NA_character_
    )

    cursor_seconds <- cursor_seconds + title_duration
    previous_year <- current_year
  }

  event_duration <- as.numeric(event$clip_duration_seconds)

  timeline_counter <- timeline_counter + 1L

  timeline_rows[[timeline_counter]] <- tibble(
    timeline_item_order = timeline_counter,
    item_type = "EVENT",
    item_id = as.character(event$event_id),
    event_id = as.character(event$event_id),
    event_year = current_year,
    event_type = as.character(event$event_type),
    start_seconds_raw = cursor_seconds,
    end_seconds_raw = cursor_seconds + event_duration,
    duration_seconds_raw = event_duration,
    rendered_clip = normalizePath(
      event$rendered_clip,
      winslash = "/",
      mustWork = TRUE
    ),
    first_relative_path = as.character(event$first_relative_path)
  )

  cursor_seconds <- cursor_seconds + event_duration
}

timeline_index <- bind_rows(timeline_rows)

pilot_duration <- as.numeric(
  av::av_media_info(pilot_movie)$duration[1]
)

raw_component_duration <- max(timeline_index$end_seconds_raw)
duration_difference <- pilot_duration - raw_component_duration

timeline_scale <- 1

if (
  apply_global_drift_correction &&
  is.finite(pilot_duration) &&
  is.finite(raw_component_duration) &&
  raw_component_duration > 0
) {
  timeline_scale <- pilot_duration / raw_component_duration
}

timeline_index <- timeline_index %>%
  mutate(
    timeline_scale_applied = timeline_scale,
    start_seconds = start_seconds_raw * timeline_scale,
    end_seconds = end_seconds_raw * timeline_scale,
    duration_seconds = end_seconds - start_seconds
  ) %>%
  select(
    timeline_item_order,
    item_type,
    item_id,
    event_id,
    event_year,
    event_type,
    start_seconds,
    end_seconds,
    duration_seconds,
    start_seconds_raw,
    end_seconds_raw,
    duration_seconds_raw,
    timeline_scale_applied,
    rendered_clip,
    first_relative_path
  )

readr::write_csv(
  timeline_index,
  timeline_index_file,
  na = ""
)

# ------------------------------------------------------------
# 5. EVENT -> ORIGINAL SOURCE-FILE LINKS
# ------------------------------------------------------------

event_source_links <- event_members %>%
  select(
    event_id,
    inventory_order,
    effective_capture_time_local,
    filename_sequence_number,
    relative_path,
    media_type,
    extension,
    initial_media_treatment,
    sequence_id,
    sequence_class,
    is_animated_gif,
    is_companion_still_for_gif
  ) %>%
  arrange(
    event_id,
    inventory_order,
    relative_path
  )

readr::write_csv(
  event_source_links,
  event_source_links_file,
  na = ""
)

# ------------------------------------------------------------
# 6. SUMMARY
# ------------------------------------------------------------

summary_lines <- c(
  "PHD MOVIE EDITOR PREPARATION SUMMARY",
  "====================================",
  "",
  paste0("Pilot movie: ", pilot_movie),
  paste0("Pilot duration, seconds: ", round(pilot_duration, 3)),
  paste0(
    "Sum of component durations before correction, seconds: ",
    round(raw_component_duration, 3)
  ),
  paste0(
    "Difference before correction, seconds: ",
    round(duration_difference, 3)
  ),
  paste0("Timeline scale applied: ", format(timeline_scale, digits = 12)),
  paste0("Timeline items: ", nrow(timeline_index)),
  paste0(
    "Event items: ",
    sum(timeline_index$item_type == "EVENT")
  ),
  paste0(
    "Year title cards: ",
    sum(timeline_index$item_type == "YEAR_TITLE_CARD")
  ),
  paste0(
    "Original source-file links: ",
    nrow(event_source_links)
  ),
  "",
  paste0("Timeline index: ", timeline_index_file),
  paste0("Event-source links: ", event_source_links_file),
  paste0("Duration cache: ", duration_cache_file),
  "",
  "NEXT STEP",
  "---------",
  "Run 05_phd_movie_editor_app.R."
)

writeLines(summary_lines, summary_file, useBytes = TRUE)

message("\nMovie-editor timeline prepared.")
message(summary_file)
