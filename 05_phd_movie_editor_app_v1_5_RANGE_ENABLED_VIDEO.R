# ============================================================
# 05. PHD MOVIE CHAPTER + SOCIAL EXPORT EDITOR
# ============================================================
#
# A local Shiny application for:
#   1. marking full-movie segments as chapters;
#   2. linking every chapter back to event IDs and source files;
#   3. assigning themes and changing thematic order;
#   4. exporting chapters or custom segments in social-media aspect ratios;
#   5. building a reordered thematic compilation.
#
# Run 04_prepare_movie_editor_timeline.R once before starting this app.
#
# Current exports are silent because the present ALL-years pilot is silent.
#
# V1.5 serves the master MP4 through a custom HTTP byte-range endpoint.
# This is required for reliable seeking in a locally run Shiny application.
#
# Transport controls include:
#   * play/pause;
#   * a draggable seek bar;
#   * direct MM:SS / HH:MM:SS time entry;
#   * +/- 1, 5, and 10 second jumps;
#   * previous/next-frame stepping at 24 fps;
#   * keyboard shortcuts for fast chapter-boundary review.
#
# Possible later additions include selective original audio, chapter title
# cards, captions, and blurred-background vertical formatting.
#
# ============================================================

packages <- c(
  "shiny",
  "DT",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "av"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(shiny)
library(DT)
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(av)

# ------------------------------------------------------------
# 1. USER SETTINGS
# ------------------------------------------------------------

root_directory <-
  "C:/Users/HP/Desktop/Shared data/PhD_photos/Pastoralism in Lebanon"

pilot_directory <- file.path(
  root_directory,
  "_movie_pilot_ALL"
)

pilot_movie <- file.path(
  pilot_directory,
  "Pastoralism_in_Lebanon_ALL_PILOT.mp4"
)

editor_directory <- file.path(
  root_directory,
  "_movie_editor"
)

timeline_index_file <- file.path(
  editor_directory,
  "timeline_index.csv"
)

event_source_links_file <- file.path(
  editor_directory,
  "event_source_links.csv"
)

chapter_file <- file.path(
  editor_directory,
  "chapter_annotations.csv"
)

chapter_event_links_file <- file.path(
  editor_directory,
  "chapter_event_links.csv"
)

chapter_source_links_file <- file.path(
  editor_directory,
  "chapter_source_links.csv"
)

social_export_directory <- file.path(
  editor_directory,
  "social_exports"
)

thematic_export_directory <- file.path(
  editor_directory,
  "thematic_exports"
)

export_manifest_file <- file.path(
  editor_directory,
  "export_manifest.csv"
)

capture_timezone <- "Asia/Beirut"
export_fps <- 24L

# ------------------------------------------------------------
# 2. INPUT CHECKS AND STATIC MEDIA PATHS
# ------------------------------------------------------------

required_files <- c(
  pilot_movie,
  timeline_index_file,
  event_source_links_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Required editor input(s) are missing:\n",
    paste(missing_files, collapse = "\n"),
    "\nRun 04_prepare_movie_editor_timeline.R first."
  )
}

dir.create(editor_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(social_export_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(thematic_export_directory, recursive = TRUE, showWarnings = FALSE)

event_clip_directory <- file.path(
  pilot_directory,
  "event_clips"
)

shiny::addResourcePath(
  "pilot_media",
  normalizePath(pilot_directory, winslash = "/", mustWork = TRUE)
)

shiny::addResourcePath(
  "event_clips",
  normalizePath(event_clip_directory, winslash = "/", mustWork = TRUE)
)

shiny::addResourcePath(
  "source_media",
  normalizePath(root_directory, winslash = "/", mustWork = TRUE)
)

shiny::addResourcePath(
  "editor_exports",
  normalizePath(editor_directory, winslash = "/", mustWork = TRUE)
)

encode_path_parts <- function(path) {
  normalized <- gsub("\\\\", "/", path)
  parts <- strsplit(normalized, "/", fixed = TRUE)[[1]]
  paste(
    vapply(
      parts,
      function(x) URLencode(x, reserved = TRUE),
      character(1)
    ),
    collapse = "/"
  )
}


# ------------------------------------------------------------
# 3. DATA
# ------------------------------------------------------------

timeline_index <- readr::read_csv(
  timeline_index_file,
  show_col_types = FALSE,
  na = c("", "NA")
) %>%
  arrange(timeline_item_order)

event_source_links <- readr::read_csv(
  event_source_links_file,
  show_col_types = FALSE,
  na = c("", "NA")
)

pilot_duration <- as.numeric(
  av::av_media_info(pilot_movie)$duration[1]
)

empty_chapters <- function() {
  tibble(
    chapter_id = character(),
    chapter_order = integer(),
    chapter_title = character(),
    theme = character(),
    tags = character(),
    start_seconds = numeric(),
    end_seconds = numeric(),
    duration_seconds = numeric(),
    sharing_status = character(),
    sensitive_content = logical(),
    include_in_thematic_compilation = logical(),
    event_count = integer(),
    source_file_count = integer(),
    notes = character(),
    created_at = character(),
    updated_at = character()
  )
}

load_chapters <- function() {
  if (!file.exists(chapter_file)) {
    return(empty_chapters())
  }

  loaded <- readr::read_csv(
    chapter_file,
    show_col_types = FALSE,
    na = c("", "NA")
  )

  template <- empty_chapters()

  missing_columns <- setdiff(
    names(template),
    names(loaded)
  )

  for (column_name in missing_columns) {
    template_column <- template[[column_name]]

    if (is.logical(template_column)) {
      loaded[[column_name]] <- rep(NA, nrow(loaded))
    } else if (is.integer(template_column)) {
      loaded[[column_name]] <- rep(NA_integer_, nrow(loaded))
    } else if (is.numeric(template_column)) {
      loaded[[column_name]] <- rep(NA_real_, nrow(loaded))
    } else {
      loaded[[column_name]] <- rep(NA_character_, nrow(loaded))
    }
  }

  loaded %>%
    select(all_of(names(template))) %>%
    arrange(chapter_order, start_seconds)
}

format_seconds <- function(seconds) {
  if (length(seconds) == 0 || is.na(seconds) || !is.finite(seconds)) {
    return("--:--.---")
  }

  seconds <- max(0, seconds)
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  remaining <- seconds %% 60

  if (hours > 0) {
    sprintf("%02d:%02d:%06.3f", hours, minutes, remaining)
  } else {
    sprintf("%02d:%06.3f", minutes, remaining)
  }
}

parse_timecode <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return(NA_real_)
  }

  x <- trimws(as.character(x))

  if (!nzchar(x)) {
    return(NA_real_)
  }

  # Plain numeric input is interpreted as seconds.
  if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
    return(suppressWarnings(as.numeric(x)))
  }

  pieces <- strsplit(x, ":", fixed = TRUE)[[1]]
  pieces <- trimws(pieces)

  if (
    length(pieces) < 2 ||
    length(pieces) > 3 ||
    any(!grepl("^[0-9]+(\\.[0-9]+)?$", pieces))
  ) {
    return(NA_real_)
  }

  values <- suppressWarnings(as.numeric(pieces))

  if (any(!is.finite(values))) {
    return(NA_real_)
  }

  if (length(values) == 2) {
    minutes <- values[1]
    seconds <- values[2]

    if (seconds >= 60) {
      return(NA_real_)
    }

    return(minutes * 60 + seconds)
  }

  hours <- values[1]
  minutes <- values[2]
  seconds <- values[3]

  if (minutes >= 60 || seconds >= 60) {
    return(NA_real_)
  }

  hours * 3600 + minutes * 60 + seconds
}

sanitize_filename <- function(x) {
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x)) {
    x <- "movie_segment"
  }

  x
}

events_overlapping <- function(segment_start, segment_end) {
  timeline_index %>%
    filter(
      item_type == "EVENT",
      .data$start_seconds < segment_end,
      .data$end_seconds > segment_start
    ) %>%
    arrange(timeline_item_order)
}

source_files_for_events <- function(event_ids) {
  if (length(event_ids) == 0) {
    return(event_source_links[0, , drop = FALSE])
  }

  event_source_links %>%
    filter(event_id %in% event_ids) %>%
    arrange(event_id, inventory_order, relative_path)
}

chapter_with_counts <- function(chapter_row) {
  overlapping <- events_overlapping(
    chapter_row$start_seconds,
    chapter_row$end_seconds
  )

  source_files <- source_files_for_events(
    overlapping$event_id
  )

  chapter_row$event_count <- nrow(overlapping)
  chapter_row$source_file_count <- nrow(source_files)
  chapter_row$duration_seconds <-
    chapter_row$end_seconds - chapter_row$start_seconds

  chapter_row
}

save_chapter_system <- function(chapters) {
  chapters <- chapters %>%
    arrange(chapter_order, start_seconds) %>%
    mutate(chapter_order = row_number())

  readr::write_csv(
    chapters,
    chapter_file,
    na = ""
  )

  if (nrow(chapters) == 0) {
    chapter_event_links <- tibble(
      chapter_id = character(),
      chapter_order = integer(),
      event_id = character(),
      event_year = integer(),
      event_type = character(),
      event_start_seconds = numeric(),
      event_end_seconds = numeric()
    )

    chapter_source_links <- tibble(
      chapter_id = character(),
      chapter_order = integer(),
      event_id = character(),
      relative_path = character(),
      media_type = character(),
      extension = character()
    )
  } else {
    event_link_rows <- vector("list", nrow(chapters))
    source_link_rows <- vector("list", nrow(chapters))

    for (i in seq_len(nrow(chapters))) {
      chapter <- chapters[i, , drop = FALSE]

      overlapping <- events_overlapping(
        chapter$start_seconds,
        chapter$end_seconds
      )

      event_link_rows[[i]] <- overlapping %>%
        transmute(
          chapter_id = chapter$chapter_id,
          chapter_order = chapter$chapter_order,
          event_id,
          event_year,
          event_type,
          event_start_seconds = start_seconds,
          event_end_seconds = end_seconds
        )

      source_link_rows[[i]] <- source_files_for_events(
        overlapping$event_id
      ) %>%
        transmute(
          chapter_id = chapter$chapter_id,
          chapter_order = chapter$chapter_order,
          event_id,
          relative_path,
          media_type,
          extension
        )
    }

    chapter_event_links <- bind_rows(event_link_rows)
    chapter_source_links <- bind_rows(source_link_rows)
  }

  readr::write_csv(
    chapter_event_links,
    chapter_event_links_file,
    na = ""
  )

  readr::write_csv(
    chapter_source_links,
    chapter_source_links_file,
    na = ""
  )

  invisible(chapters)
}

build_video_filter <- function(preset, framing) {
  dimensions <- switch(
    preset,
    "VERTICAL_9_16" = c(width = 1080, height = 1920),
    "SQUARE_1_1" = c(width = 1080, height = 1080),
    "LANDSCAPE_16_9" = c(width = 1280, height = 720),
    c(width = 1280, height = 720)
  )

  width <- dimensions[["width"]]
  height <- dimensions[["height"]]

  if (identical(framing, "CENTER_CROP")) {
    paste0(
      "scale=", width, ":", height,
      ":force_original_aspect_ratio=increase,",
      "crop=", width, ":", height, ",",
      "setsar=1,format=yuv420p"
    )
  } else {
    paste0(
      "scale=", width, ":", height,
      ":force_original_aspect_ratio=decrease,",
      "pad=", width, ":", height,
      ":(ow-iw)/2:(oh-ih)/2:black,",
      "setsar=1,format=yuv420p"
    )
  }
}

extract_and_encode_segment <- function(
  start_seconds,
  end_seconds,
  output_file,
  preset,
  framing,
  progress = NULL,
  progress_detail = "Extracting frames"
) {
  if (
    !is.finite(start_seconds) ||
    !is.finite(end_seconds) ||
    end_seconds <= start_seconds
  ) {
    stop("The export end must be later than its start.")
  }

  start_seconds <- max(0, start_seconds)
  end_seconds <- min(pilot_duration, end_seconds)

  temporary_directory <- tempfile("movie_editor_export_")
  raw_frame_directory <- file.path(
    temporary_directory,
    "raw_frames"
  )

  dir.create(
    raw_frame_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    unlink(temporary_directory, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  if (!is.null(progress)) {
    progress$set(
      message = progress_detail,
      detail = paste(
        format_seconds(start_seconds),
        "to",
        format_seconds(end_seconds)
      ),
      value = 0.15
    )
  }

  trim_text <- sprintf(
    "%.3f:%.3f",
    start_seconds,
    end_seconds
  )

  frames <- av::av_video_images(
    video = pilot_movie,
    destdir = raw_frame_directory,
    format = "jpg",
    fps = export_fps,
    trim = trim_text
  )

  if (length(frames) == 0) {
    stop("No frames were extracted for the selected segment.")
  }

  if (!is.null(progress)) {
    progress$set(
      message = "Encoding social clip",
      detail = paste(length(frames), "frames"),
      value = 0.55
    )
  }

  av::av_encode_video(
    input = frames,
    output = output_file,
    framerate = export_fps,
    vfilter = build_video_filter(preset, framing),
    codec = "libx264",
    verbose = TRUE
  )

  normalizePath(
    output_file,
    winslash = "/",
    mustWork = TRUE
  )
}

append_export_manifest <- function(new_row) {
  if (file.exists(export_manifest_file)) {
    old <- readr::read_csv(
      export_manifest_file,
      show_col_types = FALSE,
      na = c("", "NA")
    )
  } else {
    old <- new_row[0, , drop = FALSE]
  }

  readr::write_csv(
    bind_rows(old, new_row),
    export_manifest_file,
    na = ""
  )
}


# ------------------------------------------------------------
# 4. RANGE-ENABLED VIDEO ENDPOINT
# ------------------------------------------------------------
#
# Shiny's ordinary static-resource handler does not reliably support HTTP
# byte-range requests for large local MP4 files. Browsers need byte ranges
# to seek. This endpoint returns 206 Partial Content responses and reads only
# the requested chunk from disk.
# ------------------------------------------------------------

serve_video_range <- function(video_path, req) {
  file_details <- file.info(video_path)

  if (
    nrow(file_details) != 1 ||
    is.na(file_details$size) ||
    !file.exists(video_path)
  ) {
    return(
      shiny::httpResponse(
        status = 404L,
        content_type = "text/plain",
        content = "Video file not found."
      )
    )
  }

  file_size <- as.numeric(file_details$size)
  request_method <- if (!is.null(req$REQUEST_METHOD)) {
    toupper(req$REQUEST_METHOD)
  } else {
    "GET"
  }

  range_header <- req$HTTP_RANGE

  response_headers <- list(
    "Accept-Ranges" = "bytes",
    "Cache-Control" = "no-store, no-cache, must-revalidate",
    "Access-Control-Expose-Headers" = "Accept-Ranges, Content-Range"
  )

  if (identical(request_method, "HEAD")) {
    response_headers[["Content-Length"]] <- as.character(file_size)

    return(
      list(
        status = 200L,
        headers = c(
          list("Content-Type" = "video/mp4"),
          response_headers
        ),
        body = raw(0)
      )
    )
  }

  # Keep each response modest so the full movie is never loaded into R memory.
  maximum_chunk_bytes <- 4 * 1024 * 1024

  if (is.null(range_header) || !nzchar(range_header)) {
    range_start <- 0
    range_end <- min(
      file_size - 1,
      maximum_chunk_bytes - 1
    )
  } else {
    range_text <- sub(
      "^bytes=",
      "",
      range_header,
      ignore.case = TRUE
    )

    # Multiple ranges are not needed by the browser video element.
    range_text <- strsplit(
      range_text,
      ",",
      fixed = TRUE
    )[[1]][1]

    range_text <- trimws(range_text)

    range_match <- regexec(
      "^([0-9]*)-([0-9]*)$",
      range_text
    )

    range_groups <- regmatches(
      range_text,
      range_match
    )[[1]]

    if (length(range_groups) != 3) {
      return(
        list(
          status = 416L,
          headers = list(
            "Content-Type" = "text/plain",
            "Accept-Ranges" = "bytes",
            "Content-Range" = paste0(
              "bytes */",
              format(file_size, scientific = FALSE)
            )
          ),
          body = charToRaw("Invalid byte range.")
        )
      )
    }

    start_text <- range_groups[2]
    end_text <- range_groups[3]

    if (!nzchar(start_text)) {
      # Suffix range, for example bytes=-500.
      suffix_length <- suppressWarnings(
        as.numeric(end_text)
      )

      if (
        !is.finite(suffix_length) ||
        suffix_length <= 0
      ) {
        return(
          list(
            status = 416L,
            headers = list(
              "Content-Type" = "text/plain",
              "Accept-Ranges" = "bytes",
              "Content-Range" = paste0(
                "bytes */",
                format(file_size, scientific = FALSE)
              )
            ),
            body = charToRaw("Invalid suffix range.")
          )
        )
      }

      suffix_length <- min(
        suffix_length,
        file_size
      )

      range_start <- file_size - suffix_length
      range_end <- file_size - 1
    } else {
      range_start <- suppressWarnings(
        as.numeric(start_text)
      )

      if (
        !is.finite(range_start) ||
        range_start < 0 ||
        range_start >= file_size
      ) {
        return(
          list(
            status = 416L,
            headers = list(
              "Content-Type" = "text/plain",
              "Accept-Ranges" = "bytes",
              "Content-Range" = paste0(
                "bytes */",
                format(file_size, scientific = FALSE)
              )
            ),
            body = charToRaw("Requested range is outside the file.")
          )
        )
      }

      requested_end <- if (nzchar(end_text)) {
        suppressWarnings(as.numeric(end_text))
      } else {
        file_size - 1
      }

      if (
        !is.finite(requested_end) ||
        requested_end < range_start
      ) {
        requested_end <- file_size - 1
      }

      range_end <- min(
        requested_end,
        file_size - 1,
        range_start + maximum_chunk_bytes - 1
      )
    }
  }

  bytes_to_read <- range_end - range_start + 1

  connection <- file(
    video_path,
    open = "rb"
  )

  on.exit(
    close(connection),
    add = TRUE
  )

  seek(
    connection,
    where = range_start,
    origin = "start"
  )

  content <- readBin(
    connection,
    what = "raw",
    n = as.integer(bytes_to_read)
  )

  actual_end <- range_start + length(content) - 1

  if (length(content) == 0) {
    return(
      list(
        status = 416L,
        headers = list(
          "Content-Type" = "text/plain",
          "Accept-Ranges" = "bytes",
          "Content-Range" = paste0(
            "bytes */",
            format(file_size, scientific = FALSE)
          )
        ),
        body = charToRaw("No bytes could be read.")
      )
    )
  }

  list(
    status = 206L,
    headers = c(
      list(
        "Content-Type" = "video/mp4",
        "Content-Range" = paste0(
          "bytes ",
          format(range_start, scientific = FALSE),
          "-",
          format(actual_end, scientific = FALSE),
          "/",
          format(file_size, scientific = FALSE)
        )
      ),
      response_headers
    ),
    body = content
  )
}

# ------------------------------------------------------------
# 5. JAVASCRIPT
# ------------------------------------------------------------

movie_javascript <- "
(function () {
  'use strict';

  var video = null;
  var videoEventsBound = false;
  var shinyHandlerBound = false;
  var frameStep = 1 / 24;

  function byId(id) {
    return document.getElementById(id);
  }

  function findVideo() {
    video = byId('main_movie_player');
    return video;
  }

  function setStatus(message, className) {
    var status = byId('movie_transport_status');

    if (!status) {
      return;
    }

    status.textContent = message;
    status.className = 'transport-status ' + className;
  }

  function formatTime(seconds) {
    if (!Number.isFinite(seconds)) {
      return '--:--.---';
    }

    seconds = Math.max(0, seconds);

    var hours = Math.floor(seconds / 3600);
    var minutes = Math.floor((seconds % 3600) / 60);
    var remainder = seconds % 60;

    var minuteText = String(minutes).padStart(2, '0');
    var secondText = remainder.toFixed(3).padStart(6, '0');

    if (hours > 0) {
      return String(hours).padStart(2, '0') +
        ':' + minuteText + ':' + secondText;
    }

    return minuteText + ':' + secondText;
  }

  function reportClock() {
    if (
      !video ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== 'function'
    ) {
      return;
    }

    window.Shiny.setInputValue(
      'movie_clock',
      {
        time: video.currentTime || 0,
        duration: video.duration,
        paused: video.paused,
        ready_state: video.readyState,
        seekable_ranges: video.seekable ? video.seekable.length : 0,
        nonce: Date.now()
      },
      {priority: 'event'}
    );
  }

  function updateDisplay() {
    if (!video && !findVideo()) {
      return;
    }

    var slider = byId('movie_seek_slider');
    var currentLabel = byId('movie_current_time_label');
    var durationLabel = byId('movie_duration_label');
    var playButton = byId('movie_play_pause_button');

    if (slider) {
      if (Number.isFinite(video.duration) && video.duration > 0) {
        slider.max = String(video.duration);
      }

      if (document.activeElement !== slider) {
        slider.value = String(video.currentTime || 0);
      }
    }

    if (currentLabel) {
      currentLabel.textContent = formatTime(video.currentTime || 0);
    }

    if (durationLabel) {
      durationLabel.textContent = formatTime(video.duration);
    }

    if (playButton) {
      playButton.textContent = video.paused ? 'Play' : 'Pause';
    }

    if (video.readyState >= 1) {
      if (video.seekable && video.seekable.length > 0) {
        setStatus(
          'Ready — range-enabled movie loaded and seekable',
          'transport-ready'
        );
      } else {
        setStatus(
          'Movie loaded; establishing seek range…',
          'transport-warning'
        );
      }
    }
  }

  function bindVideoEvents() {
    if (!findVideo() || videoEventsBound) {
      return;
    }

    videoEventsBound = true;

    [
      'loadedmetadata',
      'durationchange',
      'canplay',
      'progress',
      'timeupdate',
      'seeked',
      'play',
      'pause',
      'ended'
    ].forEach(function (eventName) {
      video.addEventListener(eventName, function () {
        updateDisplay();

        if (
          eventName === 'loadedmetadata' ||
          eventName === 'timeupdate' ||
          eventName === 'seeked'
        ) {
          reportClock();
        }
      });
    });

    video.addEventListener('error', function () {
      var code = video.error ? video.error.code : 'unknown';

      setStatus(
        'Video error code ' + code,
        'transport-error'
      );
    });

    updateDisplay();
    reportClock();
  }

  function clampTime(value) {
    var target = Number(value);

    if (!Number.isFinite(target)) {
      return null;
    }

    target = Math.max(0, target);

    if (
      video &&
      Number.isFinite(video.duration) &&
      video.duration > 0
    ) {
      target = Math.min(target, video.duration);
    }

    return target;
  }

  function seekTo(value) {
    bindVideoEvents();

    if (!video) {
      setStatus(
        'Movie player is not ready',
        'transport-error'
      );
      return;
    }

    var target = clampTime(value);

    if (target === null) {
      setStatus(
        'Invalid target time',
        'transport-error'
      );
      return;
    }

    video.pause();

    try {
      video.currentTime = target;
    } catch (error) {
      setStatus(
        'Seek failed: ' + error.message,
        'transport-error'
      );
      return;
    }

    updateDisplay();

    window.setTimeout(function () {
      updateDisplay();
      reportClock();
    }, 100);
  }

  function playPause() {
    bindVideoEvents();

    if (!video) {
      return;
    }

    if (video.paused) {
      var promise = video.play();

      if (promise && typeof promise.catch === 'function') {
        promise.catch(function (error) {
          setStatus(
            'Play failed: ' + error.message,
            'transport-error'
          );
        });
      }
    } else {
      video.pause();
    }

    updateDisplay();
  }

  function parseTimecode(value) {
    var text = String(value || '').trim();

    if (text === '') {
      return null;
    }

    if (/^[0-9]+(?:[.][0-9]+)?$/.test(text)) {
      return Number(text);
    }

    var pieces = text.split(':').map(function (piece) {
      return Number(piece.trim());
    });

    if (
      pieces.length < 2 ||
      pieces.length > 3 ||
      pieces.some(function (piece) {
        return !Number.isFinite(piece);
      })
    ) {
      return null;
    }

    if (pieces.length === 2) {
      if (pieces[1] >= 60) {
        return null;
      }

      return pieces[0] * 60 + pieces[1];
    }

    if (pieces[1] >= 60 || pieces[2] >= 60) {
      return null;
    }

    return pieces[0] * 3600 +
      pieces[1] * 60 +
      pieces[2];
  }

  function sendMarker(kind) {
    bindVideoEvents();

    if (
      !video ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== 'function'
    ) {
      return;
    }

    window.Shiny.setInputValue(
      'movie_marker_capture',
      {
        kind: kind,
        time: video.currentTime || 0,
        nonce: Date.now()
      },
      {priority: 'event'}
    );
  }

  document.addEventListener('click', function (event) {
    var id = event.target ? event.target.id : '';

    if (!id) {
      return;
    }

    bindVideoEvents();

    if (id === 'movie_play_pause_button') {
      event.preventDefault();
      playPause();
    }

    if (id === 'movie_back_10_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) - 10);
    }

    if (id === 'movie_back_5_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) - 5);
    }

    if (id === 'movie_back_1_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) - 1);
    }

    if (id === 'movie_previous_frame_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) - frameStep);
    }

    if (id === 'movie_next_frame_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) + frameStep);
    }

    if (id === 'movie_forward_1_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) + 1);
    }

    if (id === 'movie_forward_5_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) + 5);
    }

    if (id === 'movie_forward_10_button') {
      event.preventDefault();
      seekTo((video.currentTime || 0) + 10);
    }

    if (id === 'jump_to_timecode_button') {
      event.preventDefault();

      var input = byId('jump_timecode');
      var parsed = input ? parseTimecode(input.value) : null;

      if (parsed === null) {
        setStatus(
          'Use seconds, MM:SS, or HH:MM:SS',
          'transport-error'
        );
      } else {
        seekTo(parsed);
      }
    }

    if (id === 'mark_chapter_start_button') {
      event.preventDefault();
      sendMarker('START');
    }

    if (id === 'mark_chapter_end_button') {
      event.preventDefault();
      sendMarker('END');
    }
  });

  document.addEventListener('input', function (event) {
    if (
      event.target &&
      event.target.id === 'movie_seek_slider'
    ) {
      var label = byId('movie_current_time_label');

      if (label) {
        label.textContent =
          formatTime(Number(event.target.value));
      }
    }
  });

  document.addEventListener('change', function (event) {
    if (
      event.target &&
      event.target.id === 'movie_seek_slider'
    ) {
      seekTo(Number(event.target.value));
    }
  });

  document.addEventListener('keydown', function (event) {
    var activeTag = document.activeElement ?
      document.activeElement.tagName.toLowerCase() :
      '';

    if (
      activeTag === 'input' ||
      activeTag === 'textarea' ||
      activeTag === 'select'
    ) {
      return;
    }

    bindVideoEvents();

    if (!video) {
      return;
    }

    if (event.code === 'Space') {
      event.preventDefault();
      playPause();
    }

    if (event.key === 'ArrowLeft') {
      event.preventDefault();
      seekTo(
        (video.currentTime || 0) +
        (event.shiftKey ? -10 : -1)
      );
    }

    if (event.key === 'ArrowRight') {
      event.preventDefault();
      seekTo(
        (video.currentTime || 0) +
        (event.shiftKey ? 10 : 1)
      );
    }

    if (event.key === ',') {
      event.preventDefault();
      seekTo((video.currentTime || 0) - frameStep);
    }

    if (event.key === '.') {
      event.preventDefault();
      seekTo((video.currentTime || 0) + frameStep);
    }
  });

  function registerShinyHandler() {
    if (
      shinyHandlerBound ||
      !window.Shiny ||
      typeof window.Shiny.addCustomMessageHandler !== 'function'
    ) {
      return;
    }

    shinyHandlerBound = true;

    window.Shiny.addCustomMessageHandler(
      'seekMainMovie',
      function (message) {
        seekTo(Number(message.time));

        if (message.play === true && video) {
          video.play();
        }
      }
    );
  }

  function initialize() {
    setStatus(
      'Controller loaded — waiting for range-enabled movie…',
      'transport-warning'
    );

    bindVideoEvents();
    registerShinyHandler();
  }

  if (document.readyState === 'loading') {
    document.addEventListener(
      'DOMContentLoaded',
      initialize
    );
  } else {
    initialize();
  }

  if (window.jQuery) {
    window.jQuery(document).on(
      'shiny:connected',
      initialize
    );
  }

  window.setInterval(function () {
    initialize();
  }, 500);
})();
"

# ------------------------------------------------------------
# 6. UI
# ------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$style(
      HTML(
        "
          body { background-color: #f7f7f7; }
          .movie-panel {
            background: white;
            padding: 14px;
            border-radius: 8px;
            box-shadow: 0 1px 5px rgba(0,0,0,0.12);
            margin-bottom: 14px;
          }
          .marker-value {
            font-family: monospace;
            font-size: 17px;
            font-weight: 600;
          }
          .context-box {
            padding: 10px;
            background: #eef4f8;
            border-left: 4px solid #4a789c;
            min-height: 74px;
          }
          .warning-box {
            padding: 10px;
            background: #fff4dd;
            border-left: 4px solid #d99614;
          }
          .source-preview img,
          .source-preview video {
            max-width: 100%;
            max-height: 520px;
          }
          .btn-row .btn {
            margin-right: 5px;
            margin-bottom: 5px;
          }
          .transport-time {
            font-family: monospace;
            font-size: 16px;
            font-weight: 600;
          }
          .transport-slider {
            width: 100%;
            margin: 8px 0 12px 0;
          }
          .transport-controls .btn {
            min-width: 62px;
          }
          .jump-controls {
            margin-top: 8px;
          }
          .transport-status {
            display: block;
            margin: 7px 0 4px 0;
            padding: 6px 8px;
            border-radius: 4px;
            font-size: 13px;
          }
          .transport-ready {
            background: #e7f6eb;
            color: #246c35;
          }
          .transport-warning {
            background: #fff4dd;
            color: #7a5700;
          }
          .transport-error {
            background: #fde8e8;
            color: #8a1f1f;
          }
        "
      )
    ),
    tags$script(
      HTML(movie_javascript)
    )
  ),

  navbarPage(
    title = "PhD Movie Editor",
    id = "main_navigation",

    tabPanel(
    "Timeline & chapters",
    fluidRow(
      column(
        width = 8,
        div(
          class = "movie-panel",
          uiOutput("main_video_ui"),
          tags$span(
            id = "movie_transport_status",
            class = "transport-status transport-warning",
            "Initializing movie controls…"
          ),
          div(
            class = "transport-time",
            tags$span(
              id = "movie_current_time_label",
              "00:00.000"
            ),
            tags$span(" / "),
            tags$span(
              id = "movie_duration_label",
              format_seconds(pilot_duration)
            )
          ),
          tags$input(
            id = "movie_seek_slider",
            class = "transport-slider",
            type = "range",
            min = "0",
            max = as.character(round(pilot_duration, 3)),
            step = as.character(1 / export_fps),
            value = "0"
          ),
          div(
            class = "btn-row transport-controls",
            tags$button(
              id = "movie_back_10_button",
              class = "btn btn-default",
              type = "button",
              "−10 s"
            ),
            tags$button(
              id = "movie_back_5_button",
              class = "btn btn-default",
              type = "button",
              "−5 s"
            ),
            tags$button(
              id = "movie_back_1_button",
              class = "btn btn-default",
              type = "button",
              "−1 s"
            ),
            tags$button(
              id = "movie_previous_frame_button",
              class = "btn btn-default",
              type = "button",
              "◀ Frame"
            ),
            tags$button(
              id = "movie_play_pause_button",
              class = "btn btn-primary",
              type = "button",
              "Play"
            ),
            tags$button(
              id = "movie_next_frame_button",
              class = "btn btn-default",
              type = "button",
              "Frame ▶"
            ),
            tags$button(
              id = "movie_forward_1_button",
              class = "btn btn-default",
              type = "button",
              "+1 s"
            ),
            tags$button(
              id = "movie_forward_5_button",
              class = "btn btn-default",
              type = "button",
              "+5 s"
            ),
            tags$button(
              id = "movie_forward_10_button",
              class = "btn btn-default",
              type = "button",
              "+10 s"
            )
          ),
          fluidRow(
            class = "jump-controls",
            column(
              width = 8,
              textInput(
                "jump_timecode",
                "Jump to time",
                value = "00:00.000",
                placeholder = "MM:SS.000 or HH:MM:SS.000"
              )
            ),
            column(
              width = 4,
              br(),
              tags$button(
                id = "jump_to_timecode_button",
                type = "button",
                class = "btn btn-info",
                style = "width:100%;",
                "Go to time"
              )
            )
          ),
          tags$small(
            "Keyboard: Space = play/pause; Left/Right = ±1 s; ",
            "Shift+Left/Right = ±10 s; comma/period = previous/next frame."
          ),
          tags$hr(),
          div(
            class = "btn-row",
            tags$button(
              id = "mark_chapter_start_button",
              class = "btn btn-primary",
              type = "button",
              "Mark chapter start"
            ),
            tags$button(
              id = "mark_chapter_end_button",
              class = "btn btn-primary",
              type = "button",
              "Mark chapter end"
            ),
            actionButton(
              "clear_markers",
              "Clear markers",
              class = "btn-default"
            )
          ),
          fluidRow(
            column(
              6,
              strong("Start"),
              div(
                class = "marker-value",
                textOutput("start_marker_text", inline = TRUE)
              )
            ),
            column(
              6,
              strong("End"),
              div(
                class = "marker-value",
                textOutput("end_marker_text", inline = TRUE)
              )
            )
          )
        ),
        div(
          class = "movie-panel",
          h4("Current timeline context"),
          uiOutput("current_context")
        ),
        div(
          class = "movie-panel",
          h4("Underlying rendered event clip"),
          uiOutput("current_event_clip")
        )
      ),

      column(
        width = 4,
        div(
          class = "movie-panel",
          h4("Chapter annotation"),
          textInput(
            "chapter_title",
            "Chapter title"
          ),
          textInput(
            "chapter_theme",
            "Theme"
          ),
          textInput(
            "chapter_tags",
            "Tags",
            placeholder = "mobility, camps, vegetation"
          ),
          selectInput(
            "sharing_status",
            "Sharing status",
            choices = c(
              "Needs review" = "REVIEW",
              "Safe to share" = "SAFE_TO_SHARE",
              "Private — do not export" = "PRIVATE_DO_NOT_EXPORT"
            ),
            selected = "REVIEW"
          ),
          checkboxInput(
            "sensitive_content",
            "Contains sensitive people, locations, or information",
            value = FALSE
          ),
          checkboxInput(
            "include_thematic",
            "Include in thematic compilation",
            value = TRUE
          ),
          textAreaInput(
            "chapter_notes",
            "Notes",
            rows = 4
          ),
          div(
            class = "btn-row",
            actionButton(
              "add_chapter",
              "Add chapter",
              class = "btn-success"
            ),
            actionButton(
              "update_chapter",
              "Update selected",
              class = "btn-warning"
            ),
            actionButton(
              "delete_chapter",
              "Delete selected",
              class = "btn-danger"
            )
          ),
          div(
            class = "btn-row",
            actionButton(
              "move_chapter_up",
              "Move up"
            ),
            actionButton(
              "move_chapter_down",
              "Move down"
            ),
            actionButton(
              "seek_selected_chapter",
              "Seek to selected"
            )
          )
        )
      )
    ),

    div(
      class = "movie-panel",
      h4("Chapter library"),
      DTOutput("chapter_table")
    )
  ),

  tabPanel(
    "Linked source files",
    fluidRow(
      column(
        width = 7,
        div(
          class = "movie-panel",
          h4("Files linked to the selected chapter"),
          uiOutput("selected_chapter_link_summary"),
          DTOutput("linked_source_table"),
          br(),
          actionButton(
            "open_selected_source",
            "Open selected source file"
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "movie-panel source-preview",
          h4("Source preview"),
          uiOutput("source_preview")
        )
      )
    )
  ),

  tabPanel(
    "Social export",
    fluidRow(
      column(
        width = 5,
        div(
          class = "movie-panel",
          h4("Choose segment"),
          radioButtons(
            "export_source_mode",
            "Source",
            choices = c(
              "Saved chapter" = "CHAPTER",
              "Current start/end markers" = "MARKERS",
              "Custom times" = "CUSTOM"
            ),
            selected = "CHAPTER"
          ),
          selectInput(
            "export_chapter_id",
            "Chapter",
            choices = character()
          ),
          numericInput(
            "export_start_seconds",
            "Start in full movie, seconds",
            value = 0,
            min = 0,
            step = 0.001
          ),
          numericInput(
            "export_end_seconds",
            "End in full movie, seconds",
            value = 15,
            min = 0,
            step = 0.001
          ),
          uiOutput("export_range_text")
        )
      ),

      column(
        width = 4,
        div(
          class = "movie-panel",
          h4("Output format"),
          selectInput(
            "export_preset",
            "Aspect ratio",
            choices = c(
              "Vertical 9:16 — Reels/Stories" = "VERTICAL_9_16",
              "Square 1:1" = "SQUARE_1_1",
              "Landscape 16:9 — X/web" = "LANDSCAPE_16_9"
            ),
            selected = "VERTICAL_9_16"
          ),
          radioButtons(
            "export_framing",
            "Framing",
            choices = c(
              "Fit entire image with bars" = "FIT_WITH_BARS",
              "Fill frame with centre crop" = "CENTER_CROP"
            ),
            selected = "FIT_WITH_BARS"
          ),
          textInput(
            "export_name",
            "Output name",
            value = "pastoralism_segment"
          ),
          checkboxInput(
            "export_review_confirmed",
            "I reviewed this segment for people, sensitive locations, and sharing rights",
            value = FALSE
          ),
          actionButton(
            "export_social_clip",
            "Export social clip",
            class = "btn-success"
          ),
          br(),
          br(),
          uiOutput("latest_export_status"),
          downloadButton(
            "download_latest_export",
            "Download latest export"
          )
        )
      ),

      column(
        width = 3,
        div(
          class = "movie-panel warning-box",
          h4("Important"),
          p(
            "The current pilot is silent, so these exports are silent. ",
            "Audio curation can be added after the visual edit is stable."
          ),
          p(
            "Use “Fit with bars” when preserving the complete scientific ",
            "or documentary frame matters. Centre crop may remove contextual details."
          )
        )
      )
    )
  ),

  tabPanel(
    "Thematic compilation",
    fluidRow(
      column(
        width = 7,
        div(
          class = "movie-panel",
          h4("Compilation order"),
          p(
            "Chapters marked for inclusion are concatenated in chapter order, ",
            "not chronological order."
          ),
          DTOutput("thematic_chapter_table")
        )
      ),
      column(
        width = 5,
        div(
          class = "movie-panel",
          h4("Build reordered movie"),
          textInput(
            "thematic_output_name",
            "Output name",
            value = "Pastoralism_in_Lebanon_THEMATIC"
          ),
          selectInput(
            "thematic_preset",
            "Output aspect ratio",
            choices = c(
              "Landscape 16:9" = "LANDSCAPE_16_9",
              "Vertical 9:16" = "VERTICAL_9_16",
              "Square 1:1" = "SQUARE_1_1"
            ),
            selected = "LANDSCAPE_16_9"
          ),
          radioButtons(
            "thematic_framing",
            "Framing",
            choices = c(
              "Fit entire image with bars" = "FIT_WITH_BARS",
              "Fill frame with centre crop" = "CENTER_CROP"
            ),
            selected = "FIT_WITH_BARS"
          ),
          p(
            "This can take substantial time and temporary disk space because ",
            "the selected chapters are re-extracted frame by frame."
          ),
          actionButton(
            "build_thematic_compilation",
            "Build thematic compilation",
            class = "btn-primary"
          ),
          br(),
          br(),
          uiOutput("latest_thematic_status"),
          downloadButton(
            "download_latest_thematic",
            "Download latest thematic movie"
          )
        )
      )
    )
  ),

  tabPanel(
    "Data export",
    div(
      class = "movie-panel",
      h4("Portable editorial records"),
      p(
        "These CSV files preserve the chapter system and its links to original files."
      ),
      downloadButton(
        "download_chapters_csv",
        "Download chapters CSV"
      ),
      downloadButton(
        "download_chapter_event_links_csv",
        "Download chapter-event links"
      ),
      downloadButton(
        "download_chapter_source_links_csv",
        "Download chapter-source links"
      )
    )
  )
  )
)

# ------------------------------------------------------------
# 7. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  range_enabled_video_url <- session$registerDataObj(
    name = "pilot_movie_range",
    data = pilot_movie,
    filterFunc = serve_video_range
  )

  output$main_video_ui <- renderUI({
    tags$video(
      id = "main_movie_player",
      src = range_enabled_video_url,
      type = "video/mp4",
      controls = NA,
      preload = "metadata",
      playsinline = NA,
      style = "width:100%; background:black;"
    )
  })

  chapters <- reactiveVal(load_chapters())

  markers <- reactiveValues(
    start = NA_real_,
    end = NA_real_
  )

  latest_export <- reactiveVal(NA_character_)
  latest_thematic <- reactiveVal(NA_character_)

  selected_chapter_row <- reactive({
    selected <- input$chapter_table_rows_selected

    if (length(selected) != 1) {
      return(NULL)
    }

    current <- chapters() %>%
      arrange(chapter_order, start_seconds)

    if (selected < 1 || selected > nrow(current)) {
      return(NULL)
    }

    current[selected, , drop = FALSE]
  })

  observeEvent(
    input$movie_marker_capture,
    {
      marker <- input$movie_marker_capture

      if (is.null(marker$kind) || is.null(marker$time)) {
        return()
      }

      marker_time <- as.numeric(marker$time)

      if (!is.finite(marker_time)) {
        return()
      }

      if (identical(marker$kind, "START")) {
        markers$start <- marker_time
      }

      if (identical(marker$kind, "END")) {
        markers$end <- marker_time
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$clear_markers, {
    markers$start <- NA_real_
    markers$end <- NA_real_
  })

  output$start_marker_text <- renderText({
    format_seconds(markers$start)
  })

  output$end_marker_text <- renderText({
    format_seconds(markers$end)
  })

  current_movie_time <- reactive({
    clock <- input$movie_clock

    if (is.null(clock) || is.null(clock$time)) {
      return(0)
    }

    value <- as.numeric(clock$time)

    if (!is.finite(value)) {
      0
    } else {
      value
    }
  })

  current_timeline_item <- reactive({
    current_time <- current_movie_time()

    item <- timeline_index %>%
      filter(
        start_seconds <= current_time,
        end_seconds > current_time
      ) %>%
      slice_head(n = 1)

    if (nrow(item) == 0) {
      NULL
    } else {
      item
    }
  })

  output$current_context <- renderUI({
    item <- current_timeline_item()
    current_time <- current_movie_time()

    if (is.null(item)) {
      return(
        div(
          class = "context-box",
          strong("Movie time: "),
          format_seconds(current_time),
          br(),
          "No indexed item at this position."
        )
      )
    }

    if (identical(item$item_type, "YEAR_TITLE_CARD")) {
      return(
        div(
          class = "context-box",
          strong("Movie time: "),
          format_seconds(current_time),
          br(),
          strong("Year title card: "),
          item$event_year
        )
      )
    }

    source_count <- event_source_links %>%
      filter(event_id == item$event_id) %>%
      nrow()

    div(
      class = "context-box",
      strong("Movie time: "),
      format_seconds(current_time),
      br(),
      strong("Event: "),
      item$event_id,
      " — ",
      item$event_type,
      br(),
      strong("Year: "),
      item$event_year,
      br(),
      strong("Source files: "),
      source_count,
      br(),
      strong("First source: "),
      item$first_relative_path
    )
  })

  output$current_event_clip <- renderUI({
    item <- current_timeline_item()

    if (
      is.null(item) ||
      !identical(item$item_type, "EVENT") ||
      is.na(item$rendered_clip)
    ) {
      return(p("Move the movie cursor onto an event."))
    }

    clip_url <- paste0(
      "event_clips/",
      encode_path_parts(basename(item$rendered_clip))
    )

    tags$video(
      src = clip_url,
      type = "video/mp4",
      controls = NA,
      preload = "metadata",
      style = "width:100%; max-height:420px; background:black;"
    )
  })

  observe({
    current <- chapters() %>%
      arrange(chapter_order, start_seconds)

    # On the first launch there may be no saved chapters yet.
    # Handle that state explicitly rather than calling setNames()
    # with an empty value vector and a non-empty names vector.
    if (nrow(current) == 0) {
      updateSelectInput(
        session,
        "export_chapter_id",
        choices = character(0),
        selected = character(0)
      )

      return()
    }

    chapter_labels <- paste0(
      current$chapter_order,
      ". ",
      current$chapter_title,
      " [",
      vapply(current$start_seconds, format_seconds, character(1)),
      "–",
      vapply(current$end_seconds, format_seconds, character(1)),
      "]"
    )

    choices <- stats::setNames(
      as.character(current$chapter_id),
      as.character(chapter_labels)
    )

    updateSelectInput(
      session,
      "export_chapter_id",
      choices = choices,
      selected = unname(choices[[1]])
    )
  })

  output$chapter_table <- renderDT({
    display <- chapters() %>%
      arrange(chapter_order, start_seconds) %>%
      transmute(
        Order = chapter_order,
        ID = chapter_id,
        Chapter = chapter_title,
        Theme = theme,
        Start = vapply(start_seconds, format_seconds, character(1)),
        End = vapply(end_seconds, format_seconds, character(1)),
        Duration_s = round(duration_seconds, 2),
        Events = event_count,
        Files = source_file_count,
        Sharing = sharing_status,
        Sensitive = sensitive_content,
        Include = include_in_thematic_compilation
      )

    datatable(
      display,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        ordering = FALSE
      )
    )
  })

  observeEvent(
    input$chapter_table_rows_selected,
    {
      chapter <- selected_chapter_row()

      if (is.null(chapter)) {
        return()
      }

      markers$start <- chapter$start_seconds
      markers$end <- chapter$end_seconds

      updateTextInput(
        session,
        "chapter_title",
        value = chapter$chapter_title
      )

      updateTextInput(
        session,
        "chapter_theme",
        value = chapter$theme
      )

      updateTextInput(
        session,
        "chapter_tags",
        value = chapter$tags
      )

      updateSelectInput(
        session,
        "sharing_status",
        selected = chapter$sharing_status
      )

      updateCheckboxInput(
        session,
        "sensitive_content",
        value = isTRUE(chapter$sensitive_content)
      )

      updateCheckboxInput(
        session,
        "include_thematic",
        value = isTRUE(chapter$include_in_thematic_compilation)
      )

      updateTextAreaInput(
        session,
        "chapter_notes",
        value = chapter$notes
      )
    },
    ignoreInit = TRUE
  )

  validate_markers <- function() {
    if (
      !is.finite(markers$start) ||
      !is.finite(markers$end)
    ) {
      showNotification(
        "Mark both a chapter start and end.",
        type = "error"
      )
      return(FALSE)
    }

    if (markers$end <= markers$start) {
      showNotification(
        "The chapter end must be later than its start.",
        type = "error"
      )
      return(FALSE)
    }

    TRUE
  }

  observeEvent(input$add_chapter, {
    if (!validate_markers()) {
      return()
    }

    current <- chapters()

    existing_numbers <- suppressWarnings(
      as.integer(sub("^CH_", "", current$chapter_id))
    )

    next_number <- if (
      length(existing_numbers) == 0 ||
      all(is.na(existing_numbers))
    ) {
      1L
    } else {
      max(existing_numbers, na.rm = TRUE) + 1L
    }

    now_text <- format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S",
      tz = capture_timezone
    )

    new_chapter <- tibble(
      chapter_id = sprintf("CH_%03d", next_number),
      chapter_order = nrow(current) + 1L,
      chapter_title = if (
        nzchar(trimws(input$chapter_title))
      ) {
        trimws(input$chapter_title)
      } else {
        paste("Chapter", next_number)
      },
      theme = trimws(input$chapter_theme),
      tags = trimws(input$chapter_tags),
      start_seconds = as.numeric(markers$start),
      end_seconds = as.numeric(markers$end),
      duration_seconds = markers$end - markers$start,
      sharing_status = input$sharing_status,
      sensitive_content = isTRUE(input$sensitive_content),
      include_in_thematic_compilation = isTRUE(input$include_thematic),
      event_count = 0L,
      source_file_count = 0L,
      notes = input$chapter_notes,
      created_at = now_text,
      updated_at = now_text
    )

    new_chapter <- chapter_with_counts(new_chapter)

    updated <- bind_rows(current, new_chapter) %>%
      arrange(chapter_order, start_seconds)

    updated <- save_chapter_system(updated)
    chapters(updated)

    showNotification(
      paste("Added", new_chapter$chapter_id),
      type = "message"
    )
  })

  observeEvent(input$update_chapter, {
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      showNotification(
        "Select a chapter in the chapter table first.",
        type = "error"
      )
      return()
    }

    if (!validate_markers()) {
      return()
    }

    current <- chapters()
    row_index <- match(selected$chapter_id, current$chapter_id)

    updated_row <- selected
    updated_row$chapter_title <- if (
      nzchar(trimws(input$chapter_title))
    ) {
      trimws(input$chapter_title)
    } else {
      selected$chapter_title
    }
    updated_row$theme <- trimws(input$chapter_theme)
    updated_row$tags <- trimws(input$chapter_tags)
    updated_row$start_seconds <- markers$start
    updated_row$end_seconds <- markers$end
    updated_row$sharing_status <- input$sharing_status
    updated_row$sensitive_content <- isTRUE(input$sensitive_content)
    updated_row$include_in_thematic_compilation <-
      isTRUE(input$include_thematic)
    updated_row$notes <- input$chapter_notes
    updated_row$updated_at <- format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S",
      tz = capture_timezone
    )

    updated_row <- chapter_with_counts(updated_row)

    current[row_index, names(updated_row)] <- updated_row

    current <- save_chapter_system(current)
    chapters(current)

    showNotification(
      paste("Updated", selected$chapter_id),
      type = "message"
    )
  })

  observeEvent(input$delete_chapter, {
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      showNotification(
        "Select a chapter first.",
        type = "error"
      )
      return()
    }

    current <- chapters() %>%
      filter(chapter_id != selected$chapter_id)

    current <- save_chapter_system(current)
    chapters(current)

    showNotification(
      paste("Deleted", selected$chapter_id),
      type = "warning"
    )
  })

  move_selected_chapter <- function(direction) {
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      showNotification(
        "Select a chapter first.",
        type = "error"
      )
      return()
    }

    current <- chapters() %>%
      arrange(chapter_order, start_seconds)

    index <- match(selected$chapter_id, current$chapter_id)
    target <- index + direction

    if (target < 1 || target > nrow(current)) {
      return()
    }

    temporary <- current[index, , drop = FALSE]
    current[index, ] <- current[target, ]
    current[target, ] <- temporary
    current$chapter_order <- seq_len(nrow(current))

    current <- save_chapter_system(current)
    chapters(current)
  }

  observeEvent(input$move_chapter_up, {
    move_selected_chapter(-1L)
  })

  observeEvent(input$move_chapter_down, {
    move_selected_chapter(1L)
  })

  observeEvent(input$seek_selected_chapter, {
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      showNotification(
        "Select a chapter first.",
        type = "error"
      )
      return()
    }

    session$sendCustomMessage(
      "seekMainMovie",
      list(
        time = selected$start_seconds,
        play = FALSE
      )
    )
  })

  selected_chapter_sources <- reactive({
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      return(event_source_links[0, , drop = FALSE])
    }

    events <- events_overlapping(
      selected$start_seconds,
      selected$end_seconds
    )

    source_files_for_events(events$event_id)
  })

  output$selected_chapter_link_summary <- renderUI({
    selected <- selected_chapter_row()

    if (is.null(selected)) {
      return(p("Select a chapter in the chapter table."))
    }

    files <- selected_chapter_sources()
    events <- events_overlapping(
      selected$start_seconds,
      selected$end_seconds
    )

    tagList(
      strong(selected$chapter_title),
      br(),
      paste(
        nrow(events),
        "linked event(s);",
        nrow(files),
        "linked source file(s)."
      )
    )
  })

  output$linked_source_table <- renderDT({
    files <- selected_chapter_sources() %>%
      transmute(
        Event = event_id,
        Time = effective_capture_time_local,
        Type = media_type,
        Treatment = initial_media_treatment,
        Sequence = sequence_id,
        Path = relative_path
      )

    datatable(
      files,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })

  selected_source_row <- reactive({
    selected <- input$linked_source_table_rows_selected
    files <- selected_chapter_sources()

    if (
      length(selected) != 1 ||
      selected < 1 ||
      selected > nrow(files)
    ) {
      return(NULL)
    }

    files[selected, , drop = FALSE]
  })

  output$source_preview <- renderUI({
    source <- selected_source_row()

    if (is.null(source)) {
      return(p("Select a source file from the table."))
    }

    source_url <- paste0(
      "source_media/",
      encode_path_parts(source$relative_path)
    )

    extension <- tolower(source$extension)

    if (
      extension %in% c(
        "jpg", "jpeg", "png", "gif", "webp", "bmp", "tif", "tiff"
      )
    ) {
      return(
        tags$img(
          src = source_url,
          alt = source$relative_path
        )
      )
    }

    if (
      extension %in% c(
        "mp4", "mov", "m4v", "webm", "avi", "mkv"
      )
    ) {
      return(
        tags$video(
          src = source_url,
          controls = NA,
          preload = "metadata"
        )
      )
    }

    tagList(
      p("Preview is not available for this file type."),
      code(source$relative_path)
    )
  })

  observeEvent(input$open_selected_source, {
    source <- selected_source_row()

    if (is.null(source)) {
      showNotification(
        "Select a source file first.",
        type = "error"
      )
      return()
    }

    full_path <- normalizePath(
      file.path(root_directory, source$relative_path),
      winslash = "\\",
      mustWork = TRUE
    )

    if (.Platform$OS.type == "windows") {
      shell.exec(full_path)
    } else {
      browseURL(
        paste0(
          "file://",
          URLencode(full_path, reserved = FALSE)
        )
      )
    }
  })

  observeEvent(input$export_chapter_id, {
    if (
      is.null(input$export_chapter_id) ||
      length(input$export_chapter_id) == 0 ||
      !nzchar(input$export_chapter_id)
    ) {
      return()
    }

    current <- chapters()
    chapter <- current %>%
      filter(chapter_id == input$export_chapter_id) %>%
      slice_head(n = 1)

    if (nrow(chapter) == 0) {
      return()
    }

    updateNumericInput(
      session,
      "export_start_seconds",
      value = chapter$start_seconds
    )

    updateNumericInput(
      session,
      "export_end_seconds",
      value = chapter$end_seconds
    )

    updateTextInput(
      session,
      "export_name",
      value = sanitize_filename(chapter$chapter_title)
    )
  })

  observe({
    mode <- input$export_source_mode

    if (identical(mode, "CHAPTER")) {
      chapter <- chapters() %>%
        filter(chapter_id == input$export_chapter_id) %>%
        slice_head(n = 1)

      if (nrow(chapter) == 1) {
        updateNumericInput(
          session,
          "export_start_seconds",
          value = chapter$start_seconds
        )

        updateNumericInput(
          session,
          "export_end_seconds",
          value = chapter$end_seconds
        )
      }
    }

    if (
      identical(mode, "MARKERS") &&
      is.finite(markers$start) &&
      is.finite(markers$end)
    ) {
      updateNumericInput(
        session,
        "export_start_seconds",
        value = markers$start
      )

      updateNumericInput(
        session,
        "export_end_seconds",
        value = markers$end
      )
    }
  })

  export_range <- reactive({
    c(
      start = as.numeric(input$export_start_seconds),
      end = as.numeric(input$export_end_seconds)
    )
  })

  output$export_range_text <- renderUI({
    range <- export_range()
    duration <- range[["end"]] - range[["start"]]

    tagList(
      strong("Start: "),
      format_seconds(range[["start"]]),
      br(),
      strong("End: "),
      format_seconds(range[["end"]]),
      br(),
      strong("Duration: "),
      if (is.finite(duration)) {
        paste0(round(duration, 3), " seconds")
      } else {
        "invalid"
      }
    )
  })

  observeEvent(input$export_social_clip, {
    if (!isTRUE(input$export_review_confirmed)) {
      showNotification(
        "Confirm that you reviewed the segment for sharing.",
        type = "error"
      )
      return()
    }

    range <- export_range()

    if (
      !is.finite(range[["start"]]) ||
      !is.finite(range[["end"]]) ||
      range[["end"]] <= range[["start"]]
    ) {
      showNotification(
        "Enter a valid export range.",
        type = "error"
      )
      return()
    }

    if (identical(input$export_source_mode, "CHAPTER")) {
      chapter <- chapters() %>%
        filter(chapter_id == input$export_chapter_id) %>%
        slice_head(n = 1)

      if (
        nrow(chapter) == 1 &&
        identical(
          chapter$sharing_status,
          "PRIVATE_DO_NOT_EXPORT"
        )
      ) {
        showNotification(
          "This chapter is marked PRIVATE — do not export.",
          type = "error"
        )
        return()
      }
    }

    output_name <- sanitize_filename(input$export_name)

    output_path <- file.path(
      social_export_directory,
      paste0(
        output_name,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".mp4"
      )
    )

    progress <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close(), add = TRUE)

    result <- tryCatch(
      {
        progress$set(
          message = "Preparing social export",
          value = 0.05
        )

        rendered <- extract_and_encode_segment(
          start_seconds = range[["start"]],
          end_seconds = range[["end"]],
          output_file = output_path,
          preset = input$export_preset,
          framing = input$export_framing,
          progress = progress
        )

        progress$set(
          message = "Saving export record",
          value = 0.95
        )

        linked_events <- events_overlapping(
          range[["start"]],
          range[["end"]]
        )

        linked_sources <- source_files_for_events(
          linked_events$event_id
        )

        manifest_row <- tibble(
          export_created_at = format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S",
            tz = capture_timezone
          ),
          export_type = "SOCIAL_CLIP",
          output_file = rendered,
          source_mode = input$export_source_mode,
          chapter_id = if (
            identical(input$export_source_mode, "CHAPTER")
          ) {
            input$export_chapter_id
          } else {
            NA_character_
          },
          start_seconds = range[["start"]],
          end_seconds = range[["end"]],
          duration_seconds = range[["end"]] - range[["start"]],
          aspect_preset = input$export_preset,
          framing = input$export_framing,
          linked_event_count = nrow(linked_events),
          linked_source_file_count = nrow(linked_sources),
          reviewed_for_sharing = TRUE
        )

        append_export_manifest(manifest_row)
        latest_export(rendered)

        progress$set(
          message = "Export complete",
          value = 1
        )

        rendered
      },
      error = function(e) {
        showNotification(
          paste("Export failed:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NA_character_
      }
    )

    if (!is.na(result)) {
      showNotification(
        paste("Export created:", basename(result)),
        type = "message",
        duration = 8
      )
    }
  })

  output$latest_export_status <- renderUI({
    path <- latest_export()

    if (is.na(path) || !file.exists(path)) {
      return(p("No export created in this session."))
    }

    tagList(
      strong("Latest export"),
      br(),
      code(path)
    )
  })

  output$download_latest_export <- downloadHandler(
    filename = function() {
      path <- latest_export()

      if (is.na(path)) {
        "social_export.mp4"
      } else {
        basename(path)
      }
    },
    content = function(file) {
      path <- latest_export()
      req(!is.na(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    },
    contentType = "video/mp4"
  )

  output$thematic_chapter_table <- renderDT({
    display <- chapters() %>%
      arrange(chapter_order) %>%
      transmute(
        Order = chapter_order,
        Chapter = chapter_title,
        Theme = theme,
        Start = vapply(start_seconds, format_seconds, character(1)),
        End = vapply(end_seconds, format_seconds, character(1)),
        Duration_s = round(duration_seconds, 2),
        Include = include_in_thematic_compilation,
        Sharing = sharing_status
      )

    datatable(
      display,
      selection = "none",
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        ordering = FALSE
      )
    )
  })

  observeEvent(input$build_thematic_compilation, {
    selected <- chapters() %>%
      filter(include_in_thematic_compilation %in% TRUE) %>%
      arrange(chapter_order)

    if (nrow(selected) == 0) {
      showNotification(
        "No chapters are marked for thematic compilation.",
        type = "error"
      )
      return()
    }

    output_name <- sanitize_filename(
      input$thematic_output_name
    )

    output_path <- file.path(
      thematic_export_directory,
      paste0(
        output_name,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".mp4"
      )
    )

    temporary_directory <- tempfile(
      "thematic_compilation_"
    )

    dir.create(
      temporary_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )

    progress <- shiny::Progress$new(
      session,
      min = 0,
      max = nrow(selected) + 1
    )

    on.exit(
      {
        progress$close()
        unlink(
          temporary_directory,
          recursive = TRUE,
          force = TRUE
        )
      },
      add = TRUE
    )

    result <- tryCatch(
      {
        all_frames <- character(0)

        for (i in seq_len(nrow(selected))) {
          chapter <- selected[i, , drop = FALSE]

          progress$set(
            value = i - 1,
            message = paste(
              "Extracting chapter",
              i,
              "of",
              nrow(selected)
            ),
            detail = chapter$chapter_title
          )

          chapter_frame_directory <- file.path(
            temporary_directory,
            sprintf("chapter_%04d", i)
          )

          dir.create(
            chapter_frame_directory,
            recursive = TRUE,
            showWarnings = FALSE
          )

          trim_text <- sprintf(
            "%.3f:%.3f",
            chapter$start_seconds,
            chapter$end_seconds
          )

          chapter_frames <- av::av_video_images(
            video = pilot_movie,
            destdir = chapter_frame_directory,
            format = "jpg",
            fps = export_fps,
            trim = trim_text
          )

          if (length(chapter_frames) == 0) {
            stop(
              "No frames extracted for ",
              chapter$chapter_id
            )
          }

          all_frames <- c(
            all_frames,
            chapter_frames
          )
        }

        progress$set(
          value = nrow(selected),
          message = "Encoding thematic compilation",
          detail = paste(length(all_frames), "frames")
        )

        av::av_encode_video(
          input = all_frames,
          output = output_path,
          framerate = export_fps,
          vfilter = build_video_filter(
            input$thematic_preset,
            input$thematic_framing
          ),
          codec = "libx264",
          verbose = TRUE
        )

        normalized_output <- normalizePath(
          output_path,
          winslash = "/",
          mustWork = TRUE
        )

        manifest_row <- tibble(
          export_created_at = format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S",
            tz = capture_timezone
          ),
          export_type = "THEMATIC_COMPILATION",
          output_file = normalized_output,
          source_mode = "ORDERED_CHAPTERS",
          chapter_id = paste(
            selected$chapter_id,
            collapse = ";"
          ),
          start_seconds = NA_real_,
          end_seconds = NA_real_,
          duration_seconds = sum(
            selected$duration_seconds,
            na.rm = TRUE
          ),
          aspect_preset = input$thematic_preset,
          framing = input$thematic_framing,
          linked_event_count = NA_integer_,
          linked_source_file_count = NA_integer_,
          reviewed_for_sharing = FALSE
        )

        append_export_manifest(manifest_row)
        latest_thematic(normalized_output)

        progress$set(
          value = nrow(selected) + 1,
          message = "Compilation complete"
        )

        normalized_output
      },
      error = function(e) {
        showNotification(
          paste(
            "Thematic compilation failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
        NA_character_
      }
    )

    if (!is.na(result)) {
      showNotification(
        paste(
          "Thematic movie created:",
          basename(result)
        ),
        type = "message",
        duration = 8
      )
    }
  })

  output$latest_thematic_status <- renderUI({
    path <- latest_thematic()

    if (is.na(path) || !file.exists(path)) {
      return(
        p("No thematic movie created in this session.")
      )
    }

    tagList(
      strong("Latest thematic movie"),
      br(),
      code(path)
    )
  })

  output$download_latest_thematic <- downloadHandler(
    filename = function() {
      path <- latest_thematic()

      if (is.na(path)) {
        "thematic_movie.mp4"
      } else {
        basename(path)
      }
    },
    content = function(file) {
      path <- latest_thematic()
      req(!is.na(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    },
    contentType = "video/mp4"
  )

  output$download_chapters_csv <- downloadHandler(
    filename = function() {
      "chapter_annotations.csv"
    },
    content = function(file) {
      save_chapter_system(chapters())
      file.copy(chapter_file, file, overwrite = TRUE)
    },
    contentType = "text/csv"
  )

  output$download_chapter_event_links_csv <- downloadHandler(
    filename = function() {
      "chapter_event_links.csv"
    },
    content = function(file) {
      save_chapter_system(chapters())
      file.copy(
        chapter_event_links_file,
        file,
        overwrite = TRUE
      )
    },
    contentType = "text/csv"
  )

  output$download_chapter_source_links_csv <- downloadHandler(
    filename = function() {
      "chapter_source_links.csv"
    },
    content = function(file) {
      save_chapter_system(chapters())
      file.copy(
        chapter_source_links_file,
        file,
        overwrite = TRUE
      )
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
