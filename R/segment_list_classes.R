# S3 parent describing tibble's class chain so S7 preserves tbl_df/tbl
# classes on the underlying data.
.tbl_df_S3_class <- S7::new_S3_class(c("tbl_df", "tbl", "data.frame"))

#' Segment List S7 Class
#'
#' An S7 class representing a segment list returned by EMU queries.
#' This class wraps a tibble (`tbl_df`) and carries `db_uuid` / `db_path`
#' as S7 properties. Tidyverse verbs (`dplyr::filter`, `mutate`, `select`,
#' `arrange`, base bracket subsetting) preserve the class and its
#' properties when columns required by the validator remain present.
#'
#' @param data A data.frame, tibble, or compatible object with the required
#'   columns (see Structure below)
#' @param db_uuid Character; the database UUID
#' @param db_path Character; path to the database directory
#' @return A \code{segment_list} object that inherits from
#'   \code{tbl_df}, \code{tbl}, and \code{data.frame}.
#'
#' @section Structure:
#' A segment_list must contain the following columns:
#' - labels: Character vector of segment labels
#' - start: Numeric start time in milliseconds
#' - end: Numeric end time in milliseconds
#' - db_uuid: Character UUID of database
#' - session: Character session name
#' - bundle: Character bundle name
#' - start_item_id: Integer start item ID
#' - end_item_id: Integer end item ID
#' - level: Character level name
#' - attribute: Character attribute name
#' - start_item_seq_idx: Integer start sequence index
#' - end_item_seq_idx: Integer end sequence index
#' - type: Character type (SEGMENT, EVENT, ITEM)
#' - sample_start: Integer sample start
#' - sample_end: Integer sample end
#' - sample_rate: Numeric sample rate
#'
#' @examples
#' \dontrun{
#' # Query returns a segment_list
#' segs <- ask_for(corpus("path/to/db_emuDB"), "Phonetic == t")
#'
#' # Tidyverse pipe preserves segment_list and its properties
#' long_segs <- segs |> dplyr::filter(end - start > 50)
#'
#' # Apply DSP to segments
#' result <- quantify(segs, corpus, tracks = "fm")
#' }
#'
#' @name segment_list
#' @export
segment_list <- S7::new_class(
  "segment_list",
  parent = .tbl_df_S3_class,
  properties = list(
    db_uuid = S7::class_character,
    db_path = S7::class_character
  ),
  validator = function(self) {
    # Required columns from emuR::query() results
    required_cols <- c(
      "labels", "start", "end", "db_uuid", "session", "bundle",
      "start_item_id", "end_item_id", "level", "attribute",
      "start_item_seq_idx", "end_item_seq_idx", "type",
      "sample_start", "sample_end", "sample_rate"
    )

    missing_cols <- setdiff(required_cols, names(self))
    if (length(missing_cols) > 0) {
      return(paste0(
        "segment_list missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ))
    }

    # Check column types
    if (!is.character(self$labels)) {
      return("'labels' must be character")
    }
    if (!is.numeric(self$start) || !is.numeric(self$end)) {
      return("'start' and 'end' must be numeric")
    }
    if (!is.character(self$db_uuid) || !is.character(self$session) ||
        !is.character(self$bundle)) {
      return("'db_uuid', 'session', and 'bundle' must be character")
    }

    NULL
  },
  constructor = function(data, db_uuid = NULL, db_path = NULL) {
    # Always wrap in tibble so S7 sees the tbl_df/tbl/data.frame class chain
    if (!inherits(data, "tbl_df")) {
      data <- tibble::as_tibble(data)
    }

    # Extract db_uuid from data if not provided
    if (is.null(db_uuid) && "db_uuid" %in% names(data)) {
      db_uuid_vals <- unique(data$db_uuid)
      if (length(db_uuid_vals) > 1) {
        cli::cli_warn("Multiple db_uuids found; using first")
        db_uuid <- as.character(db_uuid_vals[1])
      } else {
        db_uuid <- as.character(db_uuid_vals[1])
      }
    }

    if (is.null(db_uuid) || length(db_uuid) == 0) {
      db_uuid <- ""
    }

    if (is.null(db_path) || length(db_path) == 0) {
      db_path <- ""
    }

    S7::new_object(
      .parent = data,
      db_uuid = as.character(db_uuid),
      db_path = as.character(db_path)
    )
  }
)

#' Convert data.frame to segment_list
#'
#' @param x data.frame or compatible object
#' @param db_uuid Database UUID
#' @param db_path Database path
#' @return A segment_list object
#' @export
as_segment_list <- function(x, db_uuid = NULL, db_path = NULL) {
  if (S7::S7_inherits(x, segment_list)) {
    return(x)
  }
  segment_list(x, db_uuid = db_uuid, db_path = db_path)
}

#' Check if object is a segment_list
#' @param x Object to test
#' @return Logical
#' @export
is_segment_list <- function(x) {
  S7::S7_inherits(x, segment_list)
}

#' Extended Segment List S7 Class
#'
#' An S7 class representing a segment list with DSP-derived measurements.
#' This extends segment_list with additional columns from DSP processing
#' (e.g., formants, pitch, intensity).
#'
#' @param data A data.frame with segment_list columns plus DSP measurement columns
#' @param db_uuid Character; the database UUID
#' @param db_path Character; path to the database directory
#' @param dsp_function Character; name of the DSP function used
#' @param dsp_columns Character vector of column names added by DSP processing
#' @return An \code{extended_segment_list} object (inherits from segment_list)
#'
#' @section Structure:
#' An extended_segment_list contains all segment_list columns plus
#' additional columns added by DSP processing via quantify().
#'
#' @examplesIf interactive()
#' # Query and quantify
#' segs <- query(corpus, "Phonetic == t")
#' extended <- quantify(segs, superassp::forest)
#'
#' # Extended segment list contains formant measurements
#' print(extended)
#' summary(extended)
#'
#' @export
extended_segment_list <- S7::new_class(
  "extended_segment_list",
  parent = segment_list,
  properties = list(
    dsp_function = S7::class_character,
    dsp_columns = S7::class_character
  ),
  validator = function(self) {
    # Inherits validation from segment_list
    NULL
  },
  constructor = function(data, db_uuid = NULL, db_path = NULL,
                        dsp_function = "", dsp_columns = character(0)) {
    # Always wrap in tibble so S7 sees the tbl_df/tbl/data.frame class chain
    if (!inherits(data, "tbl_df")) {
      data <- tibble::as_tibble(data)
    }

    # Extract db_uuid from data if not provided
    if (is.null(db_uuid) && "db_uuid" %in% names(data)) {
      db_uuid_vals <- unique(data$db_uuid)
      if (length(db_uuid_vals) > 1) {
        cli::cli_warn("Multiple db_uuids found; using first")
        db_uuid <- as.character(db_uuid_vals[1])
      } else {
        db_uuid <- as.character(db_uuid_vals[1])
      }
    }

    if (is.null(db_uuid) || length(db_uuid) == 0) {
      db_uuid <- ""
    }

    if (is.null(db_path) || length(db_path) == 0) {
      db_path <- ""
    }

    S7::new_object(
      .parent = data,
      db_uuid = as.character(db_uuid),
      db_path = as.character(db_path),
      dsp_function = as.character(dsp_function),
      dsp_columns = as.character(dsp_columns)
    )
  }
)

#' Check if object is an extended_segment_list
#' @param x Object to test
#' @return Logical
#' @export
is_extended_segment_list <- function(x) {
  S7::S7_inherits(x, extended_segment_list)
}
