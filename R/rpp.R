rpp_uncomment <- function(lines, ...) {
  rx <- "^(\\s*).*#\\s*!q\\s*(.*)$"
  sub(rx, "\\1\\2", lines)
}

rpp_elide <- function(lines, ...) {
  # HACK: speed up for files that are not annotated
  # FIXME: use caching like styler does
  if (!length(grep("[?]", lines))) {
    return(lines)
  }

  new_lines <-
    lines |>
    styler:::compute_parse_data_nested(NULL, 0) |>
    # FIXME: We should be eliding the argument type only for functions.
    elide_arg_type() |>
    elide_return_type() |>
    elide_types() |>
    unnest_pd()

  new_lines <- strsplit(new_lines, "\n")[[1]]

  for (line in lines) {
    if (line != "") {
      break
    }
    new_lines <- c("", new_lines)
  }

  for (line in rev(lines)) {
    if (line != "") {
      break
    }
    new_lines <- c(new_lines, "")
  }

  stopifnot(length(lines) == length(new_lines))

  different <- which(lines != new_lines)

  if (any(different)) {
    nc <- nchar(new_lines[different], type = "width")
    indent <- max(nc) + 1

    new_lines[different] <- paste0(
      new_lines[different],
      strrep(" ", indent - nc), "# !q ",
      lines[different]
    )
  }

  new_lines
}

get_spacing <- function(nested) {
  line1 <- nested$line1
  col1 <- nested$col1
  line2 <- nested$line2
  col2 <- nested$col2

  lag_line2 <- dplyr::lag(line2, default = line1[[1]])
  lag_col2 <- dplyr::lag(col2, default = col1[[1]])

  has_newline <- line1 > lag_line2
  lag_col2[has_newline] <- 0

  c(
    paste0(
      strrep("\n", line1 - lag_line2),
      strrep(" ", pmax(col1 - lag_col2 - 1, 0))
    )
  )
}

unnest_pd <- function(x) {
  is_leaf <- purrr::map_lgl(x$child, is.null)
  text <- x$text
  text[!is_leaf] <- purrr::map_chr(x$child[!is_leaf], unnest_pd)
  spacing <- get_spacing(x)
  paste0(spacing, text, collapse = "")
}

elide_arg_type <- function(x) {
  if (nrow(x) > 0 && x$token[[1]] == "FUNCTION") {
    idx_formals <- which(x$token == "EQ_FORMALS")
    idx_type_or_default <- idx_formals + 1
    new_children <- purrr::map(x$child[idx_type_or_default], elide_one_arg_type)
    x$child[idx_type_or_default] <- new_children

    new_children_empty <- which(purrr::map_int(new_children, nrow) == 0)
    elide_here <- idx_type_or_default[new_children_empty]
    if (length(elide_here) > 0) {
      x <- x[-c(elide_here, elide_here - 1), ]
    }
  }

  is_leaf <- purrr::map_lgl(x$child, is.null)
  x$child[!is_leaf] <- purrr::map(x$child[!is_leaf], elide_arg_type)
  x
}

elide_one_arg_type <- function(x) {
  idx <- which(x$token == "'?'" & x$terminal)
  if (length(idx) > 0) {
    stopifnot(length(idx) ==  1)
    x <- x[seq_len(idx - 1), ]
  }

  x
}

elide_return_type <- function(x) {
  if (nrow(x) > 1 && x$token[[2]] == "'?'" && nrow(x$child[[1]]) > 1 && x$child[[1]]$token[[2]] == "LEFT_ASSIGN") {
    x[1:2, ] <- x$child[[1]][1:2, ]
  }

  is_leaf <- purrr::map_lgl(x$child, is.null)
  x$child[!is_leaf] <- purrr::map(x$child[!is_leaf], elide_return_type)
  x
}

elide_types <- function(x) {
  idx <- which(x$token == "'?'" & x$terminal)
  if (length(idx) > 0) {
    x <- x$child[[idx + 1]]
  }

  is_leaf <- purrr::map_lgl(x$child, is.null)
  x$child[!is_leaf] <- purrr::map(x$child[!is_leaf], elide_types)
  x
}

#' @export
rpp_elide_types <- function() {
  rpp::inline_plugin(dev = rpp_uncomment, prod = rpp_elide)
}

expect_rpp_snapshot <- function(code) {
  if (length(code) == 1) {
    testthat::expect_snapshot(writeLines(rpp_elide(!!code)))
  } else {
    testthat::expect_snapshot(writeLines(rpp_elide(code)))
  }
}
