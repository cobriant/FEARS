# FEARS
google trends for terms like "gold prices", "recession", "crisis", "unemployment" for replication of Da, Engelberg, and Gao (2015)

R code:

anchored_daily_gtrends_2004_2011 <- function(keyword,
                                             anchor = "weather",
                                             geo = "US",
                                             start = as.Date("2004-07-01"),
                                             end   = as.Date("2011-12-31"),
                                             window_days = 269,     # strict 9 month safe
                                             step_days   = 240,     # overlap
                                             pause_range = c(1.5, 4.0),
                                             max_attempts = 10,
                                             min_anchor_hits = 5,
                                             prefer = c("earlier", "later")) {

  prefer <- match.arg(prefer)

  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(tibble)
    library(tidyr)
    library(gtrendsR)
  })

  hits_to_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x[x == "<1"] <- "0.5"
    suppressWarnings(as.numeric(x))
  }

  starts <- seq.Date(start, end, by = paste(step_days, "days"))

  windows <- tibble(
    win = seq_along(starts),
    d0 = starts,
    d1 = pmin(starts + window_days, end)
  ) %>% filter(d0 < d1)

  message("Keyword: ", keyword)
  message("Windows: ", nrow(windows),
          " (", window_days, " day windows, step ", step_days, ")")

  fetch_once <- function(keyword, anchor, geo, d0, d1) {
    Sys.sleep(runif(1, pause_range[1], pause_range[2]))
    time_str <- paste(format(d0), format(d1))

    gtrendsR::gtrends(
      keyword = c(keyword, anchor),
      geo = geo,
      time = time_str
    )$interest_over_time %>%
      as_tibble() %>%
      transmute(
        date = as.Date(date),
        term = as.character(keyword),
        hits = hits_to_num(hits)
      )
  }

  fetch_retry <- purrr::insistently(
    fetch_once,
    rate = purrr::rate_backoff(
      pause_base = 10,
      pause_cap  = 120,
      max_times  = max_attempts,
      jitter     = TRUE
    )
  )

  fetch_safe <- purrr::safely(
    function(keyword, anchor, geo, d0, d1)
      fetch_retry(keyword, anchor, geo, d0, d1)
  )

  pieces <- vector("list", nrow(windows))

  for (i in seq_len(nrow(windows))) {

    d0 <- windows$d0[i]
    d1 <- windows$d1[i]

    message("[", i, "/", nrow(windows), "] ",
            format(d0), " to ", format(d1))

    res <- fetch_safe(keyword, anchor, geo, d0, d1)

    if (!is.null(res$error)) {
      message("  FAILED: ", res$error$message)
      pieces[[i]] <- tibble(date = as.Date(character()),
                            index = numeric(),
                            win = integer())
      next
    }

    wide <- res$result %>%
      filter(date >= d0, date <= d1) %>%
      pivot_wider(names_from = term, values_from = hits)

    if (!(keyword %in% names(wide)) ||
        !(anchor %in% names(wide))) {
      message("  Missing keyword or anchor column")
      pieces[[i]] <- tibble(date = as.Date(character()),
                            index = numeric(),
                            win = integer())
      next
    }

    w <- wide %>%
      mutate(anchor_hits = .data[[anchor]],
             kw_hits     = .data[[keyword]]) %>%
      filter(!is.na(anchor_hits),
             !is.na(kw_hits),
             anchor_hits >= min_anchor_hits) %>%
      mutate(ratio = kw_hits / anchor_hits,
             index = 100 * ratio) %>%
      select(date, index) %>%
      arrange(date)

    message("  usable rows: ", nrow(w))

    pieces[[i]] <- w %>% mutate(win = i)
  }

  all <- bind_rows(pieces) %>%
    filter(date >= start, date <= end)

  out <- if (prefer == "earlier") {
    all %>%
      arrange(date, win) %>%
      group_by(date) %>%
      slice(1) %>%
      ungroup()
  } else {
    all %>%
      arrange(date, desc(win)) %>%
      group_by(date) %>%
      slice(1) %>%
      ungroup()
  }

  max_index <- max(out$index, na.rm = TRUE)

  if (is.finite(max_index) && max_index > 0) {
    out <- out %>%
      mutate(index = 100 * index / max_index)
  }

  out %>%
    arrange(date) %>%
    transmute(
      keyword = keyword,
      anchor = anchor,
      geo = geo,
      date = date,
      hits = index
    )
}

library(dplyr)
library(purrr)
library(readr)
library(stringr)

terms <- c(
  "gold prices", "recession", "gold price", "depression",
  "great depression", "gold", "economy", "price of gold",
  "the depression", "crisis", "frugal", "gdp", "charity",
  "bankruptcy", "unemployment", "inflation rate",
  "bankrupt", "the great depression", "car donate",
  "capitalization", "expense", "donation", "savings",
  "social security card", "the crisis", "default",
  "benefits", "unemployed", "poverty", "social security office"
)

slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

run_one <- function(term, out_dir = ".", anchor = "weather") {

  file_stub <- paste0(slugify(term), "_daily-gtrends.csv")
  file_path <- file.path(out_dir, file_stub)

  message("\n==============================")
  message("TERM: ", term)
  message("==============================")

  res <- purrr::safely(function(t) {
    anchored_daily_gtrends_2004_2011(
      t,
      anchor = anchor,
      window_days = 269,
      step_days   = 240,
      prefer = "later"
    )
  })(term)

  if (!is.null(res$error)) {
    message("FAILED: ", res$error$message)
    return(tibble(
      keyword = term,
      file = file_path,
      status = "failed",
      rows = NA_integer_,
      error = res$error$message
    ))
  }

  df <- res$result
  write_csv(df, file_path)

  message("WROTE: ", nrow(df), " rows")

  tibble(
    keyword = term,
    file = file_path,
    status = "ok",
    rows = nrow(df),
    error = NA_character_
  )
}

run_all_terms <- function(terms, out_dir = ".", anchor = "weather") {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  log <- map_dfr(terms, ~ run_one(.x, out_dir = out_dir, anchor = anchor))

  write_csv(log, file.path(out_dir, "gtrends_run_log.csv"))

  log
}

# Run everything
log <- run_all_terms(terms, out_dir = "gtrends_csv", anchor = "weather")

# See any failures
log %>% filter(status != "ok")
