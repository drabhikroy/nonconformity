# Nonconformity 0.2.0
# Find where operational data breaks its own pattern.
#
# An open source time series review app for two audiences. An operator sees
# plain charts, plain readings, and a few honest controls. A researcher sees
# the same data with the method internals exposed: the raw scores, the choice
# of detector, and the decomposition each one rests on.
#
# The detection is classical and transparent. Several methods share one event
# schema, so the chart, the reading cards, and the optional local model all
# stay method agnostic. A language model running on the same machine is
# optional throughout and never changes a single number.
#
# License: PolyForm Noncommercial 1.0.0

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(lubridate)
})

app_version <- "0.2.0"

# ---------------------------------------------------------------------------
# Cadence inference.
# The median gap between stamps decides the cadence, and the cadence decides
# the seasonal cycle worth testing: hours repeat daily, days repeat weekly.
# ---------------------------------------------------------------------------

infer_cadence <- function(stamps) {
  gaps <- as.numeric(diff(stamps), units = "secs")
  gaps <- gaps[gaps > 0]
  if (length(gaps) == 0) return(list(label = "unknown", period = 0L, step = 0))
  med <- median(gaps)
  # Each row maps a gap band to a label and a natural cycle length.
  bands <- tribble(
    ~lo,      ~hi,       ~label,     ~period,
    0,        90,        "minute",   60L,
    90,       5400,      "hourly",   24L,
    5400,     129600,    "daily",    7L,
    129600,   907200,    "weekly",   52L,
    907200,   4000000,   "monthly",  12L
  )
  hit <- bands %>% filter(med >= lo, med < hi)
  if (nrow(hit) == 0) return(list(label = "irregular", period = 0L, step = med))
  list(label = hit$label[1], period = hit$period[1], step = med)
}

# ---------------------------------------------------------------------------
# Decomposition.
# A running median gives the trend because medians shrug off the very
# outliers the app is hunting, unlike a mean smoother that bends toward
# them. Seasonal terms are per position medians of the detrended values.
# ---------------------------------------------------------------------------

decompose_series <- function(value, periods) {
  n <- length(value)
  # A cycle needs at least four full repeats before its per position
  # medians mean anything; with fewer, the layer just memorizes noise.
  periods <- periods[periods >= 2L & n >= 4L * periods]
  base_p <- if (length(periods)) max(periods) else 0L
  # The window spans whole cycles, because a window of one and a half
  # cycles makes the median itself wobble with the phase of the cycle. It
  # also runs four cycles wide when the data allows, since a running
  # median absorbs any excursion longer than half its window, and an
  # absorbed excursion is an event the app fails to show.
  cap <- max(7L, floor(n / 3))
  k <- 4L * base_p + 1L
  if (k > cap) k <- 2L * base_p + 1L
  if (k > cap) k <- base_p + 1L
  if (k > cap) k <- cap
  k <- max(7L, k)
  if (k %% 2L == 0L) k <- k + 1L
  trend <- as.numeric(stats::runmed(value, k, endrule = "median"))
  seasonal <- rep(0, n)
  # Cycles peel off one at a time: hourly operations usually carry a daily
  # rhythm and a weekly one on top of it, and removing only the first
  # leaves weekend steps that masquerade as level shifts.
  for (p in periods) {
    pos <- ((seq_len(n) - 1L) %% p) + 1L
    med_by_pos <- tibble(pos = pos, d = value - trend - seasonal) %>%
      group_by(pos) %>%
      summarise(m = median(d), .groups = "drop")
    layer <- med_by_pos$m[match(pos, med_by_pos$pos)]
    seasonal <- seasonal + (layer - median(layer))
  }
  # A second trend pass runs on the deseasonalized series. Near the ends
  # the running median falls back to tiny windows that echo the raw value,
  # and on a raw series that echo still contains the cycle, which then
  # gets subtracted twice and sprays false flags across both edges.
  trend <- as.numeric(stats::runmed(value - seasonal, k, endrule = "median"))
  remainder <- value - trend - seasonal
  list(trend = trend, seasonal = seasonal, remainder = remainder)
}

# ---------------------------------------------------------------------------
# Point events.
# Remainders are scaled by their median absolute deviation, so the
# sensitivity slider reads as multiples of the usual wobble. Consecutive
# flagged rows merge into one event: short bursts are spikes or dips, and
# longer stretches count as sustained runs.
# ---------------------------------------------------------------------------

find_point_events <- function(score, min_run = 3L) {
  # An empty result still carries the full column set, so callers that add
  # before and after levels or bind rows never meet a missing column.
  empty <- tibble(start = integer(0), end = integer(0), peak = integer(0),
                  peak_score = numeric(0), mean_score = numeric(0),
                  len = integer(0), type = character(0), direction = character(0))
  flagged <- abs(score) >= 1
  if (!any(flagged)) return(empty)
  r <- rle(flagged)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$values)
  map_dfr(keep, function(i) {
    s <- starts[i]; e <- ends[i]
    seg <- score[s:e]
    peak_rel <- which.max(abs(seg))
    tibble(
      start = s, end = e,
      peak = s + peak_rel - 1L,
      peak_score = seg[peak_rel],
      mean_score = mean(seg),
      len = e - s + 1L
    )
  }) %>%
    mutate(
      type = case_when(
        len >= min_run ~ "run",
        peak_score > 0 ~ "spike",
        TRUE ~ "dip"
      ),
      direction = if_else(mean_score > 0, "above", "below")
    )
}

# ---------------------------------------------------------------------------
# Level shifts.
# Binary segmentation on the deseasonalized series. Each candidate split is
# the one that cuts the squared error the most, and it is kept only when
# the gain clears a penalty scaled to the noise level, so the slider moves
# in honest units rather than magic numbers.
# ---------------------------------------------------------------------------

find_shifts <- function(x, scale, penalty, min_len) {
  # Each accepted cut splits a segment in two, so the depth cap bounds the
  # search at 2^8 segments. Noisy data can otherwise keep finding marginal
  # splits far past the point where any of them describe the series.
  max_depth <- 8L
  out <- integer(0)
  seg_sse <- function(v) sum((v - mean(v))^2)
  recurse <- function(lo, hi, depth) {
    n <- hi - lo + 1L
    if (n < 2L * min_len || depth > max_depth) return(invisible(NULL))
    v <- x[lo:hi]
    total <- seg_sse(v)
    cuts <- seq.int(min_len, n - min_len)
    if (length(cuts) == 0) return(invisible(NULL))
    gains <- vapply(cuts, function(t) {
      total - seg_sse(v[1:t]) - seg_sse(v[(t + 1):n])
    }, numeric(1))
    best <- which.max(gains)
    threshold <- penalty * scale^2 * log(length(x))
    if (gains[best] > threshold) {
      cut_at <- lo + cuts[best] - 1L
      out <<- c(out, cut_at)
      recurse(lo, cut_at, depth + 1L)
      recurse(cut_at + 1L, hi, depth + 1L)
    }
    invisible(NULL)
  }
  if (length(x) >= 2L * min_len && scale > 0) recurse(1L, length(x), 1L)
  sort(out)
}

# ---------------------------------------------------------------------------
# Full analysis.
# Everything the client needs comes back as one list: the decorated series,
# the event table, and a meta block for the summary line.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Shared expected line.
# Every method rests on the same decomposition so their outputs are directly
# comparable: a running median trend, stacked seasonal medians, and a resistant
# scale from the remainder. Detectors receive this and disagree only about
# what counts as a departure.
# ---------------------------------------------------------------------------

build_expected <- function(df, period_choice = "auto") {
  cad <- infer_cadence(df$stamp)
  # Auto stacks the natural cycles for the cadence: hourly data usually
  # carries a daily and a weekly rhythm, minute data an hourly and a daily
  # one. Explicit choices test a single cycle.
  periods <- switch(period_choice,
    auto = switch(cad$label,
      minute = c(60L, 1440L),
      hourly = c(24L, 168L),
      daily = 7L,
      weekly = 52L,
      monthly = 12L,
      integer(0)),
    none = integer(0),
    day  = 24L,
    week = 7L,
    year = 12L,
    integer(0)
  )
  parts <- decompose_series(df$value, periods)
  n <- nrow(df)
  scale <- stats::mad(parts$remainder)
  if (!is.finite(scale) || scale == 0) scale <- stats::sd(parts$remainder)
  if (!is.finite(scale) || scale == 0) scale <- 1
  list(cad = cad, periods = periods, parts = parts, n = n, scale = scale,
       period = if (length(periods)) periods[1] else 0L)
}

# ---------------------------------------------------------------------------
# Detector contract.
#
# Every method is a function of (df, ex, sens, penalty) returning a list of:
#
#   events    a data frame in the shared event schema, possibly with no rows
#   expected  the fitted level at every reading
#   lo, hi    the band the method judged against, one value per reading
#   scale     a single representative width, for the researcher panel
#
# Two of the three methods ignore penalty, since only segmentation reads it.
# They take it anyway so the dispatcher can call any detector the same way,
# and so the registry stays the only place that knows which is which.
#
# lo and hi are per reading rather than a single width on purpose. A method
# whose threshold moves from reading to reading would otherwise show marks
# sitting inside its own expected range, which reads as a contradiction.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Method: resistant seasonal.
# The flagship detector. Medians resist the very outliers it hunts, binary
# segmentation finds level shifts, and per segment offsets fold into the
# expected line so a step never sprays false spikes after it. This is the
# default an operator sees.
# ---------------------------------------------------------------------------

detect_resistant <- function(df, ex, sens, penalty) {
  parts <- ex$parts; n <- ex$n; scale <- ex$scale; periods <- ex$periods
  deseason <- df$value - parts$seasonal
  # A shift must outlast the longest removed cycle, or leftover rhythm
  # would read as a stack of tiny steps.
  fitted <- periods[4L * periods <= n]
  min_len <- max(5L, if (length(fitted)) max(fitted) else 0L)

  # Segmentation assumes level plus noise, so a steady drift would read as
  # a staircase of false steps. A lag difference median gives a slope that
  # a genuine step cannot fool, because only a small share of the pairs
  # straddle any one step.
  h <- max(min_len, floor(n / 6))
  slope <- if (n > 3L * h && h >= 1L) median(diff(deseason, lag = h)) / h else 0
  # Winsorizing against a rolling median keeps single outliers from buying
  # cuts with their squared error, while a genuine step passes through
  # untouched because the rolling median follows it within half a window.
  base <- deseason - slope * seq_len(n)
  km <- min(min_len, n)
  if (km %% 2L == 0L) km <- km + 1L
  local_med <- if (n >= km) as.numeric(stats::runmed(base, km, endrule = "median")) else rep(median(base), n)
  clipped <- pmin(pmax(base, local_med - 4 * scale), local_med + 4 * scale)
  cuts <- find_shifts(clipped, scale, penalty, min_len)

  # A cut only earns a card when it moves the level by at least two
  # wobbles either side of it; long segments can buy a cut with pooled
  # squared error even when the local step is trivial.
  if (length(cuts) > 0) {
    strong <- vapply(cuts, function(ct) {
      lo <- max(1L, ct - 2L * min_len + 1L)
      hi <- min(n, ct + 2L * min_len)
      abs(mean(base[(ct + 1L):hi]) - mean(base[lo:ct])) >= 2 * scale
    }, logical(1))
    cuts <- cuts[strong]
  }
  # Two nearby cuts that step away and straight back are one excursion,
  # not two regime changes. Dropping the pair lets the point scorer show
  # the stretch as a single sustained run instead.
  if (length(cuts) >= 2) {
    drop <- rep(FALSE, length(cuts))
    bnds <- c(0L, cuts, n)
    for (j in seq_len(length(cuts) - 1L)) {
      if (cuts[j + 1L] - cuts[j] < 2L * min_len) {
        m_prev <- median(base[(bnds[j] + 1L):cuts[j]])
        m_next <- median(base[(cuts[j + 1L] + 1L):bnds[j + 3L]])
        if (abs(m_next - m_prev) < 2 * scale) drop[j] <- drop[j + 1L] <- TRUE
      }
    }
    cuts <- cuts[!drop]
  }

  shifts <- map_dfr(cuts, function(ct) {
    lo <- max(1L, ct - min_len * 2L + 1L)
    hi <- min(nrow(df), ct + min_len * 2L)
    before <- mean(deseason[lo:ct])
    after <- mean(deseason[(ct + 1L):hi])
    tibble(
      start = ct, end = ct + 1L, peak = ct,
      peak_score = (after - before) / scale,
      mean_score = (after - before) / scale,
      len = 1L,
      type = "shift",
      direction = if_else(after > before, "above", "below"),
      before_level = before, after_level = after
    )
  })

  # The running median lags a genuine step, and that lag would spray false
  # spikes right after every shift. Folding a per segment offset into the
  # expected line absorbs both the step and the lag before points are
  # scored.
  bounds <- c(0L, cuts, n)
  step <- rep(0, n)
  for (b in seq_len(length(bounds) - 1L)) {
    seg <- (bounds[b] + 1L):bounds[b + 1L]
    step[seg] <- median(df$value[seg] - parts$seasonal[seg] - parts$trend[seg])
  }
  remainder <- df$value - parts$trend - parts$seasonal - step
  scale2 <- stats::mad(remainder)
  if (!is.finite(scale2) || scale2 == 0) scale2 <- stats::sd(remainder)
  if (!is.finite(scale2) || scale2 == 0) scale2 <- 1
  score <- remainder / (scale2 * sens)
  points <- find_point_events(score)
  # Right at a cut the expected level pivots, so a residual there says
  # more about boundary placement than about the data. Points hugging a
  # cut are dropped rather than shown as their own events.
  if (nrow(points) > 0 && length(cuts) > 0) {
    near_cut <- map_lgl(points$peak, function(p) any(abs(p - cuts) <= 2L))
    points <- points[!near_cut, ]
  }

  events <- bind_rows(
    points %>% mutate(before_level = NA_real_, after_level = NA_real_),
    shifts
  )
  expected <- parts$trend + parts$seasonal + step
  list(events = events, expected = expected, scale = scale2,
       lo = expected - sens * scale2, hi = expected + sens * scale2)
}

# ---------------------------------------------------------------------------
# Method: rolling z-score.
# A textbook operator method. The expected level is a trailing mean and the
# spread a trailing standard deviation, so it reacts fast to sudden spikes
# and makes no seasonal assumption. It ignores level shifts by design, which
# is exactly why offering it beside the resistant method is instructive.
# ---------------------------------------------------------------------------

detect_rolling_z <- function(df, ex, sens, penalty) {
  n <- ex$n
  w <- max(12L, min(ex$period * 2L, floor(n / 4)))
  if (w < 5L) w <- 5L
  center <- numeric(n); spread <- numeric(n)
  # A trailing window only, because an operator watching a live feed cannot
  # borrow from the future. The center is a trailing median and the spread a
  # trailing median absolute deviation, so the point under test cannot
  # inflate its own baseline the way a mean and standard deviation would and
  # then hide the very spike that should trip the alarm.
  for (i in seq_len(n)) {
    lo <- max(1L, i - w)
    seg <- df$value[lo:max(lo, i - 1L)]
    center[i] <- median(seg)
    spread[i] <- stats::mad(seg)
  }
  spread[!is.finite(spread) | spread == 0] <- ex$scale
  z <- (df$value - center) / spread
  lo <- center - sens * spread
  hi <- center + sens * spread
  if (n > w) {
    # The first window has too little history for a trustworthy baseline,
    # so nothing there is judged. The band widens to take in whatever was
    # observed, rather than drawing a verdict the method declined to give.
    warm <- seq_len(w)
    z[warm] <- 0
    lo[warm] <- pmin(lo[warm], df$value[warm])
    hi[warm] <- pmax(hi[warm], df$value[warm])
  }
  points <- find_point_events(z / sens)
  # The threshold moves with the trailing spread, so the band has to move
  # with it too. Reporting a single averaged width here would draw a band
  # that disagreed with the marks sitting on top of it.
  list(events = points %>% mutate(before_level = NA_real_, after_level = NA_real_),
       expected = center, scale = median(spread), lo = lo, hi = hi)
}

# ---------------------------------------------------------------------------
# Method: STL remainder.
# Seasonal and trend decomposition by loess, from base R stats::stl. The
# remainder is tagged where it leaves an interquartile fence, the approach
# popularized for operational data by the anomalize package. It needs a
# defined cycle, so it falls back to the resistant method when none applies.
# ---------------------------------------------------------------------------

detect_stl <- function(df, ex, sens, penalty) {
  p <- ex$period
  if (p < 2L || ex$n < 2L * p) return(detect_resistant(df, ex, sens, penalty))
  ts_obj <- stats::ts(df$value, frequency = p)
  fit <- tryCatch(stats::stl(ts_obj, s.window = "periodic", robust = TRUE),
                  error = function(e) NULL)
  if (is.null(fit)) return(detect_resistant(df, ex, sens, penalty))
  comps <- as.data.frame(fit$time.series)
  remainder <- comps$remainder
  # An interquartile fence widened by the sensitivity slider, which is the
  # classic STL rule rather than a standard deviation gate. quantile keeps
  # its percentile names, and an unnamed width is what the caller expects.
  qs <- unname(stats::quantile(remainder, c(0.25, 0.75)))
  half_iqr <- (qs[2] - qs[1]) / 2
  if (!is.finite(half_iqr) || half_iqr == 0) half_iqr <- ex$scale
  points <- find_point_events(remainder / (half_iqr * sens))
  expected <- comps$trend + comps$seasonal
  list(events = points %>% mutate(before_level = NA_real_, after_level = NA_real_),
       expected = expected, scale = half_iqr,
       lo = expected - sens * half_iqr, hi = expected + sens * half_iqr)
}

# ---------------------------------------------------------------------------
# Method registry.
# One place names every method, its audience, and the function that runs it,
# so the UI and the dispatcher never drift apart.
# ---------------------------------------------------------------------------

method_registry <- tibble::tribble(
  ~key,        ~label,                ~audience,     ~fn,                 ~uses_penalty,
  "resistant", "Resistant seasonal",  "operator",    "detect_resistant",     TRUE,
  "rolling_z", "Rolling z-score",     "operator",    "detect_rolling_z",  FALSE,
  "stl",       "STL remainder",       "researcher",  "detect_stl",        FALSE
)

# Only segmentation based methods read the shift penalty, so the control
# that sets it is shown for those and hidden for the rest. Deriving the
# condition here keeps the panel honest when a method is added later.
penalty_condition <- paste(
  sprintf("input.method == '%s'", method_registry$key[method_registry$uses_penalty]),
  collapse = " || ")

# ---------------------------------------------------------------------------
# Full analysis.
# The dispatcher builds the shared expected line, runs the chosen detector,
# then decorates and ranks its events into the one schema every downstream
# piece reads.
# ---------------------------------------------------------------------------

analyze_series <- function(df, sens = 4.5, penalty = 10, period_choice = "auto",
                           method = "resistant") {
  df <- df %>% arrange(stamp) %>% distinct(stamp, .keep_all = TRUE)
  # An unrecognized key falls back rather than failing, but it falls back
  # completely: reporting the requested name beside results the default
  # produced would misdescribe the run.
  if (!method %in% method_registry$key) method <- "resistant"
  spec <- method_registry[method_registry$key == method, ]

  ex <- build_expected(df, period_choice)
  out <- get(spec$fn)(df, ex, sens, penalty)

  events <- out$events
  if (nrow(events) > 0) {
    events <- events %>%
      mutate(
        weight = if_else(type == "shift",
                         abs(peak_score) * 2,
                         abs(peak_score) * sens * sqrt(len)),
        id = paste0("ev", row_number())
      ) %>%
      arrange(start)
  }

  # Each detector reports the band it actually judged against. A band built
  # from one averaged width would contradict its own marks wherever the
  # threshold moves from reading to reading.
  series <- df %>%
    mutate(
      trend = ex$parts$trend,
      expected = out$expected,
      lo = out$lo,
      hi = out$hi
    )

  season_share <- if (ex$period >= 2L && var(df$value) > 0) {
    round(var(ex$parts$seasonal) / var(df$value), 3)
  } else 0

  list(
    series = series,
    events = events,
    meta = list(
      n = nrow(df),
      from = min(df$stamp), to = max(df$stamp),
      cadence = ex$cad$label,
      period = ex$period,
      scale = out$scale,
      sens = sens,
      method = method,
      method_label = spec$label,
      season_share = season_share
    )
  )
}

# ---------------------------------------------------------------------------
# Reading engine.
# Deterministic prose built from the numbers alone, so the app reads the
# same with or without a local model. The model, when present, only adds a
# labeled note under this text.
# ---------------------------------------------------------------------------

fmt_stamp <- function(x, cadence) {
  if (cadence %in% c("minute", "hourly")) format(x, "%b %d, %H:%M")
  else format(x, "%b %d, %Y")
}

fmt_num <- function(x) {
  if (!is.finite(x)) return("?")
  if (abs(x) >= 1000) formatC(x, format = "d", big.mark = ",")
  else formatC(x, format = "g", digits = 4)
}

event_title <- function(type, direction) {
  switch(type,
    spike = "Spike",
    dip = "Dip",
    run = if (direction == "above") "Sustained high stretch" else "Sustained low stretch",
    shift = if (direction == "above") "Level shift up" else "Level shift down"
  )
}

event_prose <- function(ev, series, meta) {
  at <- fmt_stamp(series$stamp[ev$peak], meta$cadence)
  obs <- series$value[ev$peak]
  exp <- series$expected[ev$peak]
  mult <- abs(ev$peak_score) * (if (ev$type == "shift") 1 else meta$sens)
  if (ev$type %in% c("spike", "dip")) {
    dir_word <- if (ev$type == "spike") "above" else "below"
    sprintf(
      paste0("At %s the value reached %s while the expected level was near %s, ",
             "about %.1f times the usual wobble %s it. Single point departures ",
             "like this often trace back to one discrete cause."),
      at, fmt_num(obs), fmt_num(exp), mult, dir_word)
  } else if (ev$type == "run") {
    from <- fmt_stamp(series$stamp[ev$start], meta$cadence)
    to <- fmt_stamp(series$stamp[ev$end], meta$cadence)
    sprintf(
      paste0("From %s to %s the series stayed %s its expected range for %d ",
             "readings in a row, peaking at about %.1f times the usual wobble. ",
             "A stretch this long points to a condition that persisted rather ",
             "than a one off blip."),
      from, to, ev$direction, ev$len, mult)
  } else {
    sprintf(
      paste0("Around %s the typical level moved from about %s to about %s and ",
             "stayed there. The change equals %.1f times the usual wobble, and ",
             "a step that holds usually means something structural changed, such ",
             "as a deploy, a price change, or a policy taking effect."),
      at, fmt_num(ev$before_level), fmt_num(ev$after_level), mult)
  }
}

overall_prose <- function(res) {
  m <- res$meta
  ev <- res$events
  span <- sprintf("%s to %s", fmt_stamp(m$from, m$cadence), fmt_stamp(m$to, m$cadence))
  season_bit <- if (m$period >= 2L && m$season_share >= 0.05) {
    sprintf(" A repeating cycle explains about %d percent of the movement.",
            round(m$season_share * 100))
  } else ""
  if (is.null(ev) || nrow(ev) == 0) {
    return(sprintf(
      paste0("This is a %s series of %s readings from %s. Nothing crossed the ",
             "current sensitivity line.%s Lower the sensitivity to look closer."),
      m$cadence, formatC(m$n, big.mark = ","), span, season_bit))
  }
  counts <- ev %>% count(type)
  label_for <- c(spike = "spike", dip = "dip", run = "sustained stretch", shift = "level shift")
  bits <- counts %>%
    mutate(word = label_for[type],
           text = sprintf("%d %s%s", n, word, if_else(n > 1, "s", ""))) %>%
    pull(text)
  listing <- if (length(bits) == 1) bits else
    paste(paste(bits[-length(bits)], collapse = ", "), "and", bits[length(bits)])
  sprintf(
    paste0("This is a %s series of %s readings from %s.%s The current settings ",
           "surface %s. Cards below rank them by weight, and each marker on the ",
           "chart matches one card."),
    m$cadence, formatC(m$n, big.mark = ","), span, season_bit, listing)
}

# ---------------------------------------------------------------------------
# Sample data.
# Three worked datasets with story beats planted at known places, so the
# walkthrough can point at real events and the tests can assert on them.
# ---------------------------------------------------------------------------

sample_server_load <- function() {
  hours <- seq(ymd_h("2026-05-04 00"), by = "hour", length.out = 24 * 42)
  base <- crossing(day = 0:41, hour = 0:23) %>%
    mutate(
      weekday = wday(hours[day * 24 + hour + 1], week_start = 1),
      daily = 120 * sin((hour - 6) / 24 * 2 * pi) + 130,
      weekly = if_else(weekday >= 6, -60, 25),
      value = 240 + daily + weekly
    ) %>%
    pull(value)
  set.seed(42)
  value <- base + rnorm(length(base), 0, 14)
  value[300] <- value[300] + 260          # traffic surge, one hour
  value[520:526] <- value[520:526] - 190  # outage, seven hours
  value[720:length(value)] <- value[720:length(value)] + 90  # capacity added
  tibble(stamp = hours, value = round(value, 1))
}

sample_transactions <- function() {
  days <- seq(ymd("2024-07-01"), by = "day", length.out = 730)
  set.seed(7)
  weekday_lift <- c(1.0, 1.05, 1.05, 1.08, 1.25, 1.35, 0.8)
  value <- tibble(stamp = days) %>%
    mutate(
      i = row_number(),
      trend = 1800 + i * 1.1,
      wd = weekday_lift[wday(stamp, week_start = 1)],
      value = trend * wd + rnorm(n(), 0, 90)
    ) %>%
    pull(value)
  value[178] <- value[178] * 0.35    # payment processor outage
  value[365:730] <- value[365:730] + 420  # second market opened
  value[540] <- value[540] * 1.8     # promotion day
  tibble(stamp = days, value = round(value, 2))
}

sample_sensor <- function() {
  stamps <- seq(ymd_h("2026-06-01 00"), by = "hour", length.out = 24 * 21)
  set.seed(11)
  value <- tibble(stamp = stamps) %>%
    mutate(
      hour = hour(stamp),
      daily = 4.5 * sin((hour - 4) / 24 * 2 * pi),
      value = 21.5 + daily + rnorm(n(), 0, 0.5)
    ) %>%
    pull(value)
  value[200:230] <- value[200:230] + 6.5  # cooling fault, sustained rise
  value[400] <- value[400] - 9            # sensor glitch, one reading
  tibble(stamp = stamps, value = round(value, 2))
}

# ---------------------------------------------------------------------------
# Upload parsing.
# The reader accepts a stamp column and a value column under several common
# names, and reports a plain reason when the shape does not fit.
# ---------------------------------------------------------------------------

parse_upload <- function(path) {
  raw <- tryCatch(read_csv(path, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 10) {
    return(list(ok = FALSE, why = "The file could not be read as CSV or has fewer than ten rows."))
  }
  stamp_names <- c("stamp", "timestamp", "time", "date", "datetime", "ds")
  value_names <- c("value", "y", "count", "load", "amount", "measure")
  sn <- intersect(tolower(names(raw)), stamp_names)
  vn <- intersect(tolower(names(raw)), value_names)
  names(raw) <- tolower(names(raw))
  if (length(sn) == 0 || length(vn) == 0) {
    # Fall back to first parseable column plus first numeric column.
    sn <- names(raw)[map_lgl(raw, ~ !all(is.na(suppressWarnings(parse_datetime(as.character(.x))))) ||
                               !all(is.na(suppressWarnings(parse_date(as.character(.x))))))][1]
    vn <- names(raw)[map_lgl(raw, is.numeric)][1]
  } else {
    sn <- sn[1]; vn <- vn[1]
  }
  if (is.na(sn) || is.na(vn) || is.null(sn) || is.null(vn)) {
    return(list(ok = FALSE, why = "No timestamp column and numeric value column pair was found."))
  }
  stamp <- suppressWarnings(parse_datetime(as.character(raw[[sn]])))
  if (all(is.na(stamp))) stamp <- suppressWarnings(as_datetime(parse_date(as.character(raw[[sn]]))))
  df <- tibble(stamp = stamp, value = as.numeric(raw[[vn]])) %>%
    filter(!is.na(stamp), is.finite(value))
  if (nrow(df) < 10) {
    return(list(ok = FALSE, why = "Fewer than ten rows had both a readable timestamp and a numeric value."))
  }
  list(ok = TRUE, df = df)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

marker_glyph <- function(type, size = 16) {
  # Small inline SVG glyphs mirror the chart markers so the legend, the
  # cards, and the map all speak the same shape language.
  h <- size / 2
  shape <- switch(type,
    spike = sprintf("<polygon points='%s,1 %s,%s 1,%s' />", h, size - 1, size - 1, size - 1),
    dip = sprintf("<polygon points='1,1 %s,1 %s,%s' />", size - 1, h, size - 1),
    run = sprintf("<rect x='1' y='%s' width='%s' height='%s' rx='2' />", h - 3, size - 2, 7),
    shift = sprintf("<polygon points='%s,1 %s,%s %s,%s 1,%s' />", h, size - 1, h, h, size - 1, h)
  )
  HTML(sprintf(
    "<svg class='event-glyph' width='%d' height='%d' viewBox='0 0 %d %d' aria-hidden='true' style='fill: var(--%s)'>%s</svg>",
    size, size, size, size, type, shape))
}

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Nonconformity"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "ui.js"),
    tags$script(src = "chart.js"),
    tags$script(src = "walkthrough.js"),
    tags$script(src = "model.js")
  ),
  tags$header(class = "app-header",
    div(class = "brand",
      HTML("<svg width='34' height='34' viewBox='0 0 34 34' aria-hidden='true'>
        <path d='M2 22 L8 22 L11 10 L15 28 L19 6 L22 22 L32 22'
              fill='none' stroke='var(--accent)' stroke-width='2.5'
              stroke-linecap='round' stroke-linejoin='round'/></svg>"),
      div(
        div(class = "brand-name", "Nonconformity"),
        div(class = "brand-tag", "Find where operational data breaks its own pattern.")
      )
    ),
    div(class = "header-controls",
      # The persona switch reshapes the whole app between a plain operator
      # view and a researcher view that exposes the method internals.
      div(class = "seg-group", role = "group", `aria-label` = "View",
        tags$button(class = "seg-btn persona-btn", type = "button",
                    `data-persona` = "operator", `aria-pressed` = "true", "Operator"),
        tags$button(class = "seg-btn persona-btn", type = "button",
                    `data-persona` = "researcher", `aria-pressed` = "false", "Researcher")
      ),
      tags$button(id = "tour-open", class = "btn-ghost", type = "button", "Walkthrough"),
      actionButton("data_guide", "Data guide", class = "btn-ghost"),
      actionButton("model_guide", "Local model", class = "btn-ghost"),
      div(class = "seg-group", role = "group", `aria-label` = "Color vision palette",
        tags$button(class = "seg-btn", type = "button", `data-palette` = "standard", "Standard"),
        tags$button(class = "seg-btn", type = "button", `data-palette` = "cb-deutan", "Deutan"),
        tags$button(class = "seg-btn", type = "button", `data-palette` = "cb-tritan", "Tritan"),
        tags$button(class = "seg-btn", type = "button", `data-palette` = "cb-mono", "Mono")
      ),
      tags$button(id = "theme-toggle", class = "btn-ghost", type = "button", "Light mode")
    )
  ),
  div(class = "layout",
    div(
      div(class = "panel-card",
        h2("Your data"),
        p(class = "hint", "A CSV with one timestamp column and one numeric value column."),
        div(class = "upload-zone", id = "drop-zone",
          fileInput("upload", label = NULL, accept = c(".csv"), width = "100%"),
          p("Drop a CSV here or browse. Nothing leaves your computer.")
        ),
        downloadLink("template_csv", "Download a template CSV"),
        h3("Or open a sample"),
        actionButton("s_server", label = HTML(
          "<span class='sample-title'>Server load</span>
           <span class='sample-sub'>Hourly, six weeks, one outage and one deploy</span>"),
          class = "sample-btn"),
        actionButton("s_transactions", label = HTML(
          "<span class='sample-title'>Daily transactions</span>
           <span class='sample-sub'>Two years, weekly rhythm, one big step up</span>"),
          class = "sample-btn"),
        actionButton("s_sensor", label = HTML(
          "<span class='sample-title'>Room temperature</span>
           <span class='sample-sub'>Three weeks, one cooling fault, one glitch</span>"),
          class = "sample-btn")
      ),
      div(class = "panel-card",
        h2("Detection settings"),
        div(class = "form-group",
          tags$label(`for` = "method", "Method"),
          p(class = "hint", "How departures are judged. Resistant seasonal is the default and suits data with a daily or weekly rhythm."),
          uiOutput("method_choices")
        ),
        div(class = "form-group",
          tags$label(`for` = "sens", "Sensitivity"),
          p(class = "hint", "How far a reading must sit from its expected level, in multiples of the usual wobble. Lower catches more."),
          sliderInput("sens", label = NULL, min = 2, max = 6, value = 4.5, step = 0.25, width = "100%")
        ),
        conditionalPanel(condition = penalty_condition,
          div(class = "form-group",
            tags$label(`for` = "penalty", "Shift caution"),
            p(class = "hint", "How much proof a lasting level change needs before it counts. Higher demands more."),
            sliderInput("penalty", label = NULL, min = 2, max = 30, value = 10, step = 1, width = "100%")
          )
        ),
        div(class = "form-group",
          tags$label(`for` = "period_choice", "Repeating cycle"),
          p(class = "hint", "Auto reads the cadence and tests the natural cycle. Pick None for data with no rhythm."),
          selectInput("period_choice", label = NULL, width = "100%",
            choices = c("Auto" = "auto", "None" = "none",
                        "Daily cycle for hourly data" = "day",
                        "Weekly cycle for daily data" = "week",
                        "Yearly cycle for monthly data" = "year"))
        )
      ),
      # The local model card grows into a small assistant. Beyond the per
      # card notes it can triage the whole run and answer questions about
      # the loaded series in a chat, all on the same machine.
      div(class = "panel-card",
        h2("Local model"),
        p(class = "hint", "Optional and fully local. It reads the analysis, not your raw series, and never changes a number."),
        div(class = "form-group",
          tags$label(`for` = "ollama-url", "Ollama address"),
          tags$input(id = "ollama-url", type = "text", value = "http://localhost:11434")
        ),
        tags$button(id = "model-check", class = "btn-ghost", type = "button", "Check connection"),
        tags$div(id = "model-status", class = "hint", role = "status", "Not checked yet."),
        tags$select(id = "model-pick", `aria-label` = "Model choice", style = "display:none;"),
        div(id = "model-actions", style = "display:none;",
          tags$button(id = "model-triage", class = "btn-solid", type = "button",
                      style = "margin-top:10px;", "Summarize this run"),
          tags$button(id = "model-notes", class = "btn-ghost", type = "button",
                      style = "margin-top:10px;", "Add a note to each event"),
          tags$button(id = "model-incident", class = "btn-ghost", type = "button",
                      style = "margin-top:10px;", "Draft an incident writeup")
        )
      )
    ),
    div(
      div(class = "panel-card chart-card",
        h2("The series"),
        htmlOutput("summary_line"),
        div(class = "chart-toolbar",
          tags$label(class = "toggle", tags$input(type = "checkbox", id = "show-band", checked = "checked"), "Expected range"),
          tags$label(class = "toggle", tags$input(type = "checkbox", id = "show-trend", checked = "checked"), "Trend"),
          tags$label(class = "toggle", tags$input(type = "checkbox", id = "show-marks", checked = "checked"), "Event markers"),
          div(style = "margin-left:auto;",
            tags$label(class = "control-label", `for` = "focus-window", "Focus"),
            tags$select(id = "focus-window", `aria-label` = "Focus window", style = "min-width: 170px; margin-left: 8px;",
              tags$option(value = "all", "Whole series"),
              tags$option(value = "biggest", "Around the biggest event"),
              tags$option(value = "tail", "Most recent quarter")
            )
          )
        ),
        div(class = "chart-wrap",
          div(id = "chart-host"),
          div(id = "chart-tip", class = "chart-tip"),
          div(id = "chart-live", class = "sr-live", `aria-live` = "polite")
        ),
        p(class = "hint", "Tab reaches the first marker. Arrow keys move between events. Each marker announces its card.")
      ),
      # The triage summary and the chat panel are model features, hidden
      # until a model produces something, so the app is whole without one.
      div(class = "panel-card model-summary-card", id = "triage-card", style = "display:none;",
        h2(id = "triage-title", "Model summary of this run"),
        div(id = "triage-body", class = "event-text")
      ),
      div(class = "panel-card", id = "chat-card", style = "display:none;",
        h2("Ask about this data"),
        p(class = "hint", "The model sees the analysis summary and the event list, not the raw readings. Answers are model output and worth the same skepticism."),
        div(id = "chat-log", class = "chat-log", `aria-live` = "polite"),
        div(class = "chat-row",
          tags$input(id = "chat-input", type = "text",
                     placeholder = "For example, which event looks most serious and why?",
                     `aria-label` = "Question about this data"),
          tags$button(id = "chat-send", class = "btn-solid", type = "button", "Ask")
        )
      ),
      div(class = "panel-card",
        h2("What stands out"),
        p(class = "hint", "One card per event, heaviest first. Click a card chip to light up its marker."),
        uiOutput("reading_cards")
      ),
      # The researcher panel is empty markup until the persona switch reveals
      # it, and it renders the method internals for the current run.
      div(class = "panel-card researcher-only", id = "researcher-panel",
        h2("Method detail"),
        p(class = "hint", "The internals behind this run, for checking the method rather than the data."),
        htmlOutput("method_detail"),
        h3("Event scores"),
        div(class = "table-scroll", tableOutput("score_table")),
        h3("Reproduce this call"),
        tags$pre(id = "repro-code", class = "repro-code", textOutput("repro_call", inline = TRUE))
      )
    )
  ),
  tags$footer(style = "padding: 8px 22px 20px; color: var(--muted); font-size: 12.5px;",
    HTML(sprintf(
      paste0("Nonconformity %s. Open source, local analysis only. ",
             "PolyForm Noncommercial 1.0.0. Built by Abhik Roy with ",
             "<a href='https://www.r-project.org/' target='_blank' rel='noopener'>R</a> and ",
             "<a href='https://shiny.posit.co/' target='_blank' rel='noopener'>Shiny</a>."),
      app_version))
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  dataset <- reactiveVal(NULL)
  source_name <- reactiveVal("")

  observeEvent(input$s_server, {
    dataset(sample_server_load()); source_name("Server load sample")
  })
  observeEvent(input$s_transactions, {
    dataset(sample_transactions()); source_name("Daily transactions sample")
  })
  observeEvent(input$s_sensor, {
    dataset(sample_sensor()); source_name("Room temperature sample")
  })

  observeEvent(input$upload, {
    parsed <- parse_upload(input$upload$datapath)
    if (!parsed$ok) {
      showModal(modalDialog(title = "That file did not fit",
        p(parsed$why),
        p("The data guide under the header explains the expected shape and offers a template."),
        easyClose = TRUE, footer = modalButton("Close")))
      return(invisible(NULL))
    }
    dataset(parsed$df); source_name(input$upload$name)
  })

  # The method list is filtered by persona: an operator sees the two plain
  # methods, a researcher sees all of them. The default method is resistant seasonal.
  output$method_choices <- renderUI({
    persona <- input$persona %||% "operator"
    avail <- if (persona == "researcher") method_registry
             else method_registry %>% filter(audience == "operator")
    choices <- setNames(avail$key, avail$label)
    radioButtons("method", label = NULL, choices = choices,
                 selected = "resistant", inline = FALSE)
  })

  result <- reactive({
    df <- dataset()
    if (is.null(df)) return(NULL)
    # An error inside a reactive takes the whole session down with it, which
    # presents to the person as the app dying with no explanation. Catching
    # it here keeps the rest of the app alive and says what went wrong.
    tryCatch(
      analyze_series(df,
        sens = input$sens,
        penalty = input$penalty,
        period_choice = input$period_choice,
        method = input$method %||% "resistant"),
      error = function(e) {
        showNotification(
          paste0("The analysis could not finish: ", conditionMessage(e),
                 ". Try another method or a different cycle setting."),
          type = "error", duration = NULL)
        NULL
      })
  })

  # The chart lives client side. The payload carries the series and events
  # as data frames, which Shiny serializes column wise, and chart.js turns
  # them back into row records before any drawing happens.
  observe({
    res <- result()
    if (is.null(res)) return(invisible(NULL))
    session$sendCustomMessage("render-chart", list(
      series = res$series %>% mutate(stamp = format(stamp, "%Y-%m-%dT%H:%M:%SZ")),
      events = if (nrow(res$events)) {
        res$events %>%
          mutate(
            title = map2_chr(type, direction, event_title),
            when = fmt_stamp(res$series$stamp[peak], res$meta$cadence)
          )
      } else list(),
      meta = list(
        cadence = res$meta$cadence,
        source = source_name(),
        method = res$meta$method,
        method_label = res$meta$method_label,
        season_share = res$meta$season_share,
        sens = res$meta$sens,
        n = res$meta$n,
        summary = overall_prose(res)
      )
    ))
  })

  output$summary_line <- renderUI({
    res <- result()
    if (is.null(res)) {
      return(p(class = "summary-line",
        "Open a sample on the left or bring your own CSV. The chart, the expected range, and the reading cards all appear the moment data lands."))
    }
    p(class = "summary-line", overall_prose(res))
  })

  output$reading_cards <- renderUI({
    res <- result()
    if (is.null(res) || nrow(res$events) == 0) {
      return(p(class = "hint", "No events under the current settings."))
    }
    ev <- res$events %>% arrange(desc(weight))
    cards <- pmap(ev, function(...) {
      e <- list(...)
      unit_mult <- if (e$type == "shift") abs(e$peak_score) else abs(e$peak_score) * res$meta$sens
      div(class = paste0("event-card type-", e$type), id = paste0("card-", e$id),
        div(class = "event-head",
          marker_glyph(e$type),
          div(
            div(class = "event-title", event_title(e$type, e$direction)),
            div(class = "event-when", fmt_stamp(res$series$stamp[e$peak], res$meta$cadence))
          )
        ),
        div(class = "event-text", event_prose(e, res$series, res$meta)),
        div(class = "chip-row",
          span(class = "stat-chip", sprintf("%.1fx usual wobble", unit_mult)),
          if (e$type == "run") span(class = "stat-chip", sprintf("%d readings", e$len)),
          if (e$type == "shift") span(class = "stat-chip",
            sprintf("%s to %s", fmt_num(e$before_level), fmt_num(e$after_level))),
          tags$button(class = "stat-chip mark-jump", type = "button",
            `data-event` = e$id, "Show on chart")
        ),
        div(class = "model-note", id = paste0("note-", e$id), style = "display:none;")
      )
    })
    div(class = "reading-grid", cards)
  })

  # Researcher outputs. These render regardless of persona, since the panel
  # that holds them is shown or hidden on the client, and computing them is
  # cheap next to the analysis itself.
  output$method_detail <- renderUI({
    res <- result()
    if (is.null(res)) return(p(class = "hint", "Load data to see method internals."))
    m <- res$meta
    cyc <- if (m$period >= 2L) sprintf("period %d, explaining %d percent of variance",
                                       m$period, round(m$season_share * 100)) else "none detected"
    tagList(
      tags$table(class = "kv-table",
        tags$tr(tags$td("Method"), tags$td(m$method_label)),
        tags$tr(tags$td("Readings"), tags$td(formatC(m$n, big.mark = ","))),
        tags$tr(tags$td("Cadence"), tags$td(m$cadence)),
        tags$tr(tags$td("Seasonal cycle"), tags$td(cyc)),
        tags$tr(tags$td("Resistant scale"), tags$td(sprintf("%.4g", m$scale))),
        tags$tr(tags$td("Sensitivity"), tags$td(sprintf("%.2f wobbles", m$sens))),
        tags$tr(tags$td("Events found"), tags$td(nrow(res$events)))
      )
    )
  })

  output$score_table <- renderTable({
    res <- result()
    if (is.null(res) || nrow(res$events) == 0) return(NULL)
    res$events %>%
      transmute(
        id, type,
        at = fmt_stamp(res$series$stamp[peak], res$meta$cadence),
        `raw score` = round(peak_score, 3),
        length = len,
        weight = round(weight, 2)
      ) %>%
      arrange(desc(weight))
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$repro_call <- renderText({
    res <- result()
    if (is.null(res)) return("analyze_series(df)")
    m <- res$meta
    sprintf('analyze_series(df, method = "%s", sens = %g, penalty = %g, period_choice = "%s")',
            m$method, m$sens, isolate(input$penalty %||% 10), isolate(input$period_choice %||% "auto"))
  })

  output$template_csv <- downloadHandler(
    filename = function() "nonconformity_template.csv",
    content = function(file) {
      sample_sensor() %>% slice_head(n = 48) %>% write_csv(file)
    }
  )

  observeEvent(input$data_guide, {
    showModal(modalDialog(title = "Data guide", size = "l", easyClose = TRUE,
      p("Nonconformity reads a plain CSV with two columns."),
      tags$table(
        tags$tr(tags$th("Column"), tags$th("Accepted names"), tags$th("Format")),
        tags$tr(tags$td("Timestamp"), tags$td("stamp, timestamp, time, date, datetime, ds"),
                tags$td("ISO dates or datetimes, one per row, any regular cadence")),
        tags$tr(tags$td("Value"), tags$td("value, y, count, load, amount, measure"),
                tags$td("Plain numbers, no currency signs or separators"))
      ),
      p("Ten readings is the floor, and a few full cycles work far better. Hourly data earns a daily cycle after two days, daily data earns a weekly cycle after two weeks."),
      p("The template link under the upload box gives a small working example."),
      footer = modalButton("Close")))
  })

  observeEvent(input$model_guide, {
    showModal(modalDialog(title = "Local model", size = "l", easyClose = TRUE,
      p("Nonconformity can put a language model running on your own machine to work on top of the analysis. The detection itself never touches the model, so every number on screen stays the same with or without one. The model reads the analysis, the event list, and the summary numbers, never your raw readings."),
      h3("What the model can do here"),
      tags$ul(
        tags$li("Summarize the whole run and say which events matter most and why."),
        tags$li("Add a short note to each event card naming likely operational causes and one check to run next."),
        tags$li("Answer typed questions about the loaded data in the Ask panel."),
        tags$li("Draft an incident writeup you can copy into a ticket or report.")
      ),
      h3("Setup"),
      tags$ol(
        tags$li(HTML("Install Ollama from <a href='https://ollama.com' target='_blank' rel='noopener'>ollama.com</a> and start it.")),
        tags$li("Pull a model in a terminal, for example: ollama pull llama3.2"),
        tags$li("Back here, press Check connection, pick the model, then use any of the model actions.")
      ),
      h3("Four options and their tradeoffs"),
      tags$table(
        tags$tr(tags$th("Model"), tags$th("Size"), tags$th("Character")),
        tags$tr(tags$td("llama3.2:3b"), tags$td("about 2 GB"), tags$td("Fast on most laptops, short and direct")),
        tags$tr(tags$td("qwen2.5:7b"), tags$td("about 4.7 GB"), tags$td("Stronger reasoning about operational causes")),
        tags$tr(tags$td("mistral:7b"), tags$td("about 4.1 GB"), tags$td("Balanced pace and quality")),
        tags$tr(tags$td("gemma2:9b"), tags$td("about 5.4 GB"), tags$td("Most careful wording, slowest of the four"))
      ),
      p("Every model output is labeled as such, kept apart from the deterministic reading, and worth the same skepticism as any model output."),
      footer = modalButton("Close")))
  })
}

shinyApp(ui, server)
