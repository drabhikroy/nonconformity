# Engine tests for Nonconformity.
# The sample generators plant events at known rows, so these checks assert
# the pipeline recovers each planted story beat without hand tuning.

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(tibble)
  library(readr); library(lubridate)
})

# Sourcing app.R would boot Shiny, so the engine block is extracted by its
# section fences and evaluated on its own.
src <- readLines("app.R")
start <- grep("^# Cadence inference ----", src)[1]
stop <- grep("^# UI ----", src)[1] - 1L
eval(parse(text = src[start:stop]))

ok <- 0; bad <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1; cat("pass ", label, "\n") }
  else { bad <<- bad + 1; cat("FAIL ", label, "\n") }
}

# Cadence inference reads each sample correctly.
check("hourly cadence read from server sample",
      infer_cadence(sample_server_load()$stamp)$label == "hourly")
check("daily cadence read from transactions sample",
      infer_cadence(sample_transactions()$stamp)$label == "daily")
check("hourly period is 24",
      infer_cadence(sample_server_load()$stamp)$period == 24L)

# Server load sample: one spike near row 300, one run near rows 520 to 526,
# one shift near row 720.
res <- analyze_series(sample_server_load())
ev <- res$events
check("server sample yields events", nrow(ev) >= 3)
check("spike found near planted hour",
      any(ev$type == "spike" & abs(ev$peak - 300) <= 2))
check("outage found as a run overlapping planted rows",
      any(ev$type == "run" & ev$start <= 526 & ev$end >= 520))
check("deploy found as a shift near planted row",
      any(ev$type == "shift" & abs(ev$peak - 720) <= 30))
check("shift direction is above",
      ev %>% filter(type == "shift") %>% slice(1) %>% pull(direction) == "above")

# Transactions sample: dip near day 178, shift near day 365, spike near 540.
res2 <- analyze_series(sample_transactions())
ev2 <- res2$events
check("transactions dip found", any(ev2$type == "dip" & abs(ev2$peak - 178) <= 2))
check("transactions shift found", any(ev2$type == "shift" & abs(ev2$peak - 365) <= 40))
check("transactions spike found", any(ev2$type == "spike" & abs(ev2$peak - 540) <= 2))
check("weekly rhythm carries real weight", res2$meta$season_share > 0.05)

# Sensor sample: a sustained rise and a single glitch dip.
res3 <- analyze_series(sample_sensor())
ev3 <- res3$events
check("sensor fault found as run or shift pair",
      any(ev3$type %in% c("run", "shift") & ev3$start >= 180 & ev3$start <= 240))
check("sensor glitch found as dip", any(ev3$type == "dip" & abs(ev3$peak - 400) <= 1))

# At default settings each sample shows its planted beats and nothing else,
# so a first open tells a clean story.
check("server sample shows exactly three events", nrow(ev) == 3)
check("transactions sample shows exactly three events", nrow(ev2) == 3)
check("sensor sample shows exactly two events", nrow(ev3) == 2)

# Every registered method runs and returns the shared schema, so the chart,
# cards, and model layer stay method agnostic.
schema_cols <- c("type", "peak", "direction", "weight", "id")
for (mkey in method_registry$key) {
  r <- analyze_series(sample_transactions(), method = mkey)
  has_schema <- nrow(r$events) == 0 || all(schema_cols %in% names(r$events))
  check(paste("method", mkey, "runs and returns the shared schema"),
        is.list(r) && has_schema && !is.null(r$series$expected))
}

# The resistant method still finds the transactions step up that a plain
# rolling z-score cannot, which is the point of offering more than one.
res_shift <- analyze_series(sample_transactions(), method = "resistant")
check("resistant method finds the transactions shift",
      any(res_shift$events$type == "shift"))

# The band on the chart must agree with what the method flags. Rolling z-score
# judges against a threshold that moves reading by reading, so an averaged
# band width would put most of its own marks inside the band. Every method
# is checked against every sample here.
band_mismatch <- function(r) {
  sr <- r$series
  outside <- (sr$value > sr$hi) | (sr$value < sr$lo)
  flagged <- rep(FALSE, nrow(sr))
  if (nrow(r$events) > 0) {
    for (i in seq_len(nrow(r$events))) {
      # Shifts mark a boundary rather than a reading outside the band.
      if (r$events$type[i] != "shift") {
        flagged[r$events$start[i]:r$events$end[i]] <- TRUE
      }
    }
  }
  sum(xor(flagged, outside))
}
for (mkey in method_registry$key) {
  for (nm in c("server", "transactions", "sensor")) {
    d <- switch(nm, server = sample_server_load(),
                transactions = sample_transactions(), sensor = sample_sensor())
    check(sprintf("band agrees with flags for %s on %s sample", mkey, nm),
          band_mismatch(analyze_series(d, method = mkey)) == 0)
  }
}

# An unknown method falls back completely rather than reporting a name
# beside results the default produced.
fb <- analyze_series(sample_sensor(), method = "no_such_method")
check("unknown method falls back to a consistent name and key",
      fb$meta$method == "resistant" && fb$meta$method_label == "Resistant seasonal")

# Sensitivity behaves monotonically: lower threshold, at least as many events.
loose <- analyze_series(sample_server_load(), sens = 2.5)
tight <- analyze_series(sample_server_load(), sens = 5.5)
check("lower sensitivity finds at least as many events",
      nrow(loose$events) >= nrow(tight$events))

# The reading engine returns nonempty prose for every event and the summary.
check("overall prose is nonempty", nchar(overall_prose(res)) > 80)
prose_ok <- all(map_lgl(seq_len(nrow(ev)), function(i) {
  nchar(event_prose(as.list(ev[i, ]), res$series, res$meta)) > 60
}))
check("every event card has prose", prose_ok)

# Upload parsing round trip through the template shape.
tmp <- tempfile(fileext = ".csv")
readr::write_csv(sample_sensor(), tmp)
parsed <- parse_upload(tmp)
check("template round trip parses", parsed$ok && nrow(parsed$df) == nrow(sample_sensor()))

# A malformed file gives a reason, not an error.
tmp2 <- tempfile(fileext = ".csv")
writeLines(c("a,b", "x,y"), tmp2)
parsed2 <- parse_upload(tmp2)
check("bad file returns a plain reason", !parsed2$ok && nchar(parsed2$why) > 10)

# ---------------------------------------------------------------------------
# Behavior checks with expected values taken from theory or hand
# arithmetic rather than from running the code first.
# ---------------------------------------------------------------------------

# The slider promises the same unit for every method, so on pure Gaussian
# noise each one should flag at roughly the normal tail rate. At 4.5 that
# rate is about 7 in a million; the bound allows generous slack for short
# trailing windows while still catching a unit mismatch, which shows up as
# hundreds of times the expected rate.
noise_rate <- function(method, seasonal) {
  st <- seq(as.POSIXct("2025-01-01", tz = "UTC"), by = "hour", length.out = 24 * 120)
  rates <- vapply(1:3, function(seed) {
    set.seed(seed)
    v <- 50 + rnorm(length(st), 0, 2)
    if (seasonal) v <- v + 8 * sin(seq_along(st) / 24 * 2 * pi)
    r <- analyze_series(tibble(stamp = st, value = v), method = method,
                        period_choice = if (seasonal) "auto" else "none")
    pts <- r$events[r$events$type != "shift", ]
    sum(pts$len) / length(st)
  }, numeric(1))
  mean(rates)
}
for (mkey in method_registry$key) {
  check(sprintf("%s flags flat noise at a tail rate", mkey),
        noise_rate(mkey, FALSE) < 0.0005)
  check(sprintf("%s flags seasonal noise at a tail rate", mkey),
        noise_rate(mkey, TRUE) < 0.0005)
}

# With units consistent, the alternative methods find the planted spike
# and dip in the transactions sample without burying them in noise.
for (mkey in c("rolling_z", "stl")) {
  r <- analyze_series(sample_transactions(), method = mkey)
  check(sprintf("%s finds the planted dip and spike and little else", mkey),
        any(r$events$type == "dip" & abs(r$events$peak - 178) <= 2) &&
        any(r$events$type == "spike" & abs(r$events$peak - 540) <= 2) &&
        nrow(r$events) <= 4)
}

# A short series climbing in a perfect line has no events, which depends
# on the drift estimate staying on for short files.
line10 <- analyze_series(tibble(
  stamp = seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "hour", length.out = 10),
  value = as.numeric(1:10)))
check("a ten reading straight line reports no events", nrow(line10$events) == 0)

# The level shift search gives the same cuts as a direct, slow computation
# of every split, written here independently of the app.
naive_cuts <- function(x, scale, penalty, min_len) {
  out <- integer(0)
  sse <- function(v) sum((v - mean(v))^2)
  go <- function(lo, hi, depth) {
    n <- hi - lo + 1L
    if (n < 2L * min_len || depth > 8L) return(invisible())
    v <- x[lo:hi]; cuts <- min_len:(n - min_len)
    g <- sapply(cuts, function(t) sse(v) - sse(v[1:t]) - sse(v[(t + 1):n]))
    if (max(g) > penalty * scale^2 * log(length(x))) {
      ct <- lo + cuts[which.max(g)] - 1L; out <<- c(out, ct)
      go(lo, ct, depth + 1L); go(ct + 1L, hi, depth + 1L)
    }
  }
  go(1L, length(x), 1L); sort(out)
}
set.seed(11)
agree <- all(vapply(1:40, function(i) {
  x <- rnorm(300); x[120:300] <- x[120:300] + 4; if (i %% 4 == 0) x <- x + 3e9
  identical(as.integer(find_shifts(x, 1, 10, 5L)), as.integer(naive_cuts(x, 1, 10, 5L)))
}, logical(1)))
check("fast level shift search matches the direct computation", agree)

# Speed matters because every slider move recomputes. Sixty thousand
# minute readings is a few weeks of data.
st60 <- seq(as.POSIXct("2025-01-01", tz = "UTC"), by = "min", length.out = 60000)
set.seed(9)
t60 <- system.time(analyze_series(tibble(stamp = st60,
  value = 100 + 10 * sin(seq_along(st60) / 60 * 2 * pi) + rnorm(60000))))[["elapsed"]]
check(sprintf("resistant method handles 60,000 readings in under 15 seconds (%.1f s)", t60),
      t60 < 15)

# Number formatting, including values past the 32 bit integer limit.
check("three billion formats with digit groups",
      fmt_num(3e9) == "3,000,000,000" && fmt_num(-2.2e9) == "-2,200,000,000")
check("very large values switch to scientific notation", grepl("e\\+", fmt_num(1e15)))
check("small values keep four significant digits", fmt_num(12.3456) == "12.35")
set.seed(3)
st_big <- seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "hour", length.out = 24 * 21)
v_big <- 3e9 + 4e8 * sin(seq_along(st_big) / 24 * 2 * pi) + rnorm(length(st_big), 0, 5e7)
v_big[200] <- v_big[200] + 2e9
r_big <- analyze_series(tibble(stamp = st_big, value = v_big))
big_text <- c(overall_prose(r_big), vapply(seq_len(nrow(r_big$events)), function(i)
  event_prose(as.list(r_big$events[i, ]), r_big$series, r_big$meta), character(1)))
check("readings for billion scale data contain no NA", !any(grepl("\\bNA\\b", big_text)))

# Grammar in the summary sentence.
check("articles follow the cadence word",
      with_article("hourly") == "an hourly" && with_article("daily") == "a daily" &&
      with_article("unknown") == "an unknown" && with_article("minute") == "a minute")
check("counts agree in number", plural(1, "reading") == "1 reading" &&
      plural(1200, "reading") == "1,200 readings")
check("the hourly sample summary reads an hourly series",
      grepl("^This is an hourly series of 1,008 readings", overall_prose(res)))

# Uploads with repeated timestamps: the drop is reported, and too few
# distinct timestamps is refused with a reason.
st_dup <- format(seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "hour", length.out = 30),
                 "%Y-%m-%dT%H:%M:%SZ")
f_dup <- tempfile(fileext = ".csv")
writeLines(c("stamp,value", paste(rep(st_dup, 2), round(rnorm(60, 100, 5), 2), sep = ",")), f_dup)
p_dup <- parse_upload(f_dup)
check("duplicate timestamps are reported, not dropped silently",
      p_dup$ok && nrow(p_dup$df) == 30 && grepl("^30 rows", p_dup$note))
f_one <- tempfile(fileext = ".csv")
writeLines(c("stamp,value", paste(rep(st_dup[1], 40), 1:40, sep = ",")), f_one)
p_one <- parse_upload(f_one)
check("a file with one repeated timestamp is refused with a reason",
      !p_one$ok && grepl("distinct timestamps", p_one$why))
check("a clean upload carries no note", is.null(parsed$note))

cat(sprintf("\n%d passed, %d failed\n", ok, bad))
if (bad > 0) quit(status = 1)
