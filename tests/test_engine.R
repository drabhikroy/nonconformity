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
start <- grep("^# Cadence inference", src)[1] - 2L
stop <- grep("^# UI$", src)[1] - 2L
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

# The drawn band must agree with what the method flags. Rolling z-score
# judges against a threshold that moves reading by reading, so an averaged
# band width once put most of its own marks inside the band. Every method
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

cat(sprintf("\n%d passed, %d failed\n", ok, bad))
if (bad > 0) quit(status = 1)
