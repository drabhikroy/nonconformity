# Server tests for Nonconformity.
# testServer runs the real server function with simulated inputs, so these
# checks cover behavior that lives between the engine and the browser: what
# the reading panel says before data arrives, how out of range settings are
# handled, and what a person is told about an awkward upload.

suppressPackageStartupMessages(library(shiny))

# Sourcing app.R builds the app object without starting it.
app_env <- new.env()
sys.source("app.R", envir = app_env)
server <- app_env$server

ok <- 0; bad <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1; cat("pass ", label, "\n") }
  else { bad <<- bad + 1; cat("FAIL ", label, "\n") }
}
html_of <- function(x) paste(as.character(x$html %||% x), collapse = "")
`%||%` <- function(a, b) if (is.null(a)) b else a

testServer(server, {
  session$setInputs(sens = 4.5, penalty = 10, period_choice = "auto",
                    method = "resistant", persona = "operator")

  # Before data, the panel says data is needed, not that nothing was found.
  check("reading panel asks for data before any is loaded",
        grepl("once data is loaded", html_of(output$reading_cards)))

  session$setInputs(s_server = 1)
  check("server sample produces three events", nrow(result()$events) == 3)
  check("reading panel lists event cards once data is loaded",
        grepl("event-card", html_of(output$reading_cards)))
  check("summary uses the right article",
        grepl("This is an hourly series", html_of(output$summary_line)))

  # Settings the sliders cannot produce are held to the slider range rather
  # than turning the expected band inside out.
  session$setInputs(sens = -5)
  r <- result()
  check("negative sensitivity is held to the slider minimum",
        !is.null(r) && r$meta$sens == 2 && all(r$series$hi >= r$series$lo))
  session$setInputs(sens = "abc")
  check("a non numeric sensitivity falls back to the default",
        !is.null(result()) && result()$meta$sens == 4.5)
  session$setInputs(sens = 4.5, method = "<script>x</script>")
  check("an unknown method key falls back to the resistant method",
        result()$meta$method == "resistant")

  # Every method and cycle combination runs to a result for every sample.
  all_ran <- TRUE
  for (smp in c("s_server", "s_transactions", "s_sensor")) {
    do.call(session$setInputs, setNames(list(1 + sample(1000, 1)), smp))
    for (m in c("resistant", "rolling_z", "stl"))
      for (pc in c("auto", "none", "day", "week", "year")) {
        session$setInputs(method = m, period_choice = pc)
        if (is.null(result())) all_ran <- FALSE
      }
  }
  check("every sample, method, and cycle combination produces a result", all_ran)
})

cat(sprintf("\n%d passed, %d failed\n", ok, bad))
if (bad > 0) quit(status = 1)
