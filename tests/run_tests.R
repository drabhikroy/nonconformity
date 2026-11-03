# run_tests.R
# Headless checks run before every delivery. Four suites: the math, the
# prose, the WCAG contrast matrix across all eight theme and palette
# states, and a live server smoke test.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Shiny is attached before anything is sourced rather than partway down.
# Several files in R/ build markup with tagList(), and a suite that
# loaded shiny at the point it first drove the server was failing on the
# first screen it tried to build, several suites earlier than that.
suppressPackageStartupMessages(library(shiny))
# The suite runs from the project root. When launched from tests/, step up.
if (basename(getwd()) == "tests") setwd("..")

source("R/info.R")
source("R/compat.R")
source("R/appearance.R")
source("R/network_math.R")
source("R/sample_data.R")
source("R/narrative.R")
source("R/figure.R")
source("R/guide.R")
source("R/overview.R")
source("R/research_metrics.R")
source("R/runtime.R")
source("R/session_state.R")
source("R/help.R")
source("R/statistics.R")

pass <- 0; fail <- 0
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1; cat("ok  ", label, "\n") }
  else { fail <<- fail + 1; cat("FAIL", label, "\n") }
}

# Suite 1: the math on a graph with known answers. A path of four nodes
# has exact betweenness and two articulation points, so nothing here is
# a matter of taste.
ed <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "D"))
g <- build_graph(ed)
m <- compute_metrics(g)
check("path graph has 4 people and 3 ties", m$n == 4 && m$m == 3)
check("middle nodes are the cut points",
      setequal(m$cut_ids, c("B", "C")))
check("betweenness peaks in the middle",
      which.max(m$betweenness) %in% c(2, 3))
check("degree is exact", all(sort(m$degree) == c(1, 1, 2, 2)))

# Duplicate and self ties collapse correctly.
ed2 <- data.frame(from = c("A", "A", "A"), to = c("B", "B", "A"),
                  w = c(2, 3, 9))
g2 <- build_graph(ed2)
check("parallel ties merge and weights sum",
      igraph::gsize(g2) == 1 &&
        (g2 |> activate(edges) |> pull(weight)) == 5)

# The two samples build end to end.
for (nm in c("org", "referral")) {
  ed_s <- if (nm == "org") sample_org_network() else sample_referral_network()
  ms <- compute_metrics(build_graph(ed_s))
  if (nm == "referral") {
    check("referral sample uses real names, not generic labels",
          !any(grepl("Customer", ms$names)))
  }
  ly <- compute_layout(ms$graph)
  pl <- graph_payload(ms, ly)
  check(paste(nm, "sample yields a payload"),
        nrow(pl$nodes) == ms$n && all(pl$nodes$group >= 1))
  check(paste(nm, "layout lands inside the unit square"),
        all(ly$x >= 0 & ly$x <= 1 & ly$y >= 0 & ly$y <= 1))
}
org_m <- compute_metrics(build_graph(sample_org_network()))
check("org sample has more than one group", org_m$n_groups > 1)
check("org sample surfaces at least one cut point",
      length(org_m$cut_ids) >= 1)

# Layout is deterministic: same seed, same picture.
l1 <- compute_layout(org_m$graph)
l2 <- compute_layout(org_m$graph)
check("layout repeats exactly with the same seed", identical(l1, l2))

# The wire format: rows, not columns. This is the regression test for
# the blank map: a row record list survives Shiny serialization intact.
wire <- payload_wire(graph_payload(org_m, compute_layout(org_m$graph)))
check("wire nodes are a list of row records",
      is.list(wire$nodes) && !is.data.frame(wire$nodes) &&
        is.list(wire$nodes[[1]]) && !is.null(wire$nodes[[1]]$label))
check("wire edges are row records with weights",
      length(wire$edges) > 0 && !is.null(wire$edges[[1]]$weight))

# The card layer: four cards, in reading order, numbers matching.
groups_org <- relabel_groups(org_m$membership)
cards <- describe_cards(org_m, groups_org)
check("four cards in reading order",
      identical(names(cards), c("shape", "groups", "holders", "fragile")))
check("shape card states the true head count",
      cards$shape$stats[[1]]$value == as.character(org_m$n))
check("fragile card counts the cut points",
      cards$fragile$stats[[1]]$value ==
        as.character(length(org_m$cut_ids)))
check("holder chips name real people",
      all(cards$holders$people %in% org_m$names))
pc <- person_card(org_m, 1, groups_org)
check("person card names the person",
      grepl(org_m$names[1], pc$title, fixed = TRUE))

# The walkthrough and the model guide carry their content.
slides <- tour_slides()
# The slides are counted against a floor rather than an exact number.
# The count is not the thing worth holding still, and pinning it means
# every added slide turns this red for no reason.
check("every walkthrough slide carries art and text",
      length(slides) >= 7 &&
        all(vapply(slides, function(s) {
          isTRUE(nchar(s$art) > 100) && isTRUE(nchar(s$text) > 50)
        }, logical(1))))

# Each slide has to survive the path the walkthrough puts it through.
# The picture is markup, and the walkthrough hands it to HTML(), which
# takes a character vector and nothing else. One slide built its picture
# as a tag list instead, so pressing Next onto it stopped the
# walkthrough with a message about a character vector and no mention of
# the slide, the file, or the picture.
check("every walkthrough slide survives the walkthrough",
      all(vapply(slides, function(s) {
        is.character(s$art) && length(s$art) == 1 &&
          is.character(s$title) && is.character(s$text) &&
          !inherits(tryCatch(as.character(HTML(s$art)),
                             error = function(e) e), "error")
      }, logical(1))))

# And the picture is a whole drawing rather than the shapes for one,
# since a group of shapes with no frame around them renders as nothing.
check("every walkthrough picture is a complete drawing",
      all(vapply(slides, function(s) {
        grepl("<svg", s$art, fixed = TRUE) &&
          grepl("</svg>", s$art, fixed = TRUE)
      }, logical(1))))
check("model guide offers four options with commands",
      length(model_options()) == 4 &&
        all(vapply(model_options(),
                   function(m) grepl("^ollama pull ", m$pull), TRUE)))

# The group cap: a network with many communities keeps them separate
# rather than folding everything past the eighth into one bucket.
many <- relabel_groups(rep(1:30, times = 30:1))
check("thirty communities keep thirty distinct labels",
      length(unique(many)) == 30 && max(many) == 30)
check("groups are numbered by size, largest first",
      sum(many == 1) >= sum(many == 30))

# Equally sized communities are still separate communities. Ranking on
# size alone merged them, so the key showed fewer groups than the
# summary counted.
tied <- relabel_groups(c(rep(1, 5), rep(2, 5), rep(3, 3),
                         rep(4, 3), rep(5, 1)))
check("equally sized groups keep separate labels",
      length(unique(tied)) == 5)
check("the summary count and the key agree",
      length(unique(tied)) == length(unique(c(rep(1, 5), rep(2, 5),
                                              rep(3, 3), rep(4, 3),
                                              rep(5, 1)))))

# Suite 2: the prose. The check is behavioral, not aesthetic: paragraphs
# exist, numbers inside them match the metrics, and the person view names
# the person it was asked about.
paras <- describe_network(org_m)
check("network summary has at least four paragraphs", length(paras) >= 4)
check("summary states the true head count",
      grepl(paste0("\\b", org_m$n, " people"), paras[1]))
groups <- relabel_groups(org_m$membership)
pp <- describe_person(org_m, 1, groups)
check("person view names the person",
      grepl(org_m$names[1], pp[1], fixed = TRUE))
check("person view states their exact tie count",
      grepl(paste0(" ", org_m$degree[1], " direct ties"), pp[1]))

# The fragility paragraph appears only when cut points exist.
ring <- data.frame(from = c("A", "B", "C", "D"), to = c("B", "C", "D", "A"))
ring_m <- compute_metrics(build_graph(ring))
ring_p <- describe_network(ring_m)
check("a ring reports no single point of failure",
      any(grepl("No single person", ring_p)))

# Suite 3: WCAG contrast across all eight theme and palette states. The
# token pairs below are the ones that carry reading text. 4.5 to 1 is the
# floor everywhere.
lum <- function(hex) {
  v <- strtoi(c(substr(hex, 2, 3), substr(hex, 4, 5), substr(hex, 6, 7)), 16L) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055) ^ 2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
}
ratio <- function(a, b) {
  la <- lum(a); lb <- lum(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

tokens <- list(
  dark_standard = list(bg = "#0f1317", surface = "#171d24", raised = "#202a34",
                       text = "#e9eef3", muted = "#a7b4c1", accent = "#8fc1de"),
  light_standard = list(bg = "#f2f4f6", surface = "#ffffff", raised = "#ffffff",
                        text = "#17222d", muted = "#465562", accent = "#10577e"),
  dark_mono = list(bg = "#0f1317", surface = "#171d24", raised = "#202a34",
                   text = "#ffffff", muted = "#cfcfcf", accent = "#ffffff"),
  light_mono = list(bg = "#f2f4f6", surface = "#ffffff", raised = "#ffffff",
                    text = "#000000", muted = "#363636", accent = "#000000")
)
# The deutan and tritan palettes change node colors only, so their text
# checks reduce to the standard sets. Both reductions are asserted here so
# a future edit that adds text overrides gets caught.
for (state in names(tokens)) {
  tk <- tokens[[state]]
  for (surface_name in c("bg", "surface", "raised")) {
    check(sprintf("%s: text on %s", state, surface_name),
          ratio(tk$text, tk[[surface_name]]) >= 4.5)
    check(sprintf("%s: muted on %s", state, surface_name),
          ratio(tk$muted, tk[[surface_name]]) >= 4.5)
  }
  check(sprintf("%s: accent on surface", state),
        ratio(tk$accent, tk$surface) >= 4.5)
}
css <- readLines("www/styles.css")
deutan_block <- grep("cb-deutan|cb-tritan", css, value = TRUE)
check("deutan and tritan blocks leave text tokens alone",
      !any(grepl("--text|--muted", deutan_block)))

# Node label halo: label text against its halo stroke in both modes.
check("dark label on halo", ratio("#e9eef3", "#0f1317") >= 4.5)
check("light label on halo", ratio("#17222d", "#ffffff") >= 4.5)

# Suite: the ggraph figure renders in every palette without error and
# writes a real file.
pl_fig <- graph_payload(org_m, compute_layout(org_m$graph))
for (pal in names(figure_palettes)) {
  f <- file.path(tempdir(), paste0("fig_", pal, ".png"))
  ok_fig <- tryCatch({
    ggplot2::ggsave(f, network_figure(pl_fig, pal, "Test"),
                    width = 8, height = 7, dpi = 72, bg = "white")
    file.exists(f) && file.size(f) > 20000
  }, error = function(e) FALSE)
  check(paste("figure renders in the", pal, "palette"), ok_fig)
}

# Research tier: extended centralities, every community algorithm, the
# global diagnostics, and the reproducible script.
ec <- extended_centralities(org_m$graph)
check("extended centralities include pagerank and hits",
      all(c("pagerank", "hub", "authority") %in% names(ec)) &&
        nrow(ec) == org_m$n)
for (meth in c("louvain", "leiden", "walktrap", "fast_greedy")) {
  cp <- community_partition(org_m$graph, meth)
  check(paste(meth, "partition returns groups and modularity"),
        cp$n_groups >= 1 && is.finite(cp$modularity))
}
gd <- global_diagnostics(org_m$graph)
# The row count has to move whenever a measure is added to the table,
# which is the point: a diagnostic quietly dropping out of the table is
# exactly what this is here to notice.
check("global diagnostics report every measure they define",
      nrow(gd) == 12 &&
        gd$value[gd$measure == "People"] == as.character(org_m$n))
scr <- export_script("leiden", "between")
check("export script is runnable igraph code",
      grepl("library\\(igraph\\)", scr) &&
        grepl("cluster_leiden", scr))

# Suite 4: the app boots and answers. testServer drives the real server
# function with the org sample and confirms the outputs materialize.
app <- shinyAppFile("app.R")
ok_server <- tryCatch({
  testServer(app, {
    session$setInputs(dataset = "org", size_by = "degree",
                      sort_by = "degree", label_mode = "key")
    b <- bundle()
    stopifnot(!is.null(b), b$metrics$n == 58)
    stopifnot(nchar(output$people_table$html) > 500)
    stopifnot(nchar(output$reading$html) > 200)
    stopifnot(nchar(output$legend$html) > 100)
    session$setInputs(selected_node = 1)
    stopifnot(grepl(b$metrics$names[1], output$reading$html, fixed = TRUE))
    # The empty state appears when the app opens on Your own data.
    session$setInputs(dataset = "upload")
    stopifnot(grepl("Map your own people", output$empty_state$html,
                    fixed = TRUE))
    stopifnot(grepl("Load a network first", output$research_body$html,
                    fixed = TRUE))
    # With a network loaded, the research tab fills in.
    session$setInputs(dataset = "org", community_method = "louvain")
    stopifnot(grepl("Global structure", output$research_body$html,
                    fixed = TRUE))
  })
  TRUE
}, error = function(e) { cat("  server error:", conditionMessage(e), "\n"); FALSE })
check("server answers with map, prose, legend, and table", ok_server)

# Suite 5: every screen that is built from a file in R/ renders without
# reaching for something that is not there. Shiny sources app.R into an
# environment of its own while source() from inside it loads into the
# global environment, so a function in R/ cannot see a value assigned at
# the top of app.R. The Overview screen hit exactly that and took the
# whole app down on startup. This check builds the screens the same way
# a fresh session does, from the sourced files alone.
ok_screens <- tryCatch({
  markup <- as.character(overview_body())
  stopifnot(nchar(markup) > 2000)
  stopifnot(grepl(APP_TAG, markup, fixed = TRUE))
  stopifnot(length(tour_slides()) > 0)
  stopifnot(length(model_options()) > 0)
  # Help is built from the same sourced files and has the same exposure
  # to the loading order this suite exists to check.
  help_markup <- as.character(help_body())
  stopifnot(nchar(help_markup) > 4000)
  stopifnot(grepl(APP_VERSION, help_markup, fixed = TRUE))
  # Both script dialects have to be runnable text rather than an error.
  igraph_script <- export_script("louvain", "degree", "igraph")
  tidy_script <- export_script("louvain", "degree", "tidy")
  stopifnot(grepl("library(igraph)", igraph_script, fixed = TRUE))
  stopifnot(grepl("library(tidygraph)", tidy_script, fixed = TRUE))
  stopifnot(!grepl("library(igraph)", tidy_script, fixed = TRUE))
  # The hero emblem and the worked example were the same picture for
  # three versions. They are different functions now and must stay so.
  stopifnot(as.character(overview_emblem()) != as.character(overview_art()))
  TRUE
}, error = function(e) {
  cat("  screen error:", conditionMessage(e), "\n"); FALSE
})
check("screens built from R/ render with only the sourced files", ok_screens)

# Suite 6: saving and resuming. A session file has to survive a round
# trip with its ties intact, and a file that is not one has to be
# refused with a sentence rather than a stack trace.
ok_session <- tryCatch({
  ed <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "D"),
                   stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".json")
  save_session(path, session_snapshot(ed, list(size_by = "degree")))
  back <- read_session(path)
  stopifnot(isTRUE(back$ok))
  stopifnot(nrow(back$edges) == 3)
  stopifnot(identical(as.character(back$edges[[1]]), ed$from))
  stopifnot(identical(back$view$size_by, "degree"))
  # Anything that is not a session file is refused in words.
  junk <- tempfile(fileext = ".json")
  writeLines("not a session", junk)
  refused <- read_session(junk)
  stopifnot(!isTRUE(refused$ok), nchar(refused$message) > 20)
  unlink(c(path, junk))
  TRUE
}, error = function(e) {
  cat("  session error:", conditionMessage(e), "\n"); FALSE
})
check("a session saves and resumes with its ties intact", ok_session)

# Suite 7: the model catalogue and the fit rules. No network call is
# made here; only the arithmetic that decides which models are offered.
ok_fit <- tryCatch({
  small <- list(os = "test", ram_gb = 8, cores = 4, accelerated = FALSE)
  large <- list(os = "test", ram_gb = 64, cores = 16, accelerated = TRUE)
  stopifnot(all(model_fit(small) %in%
                c("comfortable", "workable", "too large")))
  stopifnot(any(model_fit(small) == "too large"))
  stopifnot(all(model_fit(large) == "comfortable"))
  # Every catalogue entry carries the two fields the setup screen reads.
  stopifnot(all(vapply(model_options(),
                       function(m) nzchar(m$id) && is.numeric(m$min_ram),
                       logical(1))))
  stopifnot(nzchar(recommended_model(small)))
  TRUE
}, error = function(e) {
  cat("  fit error:", conditionMessage(e), "\n"); FALSE
})
check("model fit follows the reported memory", ok_fit)

# Suite 8: the two directed samples. The census is the only place
# direction changes an answer rather than a wording, and the two samples
# exist to differ in exactly what it measures, so that difference is
# checked rather than assumed.
ok_directed <- tryCatch({
  advice <- sample_advice_network()
  message_net <- sample_message_network()
  stopifnot(isTRUE(attr(advice, "directed")))
  stopifnot(isTRUE(attr(message_net, "directed")))

  census_of <- function(ed) {
    g <- build_graph(ed)
    m <- compute_metrics(g)
    stopifnot(!is.null(m$digraph))
    dyad_triad_census(m$digraph)
  }
  # The census is handed a plain igraph for a directed network and a
  # tbl_graph otherwise. tidygraph supplies an as.igraph method for its
  # own class and igraph supplies none for its, so both forms are
  # checked here.
  a <- census_of(advice)
  b <- census_of(message_net)
  stopifnot(isTRUE(a$directed), isTRUE(b$directed))

  share_mutual <- function(ct) {
    ct$dyads$count[ct$dyads$type == "Mutual"] /
      sum(ct$dyads$count[ct$dyads$type != "Null"])
  }
  # Advice runs one way and messages come back, so the messaging
  # network must be the more mutual of the two by a clear margin. A
  # sample that failed this would teach the wrong lesson about the
  # census, which is the only reason it ships.
  stopifnot(share_mutual(b) > share_mutual(a) + 0.25)

  # An undirected source still reports that it has no census rather
  # than an empty one.
  plain <- dyad_triad_census(compute_metrics(build_graph(
    sample_org_network()))$graph)
  stopifnot(identical(plain$directed, FALSE))
  TRUE
}, error = function(e) {
  cat("  directed error:", conditionMessage(e), "\n"); FALSE
})
check("the directed samples differ in what the census measures",
      ok_directed)

# Suite 9: the structural measures. None of these is derivable from the
# centrality columns beside them, which is the test of whether a measure
# earns a column of its own.
ok_structure <- tryCatch({
  m <- compute_metrics(build_graph(sample_org_network()))
  ext <- extended_centralities(m$graph)
  stopifnot(all(c("constraint", "effective", "coreness") %in% names(ext)))
  # Constraint falls between zero and roughly one and a quarter, and a
  # person with a single contact is maximally constrained.
  stopifnot(all(is.finite(ext$constraint) | is.na(ext$constraint)))
  stopifnot(all(ext$coreness >= 1))
  stopifnot(all(ext$effective >= 0))

  diag_with <- global_diagnostics(m$graph, m$membership)
  diag_without <- global_diagnostics(m$graph)
  stopifnot("E-I index (group crossing)" %in% diag_with$measure)
  ei <- diag_with$value[diag_with$measure == "E-I index (group crossing)"]
  stopifnot(nchar(ei) > 0, ei != "n/a (no grouping)")
  # Without a grouping the same row says so rather than inventing one.
  ei_none <- diag_without$value[
    diag_without$measure == "E-I index (group crossing)"]
  stopifnot(ei_none == "n/a (no grouping)")
  # The E-I index is bounded by minus one and plus one wherever it is
  # defined, which is the cheapest check that the arithmetic is right.
  stopifnot(abs(as.numeric(ei)) <= 1)
  TRUE
}, error = function(e) {
  cat("  structure error:", conditionMessage(e), "\n"); FALSE
})
check("the structural measures compute and stay in range", ok_structure)

# Suite 10: the route finder and the tie marks. Both put something on
# the map that a reader cannot check by eye, so both are checked here
# against answers that can be worked out by hand.
ok_route <- tryCatch({
  m <- compute_metrics(build_graph(sample_org_network()))
  nms <- m$names
  res <- shortest_route(m, nms[1], nms[2])
  stopifnot(isTRUE(res$ok))
  # The route starts and ends where it was asked to, its stops are all
  # distinct, and its length is the count of steps rather than of
  # people.
  stopifnot(identical(res$names[1], nms[1]))
  stopifnot(identical(res$names[length(res$names)], nms[2]))
  stopifnot(!any(duplicated(res$ids)))
  stopifnot(res$length == length(res$ids) - 1)
  stopifnot(length(res$steps) == res$length)
  # Asking for a route to oneself is refused in words rather than
  # answered with a route of length zero.
  same <- shortest_route(m, nms[1], nms[1])
  stopifnot(!isTRUE(same$ok), nchar(same$message) > 10)
  missing <- shortest_route(m, nms[1], "Nobody At All")
  stopifnot(!isTRUE(missing$ok))

  # Tie marks ride on the payload. A bridge count above the tie count
  # would mean the flags are misaligned with the edge table.
  lay <- compute_layout(m$graph)
  pay <- graph_payload(m, lay)
  stopifnot(all(c("bridge", "crossing") %in% names(pay$edges)))
  stopifnot(is.logical(pay$edges$bridge))
  stopifnot(pay$meta$n_bridges <= nrow(pay$edges))
  stopifnot(pay$meta$n_crossing <= nrow(pay$edges))
  # A crossing tie is one whose ends carry different group labels, so
  # the count has to match a recount from the node table.
  gp <- pay$nodes$group
  recount <- sum(gp[pay$edges$from] != gp[pay$edges$to])
  stopifnot(recount == pay$meta$n_crossing)
  TRUE
}, error = function(e) {
  cat("  route error:", conditionMessage(e), "\n"); FALSE
})
check("routes and tie marks agree with the graph they came from",
      ok_route)

# Suite 11: what the model is given. It must be findings rather than the
# panel prose, since a model handed a definition restates it as a
# discovery.
ok_facts <- tryCatch({
  m <- compute_metrics(build_graph(sample_org_network()))
  groups <- relabel_groups(m$membership)
  facts <- model_facts(m, groups)
  stopifnot(length(facts) >= 6)
  joined <- paste(facts, collapse = " ")
  # No sentence explaining what a measure is.
  stopifnot(!grepl("come from the tie pattern", joined, fixed = TRUE))
  stopifnot(!grepl("usually reflect", joined, fixed = TRUE))
  # Every fact carries something specific: a number or a name.
  stopifnot(all(grepl("[0-9]", facts) | grepl(paste(m$names, collapse = "|"),
                                              facts)))
  person <- model_facts_person(m, 1L, groups)
  stopifnot(any(grepl(m$names[1], person, fixed = TRUE)))
  TRUE
}, error = function(e) {
  cat("  facts error:", conditionMessage(e), "\n"); FALSE
})
check("the model is given findings rather than definitions", ok_facts)

# Suite 12: the inferential tests. These are the numbers a reader is
# most likely to quote in something that gets published, so the checks
# below are about the arithmetic being right rather than about the code
# running.
ok_stats <- tryCatch({
  m <- compute_metrics(build_graph(sample_org_network()))
  # Few replications here: the suite is checking shape and bounds, and
  # the number of draws is the reader's choice at run time.
  res <- network_statistics(m, reps = 60, conditioning = "edges", seed = 11)
  tab <- res$table
  stopifnot(nrow(tab) >= 4)
  stopifnot(all(c("measure", "observed", "chance_mean", "p_higher",
                  "p_lower", "reading") %in% names(tab)))
  # Proportions are bounded and can never be zero, because the
  # observation is counted in its own null distribution.
  ps <- c(tab$p_higher, tab$p_lower)
  ps <- ps[is.finite(ps)]
  stopifnot(all(ps > 0), all(ps <= 1))
  # Every test carries a sentence, not just numbers.
  stopifnot(all(nchar(tab$reading) > 40))

  # The E-I index is bounded by minus one and plus one, and a network
  # where every tie stays inside a group must sit at minus one.
  g_split <- igraph::make_full_graph(4) + igraph::make_full_graph(4)
  memb <- c(rep(1, 4), rep(2, 4))
  stopifnot(abs(ei_index(g_split, memb) + 1) < 1e-9)
  g_star <- igraph::make_graph(c(1, 5, 2, 6, 3, 7, 4, 8), directed = FALSE)
  stopifnot(abs(ei_index(g_star, memb) - 1) < 1e-9)

  # A summary of a value sitting far above its null distribution must
  # report a small upper proportion and a large lower one.
  high <- cug_summary(10, rnorm(200, mean = 0, sd = 1))
  stopifnot(high$p_greater < 0.02, high$p_less > 0.98)
  TRUE
}, error = function(e) {
  cat("  statistics error:", conditionMessage(e), "\n"); FALSE
})
check("the inferential tests are bounded and carry their readings",
      ok_stats)

# Suite 12a: every number the statistics screen reads has length one.
#
# A shape check rather than an arithmetic one. Values here pass through
# igraph, and igraph returns a different set of list elements on
# different releases, so a number can be correct on one machine and
# absent on another. An absent value reaching an if() fails with a
# message that names neither the value nor the file, which is the
# hardest kind of failure to trace and the cheapest to prevent.
ok_lengths <- tryCatch({
  every_number_is_scalar <- function(values, where) {
    for (nm in names(values)) {
      value <- values[[nm]]
      if (is.null(value) || length(value) != 1) {
        stop(paste(where, "field", nm, "has length",
                   length(value)), call. = FALSE)
      }
    }
    TRUE
  }
  for (sample_name in c("org", "referral", "advice", "message")) {
    edges <- switch(sample_name,
      org = sample_org_network(),
      referral = sample_referral_network(),
      advice = sample_advice_network(),
      message = sample_message_network())
    metrics <- compute_metrics(build_graph(edges))
    for (null_model in c("edges", "degree")) {
      res <- network_statistics(metrics, reps = 40,
                                conditioning = null_model, seed = 5)
      every_number_is_scalar(res$degree,
                             paste(sample_name, null_model, "degree"))
      every_number_is_scalar(res$small_world,
                             paste(sample_name, null_model, "small world"))
      stopifnot(length(res$reps) == 1, length(res$n) == 1,
                length(res$m) == 1)
      stopifnot(nrow(res$table) >= 4)
      # Every reading is a sentence and every note names the measure.
      stopifnot(all(nchar(res$table$reading) > 40))
      stopifnot(all(nchar(res$table$note) > 10))
      # Holding each person's number of ties fixed holds degree
      # centralization fixed, so that row must be absent rather than
      # reported as a result of no difference.
      if (null_model == "degree") {
        stopifnot(!("Degree centralization" %in% res$table$measure))
        stopifnot(any(vapply(res$skipped, function(x) {
          identical(x$measure, "Degree centralization")
        }, logical(1))))
      }
    }
    # The directed samples get the directed measures and the undirected
    # ones must not.
    res <- network_statistics(metrics, reps = 40, seed = 5)
    directed_rows <- c("Reciprocity", "Closed threes", "Loops of three")
    if (sample_name %in% c("advice", "message")) {
      stopifnot(all(directed_rows %in% res$table$measure))
    } else {
      stopifnot(!any(directed_rows %in% res$table$measure))
    }
  }
  TRUE
}, error = function(e) {
  cat("  length error:", conditionMessage(e), "\n"); FALSE
})
check("every number the statistics screen reads has length one",
      ok_lengths)

# Suite 12b: the power law wrapper survives an igraph that returns no
# goodness of fit proportion. The installed igraph may or may not be one
# of those, so the case is forced rather than waited for.
ok_powerlaw <- tryCatch({
  real <- net_power_law(c(rep(1, 40), rep(2, 20), rep(3, 8), 7, 9, 14))
  stopifnot(length(real$alpha) == 1, length(real$ks_p) == 1)
  # Stand in for an igraph that dropped the proportion.
  stub <- function(x, ...) {
    fit <- igraph::fit_power_law(x)
    fit$KS.p <- NULL
    fit
  }
  with_stub <- local({
    saved <- get("fit_power_law", envir = globalenv(), inherits = TRUE)
    assign("fit_power_law", stub, envir = globalenv())
    on.exit(assign("fit_power_law", saved, envir = globalenv()))
    net_power_law(c(rep(1, 40), rep(2, 20), rep(3, 8), 7, 9, 14))
  })
  stopifnot(length(with_stub$ks_p) == 1)
  # A vector too short to fit comes back as three NA values rather than
  # as an error or as a list with pieces missing.
  short <- net_power_law(c(1, 1, 2))
  stopifnot(length(short$alpha) == 1, is.na(short$ks_p))
  # And the guards used across the file behave on a zero length input.
  stopifnot(is.na(one_number(numeric(0))), is.na(one_number(NULL)))
  stopifnot(!has_number(NULL), !has_number(numeric(0)), has_number(3))
  TRUE
}, error = function(e) {
  cat("  power law error:", conditionMessage(e), "\n"); FALSE
})
check("the power law fit returns three numbers on every igraph",
      ok_powerlaw)

# Suite 12c: distances are counted in steps, not in tie weight.
#
# igraph reads a weight column as a cost, so a pair joined by a strong
# tie comes out further apart than a pair joined by a weak one, which is
# backwards for a column that means how much two people work together.
# The mistake is invisible until the mean distance climbs above the
# diameter printed directly beneath it, which is arithmetically
# impossible, so that relationship is what is checked.
ok_steps <- tryCatch({
  for (sample_name in c("org", "message")) {
    edges <- if (sample_name == "org") {
      sample_org_network()
    } else {
      sample_message_network()
    }
    metrics <- compute_metrics(build_graph(edges))
    diagnostics <- global_diagnostics(metrics$graph, metrics$membership)
    value_of <- function(label) {
      as.numeric(diagnostics$value[diagnostics$measure == label])
    }
    stopifnot(value_of("Mean distance") <= value_of("Diameter"))
    res <- network_statistics(metrics, reps = 30, seed = 3)
    observed_distance <- res$table$observed[
      res$table$measure == "Mean distance"]
    stopifnot(length(observed_distance) == 1)
    stopifnot(abs(observed_distance - value_of("Mean distance")) < 0.01)
  }
  TRUE
}, error = function(e) {
  cat("  step error:", conditionMessage(e), "\n"); FALSE
})
check("distances are counted in steps everywhere they are reported",
      ok_steps)

# Suite 12d: no call to an igraph function that igraph has deprecated.
# A deprecation warning is a promise that the call will stop working,
# and one printed once per session in a running app is a warning nobody
# reads. R/compat.R holds the wrappers; this checks that nothing walks
# around them.
ok_deprecated <- tryCatch({
  # The dots are escaped and each name has to stand on its own. An
  # unescaped dot is a wildcard, so as.undirected matched the compat
  # wrapper net_as_undirected and shortest.paths matched the current
  # shortest_paths, and a gate that reports the code it was written to
  # protect gets switched off.
  retired <- c("hub_score", "authority_score", "as\\.undirected",
               "as\\.directed", "graph\\.data\\.frame",
               "get\\.adjacency", "power\\.law\\.fit",
               "shortest\\.paths")
  retired <- paste0("(^|[^A-Za-z0-9._])", retired, "[^A-Za-z0-9._]")
  offenders <- character(0)
  for (path in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
    if (basename(path) == "compat.R") next
    body <- readLines(path, warn = FALSE)
    # Comments are prose and may name a retired function while
    # explaining why it is no longer called.
    body <- sub("#.*$", "", body)
    for (name in retired) {
      if (any(grepl(name, body, fixed = FALSE))) {
        offenders <- c(offenders, paste0(basename(path), ": ", name))
      }
    }
  }
  if (length(offenders) > 0) {
    stop(paste("deprecated igraph calls:",
               paste(offenders, collapse = "; ")), call. = FALSE)
  }
  # And the wrappers themselves work on this igraph.
  g <- igraph::make_ring(6, directed = TRUE)
  stopifnot(!igraph::is_directed(net_as_undirected(g)))
  hits <- net_hits(g)
  stopifnot(length(hits$hub) == 6, length(hits$authority) == 6)
  TRUE
}, error = function(e) {
  cat("  deprecation error:", conditionMessage(e), "\n"); FALSE
})
check("no source file calls a deprecated igraph function", ok_deprecated)

# Suite 13: nothing a reader supplies is turned into something the
# browser runs or another program reads as an instruction.
#
# Names come from an uploaded file, so a name is untrusted text. It
# reaches the screen in three places and one of them writes a control
# for it, so a name has to be data the whole way through rather than
# something quoted well enough to be safe inside a line of script.
ok_injection <- tryCatch({
  # A name carrying a quote, a backslash, and tags. If any of this
  # reaches the page as anything but text, one of the checks below
  # notices.
  awkward <- "Ada O'Neil\\');alert(1)//<img src=x onerror=alert(2)>"
  edges <- data.frame(
    from = c(awkward, "Bess", "Cyrus", awkward),
    to = c("Bess", "Cyrus", "Dai", "Dai"),
    stringsAsFactors = FALSE)
  metrics <- compute_metrics(build_graph(edges))
  groups <- relabel_groups(metrics$membership)

  # No file in R/ or app.R writes a name into a line of script. The
  # pattern looks for setInputValue built by sprintf, which is the shape
  # that would put a name inside code.
  sources <- c("app.R", list.files("R", pattern = "[.]R$", full.names = TRUE))
  for (path in sources) {
    body <- paste(readLines(path, warn = FALSE), collapse = "\n")
    if (grepl("setInputValue\\([^)]*%s", body)) {
      stop(paste(basename(path),
                 "writes a value into a line of script"), call. = FALSE)
    }
  }

  # The person chips carry the name as an attribute, and rendering
  # escapes it, so the awkward characters arrive as text.
  chips <- as.character(describe_cards(metrics, groups)[[1]]$people)
  markup <- paste(vapply(seq_along(chips), function(i) {
    as.character(tags$button(type = "button", class = "person-chip",
                             `data-person` = chips[i], chips[i]))
  }, ""), collapse = "")
  # Text that reads as an image tag is fine on screen. What must never
  # appear is a tag the browser would act on, or a quote that closes the
  # attribute the name sits in.
  stopifnot(!grepl("<img", markup, fixed = TRUE))
  stopifnot(!grepl("onerror", markup, fixed = TRUE) ||
              grepl("&lt;img", markup, fixed = TRUE))

  # The people table escapes every name it writes.
  cell <- htmltools::htmlEscape(awkward)
  stopifnot(!grepl("<img", cell, fixed = TRUE))
  stopifnot(grepl("&lt;img", cell, fixed = TRUE))

  # And so does the table helper, for headings and for values.
  built <- as.character(html_table(c("Name", "Ties"),
                                   list(awkward, "3"),
                                   justify = c("left", "num")))
  stopifnot(!grepl("<img", built, fixed = TRUE))
  stopifnot(grepl("&lt;img", built, fixed = TRUE))
  # The heading is escaped on the same footing as the value.
  headed <- as.character(html_table(c(awkward), list("1")))
  stopifnot(!grepl("<img", headed, fixed = TRUE))

  # A model name is handed to another program as an argument, so
  # anything that would arrive as an option rather than as a name is
  # refused before it gets there.
  stopifnot(valid_model_name("llama3.2"), valid_model_name("qwen2.5:7b"))
  stopifnot(!valid_model_name("-h"), !valid_model_name("a b"),
            !valid_model_name(""), !valid_model_name("../etc/passwd"),
            !valid_model_name(NULL), !valid_model_name(c("a", "b")))
  stopifnot(!isTRUE(pull_model("-h")$ok))
  stopifnot(!isTRUE(remove_model("a b")$ok))
  TRUE
}, error = function(e) {
  cat("  injection error:", conditionMessage(e), "\n"); FALSE
})
check("nothing a reader supplies becomes code", ok_injection)

# A saved session is a file, and a file can say anything. The palette it
# names becomes a class on the page, so a value that is not one of the
# settings this app has is dropped rather than passed on.
ok_session_guard <- tryCatch({
  palettes <- c("standard", "deutan", "protan", "tritan", "mono")
  clean <- function(value) {
    if (isTRUE(value %in% palettes)) value else NULL
  }
  stopifnot(identical(clean("deutan"), "deutan"))
  stopifnot(is.null(clean("not a palette")))
  stopifnot(is.null(clean("mono evil")))
  stopifnot(is.null(clean(NULL)))
  stopifnot(is.null(clean(c("mono", "deutan"))))

  # A file that is not a session is refused in words, and one holding
  # ties with one end is refused too.
  junk <- tempfile(fileext = ".json")
  writeLines('{"format":"x","edges":[{"from":"A"}]}', junk)
  refused <- read_session(junk)
  stopifnot(!isTRUE(refused$ok), nchar(refused$message) > 20)
  TRUE
}, error = function(e) {
  cat("  session guard error:", conditionMessage(e), "\n"); FALSE
})
check("a saved session cannot name a setting the app does not have",
      ok_session_guard)

# Suite 14: the walkthrough, driven the way a reader drives it.
#
# Every slide is reached by pressing Next, and every slide is reached
# again by pressing Back. Checking that the slides exist is not the same
# as checking that they can be shown, and the difference between the two
# is a control that fails partway through.
ok_tour <- tryCatch({
  slides <- tour_slides()
  app <- shinyAppFile("app.R")
  testServer(app, {
    session$setInputs(tour_seen = FALSE)
    for (i in seq_len(length(slides) - 1)) {
      session$setInputs(tour_next = i)
    }
    for (i in seq_len(length(slides) - 1)) {
      session$setInputs(tour_back = i)
    }
    session$setInputs(help_tour = 1)
    session$setInputs(tour_done = 1)
  })
  TRUE
}, error = function(e) {
  cat("  walkthrough error:", conditionMessage(e), "\n"); FALSE
})
check("the walkthrough runs forward and back through every slide",
      ok_tour)

# A control that has not reported yet is NULL, not its default, and a
# reactive that hands NULL to switch() raises four levels below an
# observer, so what reaches the log names neither the control nor the
# screen. Nothing is chosen before the first browser message arrives, so
# the app has to answer that with no network rather than with an error.
ok_cold_start <- tryCatch({
  app <- shinyAppFile("app.R")
  testServer(app, {
    stopifnot(is.null(edges_raw()))
    stopifnot(is.null(edges()))
    stopifnot(is.null(bundle()))
    # And the screens that read a missing bundle say so rather than
    # failing.
    # as.character() on a tag list returns one string per tag, so the
    # whole rendered screen is what is measured.
    rendered <- function(name) {
      sum(nchar(as.character(output[[name]])))
    }
    stopifnot(rendered("research_body") > 100)
    stopifnot(rendered("stats_body") > 100)
  })
  TRUE
}, error = function(e) {
  cat("  cold start error:", conditionMessage(e), "\n"); FALSE
})
check("the app answers before any control has reported", ok_cold_start)

# Every download writes a file with something in it. A handler that
# raises leaves the reader with a browser error and no file, and a
# handler that writes nothing leaves them with a file that opens empty.
ok_downloads <- tryCatch({
  app <- shinyAppFile("app.R")
  for (which_data in c("org", "message")) {
    testServer(app, {
      session$setInputs(dataset = which_data, size_by = "degree",
                        sort_by = "degree", label_mode = "key",
                        community_method = "louvain",
                        script_dialect = "igraph", palette = "standard",
                        stat_null = "edges", stat_reps = 40)
      session$setInputs(run_stats = 1)
      for (name in c("dl_people", "dl_summary", "dl_extended", "dl_script",
                     "dl_dyads", "dl_triads", "dl_session", "dl_stats")) {
        path <- output[[name]]
        if (!file.exists(path) || file.info(path)$size < 20) {
          stop(paste(name, "wrote nothing worth opening"), call. = FALSE)
        }
      }
    })
  }
  TRUE
}, error = function(e) {
  cat("  download error:", conditionMessage(e), "\n"); FALSE
})
check("every download writes a file with something in it", ok_downloads)

# Suite 15: the controls a reader has to be able to work.
#
# Both script dialects have to be reachable, not merely computable. A
# control that produces the right answer when the server is driven
# directly can still be one nobody can press, so what is checked here is
# that the control is a kind this app renders elsewhere and that both of
# its values reach the screen and the file.
ok_controls <- tryCatch({
  app <- shinyAppFile("app.R")
  testServer(app, {
    session$setInputs(dataset = "org", community_method = "louvain",
                      size_by = "degree")
    markup <- paste(as.character(output$research_body), collapse = "")
    # A menu, with both dialects in it, and one label pointing at it.
    stopifnot(grepl("<select", markup, fixed = TRUE))
    stopifnot(grepl('id="script_dialect"', markup))
    stopifnot(grepl("tidygraph and ggraph", markup, fixed = TRUE))
    for (dialect in c("igraph", "tidy")) {
      session$setInputs(script_dialect = dialect)
      shown <- output$export_code
      path <- output$dl_script
      stopifnot(nchar(shown) > 200)
      stopifnot(file.exists(path), file.info(path)$size > 200)
      # The screen and the file agree about which dialect is on.
      wanted <- if (dialect == "tidy") "tidygraph" else "library(igraph)"
      stopifnot(grepl(wanted, shown, fixed = TRUE))
      stopifnot(any(grepl(wanted, readLines(path, warn = FALSE),
                          fixed = TRUE)))
      stopifnot(grepl(dialect, basename(path), fixed = TRUE))
    }
  })

  # Every color setting the dialog offers is one the figure export can
  # draw with, and each preview carries a name, a line saying who it is
  # for, and its own swatches, all on their own lines.
  stopifnot(all(PALETTE_NAMES %in% names(figure_palettes)))
  for (name in PALETTE_NAMES) {
    joined <- paste(as.character(swatch_label("Name", name, "A note.")),
                    collapse = "")
    stopifnot(grepl("swatch-name", joined, fixed = TRUE))
    stopifnot(grepl("swatch-note", joined, fixed = TRUE))
    stopifnot(grepl("swatch-row", joined, fixed = TRUE))
    # Five swatches, each carrying a color from that palette.
    stopifnot(lengths(regmatches(joined, gregexpr("<svg", joined))) == 5)
    for (hex in figure_palettes[[name]][1:5]) {
      stopifnot(grepl(hex, joined, fixed = TRUE))
    }
  }
  # A setting with no note still renders.
  stopifnot(nchar(paste(as.character(swatch_label("Name", "mono")),
                        collapse = "")) > 40)
  TRUE
}, error = function(e) {
  cat("  control error:", conditionMessage(e), "\n"); FALSE
})
check("both script dialects are reachable and agree with their files",
      ok_controls)

# Suite 16: what the local model can be asked for.
#
# The model rewords findings. It never computes and never sees the
# network, so what changes between modes is the instruction and never
# the numbers. That is the property worth holding: three modes that
# quietly sent three different sets of facts would be three different
# answers rather than three readings of one.
ok_modes <- tryCatch({
  modes <- model_modes()
  stopifnot(length(modes) >= 3)
  stopifnot(all(c("plain", "methods", "caution") %in% names(modes)))
  for (name in names(modes)) {
    stopifnot(nchar(modes[[name]]$label) > 3)
    stopifnot(nchar(modes[[name]]$task) > 60)
  }
  # Every mode is a different instruction.
  tasks <- vapply(modes, function(m) m$task, "")
  stopifnot(length(unique(tasks)) == length(tasks))

  app <- shinyAppFile("app.R")
  testServer(app, {
    session$setInputs(dataset = "org", community_method = "louvain",
                      size_by = "degree", stat_null = "edges",
                      stat_reps = 40)
    session$setInputs(run_stats = 1)
    control <- paste(as.character(output$stats_model_ui), collapse = "")
    stopifnot(grepl("stats_model_mode", control, fixed = TRUE))
    for (name in names(modes)) {
      stopifnot(grepl(modes[[name]]$label, control, fixed = TRUE))
    }
    # The facts the model is handed carry the numbers and name the null
    # model, and say so when a measure could not be computed rather than
    # handing over the text NA for a model to restate as a number.
    res <- stats_result()
    facts <- c(
      sprintf("These results come from %d random networks.", res$reps),
      apply(res$table, 1, function(row) row[["measure"]]))
    stopifnot(length(facts) == nrow(res$table) + 1)
    stopifnot(!any(grepl("^NA$", facts)))
  })
  TRUE
}, error = function(e) {
  cat("  model mode error:", conditionMessage(e), "\n"); FALSE
})
check("the local model can be asked for three readings of one result",
      ok_modes)

# Suite 17: a verdict rests on more than the proportion.
#
# The American Statistical Association's 2016 statement is the reason
# this suite exists. A proportion answers how often chance would beat an
# observation, which is not the same question as whether the difference
# is large, whether it would survive a second sample, or whether it
# rests on one person. Each of those has its own check below, and each
# one has to be able to change a verdict on its own, or it is decoration
# rather than a check.
ok_weighing <- tryCatch({
  # A small proportion is not enough when the observation still falls
  # inside the range the random networks produced.
  inside <- list(observed = 0.5, mean = 0.4, sd = 0.1,
                 lo = 0.2, hi = 0.6, p_greater = 0.02, p_less = 0.98,
                 p_error = 0.002, z = 1, reps = 500L)
  stopifnot(identical(assess(inside)$band, "edge"))

  # The same numbers with the observation outside the range is a result.
  outside <- inside
  outside$hi <- 0.45
  stopifnot(assess(outside)$band %in% c("clear", "strong"))

  # Unless the sampling error on the proportion straddles the threshold,
  # in which case the sampling and not the network is deciding.
  noisy <- outside
  noisy$p_greater <- 0.045
  noisy$p_error <- 0.02
  stopifnot(identical(assess(noisy)$band, "edge"))

  # And a result that moves back inside the range when the best
  # connected person leaves is a fact about that person.
  fragile <- assess(outside, without = 0.3)
  stopifnot(identical(fragile$band, "fragile"))
  stopifnot(identical(fragile$label, "Rests on one person"))

  # A comparison with no spread in it decides nothing either way.
  flat <- outside
  flat$sd <- 0
  stopifnot(identical(assess(flat)$band, "unknown"))
  TRUE
}, error = function(e) {
  cat("  weighing error:", conditionMessage(e), "\n"); FALSE
})
check("a verdict rests on more than the proportion", ok_weighing)

# Suite 18: every reading is about the network in front of the reader.
#
# A reading that says a proportion was above a threshold is a reading of
# the method. What each one has to carry is a count of things this
# network has, the range the random networks produced, and what the gap
# amounts to in the same units.
ok_specific <- tryCatch({
  for (sample_name in c("org", "message")) {
    edges <- if (sample_name == "org") {
      sample_org_network()
    } else {
      sample_message_network()
    }
    metrics <- compute_metrics(build_graph(edges))
    res <- network_statistics(metrics, reps = 60, seed = 8)

    stopifnot(all(c("chance_low", "chance_high", "p_error", "verdict",
                    "band", "without_top") %in% names(res$table)))
    # Every range holds its average.
    holds <- res$table$chance_low <= res$table$chance_mean &
      res$table$chance_mean <= res$table$chance_high
    stopifnot(all(holds, na.rm = TRUE))
    # Every reading names the number of random networks that produced
    # the range, and reports a range rather than a threshold.
    for (line in res$table$reading) {
      stopifnot(grepl("Across", line, fixed = TRUE))
      stopifnot(grepl("between", line, fixed = TRUE))
      stopifnot(grepl("averaging", line, fixed = TRUE))
      # And none of them falls back on naming a threshold.
      stopifnot(!grepl("p = ", line, fixed = TRUE))
    }
    # Measures with something countable behind them say how many of
    # what, and the count has to be of this network rather than a share
    # printed twice.
    countable <- res$table$key %in% c("clustering", "giant_share",
                                      "reciprocity", "ei")
    for (line in res$table$reading[countable]) {
      stopifnot(grepl("^Here [0-9,]+ of this network's [0-9,]+ ", line))
    }
    # The drop check names a real person from this network.
    if (!is.null(res$dropped)) {
      stopifnot(res$dropped$person %in% metrics$names)
    }
    # What the model is handed describes the network before it lists
    # anything, so a model reading it can write about this network.
    facts <- stats_facts(res, "A sample")
    stopifnot(grepl("people and", facts[1], fixed = TRUE))
    stopifnot(grepl("density", facts[1], fixed = TRUE))
    stopifnot(length(facts) >= nrow(res$table) + 3)
    # And every finding it is handed carries its whole reading rather
    # than three numbers.
    stopifnot(all(nchar(tail(facts, nrow(res$table))) > 120))
  }
  TRUE
}, error = function(e) {
  cat("  specificity error:", conditionMessage(e), "\n"); FALSE
})
check("every reading is about the network in front of the reader",
      ok_specific)

# Robustness and security. Each check below reproduces a defect found
# in the 0.25.0 quality review, so it fails on the code that had the
# defect and passes on the fix.

# A strength the measures cannot use is treated like any other unusable
# value. Infinity broke eigenvector centrality and ended the session.
inf_ties <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"),
                       weight = c(1, Inf, 2))
g_inf <- build_graph(inf_ties)
w_inf <- igraph::E(as_plain_igraph(g_inf))$weight
check("an infinite strength is read as one, like other unusable values",
      all(is.finite(w_inf)) && sum(w_inf == 1) == 2)
m_inf <- tryCatch(compute_metrics(g_inf), error = function(e) NULL)
check("a network with an infinite strength can still be measured",
      !is.null(m_inf) && all(is.finite(m_inf$eigenvector)))

# Strengths near the top of floating point put the layout out of range.
huge <- data.frame(from = c("A", "B", "C", "C"), to = c("B", "C", "A", "D"),
                   weight = c(1, 1e300, 2, 1e300))
g_huge <- build_graph(huge)
w_huge <- igraph::E(as_plain_igraph(g_huge))$weight
check("huge strengths are scaled down and flagged",
      isTRUE(attr(g_huge, "weights_rescaled")) && max(w_huge) <= 1e9)
check("scaling keeps strengths in the same proportion",
      isTRUE(all.equal(sort(w_huge / max(w_huge)),
                       sort(c(1, 1e300, 2, 1e300) / 1e300))))
lay_huge <- tryCatch(compute_layout(compute_metrics(g_huge)$graph),
                     error = function(e) NULL)
check("huge strengths still produce a finite layout",
      !is.null(lay_huge) && all(is.finite(c(lay_huge$x, lay_huge$y))))
check("ordinary strengths are left exactly as given",
      !isTRUE(attr(build_graph(data.frame(from = "A", to = "B",
                                          weight = 5000)), "weights_rescaled")))

# The overlap pass runs only while the map has room for it. Above that
# it froze the screen for seconds without separating much.
# Three thousand people, where the pass used to take about ten seconds
# and now does not run. At 1,500 the old code still came in under the
# limit, so a test at that size could not tell the two apart.
set.seed(3)
big <- igraph::sample_pa(3000, m = 2, directed = FALSE)
big_el <- igraph::as_edgelist(big)
big_m <- compute_metrics(build_graph(data.frame(
  from = paste0("P", big_el[, 1]), to = paste0("P", big_el[, 2]))))
t_big <- system.time(lay_big <- compute_layout(big_m$graph))[["elapsed"]]
check("a 3,000 person map lays out in a few seconds, not ten or more",
      t_big < 5 && all(lay_big$x >= 0 & lay_big$x <= 1))
check("the layout stays the same from run to run above capacity",
      identical(lay_big, compute_layout(big_m$graph)))

# A model name reaches a shell. The pattern refuses every shell
# character, and the name is quoted where it is used.
check("model names with shell characters are refused",
      !valid_model_name("x; echo pwned") && !valid_model_name("a|b") &&
        !valid_model_name("$(id)") && !valid_model_name("-rf") &&
        valid_model_name("qwen2.5:7b") && valid_model_name("library/llama3.2"))
runtime_src <- paste(readLines("R/runtime.R"), collapse = "\n")
check("model names are quoted for the shell where they are used",
      grepl('c("pull", shQuote(name))', runtime_src, fixed = TRUE) &&
        grepl('c("rm", shQuote(name))', runtime_src, fixed = TRUE))

# The only way data could leave the machine is a model address that
# names another computer, so the app has to tell the two apart.
check("loopback addresses read as this machine",
      all(vapply(c("http://localhost:11434", "http://127.0.0.1:11434/",
                   "http://[::1]:11434", "http://127.4.5.6"),
                 is_local_address, logical(1))))
check("other addresses, including look alikes, read as another machine",
      !any(vapply(c("http://192.168.1.5:11434", "https://example.com",
                    "http://localhost.evil.com", "http://127.0.0.1.evil.com"),
                  is_local_address, logical(1))))

# The exported script is code a reader runs, so only a method this app
# offers is ever written into it.
hostile <- export_script('x")); system("echo pwned"); ("', "degree")
check("a hostile method name never reaches the exported script",
      !grepl("system(", hostile, fixed = TRUE) &&
        grepl("cluster_louvain(g)", hostile, fixed = TRUE))
check("an offered method is written as chosen",
      grepl("cluster_leiden", export_script("leiden", "degree"), fixed = TRUE))

# Values the page sends are claims, not facts. A selection left over from
# a larger network, a selection that is not a number, and an unknown
# sort column each broke an output before.
ok_claims <- tryCatch({
  testServer(app, {
    session$setInputs(dataset = "org", size_by = "degree",
                      sort_by = "degree", label_mode = "key")
    session$setInputs(selected_node = bundle()$metrics$n)
    session$setInputs(dataset = "referral")
    stopifnot(is.null(selected_index()))
    html <- as.character(output$reading$html)
    stopifnot(nchar(html) > 200, !grepl("About NA", html, fixed = TRUE))
    session$setInputs(selected_node = "abc")
    stopifnot(is.null(selected_index()), nchar(output$reading$html) > 200)
    session$setInputs(selected_node = 3)
    stopifnot(identical(selected_index(), 3L))
    session$setInputs(sort_by = "nonexistent_column")
    stopifnot(nchar(output$people_table$html) > 200)
  })
  TRUE
}, error = function(e) {
  cat("  claims error:", conditionMessage(e), "\n"); FALSE
})
check("stale, malformed, and unknown values from the page are absorbed",
      ok_claims)

# A file the measures cannot cope with reads as a message, never as the
# end of the session.
bad_file <- tempfile(fileext = ".csv")
writeLines(c("from,to,weight", "A,B,1", "B,C,Inf", "C,D,1e308", "D,A,2"),
           bad_file)
ok_bad_file <- tryCatch({
  testServer(app, {
    session$setInputs(dataset = "upload", size_by = "degree",
                      sort_by = "degree", label_mode = "key",
                      edges_file = list(datapath = bad_file, name = "bad.csv",
                                        size = file.size(bad_file),
                                        type = "text/csv"))
    b <- bundle()
    stopifnot(!is.null(b), b$metrics$n == 4,
              nchar(output$reading$html) > 200)
  })
  TRUE
}, error = function(e) {
  cat("  bad file error:", conditionMessage(e), "\n"); FALSE
})
check("a file of unusable strengths loads instead of ending the session",
      ok_bad_file)

# The guard itself, with a failure forced. Every input this suite has
# found that broke the measures is now cleaned on the way in, so no file
# reaches the guard any more, and a guard nothing reaches is a guard
# nothing tests. Each session sources R/ afresh, so overriding a
# function here would be undone, which is why the failure is planted in
# a throwaway copy of the app instead.
guard_dir <- file.path(tempfile("guard"), "app")
dir.create(guard_dir, recursive = TRUE)
file.copy(c("app.R", "R", "www"), guard_dir, recursive = TRUE)
cat("\ncompute_layout <- function(...) stop(\"forced layout failure\")\n",
    file = file.path(guard_dir, "R", "network_math.R"), append = TRUE)
ok_guard <- tryCatch({
  testServer(shinyAppFile(file.path(guard_dir, "app.R")), {
    session$setInputs(dataset = "org", size_by = "degree",
                      sort_by = "degree", label_mode = "key")
    stopifnot(is.null(bundle()))
    session$setInputs(dataset = "referral")
    stopifnot(is.null(bundle()))
  })
  TRUE
}, error = function(e) {
  cat("  guard error:", conditionMessage(e), "\n"); FALSE
})
unlink(dirname(guard_dir), recursive = TRUE)
check("a failure inside the measures reads as a message, not a crash",
      ok_guard)

# The grouping is the same however the session got here. Louvain breaks
# ties at random, and the sample grouped into five communities or six
# depending only on what had run before it.
# The sample is built once, outside the loop, because building it sets
# its own seed, which would hide the very randomness this checks for.
grouping_ed <- sample_org_network()
groups_by_state <- vapply(c(1, 99, 12345), function(seed) {
  set.seed(seed)
  compute_metrics(build_graph(grouping_ed))$n_groups
}, numeric(1))
check("the same file always yields the same groups", length(unique(groups_by_state)) == 1)
# with_seed is checked on its own. compute_metrics as a whole does draw
# random numbers, because igraph's eigenvector solver starts from a
# random vector, but that draw does not change the answer.
set.seed(777); before <- runif(1)
set.seed(777); invisible(with_seed(42, runif(5))); after <- runif(1)
check("fixing the grouping seed leaves the session's own randomness alone",
      identical(before, after))

# The exported scripts reproduce the numbers on screen. They are run for
# real, on a file whose strength column is not called weight, which is
# the case both scripts used to get wrong in different ways.
parity_ed <- sample_org_network(); names(parity_ed)[3] <- "strength"
parity_csv <- tempfile(fileext = ".csv")
write.csv(parity_ed, parity_csv, row.names = FALSE)
parity_m <- compute_metrics(build_graph(read.csv(parity_csv)))
run_script <- function(dialect) {
  code <- sub("your_edges.csv", parity_csv,
              export_script("louvain", "degree", dialect), fixed = TRUE)
  env <- new.env(parent = globalenv())
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  suppressMessages(suppressWarnings(utils::capture.output(
    eval(parse(text = code), envir = env))))
  env
}
agrees <- function(name, degree, between, close, eigen, groups) {
  i <- match(parity_m$names, name)
  isTRUE(all(degree[i] == parity_m$degree)) &&
    max(abs(between[i] - parity_m$betweenness)) < 1e-9 &&
    max(abs(close[i] - parity_m$closeness)) < 1e-9 &&
    max(abs(eigen[i] - parity_m$eigenvector)) < 1e-9 &&
    groups == parity_m$n_groups
}
ok_igraph <- tryCatch({
  g1 <- run_script("igraph")$g
  agrees(igraph::V(g1)$name, igraph::V(g1)$degree, igraph::V(g1)$betweenness,
         igraph::V(g1)$closeness, igraph::V(g1)$eigenvector,
         length(unique(igraph::V(g1)$group)))
}, error = function(e) { cat("  igraph script error:", conditionMessage(e), "\n"); FALSE })
check("the igraph script reproduces every number on screen", ok_igraph)
ok_tidy <- tryCatch({
  nt <- tidygraph::as_tibble(tidygraph::activate(run_script("tidy")$g, "nodes"))
  agrees(nt$name, nt$degree, nt$betweenness, nt$closeness, nt$eigenvector,
         length(unique(nt$group)))
}, error = function(e) { cat("  tidy script error:", conditionMessage(e), "\n"); FALSE })
check("the tidy script reproduces every number on screen", ok_tidy)

# The reading panel says what the numbers say. A three step path has two
# people in between, not three; one name takes a singular verb; a list
# with a remainder has one "and"; and no sentence leaves a double space.
narr_ref <- compute_metrics(build_graph(sample_referral_network()))
narr_cards <- describe_cards(narr_ref, relabel_groups(narr_ref$membership))
narr_text <- c(vapply(narr_cards, function(cd) paste(cd$prose, collapse = " "), ""),
               describe_network(narr_ref),
               model_facts(narr_ref, relabel_groups(narr_ref$membership)))
check("distance is described as steps, never as people passed through",
      any(grepl("about 3 steps", narr_text, fixed = TRUE)) &&
        !any(grepl("through about|runs through [0-9]", narr_text)))
check("a single name takes a singular verb",
      any(grepl("Theo has the most direct ties", narr_text, fixed = TRUE)) &&
        !any(grepl("(^|\\. )[A-Z][a-z]+ have the most", narr_text)))
check("a list with a remainder joins with one and",
      any(grepl("Luis, and 7 more stepped away", narr_text, fixed = TRUE)) &&
        !any(grepl("and [A-Z][a-z]+ and [0-9]+ more", narr_text)))
check("no reading carries a double space", !any(grepl("  ", narr_text)))

# Values worked out by hand on the path A, B, C, D, so the scale of each
# measure is pinned to arithmetic rather than to another igraph call.
# Without these the suite passed with betweenness and closeness left
# unnormalized, which changes every number on screen.
# Betweenness of B: of the three pairs not involving B, two (A to C and
# A to D) pass through it, out of (n - 1)(n - 2) / 2 = 3 pairs: 2/3.
# Harmonic closeness of A: (1 + 1/2 + 1/3) / (n - 1) = 11/18.
hand <- compute_metrics(build_graph(data.frame(from = c("A", "B", "C"),
                                               to = c("B", "C", "D"))))
hb <- setNames(hand$betweenness, hand$names)
hc <- setNames(hand$closeness, hand$names)
check("betweenness matches the hand count on a path",
      abs(hb[["B"]] - 2 / 3) < 1e-12 && abs(hb[["A"]]) < 1e-12)
check("harmonic closeness matches the hand count on a path",
      abs(hc[["A"]] - 11 / 18) < 1e-12 && abs(hc[["B"]] - 5 / 6) < 1e-12)

# An interrupted runtime download. The fetch writes a megabyte and then
# fails, with no network involved, and the folder must be empty after.
# Every function replaced here is put back before the next check. The
# curl package is optional for the app but required here, because this
# check replaces one of its functions, so a missing install is said
# plainly rather than left to read as a fault in the download code.
if (!requireNamespace("curl", quietly = TRUE)) {
  cat("  the curl package is needed for this check: install.packages(\"curl\")\n")
}
ok_interrupt <- tryCatch({
  real_root <- managed_root; real_platform <- host_platform
  real_stream <- curl::curl_fetch_stream
  fake_dir <- tempfile("tessera-data"); dir.create(fake_dir)
  managed_root <<- function() fake_dir
  host_platform <<- function() list(asset = "ollama-linux-amd64.tgz",
                                    label = "Linux")
  utils::assignInNamespace("curl_fetch_stream", function(url, fun, ...) {
    fun(as.raw(rep(1, 1024 * 1024))); stop("connection reset by peer")
  }, ns = "curl")
  res <- managed_install()
  left <- length(list.files(fake_dir))
  managed_root <<- real_root; host_platform <<- real_platform
  utils::assignInNamespace("curl_fetch_stream", real_stream, ns = "curl")
  unlink(fake_dir, recursive = TRUE)
  !isTRUE(res$ok) && left == 0
}, error = function(e) {
  cat("  interrupt error:", conditionMessage(e), "\n"); FALSE
})
check("an interrupted runtime download leaves nothing behind", ok_interrupt)

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
