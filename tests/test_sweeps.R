# Writing sweeps for Nonconformity.
# Every delivery rule with a mechanical test lives here: banned lexemes in
# any file, em or en dashes anywhere, contractions in text, apostrophes in
# the stylesheet, and a comment density near fifteen percent.

files <- c("app.R", "www/styles.css", "www/ui.js", "www/chart.js",
           "www/walkthrough.js", "www/model.js", "README.md", "CONTRIBUTING.md")

ok <- 0; bad <- 0
check <- function(label, cond, detail = "") {
  if (isTRUE(cond)) { ok <<- ok + 1; cat("pass ", label, "\n") }
  else { bad <<- bad + 1; cat("FAIL ", label, if (nzchar(detail)) paste0("\n", detail), "\n") }
}

# The banned list, one stem per entry, matched at a word start so every
# inflection is caught. CSS layout properties that contain a banned stem
# as pure syntax are excluded line by line below.
banned <- c(
  "actionable", "aim", "align", "bolster", "commendable", "delve", "drawn",
  "enabl", "encompass", "enhanc", "ensur", "equip", "esteemed", "facilitat",
  "foster", "friendly", "functionalit", "grasp", "guarantee",
  "hone\\b", "hones\\b", "honed\\b", "honing\\b",
  "influenc", "instrumental", "intersection", "intricate", "invaluable",
  "journey", "landscape", "leverag", "maximiz", "meticulous", "multifaceted",
  "nuance", "passionate", "perspective", "pivotal", "plethora", "realm",
  "rigor", "robust", "sacrific", "seamless", "showcas", "strengthen",
  "striv", "synerg", "technique", "transformativ", "translat", "tweak",
  "utiliz", "vital", "wish list"
)
syntax_ok <- "align-items|text-align|align-self|vertical-align|place-items"
# One exemption remains. stats::stl takes an argument literally named robust,
# and a caller does not get to rename another package's arguments. Everything
# else that once needed exempting was renamed instead, so the method is keyed
# and labeled resistant throughout.
code_ok <- "robust = TRUE"

pattern <- paste0("\\b(", paste(banned, collapse = "|"), ")")
for (f in files) {
  lines <- readLines(f, warn = FALSE)
  content <- lines[!grepl(syntax_ok, lines)]
  content <- gsub(code_ok, "", content)
  hits <- grep(pattern, content, ignore.case = TRUE, value = TRUE)
  check(paste("no banned lexemes in", f), length(hits) == 0,
        paste(head(hits, 5), collapse = "\n"))
}

# Em dashes and en dashes are out everywhere, including code.
for (f in files) {
  txt <- readLines(f, warn = FALSE)
  hits <- grep("\u2014|\u2013", txt, value = TRUE)
  check(paste("no em or en dashes in", f), length(hits) == 0,
        paste(head(hits, 3), collapse = "\n"))
}

# Contractions are out of every file, comments included.
contr <- paste0("\\b(can't|won't|don't|doesn't|isn't|aren't|wasn't|weren't|",
                "it's|you're|we're|they're|let's|that's|there's|what's|",
                "couldn't|shouldn't|wouldn't|hasn't|haven't|didn't|i'm|i've|",
                "you've|we've|they've)\\b")
for (f in files) {
  txt <- readLines(f, warn = FALSE)
  hits <- grep(contr, txt, ignore.case = TRUE, value = TRUE)
  check(paste("no contractions in", f), length(hits) == 0,
        paste(head(hits, 3), collapse = "\n"))
}

# The stylesheet stays free of apostrophes outside attribute selectors, so
# it can move into an R string without a single edit. Attribute selectors
# use them as syntax, which is why the check strips those first.
css <- readLines("www/styles.css", warn = FALSE)
css_stripped <- gsub("\\[[^]]*\\]", "", css)
css_stripped <- gsub("'[^']*'", "", css_stripped)
check("no stray apostrophes in styles.css",
      length(grep("'", css_stripped, value = TRUE)) == 0,
      paste(head(grep("'", css_stripped, value = TRUE), 3), collapse = "\n"))

# Comment density near fifteen percent in each code file.
density <- function(path, marker) {
  txt <- readLines(path, warn = FALSE)
  txt <- trimws(txt[nzchar(trimws(txt))])
  if (marker == "js") {
    # Block comments span lines, so a small state machine walks the file
    # and counts every line that sits inside one.
    inside <- FALSE
    hits <- vapply(txt, function(line) {
      starts <- grepl("^(/\\*|//)", line)
      was <- inside || starts
      if (grepl("/\\*", line) && !grepl("\\*/", line)) inside <<- TRUE
      if (grepl("\\*/", line)) inside <<- FALSE
      was
    }, logical(1))
    return(mean(hits))
  }
  mean(grepl(marker, txt))
}
r_dens <- density("app.R", "^#")
check(sprintf("app.R comment density %.0f%% within 10 to 25", r_dens * 100),
      r_dens >= 0.10 && r_dens <= 0.25)
js_files <- c("www/ui.js", "www/chart.js", "www/walkthrough.js", "www/model.js")
js_dens <- mean(sapply(js_files, function(f) density(f, "js")))
check(sprintf("js comment density %.0f%% within 10 to 25", js_dens * 100),
      js_dens >= 0.10 && js_dens <= 0.25)

cat(sprintf("\n%d passed, %d failed\n", ok, bad))
if (bad > 0) quit(status = 1)
