# Contrast audit for Nonconformity.
# The stylesheet is the single source of truth, so this script parses the
# token blocks out of www/styles.css, composes each of the eight theme and
# palette combinations the way the cascade would, and checks every pair
# that carries text meaning against WCAG 2.2 at 4.5 to 1.

css_path <- if (file.exists("www/styles.css")) "www/styles.css" else "../www/styles.css"
css <- readLines(css_path, warn = FALSE)

# Pull a named vector of custom properties from one selector block. The
# blocks are flat, so a simple brace scan is enough and keeps the audit
# free of any parsing dependency.
block_tokens <- function(selector) {
  start <- grep(paste0("^", gsub("\\.", "\\\\.", selector), " \\{$"), css)
  if (length(start) == 0) return(character(0))
  out <- character(0)
  i <- start[1] + 1
  while (i <= length(css) && !grepl("^\\}", css[i])) {
    m <- regmatches(css[i], regexec("--([a-z0-9-]+):\\s*(#[0-9a-fA-F]{6})", css[i]))[[1]]
    if (length(m) == 3) out[m[2]] <- m[3]
    i <- i + 1
  }
  out
}

# Relative luminance and contrast ratio, straight from the WCAG formula.
lum <- function(hex) {
  v <- strtoi(c(substr(hex, 2, 3), substr(hex, 4, 5), substr(hex, 6, 7)), 16L) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055) ^ 2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
}
ratio <- function(a, b) {
  la <- lum(a); lb <- lum(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

light_base <- block_tokens("body")
dark_base  <- block_tokens("body.dark")

palettes <- list(
  standard = list(light = character(0), dark = character(0)),
  deutan   = list(light = block_tokens("body.cb-deutan"),
                  dark  = block_tokens("body.dark.cb-deutan")),
  tritan   = list(light = block_tokens("body.cb-tritan"),
                  dark  = block_tokens("body.dark.cb-tritan")),
  mono     = list(light = block_tokens("body.cb-mono"),
                  dark  = block_tokens("body.dark.cb-mono"))
)

# Pairs that carry meaning as text or as essential marks. Event colors are
# audited against both card surfaces because chips and card borders sit on
# either one. The accent pair is audited per palette because the accent now
# moves with the palette rather than staying fixed.
pairs <- list(
  c("text", "bg"), c("text", "surface"), c("text", "surface2"),
  c("muted", "bg"), c("muted", "surface"), c("muted", "surface2"),
  c("accent-ink", "accent"),
  c("spike", "surface"), c("dip", "surface"),
  c("run", "surface"), c("shift", "surface"),
  c("danger", "surface")
)

fails <- 0; checks <- 0
for (theme in c("light", "dark")) {
  base <- light_base
  if (theme == "dark") base[names(dark_base)] <- dark_base
  for (pn in names(palettes)) {
    tokens <- base
    over <- palettes[[pn]][[theme]]
    if (length(over)) tokens[names(over)] <- over
    for (p in pairs) {
      if (!(p[1] %in% names(tokens)) || !(p[2] %in% names(tokens))) next
      r <- ratio(tokens[[p[1]]], tokens[[p[2]]])
      checks <- checks + 1
      tag <- sprintf("%-5s %-8s %-11s on %-9s %5.2f", theme, pn, p[1], p[2], r)
      if (r < 4.5) { fails <- fails + 1; cat("FAIL ", tag, "\n") }
      else cat("pass ", tag, "\n")
    }
  }
}
cat(sprintf("\n%d checks, %d failures\n", checks, fails))
if (fails > 0) quit(status = 1)
