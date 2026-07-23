# Contributing to Nonconformity

Thank you for considering a contribution. This is a noncommercial open source
project under the PolyForm Noncommercial License 1.0.0, and contributions are
accepted under those same terms.

## Before you start

Open an issue describing the change before writing code. A detection method,
a UI change, and a bug fix each want a different conversation, and an issue
saves you from building something that does not fit the design.

## Running the app

```r
shiny::runApp("nonconformity")
```

Requires R 4.1 or later with shiny, dplyr, tidyr, purrr, readr, tibble, and
lubridate.

## Running the tests

```sh
sh tests/run_all.sh
```

The gate must pass before a pull request is reviewed. It runs five suites:

- **Engine.** Detection is checked against events planted at known positions
  in the sample data, across every registered method.
- **Contrast.** Every color pair that carries meaning is checked against WCAG
  2.2 at 4.5 to 1, in all eight theme and palette combinations.
- **Writing sweeps.** Banned lexemes, em and en dashes, contractions, and
  comment density.
- **DOM.** The client modules under jsdom, including the renderer and the
  local model layer against a stubbed endpoint.
- **Live boot.** The app starts and serves its page.

## Adding a detection method

Methods are registered in one place. Add a row to `method_registry` in `app.R`
and write a function matching the detector contract documented above the
method definitions. A detector receives `(df, ex, sens, penalty)` and returns
a list with `events`, `expected`, `lo`, `hi`, and `scale`.

`lo` and `hi` are per reading rather than a single width. A method whose
threshold moves from reading to reading would otherwise show marks sitting
inside its own expected range, which reads as a contradiction. The engine
suite checks this for every registered method, so a new one is covered
automatically.

## Style

The codebase follows a few conventions that the sweeps enforce:

- No em dashes or en dashes anywhere, including comments.
- No contractions in code, comments, or interface text.
- Comments explain why a choice was made, not what the next line does.
- Comment density between ten and twenty five percent.

## Accessibility

Accessibility is not a later pass. Any contribution that touches the interface
should preserve the following, all of which are checked or reviewed:

- Contrast at 4.5 to 1 across all themes and palettes.
- Meaning carried by shape as well as color.
- Keyboard reachability, with visible focus.
- Touch targets of at least 44 pixels.
- Motion that honors the reduced motion preference.
