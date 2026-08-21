# Nonconformity

## Find where operational data breaks its own pattern.

Nonconformity is an open source R Shiny app for reviewing operational
time series such as server load, transaction counts, or sensor readings.

It learns the usual pattern of a time series and identifies moments that
differ from that pattern, including spikes, dips, sustained runs, and
lasting level changes. Each flagged event includes an explanation based
on the observed data.

Nothing leaves your computer. All calculations are performed locally.

The app supports two audiences from one screen:

-   Operators see clear charts, explanations, and a small set of
    controls.
-   Researchers can inspect detection methods, scores, settings, and the
    exact analysis call used for reproduction.

![The Nonconformity operator view, with the detection settings panel
open and no data loaded yet](docs/screenshot-operator.png)

## How it works

A typical workflow:

1.  Load a CSV file or open a built-in example.
2.  Choose a detection method.
3.  Review flagged events on the chart.
4.  Read the explanation for each event.
5.  Export or reproduce the analysis when needed.

The app keeps the detection method, event details, and explanations
connected so users can understand why a point was flagged.

## Example uses

Nonconformity can support reviews of:

-   Server and system performance
-   Transaction activity
-   Sensor measurements
-   Other operational time series with regular measurements

## Detection methods

Several methods use a shared event format, allowing the chart,
explanation cards, and optional local model features to work
consistently across methods.

### Resistant seasonal (default)

Uses a running median trend, seasonal medians, and binary segmentation
for lasting level changes. Median-based calculations reduce the effect
of the same extreme values the app is designed to detect.

### Rolling z-score

Uses a trailing median center and median absolute deviation spread
without assuming a seasonal pattern. It provides a useful comparison for
rhythmic and non-rhythmic data.

### STL remainder

Uses seasonal and trend decomposition with loess from base R and
identifies unusual remainder values. It requires a defined cycle and
falls back to the resistant seasonal method when one is not available.

## Understanding flagged events

Each event includes:

-   The observed change
-   The detection method used
-   The underlying score
-   A plain-language explanation

The app exposes the analysis details so users can understand how events
were identified rather than receiving only a final flag.

## Getting started

Run the app with:

``` bash
R -e "shiny::runApp('nonconformity')"
```

Requires R with:

-   `shiny`
-   `dplyr`
-   `tidyr`
-   `purrr`
-   `readr`
-   `tibble`
-   `lubridate`

Open the displayed address in a browser, then choose a sample or upload
a CSV.

## Data format

Nonconformity uses a CSV with two columns:

-   A timestamp column containing dates or datetimes
-   A numeric value column containing the measurement

Timestamp columns may use names such as:

-   `stamp`
-   `timestamp`
-   `time`
-   `date`
-   `datetime`
-   `ds`

Value columns may use names such as:

-   `value`
-   `y`
-   `count`
-   `load`
-   `amount`
-   `measure`

Data should have at least 10 readings. Longer series covering several
cycles generally produce better results.

A template is available under the upload box.

## Optional local model support

A local model running through Ollama can work with completed analysis
results.

The model receives the analysis summary and event list, not the raw
readings. It cannot change calculated values.

The local model can:

-   Summarize a complete analysis
-   Add context to event cards
-   Answer questions about the loaded data
-   Draft an incident summary

The Local model guide inside the app explains setup, model choices, and
configuration.

## Accessibility

Nonconformity is designed so detected events remain readable across
different visual settings.

Features include:

-   Dark and light themes
-   Color settings for different vision differences, including
    monochrome
-   WCAG 2.2 contrast checks across themes and palettes
-   Event types represented through marker shape as well as color
-   Keyboard navigation between events
-   Spoken announcements through a live region
-   Touch targets meeting accessibility size requirements
-   Reduced motion support

## Tests

Run the full test suite:

``` bash
sh nonconformity/tests/run_all.sh
```

The test suite checks:

-   Detection methods against known sample events
-   Contrast and accessibility settings
-   Generated writing
-   Browser rendering with jsdom
-   Application startup

## Colophon

Nonconformity is written in [R](https://www.r-project.org/) and built on
[Shiny](https://shiny.posit.co/).

The detection engine uses base R methods including:

-   `stats::runmed` for trend estimation
-   `stats::stl` for decomposition
-   A custom binary segmentation approach for lasting level changes

Data handling uses the listed tidyverse packages. The chart is
hand-built SVG, allowing each visual element and accessibility feature
to be controlled directly.

Tested against R 4.3 on Linux. Please include the output of
`sessionInfo()` with bug reports.

## Citation

If this software supports published work, please cite it.

The `CITATION.cff` file contains citation information, and GitHub
provides a formatted citation from the repository sidebar.

## License

PolyForm Noncommercial License 1.0.0.

See [LICENSE.md](LICENSE.md).

Noncommercial use is permitted under the license terms, including use by
charitable organizations, educational institutions, public research
organizations, and government institutions.
