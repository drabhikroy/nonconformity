# Security

## What Nonconformity is exposed to

Nonconformity is a single-file Shiny application. Uploaded data
reaches the app as a CSV parsed for numeric computation; the
deterministic analysis is always complete on its own. The optional
local model feature is different from the rest of the portfolio in
one respect worth stating plainly: the browser talks to Ollama
directly, so no series data touches the Shiny server for this
feature, and the connection address is a field you can edit,
defaulting to `http://localhost:11434` rather than being fixed in
code.

## What the code does about it

The model is given the finished analysis, not the raw readings: a
summary line, the detection method, and the event list with sizes and
timings, so a call is small and fast and the model never sees data it
could leak or memorize. Every model response is HTML-escaped in the
browser before it is inserted into the page, with only line breaks
substituted afterward, so nothing a model or a misconfigured endpoint
returns can inject markup. A failed connection and an empty answer are
reported as distinct outcomes rather than collapsed into one message,
so a stopped server is not mistaken for a bad prompt. The
deterministic analysis never depends on the model answering at all.

## Reporting a problem

Open a private security advisory through the repository, or open a
normal issue if the problem is not sensitive. Please include the
version, what you did, and what you saw.

## Scope

In scope: anything that causes uploaded data to be sent to the model
endpoint, that lets a model response inject markup instead of being
escaped, or that causes the deterministic analysis to depend on the
model answering. Out of scope: Ollama itself, which is reported to
its own maintainers, and anything that requires an attacker to
already be running code on the same machine or to have edited the
endpoint address themselves.
