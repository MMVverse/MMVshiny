MMVshiny demo application

This package ships a small Shiny application that demonstrates how to use MMVshiny
in the same way as the larger MMV apps (e.g. MMVSola/MMVFree):

- a state specification is stored in an Excel file
- MMVshiny generates the required JS handlers and R scripts (reactives/observers)
- the app initializes a single MMVshiny state and lets MMVshiny process GUI events

Files:

- MMVshinyDemo.zip  : the demo Shiny app (contains app.R and Resources/)

How to locate it from R:

  zip_path <- system.file("extdata", "MMVshinyDemo.zip", package = "MMVshiny")

See the vignette "MMVshiny: a step-by-step demo app" for a guided walkthrough.
