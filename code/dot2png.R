suppressWarnings(suppressMessages(library(DiagrammeR)))
suppressWarnings(suppressMessages(library(DiagrammeRsvg)))
suppressWarnings(suppressMessages(library(rsvg)))
png::writePNG(rsvg(charToRaw(export_svg(grViz(here::here("data/processed/make.dot"))))), 
              here::here("figures/make.png"))
