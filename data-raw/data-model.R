## devtools::install_github("bergant/datamodelr")
library(tidyverse)
library(here)
library(datamodelr)
library(DiagrammeR)

dm <- here("data-raw", "data-model.yml") %>%
  dm_read_yaml()

graph <- dm_create_graph(dm, rankdir = "BT")

dm_render_graph(graph)
dm_export_graph(graph, here("data-raw", "data-model.png"), file_type = "png")
