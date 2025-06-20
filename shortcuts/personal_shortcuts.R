# personal shortcuts

library(targets)

tv <- function() {
  tar_visnetwork(
    level_separation = 100
  ) # my personal shortcuts
}
tm <- function() {
  tar_make()
}
tr <- function(...) {
  tar_read(...)  
}
V <- function(...) {
  View(...)
}
ti <- function() {
  tar_invalidate(everything())
}
trV <- function(...) {
  tar_read(...) %>% View()
}
