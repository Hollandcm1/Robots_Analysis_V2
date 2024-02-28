# Import Codebook

codebook <- read.xlsx(here("data", "codebook", "Robots - Codebook.xlsx"))

codes.conditions <- codebook[, c("X7", "Cond.1", "Cond.2", "Cond.3", "Cond.4")]
codes.conditions <- codes.conditions[1:2,]

codes.participant_conditions <- codebook[, c("Participant", "first", "second", "third", "forth")]

