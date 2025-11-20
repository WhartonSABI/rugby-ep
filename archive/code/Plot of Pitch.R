# Rugby Pitch Graphic

library(ggplot2)
library(dplyr)

# Define field zones
zone_bounds <- tibble::tibble(
  xmin = c(0,   5,  22, 40, 50, 60, 78, 95),
  xmax = c(5,  22,  40, 50, 60, 78, 95,100)
) %>% mutate(idx = seq_len(n()))

pitch_height <- 70
pitch_length <- 100

# Greyscale palette — symmetric
grey_palette <- c(
  gray.colors(4, start = 0.85, end = 0.5),
  rev(gray.colors(4, start = 0.85, end = 0.5))
)

# Field lines and labels
field_lines <- tibble::tibble(
  x = c(0, 5, 22, 40, 50, 60, 78, 95, 100),
  type = ifelse(x %in% c(5, 40, 60, 95), "dashed", "solid"),
  label = case_when(
    x %in% c(5, 95) ~ "5m",
    x %in% c(22, 78) ~ "22m",
    x %in% c(40, 60) ~ "10m",
    TRUE ~ NA_character_
  )
)

# Plot
p <- ggplot() +
  # Shaded zones
  geom_rect(
    data = zone_bounds,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = pitch_height, fill = factor(idx)),
    color = NA
  ) +
  scale_fill_manual(values = grey_palette, guide = "none") +
  # Outline
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_height),
            color = "black", fill = NA, size = 0.6) +
  # Lines
  geom_segment(
    data = field_lines,
    aes(x = x, xend = x, y = 0, yend = pitch_height, linetype = type),
    color = "black", size = 0.5
  ) +
  scale_linetype_manual(values = c(solid = "solid", dashed = "dashed"), guide = "none") +
  # Labels (no label for halfway)
  geom_text(
    data = filter(field_lines, !is.na(label)),
    aes(x = x, y = pitch_height + 2, label = label),
    size = 3.5, vjust = 0, fontface = "bold"
  ) +
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(plot.margin = margin(20,10,10,10))

print(p)







