install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("colorspace")
library(colorspace)
install.packages("ggfx")
library(ggfx)
install.packages("ggtext")
library(ggtext)
install.packages("ragg")
library(ragg)
install.packages("cowplot")
library(cowplot)
install.packages("showtext")
library(showtext)
install.packages("png")
library(png)

read_tsv("basics.tsv")->basics
read_tsv("Episode.tsv")->episode
read_tsv("ratings.tsv")->ratings

basics_sc<- basics %>% 
  filter(primaryTitle == "Schitt's Creek", titleType == "tvSeries")
basics_sc

parent_title_id <- head(basics_sc, 1)  %>% 
  pull(tconst)

episodes_sc <- episode %>% 
  filter(parentTconst == parent_title_id) %>% 
  inner_join(ratings, by = "tconst") %>% 
  inner_join(basics, by = "tconst") %>%
  collect() %>% 
  mutate(across(c(seasonNumber, episodeNumber), as.numeric))
episodes_sc <- arrange(episodes_sc, seasonNumber, episodeNumber)


df_sc <- data.frame(episodes_sc['seasonNumber'],
                       episodes_sc['episodeNumber'],
                       episodes_sc['primaryTitle'],
                       episodes_sc['averageRating'],
                       episodes_sc['numVotes'])

names(df_sc) <- c('season', 'episode', 'title', 'imdb_rating', 'total_votes')

df_sc_avg <-
  df_sc %>% 
  arrange(season, episode) %>% 
  mutate(episode_id = row_number()) %>% 
  group_by(season) %>% 
  mutate(
    avg = mean(imdb_rating),
    episode_mod = episode_id + (6 * season),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(season = factor(season))

df_lines <-
  df_sc_avg%>% 
  group_by(season) %>% 
  summarize(
    start_x = min(episode_mod) - 4,
    end_x = max(episode_mod) + 4,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )


annotate_richtext <- function(label, ...) {
  annotate("richtext", size = 3, fill = NA, 
           label = label, label.color = NA, 
           label.padding = unit(0.05, "mm"),
           hjust = 0,
           ...)
}

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03,
             arrow = arrow(length = unit(1.0, "mm"), type = "closed"),
             ...) 
}


ggplot(data = df_sc_avg, aes(episode_mod, imdb_rating, group=factor(season))) +
  theme(plot.background = element_rect(color = NA, fill = '#080A0D'),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, color = lighten('#080A0D', 0.2)),
        text = element_text(color = '#e3e3e3'),
        legend.position = c(.5, .080), 
        legend.key.width = unit(1, "lines"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y =element_text(size=8), # y-numbers
        axis.title.y =element_text(size=8, color='#9BC6DA')
  ) +
  geom_hline(data = tibble(y = 6:9),
             aes(yintercept = y),
             color = lighten('#080A0D', 0.35),
             size = .5) +
  geom_segment(aes(xend = episode_mod,
                   yend = avg), 
                   col=colors["yellow"]) +
  geom_line(data = df_lines,
            aes(x_group, y), col=colors["yellow"],
            size = 1.5) +
  geom_point(aes(size = total_votes), col=colors["yellow"]) +
  geom_label(aes(mid, 5.0,
                 label = glue::glue(" Season {season} ")),
             col=colors["yellow"],
             fill = NA,
             size=3,
             label.padding = unit(.1, "lines"),
             label.r = unit(.18, "lines"),
             label.size = .4) +
  scale_x_continuous(expand = c(.015, .015)) +
  scale_y_continuous(expand = c(.03, .03),
                     limits = c(5, 10),
                     breaks = seq(5.0, 10, by = .5),
                     sec.axis = dup_axis(name = NULL)) +
  scale_size_binned(name = "Votes per Episode",
                    range = c(.5, 3)) +
  labs(x = NULL, y = "Average IMDb Rating", size=20,
       subtitle = "Each \u2022 represents one episode, its size is proportional to the number of votes. Horizontal bars indicate average season ratings",
       caption = "Visualization by Annapurani V. || Data from IMDb || Inspired by @lets_boldly_go, @CedScherer and @_ansgar") +
  guides(size = 'none', color = 'none') +
  ggtitle("'Schitt's Creek episode ratings (IMDb)") +
  
  annotate_richtext(label = "S6 E13 & E14 are the best<br>rated episodes (9.4)",
                    x = 76, y = 9.9, color = '#D9D3BE') +
  geom_curve2(aes(x = 94, xend = 90, y = 9.5, yend = 9.95), color = '#D9D3BE',
              curvature = 0.4) +
  
  annotate_richtext(label = "S1 E1 has the<br>lowest rating (7.1)",
                    x =14, y = 6.5, color = '#E1E1E1') +
  geom_curve2(aes(x = 14, xend = 7.5, y = 6.5, yend = 6.9), color = '#E1E1E1', 
              curvature = -0.4)->a

a


logo <- readPNG("SC.png")

ggdraw(a) +
  draw_image(logo, x = +.35, y = -.25, scale = .12)

