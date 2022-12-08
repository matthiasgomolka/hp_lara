library(fs)
library(tidyverse)
library(vroom)
library(tidytext)
library(ggplot2)
library(ggchicklet)

movies <- read_delim("data/movies.csv", delim = ";", col_types = "ci____") |>
    mutate(
        movie_abbr = movie |>
            str_replace("1", "One") |>
            str_replace("2", "Two") |>
            # str_remove("Part ") |>
            # str_replace("Harry Potter and the ", rep(c("", "\n"), 4L)),
            str_remove("Harry Potter and the "),
        movie_abbr = factor(movie_abbr, movie_abbr, ordered = TRUE),
        movie_num = row_number(),
        movie_name = factor(movie, levels = movie, ordered = TRUE)
    ) |>
    select(movie_num, movie_name, movie_abbr)

csv_files <- dir_ls("data", regexp = "hp[0-8]\\.csv")

dialogues <- read_csv(csv_files, col_types = "c_cc") |>
    mutate(movie = str_replace(movie, "Gobelt", "Goblet"))

tokens <- movies |>
    left_join(dialogues, by = c("movie_name" = "movie")) |>
    unnest_tokens(word, dialog)

sentiments <- tokens |>
    inner_join(get_sentiments(lexicon = "nrc"), by = "word")

characters <- c("Harry Potter", "Hermione Granger", "Ron Weasley", "Albus Dumbledore")

pdf1 <- sentiments |>
    filter(character == "Harry Potter") |>
    filter(sentiment %in% c("anger", "disgust", "fear", "joy", "sadness")) |>
    count(movie_num, movie_name, movie_abbr, sentiment) |>
    group_by(movie_num) |>
    mutate(
        score = n / sum(n),
        sentiment = factor(
            sentiment,
            levels = c("anger", "joy", "disgust", "sadness", "fear"),
            ordered = TRUE
        )
    )


# cols = c(
#     anger = "#ef476f",
#     disgust = "#06D6A0",
#     fear = "#746AC7",
#     joy = "#FAE97C",
#     sadness = "#15AEE2",
#     bg = "#343a40",
#     text = "#f8f9fa",
#     grid = "#7f7f7f"
# )
cols = c(
    anger = "#ff595e",
    joy = "#ffca3a",
    disgust = "#8ac926",
    sadness = "#1982c4",
    fear = "#6a4c93",
    bg = "#f0f0f0",
    text = "#343a40",
    grid = "#7f7f7f"
)


# https://www.qualitylogoproducts.com/blog/harry-potter-color-schemes/


ggplot(pdf1) +
    aes(x = sentiment, y = score, fill = sentiment, group = movie_name) +
    geom_col(position = "dodge", color = cols["text"]) +
    # geom_chicklet(radius = grid::unit(1, "mm"), color = cols["text"]) +
    facet_wrap(~ movie_abbr, nrow = 2L) +
    labs(
        title = "Main Sentiments in Harry Potter Movies",
        subtitle = "(relative to each other)",
        caption = "Code available on https://github.com/matthiasgomolka/hp_lara"
    ) +
    scale_fill_manual(values = c(cols["anger"], cols["disgust"], cols["fear"], cols["joy"], cols["sadness"])) +
    theme_minimal() +
    # theme_void()
    theme(
        axis.title = element_blank(),
        # axis.text = element_text(color = cols["text"]),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        # legend.spacing.x = unit(1, "cm"),
        legend.text = element_text(size = 14, margin = margin(r = 2, unit = "cm"), vjust = 0.75),
        legend.title = element_blank(),
        legend.key = element_rect(color = cols["text"], linewidth = 0.1),

        text = element_text(family = "Harry Potter", color = cols["text"]),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        strip.text = element_text(size = 14, color = cols["text"]),

        plot.background = element_rect(fill = cols["bg"]),
        panel.background = element_rect(fill = cols["bg"], color = NA),
        # panel.grid = element_line(color = cols["grid"]),
        panel.grid = element_blank(),
        panel.spacing = unit(1.5, "cm")
    )

# ggplot(pdf1, aes(x = movie_abbr, y = score, fill = sentiment, color = sentiment, group = sentiment)) +
#     # geom_line() +
#     geom_col(position = "dodge") +
#     coord_polar() +
#     theme_minimal() +
#     # facet_wrap(~ movie)
#     theme(
#         axis.title = element_blank(),
#         axis.text.y = element_blank(),
#         legend.title = element_blank()
#     )


# tar_target(
#     figure,
#     key_numbers |> #count(character) |> arrange(desc(n))
#         filter(character %in% characters) |>
#         ggplot() +
#         aes(x = released_year, y = n_words, color = character)  +
#         geom_line(linewidth = 2L) +
#         scale_x_continuous(
#             breaks = x_scale, labels = names(x_scale)
#         ) +
#         annotate(
#             geom = "curve", x = 2003, y = 12000, xend = 2002.1, yend = 11300,
#             curvature = .3, arrow = arrow(length = unit(2, "mm"))
#         ) +
#         annotate(geom = "text", x = 2003.1, y = 12000, label = "Harry", hjust = "left") +
#
#         annotate(
#             geom = "curve", x = 2005, y = 2000, xend = 2006, yend = 2500,
#             curvature = .3, arrow = arrow(length = unit(2, "mm"))
#         ) +
#         annotate(geom = "text", x = 2004.9, y = 2000, label = "Ron", hjust = "right") +
#         theme_minimal() +
#         theme(
#             axis.title = element_blank(),
#             axis.ticks = element_blank(),
#             panel.grid.minor = element_blank(),
#             panel.grid.major.y = element_blank(),
#             plot.title.position = "plot"
#         ) +
#         labs(title = "Number of spoken words per movie")
# )



