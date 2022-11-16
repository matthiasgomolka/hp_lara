library(targets)
library(tarchetypes)

tar_option_set(
    packages = c("fs", "tidyverse", "ggplot2")
)

list(
    tar_file_read(
        movies,
        "data/movies.csv",
        read_csv2(!!.x, col_types = "ciiddc") |>
            mutate(short_name = movie |>
                       str_remove("Part ") |>
                       str_replace("Harry Potter and the ", rep(c("", "\n"), 4L))
                   )
    ),

    tar_files(csv_files, dir_ls("data", regexp = "hp[0-8]\\.csv")),

    tar_target(
        df,
        read_csv(csv_files, col_types = "cccc") |>
            # mutate(character = factor(character, ))
            left_join(movies |> select(movie, released_year)),
        pattern = map(csv_files)
    ),

    tar_target(
        key_numbers,
        df |>
            group_by(movie, released_year, character) |>
            summarise(
                n_dialogues = n(),
                n_words = sum(str_count(dialog))
            )
    ),

    tar_target(
        characters,
        c("Harry Potter", "Hermione Granger", "Ron Weasley", "Albus Dumbledore")
    ),

    tar_target(
        x_scale,
        structure(
            pull(movies, released_year),
            names = pull(movies, short_name)
        )
    ),
    tar_target(
        figure,
        key_numbers |> #count(character) |> arrange(desc(n))
            filter(character %in% characters) |>
            ggplot() +
            aes(x = released_year, y = n_words, color = character)  +
            geom_line(linewidth = 2L) +
            scale_x_continuous(
                breaks = x_scale, labels = names(x_scale)
            ) +
            annotate(
                geom = "curve", x = 2003, y = 12000, xend = 2002.1, yend = 11300,
                curvature = .3, arrow = arrow(length = unit(2, "mm"))
            ) +
            annotate(geom = "text", x = 2003.1, y = 12000, label = "Harry", hjust = "left") +

            annotate(
                geom = "curve", x = 2005, y = 2000, xend = 2006, yend = 2500,
                curvature = .3, arrow = arrow(length = unit(2, "mm"))
            ) +
            annotate(geom = "text", x = 2004.9, y = 2000, label = "Ron", hjust = "right") +
            theme_minimal() +
            theme(
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                plot.title.position = "plot"
            ) +
            labs(title = "Number of spoken words per movie")
    )
)

