# This script will compare, with the help of ggplot, the optimal response against two different bet sizes on the flop.

library(tidyverse)
library(glue)
library(RPostgres)
library(DBI)

solutions_db <- dbConnect(
  drv = Postgres(),
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "157575",
  dbname = "Solutions"
)

hand_class <- dbGetQuery(solutions_db, "SELECT * FROM lookup_hand_class;")
hand_draw <- dbGetQuery(solutions_db, "SELECT * FROM lookup_hand_draw;")

# Selecting simulations with Heads-up optimal preflop ranges, in a Single Raised POT,
# where there was either an option to bet 32% or check, or bet 67% or check.

df <- dbGetQuery(
  solutions_db,
  'SELECT boards.flop, boards.iso_flop, tree.description, lookup_nodes.node_name, 
unnest(lookup_combo.combo_pre) AS "combo_pre", unnest(lookup_combo.combo) AS "combo", 
unnest(boards.combo_made_hand) AS "made_hand", unnest(boards.combo_draw) AS "draw", 
unnest(simulations.ev_oop) AS "ev_oop", unnest(simulations.ev_ip) AS "ev_ip", 
unnest(simulations.eq_oop) AS "eq_oop", unnest(simulations.eq_ip) AS "eq_ip", 
unnest(simulations.strategy) AS "range", simulations.occurrence FROM simulations
LEFT JOIN boards ON simulations.id_board = boards.id_board
LEFT JOIN preflop ON simulations.id_pre = preflop.id_pre
LEFT JOIN lookup_combo ON simulations.id_game = lookup_combo.id_game
LEFT JOIN lookup_nodes ON simulations.id_node = lookup_nodes.id_node
LEFT JOIN tree ON lookup_nodes.id_tree = tree.id_tree
WHERE tree.description IN(\'100BB SRP FLOP B32 R52 RR32\', \'100BB SRP FLOP B89 R52 RR32\') 
AND preflop.description = \'HU 50NL SRP GTO\';'
) %>%
  left_join(hand_class, join_by(made_hand == id_hand_class)) %>% 
  left_join(hand_draw, join_by(draw == id_hand_draw)) %>%
  select(-draw, -made_hand) %>%
  relocate(hand_class, .after = combo) %>%
  relocate(hand_draw, .after = hand_class)

ranks <- factor(c("A","K","Q","J","T","9","8","7","6","5","4","3","2"),
                 levels = c("2","3","4","5","6","7","8","9","T","J","Q","K","A"))
suits <- c("h","s","d","c")

# Selecting the nodes following the node where we face a bet from the preflop raiser (r:0:c:b32 / r:0:c:b67)

facing_cbet <- df %>% 
  filter(
    node_name %>% str_detect("r:0:c:b[:digit:]+:[^:]+$")
    ) %>%
  mutate(
    action = ifelse(
      node_name %>% str_detect("c$"), "CALL", ifelse(node_name %>% str_detect("f$"), "FOLD", "RAISE")
    ),
    action = factor(action,c("FOLD", "CALL", "RAISE")),    # Factorising so Raise becomes the first value in a plot.
    range = ifelse(range<0,0,range)
  ) %>%
  mutate(
    # adjusting the range to the number of isomorphic boards it represents (boards with the same strategy but different suits)
    range_adjusted = range*iso_flop
  )

flop_data <- data.frame(
  flop = facing_cbet$flop %>% unique,
  cnt_ranks = sapply(facing_cbet$flop %>% unique, function(flop) sum(!!str_count(flop, ranks %>% as.character()))),
  cnt_suits = sapply(facing_cbet$flop %>% unique, function(flop) sum(!!str_count(flop, suits))),
  # Count of cards higher then 9 
  cnt_broadways = sapply(facing_cbet$flop %>% unique, function(flop) sum(!!str_count(flop, ranks %>% as.character() %>% .[1:5]))),
  # Extracting the highest rank
  which_high = factor(sapply(facing_cbet$flop %>% unique, function(flop) str_sub(flop,1,1)), 
                      levels = c("2","3","4","5","6","7","8","9","T","J","Q","K","A"))
)

hand_class_order <- hand_class$hand_class

facing_cbet %>%
  mutate(hand_class = factor(hand_class, levels = hand_class_order)) %>%
  left_join(flop_data) %>%
  filter(cnt_suits == 3) %>%
  filter(cnt_ranks == 3) %>%
  ggplot(aes(hand_class, range_adjusted, fill = action)) +
  geom_col() +
  facet_wrap(vars(description)) + 
  coord_flip() +
  scale_fill_manual(values=c("#6da2c0","#8fbc8b", "#e9967a")) +
  ggtitle("vs C-bet on Monotone Non-Paired Boards") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
  )

facing_cbet %>%
  mutate(hand_class = factor(hand_class, levels = hand_class_order)) %>%
  left_join(flop_data) %>%
  filter(cnt_suits == 3) %>%
  filter(cnt_ranks == 3) %>%
  filter(hand_class %in% c("ace-high, king-high", "nothing")) %>%
  ggplot(aes(hand_draw, range_adjusted, fill = action)) +
  geom_col() +
  facet_wrap(vars(description)) + 
  coord_flip() +
  scale_fill_manual(values=c("#6da2c0","#8fbc8b", "#e9967a")) +
  ggtitle("A-High, K-High, Nothing") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
  )


facing_cbet %>%
  mutate(hand_class = factor(hand_class, levels = hand_class_order)) %>%
  left_join(flop_data) %>%
  filter(cnt_suits == 3) %>%
  filter(cnt_ranks == 2) %>%
  ggplot(aes(hand_class, range_adjusted, fill = action)) +
  geom_col() +
  facet_wrap(vars(description)) + 
  coord_flip() +
  scale_fill_manual(values=c("#6da2c0","#8fbc8b", "#e9967a")) +
  ggtitle("vs C-bet on Monotone Paired Boards") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
  )

facing_cbet %>%
  mutate(hand_class = factor(hand_class, levels = hand_class_order)) %>%
  left_join(flop_data) %>%
  filter(cnt_suits == 3) %>%
  filter(cnt_ranks == 2) %>%
  filter(hand_class %in% c("ace-high, king-high", "nothing")) %>%
  ggplot(aes(hand_draw, range_adjusted, fill = action)) +
  geom_col() +
  facet_wrap(vars(description)) + 
  coord_flip() +
  scale_fill_manual(values=c("#6da2c0","#8fbc8b", "#e9967a")) +
  ggtitle("A-High, K-High, Nothing") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
  )
