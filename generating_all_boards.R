# This script has the purpose of populating the boards table with all strategically relevant flops, turns and rivers.
# Isomorphism has been calculated on all streets in order to reduce the database size.
# "h" stands for hearts, "s" for spades, "d" for diamonts and "c" for clubs.

library(tidyverse)
library(glue)
library(processx)
library(RPostgres)
library(DBI)

ranks <- factor(c("A","K","Q","J","T","9","8","7","6","5","4","3","2"),
                 levels = c("2","3","4","5","6","7","8","9","T","J","Q","K","A"))
suits <- c("h","s","d","c")
cards <- sapply(ranks, function(e) sapply(suits, function(f) str_c(e,f))) %>% as.vector() 

solutions_db <- dbConnect(
  drv = Postgres(),
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "******",
  dbname = "Solutions"
)

flops <- lapply(ranks, function(e)
  lapply(ranks[as.numeric(ranks)<=as.numeric(e)], function(f)
    lapply(ranks[as.numeric(ranks)<=as.numeric(f)], function(g)
      if(e == f && e == g) {
        str_c(e,suits[1],f,suits[2],g,suits[3])
      }
      else if(e == f && e != g) {
        sapply(c(1,3), function(h)
          str_c(e,suits[1],f,suits[2],g,suits[h])
        )
      }
      else if(f == g && f !=e) {                            # Suit order reversed, board pairs always h,s
        sapply(c(1,3), function(h)
          str_c(e,suits[h],f,suits[1],g,suits[2])
        )
      } else {
        sapply(list(c(1,1,1),c(1,1,2),c(1,2,1),c(2,1,1),c(1,2,3)), function(h)
          str_c(e,suits[h[1]],f,suits[h[2]],g,suits[h[3]])
        )
      }
    )
  )
) %>% unlist()

count_suits <- function(e) {
  e %>% str_extract_all("[hsdc]") %>% 
    unlist() %>%
    table %>% 
    sort(decreasing = T) %>% 
    .[1] %>% 
    unname
}

count_ranks <- function(e) {
  e %>% str_extract_all("[23456789TJQKA]") %>% 
    lapply(factor) %>% lapply(tabulate) %>% unlist() %>% 
    length()
}

flops_df <- data.frame(
  flop = flops,
  cnt_ranks = flops %>% sapply(function(e) e %>% count_ranks, USE.NAMES = F),
  cnt_suits = flops %>% sapply(function(e) e %>% count_suits, USE.NAMES = F)
) %>% mutate(iso_suits = ifelse(
  cnt_suits == 1, 24,ifelse(cnt_suits == 2, 12, 4))) %>%
  mutate(iso_ranks = ifelse(cnt_ranks == 1, 4, ifelse(cnt_ranks == 2, 12, NA))) %>%
  mutate(iso_flop = ifelse(is.na(iso_ranks),iso_suits,iso_ranks)) %>% select(flop, iso_flop)

turns_df <- lapply(flops, function(e)
  if(e %>% count_suits()==3) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hs]")], function(f)
      list(flop = e,
           turn = f, 
           iso_turn = ifelse(
             str_detect(f,"h"),
             1,
             3)
           ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_suits()==2) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hsd]")], function(f)
      list(flop = e,
           turn = f, 
           iso_turn = ifelse(
             str_detect(f,"h"),
             1,
             ifelse(
               str_detect(f,"s"),
               1,
               2))
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_ranks == 1) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hc]")], function(f)
      list(flop = e,
           turn = f, 
           iso_turn = ifelse(
             str_detect(f,"c"),
             1,
             3)
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_ranks == 2 && e %>% count_suits == 1) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hdc]")], function(f)
      list(flop = e,
           turn = f, 
           iso_turn = ifelse(
             str_detect(f,"c|d"),
             1,
             2
           )
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else {
    sapply(cards[-str_which(e, cards)], function(f)
      list(flop = e,
           turn = f, 
           iso_turn = 1
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
) %>% 
  list_rbind() %>% 
  transmute(flop = list_c(flop),turn = list_c(turn),iso_turn = list_c(iso_turn)) %>% 
  left_join(flops_df, by = "flop") %>% 
  relocate(iso_flop, .before = iso_turn)

rivers_df <- lapply(str_c(turns_df$flop,turns_df$turn), function(e)
  if(e %>% count_suits()==4) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hs]")], function(f)
      list(flop = str_sub(e,1,6),
           turn = str_sub(e,7,8), 
           river = f,
           iso_river = ifelse(
             str_detect(f,"h"),
             1,
             3)
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_suits()==3) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hsd]")], function(f)
      list(flop = str_sub(e,1,6),
           turn = str_sub(e,7,8), 
           river = f,
           iso_river = ifelse(
             str_detect(f,"h"),
             1,
             ifelse(
               str_detect(f,"s"),
               1,
               2))
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_ranks == 1) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[h]")], function(f)
      list(flop = str_sub(e,1,6),
           turn = str_sub(e,7,8), 
           river = f,
           iso_river = 4
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else if(e %>% count_ranks == 2 && e %>% count_suits == 1) {
    sapply(cards[-str_which(e, cards)] %>% .[str_which(.,"[hdc]")], function(f)
      list(flop = str_sub(e,1,6),
           turn = str_sub(e,7,8), 
           river = f,
           iso_river = ifelse(
             str_detect(f,"c|d"),
             1,
             2
           )
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
  else {
    sapply(cards[-str_which(e, cards)], function(f)
      list(flop = str_sub(e,1,6),
           turn = str_sub(e,7,8), 
           river = f,
           iso_river = 1
      ), USE.NAMES = F
    ) %>% t() %>% as.data.frame()
  }
) %>% 
  list_rbind() %>% 
  transmute(flop = list_c(flop),turn = list_c(turn),river = list_c(river),iso_river = list_c(iso_river)) %>% 
  left_join(flops_df, by = "flop") %>% mutate(board = str_c(flop,turn)) %>% 
  left_join(turns_df %>% mutate(board = str_c(flop,turn)), by = "board") %>% 
  select(c(1:5),10) %>% 
  set_names(c("flop","turn","river","iso_river","iso_flop","iso_turn")) %>% 
  relocate(iso_river, .after = iso_turn)

all_boards_df <- flops_df %>% mutate(turn = NA, river = NA, iso_turn = NA, iso_river = NA) %>% relocate(iso_flop, .before = iso_turn) %>% 
  bind_rows(
    turns_df %>% mutate(river = NA, iso_river = NA) %>% relocate(iso_flop, .before = iso_turn) %>% relocate(river, .after = turn),
    rivers_df
  )

con <- process$new(
  command = "C:\\PioSOLVER\\PioSOLVER-pro.exe",
  stdin = "|",
  stdout = "|",
  wd = "C:\\PioSOLVER"
)

con$read_output_lines(n = -1)

for(i in 1:nrow(all_boards_df)) {
  # Using the show_categories functionality from PioSolver to get the hand categories:
  
  con$write_input(str_c("show_categories",str_sub(flops_df[i,1],1,2),str_sub(flops_df[i,1],3,4),str_sub(flops_df[i,1],5,6),str_sub(flops_df[i,1],7,8),str_sub(flops_df[i,1],9,10), " \n", sep = " "))
  categories <- character()
  if (con$poll_io(1000)["output"] == "ready") {
    categories <- c(categories, con$read_output_lines())
  }
  
  # Populating the table:
  
  dbSendQuery(
    solutions_db, 
    glue("INSERT INTO boards (flop, turn, river, iso_flop, iso_turn, iso_river, combo_made_hand, combo_draw) 
         VALUES ('{all_boards_df[i,1]}', '{all_boards_df[i,2]}', '{all_boards_df[i,3]}', {all_boards_df[i,4]}, '
         {all_boards_df[i,5]}, {all_boards_df[i,6]}, ARRAY[{categories[1] %>% str_replace_all(' ', ', ')}], 
         ARRAY[{categories[2] %>% str_replace_all(' ', ', ')}]);") %>% 
      str_replace_all("'NA'", "NULL") %>% str_replace_all("NA", "NULL")
  )
}

con$kill()
dbDisconnect(solutions_db)