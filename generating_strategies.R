# This script is for scraping the EVs, equities and strategies from PioViewer, a proprietary console application used
# to calculate the Nash Equilibrium of postflop No Limit Hold'em Poker strategies. The gametree nodes are also built
# if not already present in the databse.

library(tidyverse)
library(glue)
library(RPostgres)
library(DBI)
library(tcltk)

solutions_db <- dbConnect(
  drv = Postgres(),
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "******",
  dbname = "Solutions"
)

# Creating a small tcltk UI to choose the situation to solve:

options_preflop <- dbGetQuery(solutions_db, "SELECT id_pre, description FROM preflop;")
options_tree <- dbGetQuery(solutions_db, "SELECT id_tree, description FROM tree;")

tcl_pre <- tclVar()
tcl_tree <- tclVar()

tt <- tktoplevel()

lbl_pre <- tklabel(tt, text="Preflop:")
lbl_tree <- tklabel(tt, text="Tree:")

cbb_pre <- tcltk::ttkcombobox(tt, values = options_preflop$description, textvariable = tcl_pre, width = 40)
cbb_tree <- tcltk::ttkcombobox(tt, values = options_tree$description, textvariable = tcl_tree, width = 40)

on_ok <- function() {
  tkdestroy(tt)  
}
btn_ok <- ttkbutton(tt, text = "OK", command = on_ok)

tkgrid(lbl_pre, cbb_pre, padx = 10, pady = 5)
tkgrid(lbl_tree, cbb_tree, padx = 10, pady = 5)
tkgrid(btn_ok, columnspan = 2, pady = 10)
tkwait.window(tt)

id_pre <- options_preflop %>%
  filter(description == tclvalue(tcl_pre)) %>%
  pull(id_pre)

id_tree <- options_tree %>%
  filter(description == tclvalue(tcl_tree)) %>%
  pull(id_tree)

# Preflop starting ranges:

range_ip <- dbGetQuery(solutions_db, glue("SELECT range_ip FROM preflop WHERE id_pre = {id_pre};")) %>%
  str_remove_all("\\{|\\}") %>%
  str_replace_all(",", " ")
range_oop <- dbGetQuery(solutions_db, glue("SELECT range_oop FROM preflop WHERE id_pre = {id_pre};")) %>%
  str_remove_all("\\{|\\}") %>%
  str_replace_all(",", " ")


con <- process$new(
  command = "C:\\PioSOLVER\\PioSOLVER-pro.exe",
  stdin = "|",
  stdout = "|",
  wd = "C:\\PioSOLVER"
)

con$read_output_lines(n = -1)

parse_node <- function(e) {
  # This function adds a T and an R where a turn or river would be in the node 
  # by checking whether there is a call or a check (c:c: or b[:digit:]+:c:)
  
  streets = str_locate_all(e, 'c:c:|b[:digit:]+:c:') %>% .[[1]] %>% .[,2]
  
  if(length(streets) == 0) {
    e
  } else if(length(streets) == 1) {
    str_c(
      str_sub(e,1,streets[1]),
      "T",
      str_sub(e,streets[1],-1)
    )
  } else if(length(streets) == 2) {
    str_c(
      str_sub(e,1,streets[1]),
      "T",
      str_sub(e,streets[1],streets[2]),
      "R",
      str_sub(e,streets[2],-1)
    )
  } else {
    "Oops, something went wrong!"
  }
}

lines <- dbGetQuery(solutions_db, glue("SELECT lines FROM tree WHERE id_tree = {id_tree};"))[,1] %>%
  str_split("\n") %>% 
  unlist()

# Getting the effective stack (last number in every line in the lines column)
stack_eff <- lines %>% str_split("\n") %>% unlist() %>% .[1] %>% str_split(" ") %>% unlist %>% tail(1) 

con$write_input(str_c(glue("set_threads 8")," \n"))
con$write_input("set_accuracy 0.300000 \n")
con$write_input(str_c(glue("set_range OOP {range_oop}")," \n"))
con$write_input(str_c(glue("set_range IP {range_ip}")," \n"))
con$write_input(str_c(glue("set_eff_stack {stack_eff}")," \n"))
con$write_input("set_isomorphism 1 1 \n")
con$write_input("set_pot 0 0 100 \n")
con$write_input("clear_lines \n")
con$write_input("set_board Ah As Ad \n")
for (i in lines) {
  con$write_input(str_c(i," \n"))
}
con$write_input("build_tree \n")
con$write_input("echo done \n")

is_done = con$read_output_lines(n = -1)
while ("done" %in% is_done == F) {
  is_done = con$read_output_lines(n = -1)
}

#Creating the nodes for the gametree using PioSolver's show_all_lines functionality, if they do not already exist

if(dbGetQuery(solutions_db, glue("SELECT COUNT(*) FROM lookup_nodes WHERE id_tree = {id_tree};")) %>% pull(count) < 1) {
  
  con$write_input("show_all_lines \n")
  if (con$poll_io(1000)["output"] == "ready") {
    all_lines <- con$read_output_lines(n = -1)
  }
  nodes <- sapply(all_lines, parse_node)
  has_position <- sapply(
    nodes, function(e) (e %>% str_split(":T:|:R:") %>% unlist() %>% tail(1) %>% str_count(":")+1) %% 2 == 0
  )
  
  dbSendQuery(
    solutions_db,
    str_c(
      "INSERT INTO lookup_nodes (id_tree, node_name, has_position) VALUES \n",
      sapply(
        1:length(nodes), function(e) str_c("(20, '", nodes[e], "', ", has_position[e], ")")
      ) %>% 
        str_c(collapse = ",\n"),
      ";")
  )
  rm(nodes)
}

boards <- dbGetQuery(
  solutions_db,
  "SELECT id_board, flop FROM boards WHERE turn IS NULL;"
) %>% 
  mutate(
    # board cards need to be split to work with PioSolver
    board = str_c(str_sub(flop,1,2)," ",str_sub(flop,3,4)," ",str_sub(flop,5,6))
  )

boards_children <- lapply(
  boards$flop, function(e)
    dbGetQuery(
      solutions_db,
      glue("SELECT id_board, flop, turn, river FROM boards WHERE flop = '{e}';")
    )
)

reverseStr <- function(str) {
  reversedStr <- ""
  while (nchar(str) > 0) {
    reversedStr <- paste0(reversedStr, substr(str, nchar(str), nchar(str)))
    str <- substr(str, 1, nchar(str) - 1)
  }
  return(reversedStr)
}

nodes <- dbGetQuery(
  solutions_db, 
  glue("SELECT id_node, node_name, has_position FROM lookup_nodes WHERE id_tree = {id_tree} ORDER BY id_node;") 
) %>% 
  mutate(
    owner = ifelse(
      has_position,
      "IP",
      "OOP"
    ),
    street = node_name %>% sapply(reverseStr) %>% str_extract("T|R") %>% replace_na("F")
  ) %>%
  select(-has_position)

# The loop below is used to scrape the strategies from PioSolver and then insert them into the database
for (i in 1:length(boards)) {
  con$write_input(
    str_c(
      "set_board ", 
      boards[i,"board"], 
      " \n"
    )
  )
  con$write_input("build_tree \n")
  con$write_input("go 7200 seconds \n")
  
  # Waiting for solution to finish
  while ("SOLVER: stopped (required accuracy reached)" %in% con$read_output_lines(n = -1) == F) {
    Sys.sleep(4)
  }
  
  e = boards_children[[i]]
  
  # converting nodes back to Pio format (r:0:c:c:T becomes r:0:c:c:Ah or whatever the turn is)
  nodes_parsed = lapply(1:nrow(e), function(f)
    if(is.na(e[f,"turn"])) {
      nodes %>% 
        bind_cols(id_board = rep(e[f,1], nrow(nodes))) %>%
        filter(street == "F")
    } else if(!is.na(e[f,"turn"]) && is.na(e[f,"river"])) {
      nodes %>% 
        bind_cols(id_board = rep(e[f,1], nrow(nodes))) %>%
        filter(street == "T") %>%
        mutate(
          node_name = node_name %>% str_replace("T", e[f,"turn"])
        )
    } else {
      nodes %>% 
        bind_cols(id_board = rep(e[f,1], nrow(nodes))) %>%
        filter(street == "R") %>%
        mutate(
          node_name = node_name %>% str_replace("T", e[f,"turn"]) %>% str_replace("R", e[f,"river"])
        )
    }
  ) %>% list_rbind
  
  length_nodes_parsed = nrow(nodes_parsed)
  
  for (k in 1:length_nodes_parsed) {
    
    con$write_input(str_c(glue("calc_ev OOP {nodes_parsed[k,2]}")," \n"))
    ev_OOP = con$read_output_lines(n = -1)
    while (is_empty(ev_OOP)) {
      ev_OOP = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    con$write_input(str_c(glue("calc_ev IP {nodes_parsed[k,2]}")," \n"))
    ev_IP = con$read_output_lines(n = -1)
    while (is_empty(ev_IP)) {
      ev_IP = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    con$write_input(str_c(glue("calc_eq_node OOP {nodes_parsed[k,2]}")," \n"))
    eq_OOP = con$read_output_lines(n = -1)
    while (is_empty(eq_OOP)) {
      eq_OOP = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    con$write_input(str_c(glue("calc_eq_node IP {nodes_parsed[k,2]}")," \n"))
    eq_IP = con$read_output_lines(n = -1)
    while (is_empty(eq_IP)) {
      eq_IP = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    con$write_input(str_c(glue("show_range {nodes_parsed[k,3]} {nodes_parsed[k,2]}")," \n"))
    range = con$read_output_lines(n = -1)
    while (is_empty(range)) {
      range = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    con$write_input(str_c(glue("calc_line_freq {nodes_parsed[k,2]}")," \n"))
    occurrence = con$read_output_lines(n = -1)
    while (is_empty(occurrence)) {
      occurrence = con$read_output_lines(n = -1)
    }
    con$write_input("echo done \n")
    is_done = con$read_output_lines(n = -1)
    while ("done" %in% is_done == F) {
      is_done = con$read_output_lines(n = -1)
    }
    
    dbSendQuery(
      solutions_db,
      str_c(
        "INSERT INTO simulations (id_board, id_node, id_pre, id_tag, ev_oop, ev_ip, eq_oop, eq_ip, strategy, occurrence) VALUES \n",
        str_c(
          glue("({nodes_parsed[k,5]}, "),
          glue("{nodes_parsed[k,1]}, "),
          glue("{id_pre}, "),
          glue("{id_tag}, "),
          str_c("ARRAY[", 
                out[1] %>% 
                  str_replace_all("-inf", "NULL") %>% 
                  str_replace_all("inf", "NULL") %>% 
                  str_replace_all("nan", "NULL") %>% 
                  str_replace_all(" (?=.)", ", "), 
                "]::real[], "),
          str_c("ARRAY[", 
                ev_IP[1] %>% 
                  str_replace_all("-inf", "NULL") %>% 
                  str_replace_all("inf", "NULL") %>% 
                  str_replace_all("nan", "NULL") %>% 
                  str_replace_all(" (?=.)", ", "), 
                "]::real[], "),
          str_c("ARRAY[", 
                eq_OOP[1] %>% 
                  str_replace_all("-inf", "NULL") %>% 
                  str_replace_all("inf", "NULL") %>% 
                  str_replace_all("nan", "NULL") %>% 
                  str_replace_all(" (?=.)", ", "), 
                "]::real[], "),
          str_c("ARRAY[", 
                eq_IP[1] %>% 
                  str_replace_all("-inf", "NULL") %>% 
                  str_replace_all("inf", "NULL") %>% 
                  str_replace_all("nan", "NULL") %>% 
                  str_replace_all(" (?=.)", ", "), 
                "]::real[], "),
          str_c("ARRAY[", 
                range %>% str_replace_all(" (?=.)", ", "), "], "),
          occurrence,
          glue(")")
        ) %>% str_replace_all(" ]", "]")
      )
    )
    
    print(glue("{k} out of {length_nodes_parsed}"))
  }
  
  print(glue("{i} out of {length(boards)} - {boards[i,3]} done"))
}

con$kill()
dbDisconnect(solutions_db)
