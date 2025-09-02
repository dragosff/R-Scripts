CREATE TABLE boards (
    id_board SERIAL PRIMARY KEY,
    flop VARCHAR(6),
    turn VARCHAR(2),
    river VARCHAR(2),
    iso_flop SMALLINT,
    iso_turn SMALLINT,
    iso_river SMALLINT,
    combo_made_hand SMALLINT[],
    combo_draw SMALLINT[]
);

CREATE TABLE tree (
    id_tree SMALLSERIAL PRIMARY KEY,
    description TEXT,
    lines TEXT,
    spr REAL,
    root TEXT
);

CREATE TABLE lookup_nodes (
    id_node SERIAL PRIMARY KEY,
    id_tree SMALLINT REFERENCES tree(id_tree),
    node_name TEXT,
    has_position BOOL,
    root_node_id INT,
    parent_node_id INT
);

CREATE TABLE preflop (
    id_pre SMALLSERIAL PRIMARY KEY,
    description TEXT,
    range_ip REAL[],
    range_oop REAL[]
);

CREATE TABLE tags (
    id_tag SMALLSERIAL PRIMARY KEY,
    description TEXT
);

CREATE TABLE simulations (
    id_board INT REFERENCES boards(id_board),
    id_node INT REFERENCES lookup_nodes(id_node),
    id_pre SMALLINT REFERENCES preflop(id_pre),
    id_tag SMALLINT REFERENCES tags(id_tag),
    id_game SMALLINT DEFAULT 1,
    ev_oop REAL[],
    ev_ip REAL[],
    eq_oop REAL[],
    eq_ip REAL[],
    strategy REAL[],
    occurrence REAL,
    PRIMARY KEY(id_board, id_node, id_pre, id_tag, id_game)
);

CREATE TABLE lookup_combo (
    id_combo SMALLSERIAL PRIMARY KEY,
    id_game SMALLINT,
    combo TEXT[],
    combo_pre TEXT[],
    combo_nummeric SMALLINT[]
);

CREATE TABLE lookup_hand_class (
    id_hand_class SMALLSERIAL PRIMARY KEY,
    hand_class TEXT
);

CREATE TABLE lookup_hand_draw (
    id_hand_draw SMALLSERIAL PRIMARY KEY,
    hand_draw TEXT
);