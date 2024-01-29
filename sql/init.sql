CREATE DATABASE chess;
\c chess;

CREATE TABLE games(
    id uuid DEFAULT gen_random_uuid(),
    winner text,
    player1 text NOT NULL,
    player2 text NOT NULL
);

ALTER TABLE games
ADD CONSTRAINT pk_games PRIMARY KEY (id);

CREATE TABLE users (
    id uuid NOT NULL,
    elo int NOT NULL,
    wins int NOT NULL,
    loses int NOT NULL,
    allGames int NOT NULL,
    nickname text NOT NULL,
    role text NOT NULL,
    email text,
    hashedPassword text NOT NULL,
);

ALTER TABLE users
ADD CONSTRAINT users PRIMARY KEY (id);