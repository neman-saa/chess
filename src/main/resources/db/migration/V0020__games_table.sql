CREATE TABLE games(
    id uuid DEFAULT gen_random_uuid(),
    winner text,
    player1 text NOT NULL,
    player2 text NOT NULL
);

ALTER TABLE games
ADD CONSTRAINT pk_games PRIMARY KEY (id);