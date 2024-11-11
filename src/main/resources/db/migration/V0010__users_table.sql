CREATE TABLE users (
    id uuid DEFAULT gen_random_uuid(),
    elo int NOT NULL,
    wins int NOT NULL,
    loses int NOT NULL,
    allGames int NOT NULL,
    nickname text NOT NULL,
    role text NOT NULL,
    email text,
    hashedPassword text NOT NULL
);

ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (id);