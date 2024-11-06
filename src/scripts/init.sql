CREATE TYPE Game_Type AS ENUM ('BOARD', 'TURN_BASED');

CREATE TABLE Game (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    type Game_Type NOT NULL,
    name TEXT NOT NULL
);

CREATE TABLE GameSession (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    game_id UUID NOT NULL, 
    turn_id UUID NOT NULL,                          
    is_first BOOLEAN NOT NULL,
    is_win BOOLEAN NOT NULL,
    FOREIGN KEY (game_id) REFERENCES Game(id)      
);