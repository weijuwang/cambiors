enum Card {
    Number(u32),
    Jack,
    Queen,
    BlackKing,
    RedKing,
    Joker
}

impl Card {
    fn value(&self) -> i32 {
        match self {
            Card::Number(value) => *value as i32,
            Card::Jack | Card::Queen | Card::BlackKing => 10,
            Card::RedKing => -1,
            Card::Joker => 0,
        }
    }
}