{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
учитывая, что всего в колоде 52 карты.
-}
 
data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

data Card = Card Value Suit deriving (Show)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card v1 s1) (Card v2 s2) = s1 == s2

{-
3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
(масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 s1) `beats` (Card v2 s2) = compare v1 v2

{-
4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
с учетом правил игры «Пьяница» (один раунд игры): 
* из вершин списков берутся две карты и добавляются в конец того списка, карта из
которого старше оставшейся;
* если первые взятые карты совпадают по достоинству, то из списков берутся и
сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (cs1,[]) = (cs1,[])
game_round ([],cs2) = ([],cs2)
game_round (c1:cs1, c2:cs2) 
    | c1 `beats` c2 == GT = (cs1 ++ [c1] ++ [c2], cs2)
    | c1 `beats` c2 == LT = (cs1, cs2 ++ [c2] ++ [c1])
    | otherwise = game_round (cs1 ++ [c1], cs2 ++ [c2]) 

{-	
5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second deriving (Show)

game :: ([Card], [Card]) -> (Winner, Int)
game (cs1, cs2) = game' (cs1, cs2) 0
    where
	game' (_, []) i = (First, i)
	game' ([], _) i = (Second, i)
	game' (cs1, cs2) i = game' (game_round(cs1, cs2)) (i+1)

{-
6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
изначально должно быть не менее 10 карт).
-}

res1 = 
    game ([Card King Diamonds, Card Six Hearts, Card Queen Clubs, Card Queen Diamonds, Card Ace Hearts, Card Six Hearts, Card Ace Diamonds, Card Five Diamonds, Card Seven Spades, Card King Clubs], 
    [Card Eight Diamonds, Card Four Diamonds, Card Five Hearts, Card Two Hearts, Card Jack Spades, Card Jack Clubs, Card Four Spades, Card Eight Clubs, Card Seven Clubs, Card Four Hearts])
-- (First,24)
res2 =
    game ([Card Five Diamonds, Card King Clubs, Card Six Clubs, Card Ten Diamonds, Card Nine Hearts, Card Ten Spades, Card Six Spades, Card Queen Clubs, Card Nine Clubs, Card Three Spades],
    [Card Seven Spades, Card Queen Diamonds, Card Five Hearts, Card Three Diamonds, Card Nine Spades, Card Two Diamonds, Card Ace Hearts, Card King Diamonds, Card Queen Hearts, Card Five Spades])	
-- (Second, 98)
res3 =
    game ([Card Nine Clubs, Card Queen Clubs, Card King Spades, Card Ten Diamonds, Card Six Diamonds, Card Eight Diamonds, Card Ace Diamonds, Card Five Diamonds, Card Two Spades, Card Four Hearts],
	[Card Seven Spades, Card Queen Spades, Card Four Diamonds, Card Six Hearts, Card Nine Hearts, Card Eight Spades, Card Two Diamonds, Card Two Clubs, Card Five Clubs, Card Jack Spades])
-- (First, 44)

res4 =
    game ([Card Seven Spades, Card Queen Spades, Card Four Diamonds, Card Six Hearts, Card Six Diamonds, Card Eight Spades, Card Two Diamonds, Card Two Clubs, Card Two Spades, Card Four Hearts],
	[Card Nine Clubs, Card Ace Clubs, Card King Spades, Card Ten Diamonds, Card Nine Hearts, Card Jack Diamonds, Card Ace Diamonds, Card Five Diamonds, Card Five Clubs, Card Jack Spades])
-- (Second 10)

res5 =
    game ([Card Ace Spades, Card Eight Hearts, Card Five Spades, Card Jack Spades, Card Three Diamonds, Card Five Clubs, Card Four Spades, Card Two Spades, Card King Spades, Card Nine Hearts],
	[Card Two Clubs, Card Five Diamonds, Card Ten Clubs, Card Queen Clubs, Card Six Hearts, Card Queen Spades, Card Ten Spades, Card Jack Diamonds, Card Four Diamonds, Card Nine Clubs])
-- (First, 58)