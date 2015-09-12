-- Solitaire Cipher
-- http://rubyquiz.com/quiz1.html
--
-- Usage:
--
--  runhaskell SolitaireCipher.hs [message_to_encrypt]

module SolitaireCipher where

import Data.List
import Data.Char (toUpper, isUpper, ord)
import System.Environment (getArgs)

data Card = JokerA
    | JokerB
    | Club Int
    | Diamond Int
    | Heart Int
    | Spade Int
    deriving (Show, Eq)
type Deck = [Card]

value :: Card -> Int
value JokerA = 53
value JokerB = 53
value (Club n) = n
value (Diamond n) = n + 13
value (Heart n) = n + 26
value (Spade n) = n + 39

alphabet :: [Char]
alphabet = ['A'..'Z']

 -- When the cards must represent a letter Clubs and Diamonds values are taken
 -- to be the number of the letter (1 to 26), as are Hearts and Spades after
 -- subtracting 26 from their value (27 to 52 drops to 1 to 26).
letter :: Card -> String
letter JokerA = ""
letter JokerB = ""
letter (Club n) = [alphabet !! (n-1)]
letter (Diamond n) = [alphabet !! (n+12)]
letter (Heart n) = [alphabet !! (n-1)]
letter (Spade n) = [alphabet !! (n+12)]

bottomValue :: Deck -> Int
bottomValue = value . last

topValue :: Deck -> Int
topValue = value . head

buildUnkeyedDeck :: Deck
buildUnkeyedDeck = ([Club, Diamond, Heart, Spade] <*> [1..13]) ++ [JokerA, JokerB]

-- Move a Card down one position in a Deck.
-- If the Card is on the bottom of the deck, move it to the top (the deck is
-- circular).
moveDown :: Card -> Deck -> Deck
moveDown c d = case elemIndex c d of
    (Just i) -> if i == (length d - 1)
        then head d:c:drop 1 (delete c d)
        else insertAt c (i+1) $ delete c d
    Nothing -> error $ (show c) ++ " should be there"

-- Move a Card down two positions in a Deck.
moveDown2 :: Card -> Deck -> Deck
moveDown2 c = moveDown c . moveDown c

-- Perform a triple cut around two cards:
-- All cards above the top card move to below the bottom card and vice versa.
-- The cut cards and the cards between them do not move.
tripleCut :: Card -> Card -> Deck -> Deck
tripleCut c1 c2 d = bottom ++ middle ++ top
    where
        top = takeWhile (/= topCard) d
        middle = reverse $ dropWhile (/= bottomCard) $ reverse $ dropWhile (/= topCard) d
        bottom = reverse $ takeWhile (/= bottomCard) $ reverse d
        (topCard, bottomCard) = byPosition c1 c2 d

byPosition :: Card -> Card -> Deck -> (Card, Card)
byPosition c1 c2 d = case (mp1, mp2) of
    (Just p1, Just p2) -> if p1 > p2 then (c2, c1) else (c1, c2)
    _ -> error $ "couldn't find " ++ (show c1) ++ " and " ++ (show c2)
    where
        mp1 = elemIndex c1 d
        mp2 = elemIndex c2 d

-- Perform a count cut around the bottom card:
-- Cut the bottom card's value in cards off the top of the deck and reinsert
-- them just above the bottom card.
-- countCut :: Deck -> Deck
countCut d = (init $ drop (bottomValue d) d) ++ cards ++ [bottomCard]
    where
        bottomCard = last d
        cards = take (bottomValue d) d

--  Find the output letter.
--  Convert the top card to its value and count down that many cards from the
--  top of the deck, with the top card itself being card number one. Look at the
--  card immediately after your count and convert it to a letter.
--  This is the next letter in the keystream.
--  If the output card is a joker, no letter is generated.
outputLetter :: Deck -> String
outputLetter d = letter $ head $ drop (topValue d) d

-- Insert an element at a position in a list.
insertAt :: a -> Int -> [a] -> [a]
insertAt x n xs = ys ++ (x:zs)
    where (ys, zs) = splitAt n xs

-- Generate the next letter in the keystream for a Deck.
generateLetter :: Deck -> String
generateLetter = outputLetter . shuffleDeck

-- Generate the new version of a deck suitable for generating the next letter.
shuffleDeck :: Deck -> Deck
shuffleDeck d = countCut
    $ tripleCut JokerA JokerB
    $ moveDown2 JokerB
    $ moveDown JokerA d

-- Generate `length s` keystream letters based on the given deck
generateKeystream :: String -> Deck -> String
generateKeystream s d = take (length s) $ concatMap generateLetter $ iterate shuffleDeck d

-- Discard any non A to Z characters, and uppercase all remaining letters
clean :: String -> String
clean = filter isUpper . map toUpper

-- If message length is not a multiple of 5, pad with X's
pad :: String -> String
pad s
    | lenMod5 == 0 = s
    | otherwise = s ++ replicate charsRequired 'X'
    where
        charsRequired = 5 - lenMod5
        lenMod5 = length s `mod` 5

alphabetNumber :: Char -> Int
alphabetNumber c = ord c - 64

numberAlphabet :: Int -> Char
numberAlphabet n = alphabet !! (n-1)

encrypt :: String -> Deck -> String
encrypt s d = zipWith encryptor messageNumbers (keystreamNumbers cleaned d)
    where
        encryptor m k = numberAlphabet $ addMod26 m k
        addMod26 a b = (a + b) `mod` 26
        messageNumbers = map alphabetNumber cleaned
        cleaned = pad $ clean s

keystreamNumbers :: String -> Deck -> [Int]
keystreamNumbers s d = map alphabetNumber $ generateKeystream s d

decrypt :: String -> Deck -> String
decrypt s d = zipWith decryptor messageNumbers (keystreamNumbers cleaned d)
    where
        decryptor m k = numberAlphabet $ subtractMod26 m k
        subtractMod26 m k = if m <= k then (m+26) - k else m - k
        messageNumbers = map alphabetNumber cleaned
        cleaned = pad $ clean s

main = do
    args <- getArgs
    let message = if null args then "Code in Ruby, live longer!" else args !! 0
    let d = buildUnkeyedDeck
    let encrypted = encrypt message d
    let decrypted = decrypt encrypted d
    putStrLn $ "Message:   " ++ message
    putStrLn $ "Encrypted: " ++ encrypted
    putStrLn $ "Decrypted: " ++ decrypted
