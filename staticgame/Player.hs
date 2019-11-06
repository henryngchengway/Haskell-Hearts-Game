-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types


playCard :: PlayFunc
playCard = error "You need to implement the playCard function."

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types

-- Henry 
-- Nicholas: You need to read Types.hs and Cards.hs at minimum to start doing things.
-- Play.hs contains the sequence of play for every round, but it's not necessary to fully understand it.
-- You only need to know how the game works, and you just need to play a single card, everything else is automated.
-- The problem is choosing the right card to play. This is what you'll need to solve.

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand, "")
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick _ = (renege (suit $ fst $ last trick) hand, "")
-- the card return here should be stored in memory (created by self, with an array)
-- the card return from lead or renege should convert the return card to string type then stored it
-- the fourth parameter should not be _ and should change to be our memory

-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- Will throw an error if no cards of that suit is available. This renege is incomplete.
renege :: Suit -> [Card] -> Card
renege leader hand 
    | length array_followLeadSuit > 0 && length hand == 13 = if find (\x -> suit x == Spade && rank x == Queen) array_followLeadSuit == Nothing
        then head array_followLeadSuit
        else fromJust $ find (\x -> suit x /= Spade && rank x /= Queen) array_followLeadSuit 
        -- given that if there contains same suit with leader & is the first round of game, if SQ does not exists,
        -- then head of any card with same suit as leader
        -- else output any card with same suit that is not SQ
    | length array_followLeadSuit > 0 && length hand < 13 = if find (\x -> suit x == Spade && rank x == Queen) array_followLeadSuit /= Nothing
        then fromJust $ find (\x -> suit x == Spade && rank x == Queen) hand
        else head array_followLeadSuit
        
        -- given that if there contains same suit with leader & is not the first round of game, if SQ does not exists,
        -- then head of any card with same suit as leader
        -- else output SQ
    | length array_Heartcards == length hand = head array_Heartcards -- left only with hearts suited cards then play
-- | length array_pointcards > 0  = head $ filter(\x -> suit x == Spade && rank x == Queen) array_pointcards

    -- | length array_pointcards_renege == length hand = if find (\x -> suit x == Spade && rank x == Queen) array_pointcards_renege == Nothing 
    --     then head array_pointcards_renege 
    --     else fromJust $ find (\x -> suit x == Spade && rank x == Queen) array_pointcards_renege
    | otherwise = head array_noHeartcards
    where array_followLeadSuit = filter (\x -> suit x == leader) hand
          array_noHeartcards = filter (\x -> suit x /= Heart) hand ++ filter (\x -> suit x /= Spade && rank x /= Queen) hand
          array_Heartcards = filter (\x -> suit x == Heart) hand 
         -- array_pointcards_renege = filter (\x -> suit x == Heart) hand ++ filter (\x -> suit x == Spade && rank x == Queen) hand 




-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> Card
    select Nothing 
        | length array_nopointcards_lead > 0 = head array_nopointcards_lead
        | length array_Heartcards_lead == length hand = head array_Heartcards_lead -- player can start with the point cards given that player only left with points cards
        | length array_pointcards_lead == length hand = fromJust $ find (\x -> suit x == Spade && rank x == Queen) array_pointcards_lead
        | otherwise = fromJust $ find(\x -> suit x /= Spade && rank x /= Queen) hand
        where
              array_nopointcards_lead = filter (\x -> cardPoints x == 0) hand
              array_Heartcards_lead = filter (\x -> suit x == Heart) hand
              array_pointcards_lead = filter (\x -> cardPoints /= 0) hand 
    select card = fromJust card


-- FilterHearts :: [Card] -> Bool

-- FilterHearts previousPlayedCard 
--     | length HeartArray > 0 = True
--     | otherwise = False
--     where HeartArray = filter (\x -> suit x == Heart) 
    
-- loop through the memory to check if hearts exist in the memory. If it does not exist, check if the player is leading or not
-- if the memory contains hearts, then allow the user to output hearts if and only if it the player only have card of hearts left
    
-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s 

-- | Given a card, select its rank
rank :: Card -> Rank
rank (Card _ r) = r

-- | Given a list of cards, return the highest rank card
-- highestRank :: [Card] -> Card
-- highestRank deck_card = head $ filter (\x -> rank x)

-- | Given the card that the player plays, we store the card into an array of cards
-- Memory :: (Card,String) -> [String]
-- Memory = undefined 

-- | Given a card, convert the card to a string

-- maybeListToString :: [(Card,PlayerId)] -> String
-- maybeListToString [] = ""
-- maybeListToString [(a,b)] = cardToString a 

-- dsfds dasfdsadsfdsfadsfdsdsfdsf
-- -- | Given string from the memory, convert it to a card
-- stringToCard :: String -> Card
-- stringToCard string = read string


-- addToMemory :: Maybe([(Card,PlayerId)],String) -> String
-- addToMemory Nothing = ""
-- addToMemory (Just (a b)) = maybeListToString a ++ b

-- -- | convert the card to list
-- cardToString :: Card -> String
-- cardToString card = show card
 -- where memory = [] ++ [show card]


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

