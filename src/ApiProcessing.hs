{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}



module ApiProcessing where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple 
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException(..), try)
import Text.ParserCombinators.ReadP (get)




data NamedAPIResource = NamedAPIResource 
  { resourceName :: String
  , url :: String
  } deriving (Show)

instance FromJSON NamedAPIResource where
    parseJSON (Object v) = do 
        resourceName <- v .: "name"
        url <- v .: "url"
        return (NamedAPIResource resourceName url)
    
    parseJSON invalid =
        prependFailure "parsing NamedAPIResource failed"
        (typeMismatch "Object" invalid)

data Stat = Stat
  { baseStat :: Int
  , statAPI :: NamedAPIResource 
  } deriving (Show)

instance FromJSON Stat where
  parseJSON (Object v) = do 
    baseStat <- v .: "base_stat"
    statAPI <- v .: "stat"
    return (Stat baseStat statAPI)

  parseJSON invalid =
    prependFailure "parsing stat failed"
      (typeMismatch "Object" invalid)

data Species = Species 
  { pokemonSpeciesId :: Int
  , isLegendary :: Bool
  , isMythical :: Bool
  , evolutionFrom :: Maybe NamedAPIResource
  , eggGroups :: [NamedAPIResource]
  , captureRate :: Int
  , speciesName :: String
  } deriving (Show)

instance FromJSON Species where
    parseJSON (Object v) = do 
        speciesId <- v .: "id"
        isLegendary <- v .: "is_legendary"
        isMythical <- v .: "is_mythical"
        evolutionFrom <- v .:? "evolves_from_species"
        eggGroups <- v .: "egg_groups"
        captureRate <- v .: "capture_rate"
        speciesName <- v .: "name"
        return (Species speciesId isLegendary isMythical evolutionFrom eggGroups captureRate speciesName)
    
    parseJSON invalid =
        prependFailure "parsing species failed"
        (typeMismatch "Object" invalid)

data PokemonLearnedBy = PokemonLearnedBy
    { isHidden :: Bool
    , slot :: Int
    , pokemonAPI :: NamedAPIResource
    } deriving (Show)

instance FromJSON PokemonLearnedBy where
    parseJSON (Object v) = do 
        isHidden <- v .: "is_hidden"
        slot <- v .: "slot"
        pokemonAPI <- v .: "pokemon"
        return (PokemonLearnedBy isHidden slot pokemonAPI)


    parseJSON invalid = 
        prependFailure "parsing ability failed"
        (typeMismatch "Object" invalid)

data ActualAbility = ActualAbility
    { abilityId :: Int
    ,  abilityName :: String
    , pokemonLearnedBy :: [PokemonLearnedBy]
    } deriving (Show)

instance FromJSON ActualAbility where
    parseJSON (Object v) = do 
        abilityId <- v .: "id"
        abilityName <- v .: "name"
        pokemonLearnedBy <- v .: "pokemon"
        return (ActualAbility abilityId abilityName pokemonLearnedBy)

    parseJSON invalid = 
        prependFailure "parsing ability failed"
        (typeMismatch "Object" invalid)

data Type = Type
  { typeId :: Int
  , typeName :: String
  , damageRelations :: TypeRelations
  } deriving (Show)

instance FromJSON Type where
    parseJSON (Object v) = do 
        typeId <- v .: "id"
        typeName <- v .: "name"
        damageRelations <- v .: "damage_relations"
        return (Type typeId typeName damageRelations)
    
    parseJSON invalid =
        prependFailure "parsing type failed"
        (typeMismatch "Object" invalid)

data TypeRelations = TypeRelations
  { doubleDamageTo :: [NamedAPIResource]
  , halfDamageTo :: [NamedAPIResource]
  , noDamageTo :: [NamedAPIResource]
  } deriving (Show)


instance FromJSON TypeRelations where
    parseJSON (Object v) = do 
        doubleDamageTo <- v .: "double_damage_to"
        halfDamageTo <- v .: "half_damage_to"
        noDamageTo <- v .: "no_damage_to"
        return (TypeRelations doubleDamageTo halfDamageTo noDamageTo)
    
    parseJSON invalid =
        prependFailure "parsing type relations failed"
        (typeMismatch "Object" invalid)

data TypeApi = TypeApi 
  { typeSlot :: Int
  , typeApiResource :: NamedAPIResource
  } deriving (Show)

instance FromJSON TypeApi where
    parseJSON (Object v) = do 
        typeSlot <- v .: "slot"
        typeApiResource <- v .: "type"
        return (TypeApi typeSlot typeApiResource)
    
    parseJSON invalid =
        prependFailure "parsing type failed"
        (typeMismatch "Object" invalid)

data Pokemon = Pokemon 
  { id     :: Int
  , pokemonName   :: String
  , species :: NamedAPIResource
  , stats :: [Stat]
  , typeList :: [TypeApi]
  } deriving (Show)

instance FromJSON Pokemon where
  parseJSON (Object v) = do 
    id <- v .: "id"
    pokemonName <- v .: "name"
    species <- v .: "species"
    stats <- v .: "stats"
    typeList <- v .: "types"
    return (Pokemon id pokemonName species stats typeList)

  parseJSON invalid =
    prependFailure "parsing pokemon failed"
      (typeMismatch "Object" invalid)


data ActualMove = ActualMove
    { moveName :: String
    , moveID:: Int
    , accuracy :: Maybe Int
    , power :: Maybe Int
    , pp :: Maybe Int
    , typeAPI :: NamedAPIResource
    , learnedBy :: [NamedAPIResource]
    } deriving (Show)

instance FromJSON ActualMove where
    parseJSON (Object v) = do 
        moveName <- v .: "name"
        moveID <- v .: "id"
        accuracy <- v .: "accuracy"
        power <- v .: "power"
        pp <- v .: "pp"
        typeAPI <- v .: "type"
        learnedBy <- v .: "learned_by_pokemon"
        return (ActualMove moveName moveID accuracy power pp typeAPI learnedBy)

    parseJSON invalid = 
        prependFailure "parsing move failed"
        (typeMismatch "Object" invalid)


getIDFromURL :: String -> Int
getIDFromURL = 
    let urlSplit = wordsWhen (== '/')
    in read . last  . urlSplit
    where 
        wordsWhen :: (Char -> Bool) -> String -> [String]
        wordsWhen p s =  case dropWhile p s of
            "" -> []
            s' -> w : wordsWhen p s''
                where (w, s'') = break p s'

getPikachu :: IO Pokemon
getPikachu = do 
  response <- httpJSON "https://pokeapi.co/api/v2/pokemon/pikachu" :: IO (Response Pokemon)
  case getResponseBody response of
    Pokemon{..}  ->
      print ("Pokemon's name is " <> pokemonName <> " and its id is " <> show id) >> return Pokemon{..}


-- getPokemon :: [Request] -> IO [Pokemon]
-- getPokemon = mapM getPokemon'
--   where 
--     getPokemon' :: Request -> IO Pokemon
--     getPokemon' x = do 
--       response <- httpJSON x :: IO (Response Pokemon)
--       case getResponseBody response of
--         Pokemon{..}  ->
--           print ("Pokemon's name is " <> pokemonName <> " and its id is " <> show id <> " " <> show stats) >> threadDelay 250000 >> return Pokemon{..}
      



getResultOrErrors :: forall a. (FromJSON a) => [Request] -> IO ([a], [SomeException])
getResultOrErrors = fmap foldResult . mapM getResult'
  where 
    getResult' :: Request -> IO (Either SomeException a)
    getResult' x = do
        result <- try (httpJSON x :: IO (Response a))
        case result of
            Right response -> do
                case getResponseBody response of
                    someResponse  -> return (Right someResponse)
            Left (ex :: SomeException) -> return (Left ex)

    foldResult :: [Either SomeException a] -> ([a], [SomeException])
    foldResult = foldr folder ([], [])

    folder :: Either SomeException a -> ([a], [SomeException]) -> ([a], [SomeException])
    folder (Right someResponse) (successes, errors) = (someResponse : successes, errors)
    folder (Left ex) (successes, errors) = (successes, ex : errors)

getPokemon :: [Request] -> IO ([Pokemon], [SomeException])
getPokemon = getResultOrErrors



-- getPokemon :: [Request] -> IO ([Pokemon], [SomeException])
-- getPokemon = fmap foldResult . mapM getPokemon'
--   where 
--     getPokemon' :: Request -> IO (Either SomeException Pokemon)
--     getPokemon' x = do
--         result <- try (httpJSON x :: IO (Response Pokemon))
--         case result of
--             Right response -> do
--                 case getResponseBody response of
--                     Pokemon{..}  -> return (Right Pokemon{..})
--             Left (ex :: SomeException) -> return (Left ex)

--     foldResult :: [Either SomeException Pokemon] -> ([Pokemon], [SomeException])
--     foldResult = foldr folder ([], [])

--     folder :: Either SomeException Pokemon -> ([Pokemon], [SomeException]) -> ([Pokemon], [SomeException])
--     folder (Right pokemon) (successes, errors) = (pokemon : successes, errors)
--     folder (Left ex) (successes, errors) = (successes, ex : errors)


getPokemonSpecies :: [Request] -> IO ([Species], [SomeException])
getPokemonSpecies = getResultOrErrors

-- getPokemonSpecies :: [Request] -> IO ([Species], [SomeException])
-- getPokemonSpecies = fmap foldResult . mapM getPokemonSpecies'
--   where 
--     getPokemonSpecies' :: Request -> IO (Either SomeException Species)
--     getPokemonSpecies' x = do
--         result <- try (httpJSON x :: IO (Response Species))
--         case result of
--             Right response -> do
--                 case getResponseBody response of
--                     species  -> return (Right species)
--             Left (ex :: SomeException) -> return (Left ex)

--     foldResult :: [Either SomeException Species] -> ([Species], [SomeException])
--     foldResult = foldr folder ([], [])

--     folder :: Either SomeException Species -> ([Species], [SomeException]) -> ([Species], [SomeException])
--     folder (Right species) (successes, errors) = (species : successes, errors)
--     folder (Left ex) (successes, errors) = (successes, ex : errors)


-- getPokemonMoves :: [Request] -> IO ([ActualMove], [SomeException])
-- getPokemonMoves = fmap foldResult . mapM getPokemonMoves'
--   where 
--     getPokemonMoves' :: Request -> IO (Either SomeException ActualMove)
--     getPokemonMoves' x = do
--         result <- try (httpJSON x :: IO (Response ActualMove))
--         case result of
--             Right response -> do
--                 case getResponseBody response of
--                     move  -> return (Right move)
--             Left (ex :: SomeException) -> return (Left ex)

--     foldResult :: [Either SomeException ActualMove] -> ([ActualMove], [SomeException])
--     foldResult = foldr folder ([], [])

--     folder :: Either SomeException ActualMove -> ([ActualMove], [SomeException]) -> ([ActualMove], [SomeException])
--     folder (Right move) (successes, errors) = (move : successes, errors)
--     folder (Left ex) (successes, errors) = (successes, ex : errors)


getPokemonMoves :: [Request] -> IO ([ActualMove], [SomeException])
getPokemonMoves = getResultOrErrors

getTypes :: [Request] -> IO ([Type], [SomeException])
getTypes = getResultOrErrors


getAbilities :: [Request] -> IO ([ActualAbility], [SomeException])
getAbilities = getResultOrErrors

printStat :: Pokemon -> IO ()
printStat = print . resourceName . statAPI . head . stats 


data StatBreakdown = StatBreakdown
  { hp :: Int
    , attack :: Int
    , defense :: Int
    , specialAttack :: Int
    , specialDefense :: Int
    , speed :: Int
    } deriving (Show)


statsToStatBreakdown :: [Stat] -> StatBreakdown
statsToStatBreakdown xs = 
    let hp = findStat "hp" xs
        attack = findStat "attack" xs
        defense = findStat "defense" xs
        specialAttack = findStat "special-attack" xs
        specialDefense = findStat "special-defense" xs
        speed = findStat "speed" xs
    in StatBreakdown {
        hp = hp
        , attack = attack
        , defense = defense
        , specialAttack = specialAttack
        , specialDefense = specialDefense
        , speed = speed
    }
    where 
        findStat :: String -> [Stat] -> Int
        findStat x = baseStat . head . filter (\y -> resourceName (statAPI y) == x)

