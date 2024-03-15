{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib
import TableManipulation
import Database.PostgreSQL.Simple as PGS 
import ApiProcessing
import Opaleye.Internal.PackMap (run)
import Queries

connInfo :: PGS.ConnectInfo
connInfo = PGS.ConnectInfo
  { connectPort = 5432
  , connectHost = "localhost"
  , connectDatabase = "sarah"
  , connectUser = "sarah"
  , connectPassword = ""
  }

main :: IO ()
main = do
   conn <- PGS.connect connInfo
--   (abilities, _ ) <- getAbilities $ getNAbilities 1 300
--   (types, _) <- getTypes $ getNTypes 1 18
--   (pokemon, _) <- getPokemon . getNPokemon $ 1025
--   (species, _) <- getPokemonSpecies $ getNSpecies 1 1025
--   (moves, _) <- getPokemonMoves $ getNMoves 1 925
--   abilitiesAndRelationsInserted <- insertAbilityAndAbilityPokemonRelation abilities conn
--   movesAndRelationsInserted <- insertActualMoveAndMovePokemonRelation moves conn
--   typesAndRelationsInserted <- insertTypeAndVariousTypeRelations types conn
--   numInserted <- insertPokemonList pokemon conn
--   speciesAndEggGroupInserted <- insertSpeciesAndEggGroup species conn 
--   print "Moves and Relations Inserted " <> print movesAndRelationsInserted 
--   print "Species and Egg Group Inserted " <> print speciesAndEggGroupInserted
--   print "Abilities and Relations Inserted " <> print abilitiesAndRelationsInserted
--   print "Types and Relations Inserted " <> print typesAndRelationsInserted
   pokemon <- runNameAndIdSelect conn nameAndIdSelect
   printNameAndId pokemon
   moveAndPokemon <- runPokemonMoveJoin conn pokemonMoveJoin2
   mapM_ print moveAndPokemon
   PGS.close conn
