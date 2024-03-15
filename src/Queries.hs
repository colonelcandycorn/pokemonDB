{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Queries where 

import           Prelude hiding (sum)

import           Opaleye (Field, FieldNullable, matchNullable, isNull,
                         Table, table, tableField, selectTable,
                         Select, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, sqlString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runSelect,
                         showSql, where_, Unpackspec,
                         SqlInt4, SqlInt8, SqlText, SqlDate, SqlFloat8, SqlBool)

import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)

import qualified Database.PostgreSQL.Simple as PGS
import MyLib
import GenericData
import Data.List (group)

-- Pokemon Table

pokemonSelect :: Select MyLib.PokemonSqlField
pokemonSelect = selectTable MyLib.pokemonTable

runPokemonSelect :: PGS.Connection -> Select PokemonSqlField -> IO [PokemonSql]
runPokemonSelect = runSelect

-- Selecting after getting the PokemonSql 

projectPokemonNameAndId :: PokemonSql -> (String, Int)
projectPokemonNameAndId = (,) <$> sqlPokemonName <*> sqlId

-- Versus Selecting Pokemon Name And Id directly from the table with Restriction

nameAndIdSelect :: Select (Field SqlText, Field SqlInt4)
nameAndIdSelect = do
    pokemon <- selectTable pokemonTable
    where_ (sqlId pokemon .<= 10)
    return (sqlPokemonName pokemon, sqlId pokemon)

runNameAndIdSelect :: PGS.Connection -> Select (Field SqlText, Field SqlInt4) -> IO [(String, Int)]
runNameAndIdSelect = runSelect

-- Print Strings for Results
printNameAndId :: [(String, Int)] -> IO ()
printNameAndId = mapM_ (\(name, id) -> putStrLn $ name ++ " " ++ show id)

-- Move Table
moveSelect :: Select MyLib.ActualMoveSqlField
moveSelect = selectTable MyLib.actualMoveTable

moveRelationSelect :: Select MyLib.MovePokemonRelationSqlField
moveRelationSelect = selectTable MyLib.movePokemonRelationTable

-- Inner Join Pokemon And Move Where Pokemon Id is 1

pokemonMoveJoin :: Select (Field SqlText, Field SqlText)
pokemonMoveJoin = do
    pokemon <- selectTable pokemonTable
    moveRelation <- selectTable movePokemonRelationTable
    move <- selectTable actualMoveTable
    where_ (sqlId pokemon .== pokemonWhoLearns moveRelation
        .&& moveId moveRelation .== sqlMoveId move
        .&& sqlId pokemon .== 1)
    return (sqlPokemonName pokemon, sqlMoveName move)


innerJoinManyToMany :: t1 -> t2 -> t3 ->
                      (t1 -> Field a1) ->
                      (t2 -> Field a1) ->
                      (t2 -> Field a2) ->
                      (t3 -> Field a2) ->
                      Select ()
innerJoinManyToMany a b c d e f g = do
    where_ ( d a .== e b .&& f b .== g c)


pokemonMoveJoin2 :: Select (Field SqlText, Field SqlText)
pokemonMoveJoin2 = do
    pokemon <- selectTable pokemonTable
    moveRelation <- selectTable movePokemonRelationTable
    move <- selectTable actualMoveTable
    where_ (sqlId pokemon .== 1)
    innerJoinManyToMany pokemon moveRelation move sqlId pokemonWhoLearns moveId sqlMoveId
    return (sqlPokemonName pokemon, sqlMoveName move)

runPokemonMoveJoin :: PGS.Connection -> Select (Field SqlText, Field SqlText) -> IO [(String, String)]
runPokemonMoveJoin = runSelect


pokemonMoveJoin3 :: Select (Field SqlText, Field SqlText)
pokemonMoveJoin3 = do
    pokemon <- selectTable pokemonTable
    moveRelation <- selectTable movePokemonRelationTable
    move <- selectTable actualMoveTable
    where_ (sqlId pokemon .== 1)
    innerJoinManyToMany pokemon moveRelation move sqlId pokemonWhoLearns moveId sqlMoveId
    return (sqlPokemonName pokemon, sqlMoveName move)


