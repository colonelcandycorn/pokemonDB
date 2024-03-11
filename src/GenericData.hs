module GenericData  where
import ApiProcessing (ActualAbility)


data PokemonSql' a b c d e f g h i j k = PokemonSql' 
  { sqlId :: a
  , sqlPokemonName   :: b
  , sqlSpecies :: c
  , sqlHp :: d
  , sqlAttack :: e
  , sqlDefense :: f
  , sqlSpecialAttack :: g
  , sqlSpecialDefense :: h
  , sqlSpeed :: i
  , sqlFirstType :: j 
  , sqlSecondType :: k
  } deriving (Show)

type PokemonSql = PokemonSql' Int String Int Int Int Int Int Int Int Int (Maybe Int)


data EggGroupSql' a b = EggGroupSql'
    { pokemonId :: a
    , eggGroupName :: b
    } deriving (Show)

type EggGroupSql = EggGroupSql' Int String

data PokemonSpeciesSql' a b c d e f = PokemonSpeciesSql'
    { sqlSpeciesId :: a
    , sqlIsLegendary :: b
    , sqlIsMythical :: c
    , sqlEvolutionFrom :: d
    , sqlCaptureRate :: e
    , sqlSpeciesName :: f
    } deriving (Show)

type PokemonSpeciesSql = PokemonSpeciesSql' Int Bool Bool (Maybe Int) Int String


data ActualMoveSql' a b c d e f = ActualMoveSql'
    { sqlMoveId :: a
    , sqlMoveName :: b
    , sqlMoveType :: c
    , sqlMovePower :: d
    , sqlMovePP :: e
    , sqlMoveAccuracy :: f
    } deriving (Show)

type ActualMoveSql = ActualMoveSql' Int String Int (Maybe Int) (Maybe Int) (Maybe Int)

data MovePokemonRelationSql' a b = MovePokemonRelationSql'
    { moveId :: a
    , pokemonWhoLearns :: b
    } deriving (Show)

type MovePokemonRelationSql = MovePokemonRelationSql' Int Int

data ActualAbilitySql' a b = ActualAbilitySql'
    { sqlAbilityId :: a
    , sqlAbilityName :: b
    } deriving (Show)

type ActualAbilitySql = ActualAbilitySql' Int String

data AbilityPokemonRelationSql' a b = AbilityPokemonRelationSql'
    { abilityId :: a
    , sqlAbilityPokemonId :: b
    } deriving (Show)

type AbilityPokemonRelationSql = AbilityPokemonRelationSql' Int Int

data TypeSql' a b = TypeSql'
    { typeId :: a
    , typeName :: b
    } deriving (Show)

type TypeSql = TypeSql' Int String

data TypeDoubleDamageSql' a b = TypeDoubleDamageSql'
    { doubleDamageTypeId :: a
    , doubleDamageToTypeId :: b
    } deriving (Show)

type TypeDoubleDamageSql = TypeDoubleDamageSql' Int Int

data TypeHalfDamageSql' a b = TypeHalfDamageSql'
    { halfDamageTypeId :: a
    , halfDamageToTypeId :: b
    } deriving (Show)

type TypeHalfDamageSql = TypeHalfDamageSql' Int Int

data TypeNoDamageSql' a b = TypeNoDamageSql'
    { noDamageTypeId :: a
    , noDamageToTypeId :: b
    } deriving (Show)

type TypeNoDamageSql = TypeNoDamageSql' Int Int


