{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module Main (main) where

import Text.XML.HXT.Core

data SVD = SVD
    { name          :: String
    , version       :: String
    , description   :: String
    , peripherals   :: [Peripheral]
    } deriving (Show)

data Peripheral = Peripheral
    { name          :: String
    , description   :: String
    , groupName     :: String
    , baseAddress   :: Int
    , registers     :: [Register]
    , derivedFrom   :: Maybe String
    } deriving (Show)

data Interrupt = Interrupt
    { name          :: String
    , description   :: String
    , value         :: Int
    } deriving (Show)

data Register = Register
    { name          :: String
    , displayName   :: String
    , description   :: String
    , addressOffset :: Int
    , size          :: Int
    , access        :: Maybe String
    , resetValue    :: Int
    , fields        :: [Field]
    } deriving (Show)

data Field = Field
    { name          :: String
    , description   :: String
    , bitOffset     :: Int
    , bitWidth      :: Int
    } deriving (Show)

getSVD = atTag "device" >>>
    proc x -> do
        name <- elemText "name" -< x
        version <- elemText "version" -< x
        description <- elemText "description" -< x
        ps <- hasName "peripherals" <<< isElem <<< getChildren -< x
        peripherals <- listA getPeripheral <<< list "peripherals" -< x
        returnA -< SVD
            { name = name
            , version = version
            , description = description
            , peripherals = peripherals
            }

getPeripheral = atTag "peripheral" >>>
    proc x -> do
        name <- elemText "name" -< x
        description <- elemText "description" -< x
        groupName <- elemText "groupName" -< x
        baseAddress <- elemText "baseAddress" -< x
        derivedFrom <- elemTextMay "derivedFrom" -< x
        registers <- listA getRegister <<< list "registers" -< x
        returnA -< Peripheral
            { name = name
            , description = description
            , groupName = groupName
            , baseAddress = read baseAddress
            , registers = registers
            , derivedFrom = Nothing
            }

getRegister = atTag "register" >>>
    proc x -> do
        name <- elemText "name" -< x
        displayName <- elemText "displayName" -< x
        description <- elemText "description" -< x
        addressOffset <- elemText "addressOffset" -< x
        size <- elemText "size" -< x
        access <- elemTextMay "access" -< x
        resetValue <- elemText "resetValue" -< x
        fields <- listA getField <<< list "fields" -< x
        returnA -< Register
            { name = name
            , displayName = displayName
            , description = description
            , addressOffset = read addressOffset
            , size = read size
            , access = access
            , resetValue = read resetValue
            , fields = fields
            }

getField = atTag "field" >>>
    proc x -> do
        name <- elemText "name" -< x
        description <- elemText "description" -< x
        bitOffset <- elemText "bitOffset" -< x
        bitWidth <- elemText "bitWidth" -< x
        returnA -< Field
            { name = name
            , description = description
            , bitOffset = read bitOffset
            , bitWidth = read bitWidth
            }

getInterrupt = atTag "interrupt" >>>
    proc x -> do
        name <- elemText "name" -< x
        value <- elemText "value" -< x
        description <- elemText "description" -< x
        returnA -< Interrupt
            { name = name
            , value = read value
            , description = description
            }
 
atTag tag = deep (isElem >>> hasName tag)

elemText tag
    = getChildren
    >>> isElem
    >>> hasName tag
    >>> getChildren
    >>> getText

elemTextMay tag
    = (elemText tag >>> arr Just)
    `orElse` (constA Nothing)

list tag
    = getChildren
    >>> isElem
    >>> hasName tag

main :: IO ()
main = do
    let fn = "./svd/STM32F0x0.svd"
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getSVD)
    mapM_ print xs

