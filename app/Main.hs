{-# LANGUAGE Arrows, NoMonomorphismRestriction, DuplicateRecordFields #-}
module Main (main) where

import Text.XML.HXT.Core

data SVD = SVD
    { name          :: String
    , description   :: String
    , version       :: String
    , peripherals   :: [Peripheral]
    } deriving (Show)

data Peripheral = Peripheral
    { name          :: String
    , description   :: String
    , groupName     :: String
    , baseAddress   :: Int
    , interrupts    :: [Interrupt]
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
        name <- text <<< atTag "name" -< x
        version <- text <<< atTag "version" -< x
        description <- text <<< atTag "description" -< x
        ps <- atTag "peripherals" -< x
        peripherals <- listA getPeripheral -< ps
        returnA -< SVD
            { name = name
            , version = version
            , description = description
            , peripherals = peripherals
            }

getPeripheral = atTag "peripheral" >>>
    proc x -> do
        name <- text <<< atTag "name" -< x
        description <- text <<< atTag "description" -< x
        groupName <- text <<< atTag "groupName" -< x
        baseAddress <- text <<< atTag "baseAddress" -< x
        interrupts <- listA getInterrupt -< x
        rs <- atTag "registers" -< x
        registers <- listA getRegister -< rs
        -- derivedFrom <- text <<< atTag "derivedFrom" -< x
        returnA -< Peripheral
            { name = name
            , description = description
            , groupName = groupName
            , baseAddress = read baseAddress
            , interrupts = interrupts
            , registers = registers
            , derivedFrom = Nothing
            }

getRegister = atTag "register" >>>
    proc x -> do
        name <- text <<< atTag "name" -< x
        displayName <- text <<< atTag "displayName" -< x
        description <- text <<< atTag "description" -< x
        addressOffset <- text <<< atTag "addressOffset" -< x
        size <- text <<< atTag "size" -< x
        -- access <- text <<< atTag "access" -< x
        -- resetValue <- text <<< atTag "resetValue" -< x
        fs <- atTag "fields" -< x
        fields <- listA getField -< fs
        returnA -< Register
            { name = name
            , displayName = displayName
            , description = description
            , addressOffset = read addressOffset
            , size = read size
            , access = Nothing
            , resetValue = 0
            , fields = fields
            }

getField = atTag "field" >>>
    proc x -> do
        name <- text <<< atTag "name" -< x
        description <- text <<< atTag "description" -< x
        bitOffset <- text <<< atTag "bitOffset" -< x
        bitWidth <- text <<< atTag "bitWidth" -< x
        returnA -< Field
            { name = name
            , description = description
            , bitOffset = read bitOffset
            , bitWidth = read bitWidth
            }

getInterrupt = atTag "interrupt" >>>
    proc x -> do
        name <- text <<< atTag "name" -< x
        value <- text <<< atTag "value" -< x
        description <- text <<< atTag "description" -< x
        returnA -< Interrupt
            { name = name
            , value = read value
            , description = description
            }
 
atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText

main :: IO ()
main = do
    s <- readFile "./svd/STM32F0x0.svd"
    xs <- runX (readString [ withValidate yes ] s >>> getField)
    mapM_ print xs
 
