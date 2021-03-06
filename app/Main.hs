{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module Main (main) where

import Text.XML.HXT.Core
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Control.Monad

data SVD = SVD
    { name          :: !Text
    , version       :: !Text
    , description   :: !Text
    , peripherals   :: ![Peripheral]
    , interrupts    :: ![Interrupt]
    } deriving (Show)

data Peripheral = Peripheral
    { name          :: !Text
    , description   :: !Text
    , groupName     :: !Text
    , baseAddress   :: !Int
    , registers     :: ![Register]
    , derivedFrom   :: !(Maybe Text)
    } deriving (Show)

data Interrupt = Interrupt
    { name          :: !Text
    , description   :: !Text
    , value         :: !Int
    } deriving (Show)

data Register = Register
    { name          :: !Text
    , displayName   :: !Text
    , description   :: !Text
    , addressOffset :: !Int
    , size          :: !Int
    , access        :: !(Maybe Text)
    , resetValue    :: !Int
    , fields        :: ![Field]
    } deriving (Show)

data Field = Field
    { name          :: !Text
    , description   :: !Text
    , bitOffset     :: !Int
    , bitWidth      :: !Int
    } deriving (Show)

getSVD = atTag "device" >>>
    proc x -> do
        name <- elemText "name" -< x
        version <- elemText "version" -< x
        description <- elemText "description" -< x
        peripherals <- listA getPeripheral <<< list "peripherals" -< x
        interrupts <- listA getInterrupt -< x
        returnA -< SVD
            { name = pack name
            , version = pack version
            , description = pack description
            , peripherals = peripherals
            , interrupts = interrupts
            }

getPeripheral = atTag "peripheral" >>>
    proc x -> do
        name <- elemText "name" -< x
        description <- elemTextMay "description" -< x
        groupName <- elemTextMay "groupName" -< x
        baseAddress <- elemText "baseAddress" -< x
        derivedFrom <- elemTextMay "derivedFrom" -< x
        registers <- ( listA getRegister <<< list "registers"
                     ) `orElse` (constA []) -< x
        returnA -< Peripheral
            { name = pack name
            , description = pack $ fromMaybe "" description
            , groupName = pack $ fromMaybe "" groupName
            , baseAddress = read baseAddress
            , registers = registers
            , derivedFrom = pack <$> derivedFrom
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
            { name = pack name
            , displayName = pack displayName
            , description = pack description
            , addressOffset = read addressOffset
            , size = read size
            , access = pack <$> access
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
            { name = pack name
            , description = pack description
            , bitOffset = read bitOffset
            , bitWidth = read bitWidth
            }

getInterrupt = atTag "interrupt" >>>
    proc x -> do
        name <- elemText "name" -< x
        description <- elemText "description" -< x
        value <- elemText "value" -< x
        returnA -< Interrupt
            { name = pack name
            , description = pack description
            , value = read value
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
    forM_ xs $ \SVD{..} ->
        forM_ peripherals $ \Peripheral{..} ->
            print name

