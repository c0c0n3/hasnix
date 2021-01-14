{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- JSON representation of our @expr@.
--
module Handler.Types.JsonExpr
  ( JsonExpr (..)
  )
where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as Map (toList)
import qualified Data.Vector as Vec (toList)
import Control.Monad (mzero)

import Util


data JsonExpr = Lit Int
              | Add JsonExpr JsonExpr
              | Mult JsonExpr JsonExpr
  deriving (Eq, Show)
-- NOTE. Generic deriving. The `ToJSON` instance generated through generics
-- spits out loads of JSON which is a bit too verbose for including in my
-- examples, so I'm rolling out my own little thing. See note at the bottom
-- of this file about it.

addTag, multTag ∷ Text
addTag  = "add"
multTag = "mult"

instance ToJSON JsonExpr where
  toJSON (Lit n)    = toJSON n
  toJSON (Add x y)  = object [ addTag  .= [toJSON x, toJSON y] ]
  toJSON (Mult x y) = object [ multTag .= [toJSON x, toJSON y] ]

instance FromJSON JsonExpr where
  parseJSON (Number n) = pure ∘ Lit ∘ floor $ n  -- sneakily ditch any decimals!
  parseJSON (Object x) = parseBinOp ∘ Map.toList $ x
  parseJSON _          = mzero

parseBinOp ∷ [(Text, Value)] → Parser JsonExpr
parseBinOp [(tag,  Array v)] | tag ≡ addTag  = uncurry Add  <$> operands
                             | tag ≡ multTag = uncurry Mult <$> operands
                             | otherwise     = mzero
                             where
                               operands = parseOperands ∘ Vec.toList $ v
parseBinOp _ = mzero

parseOperands ∷ [Value] → Parser (JsonExpr, JsonExpr)
parseOperands [x, y] = (,) <$> parseJSON x <*> parseJSON y
parseOperands _      = mzero


{- NOTE. Aeson generic to/from JSON conversion.
Using generics

data JsonExpr = Lit Int
              | Add JsonExpr JsonExpr
              | Mult JsonExpr JsonExpr
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

I get a bit of a bloated JSON doc. E.g.

     e = Add (Mult (Mult (Add (Lit 1) (Lit 5)) (Lit 2)) (Lit 4)) (Lit (-6))
     encode e  ~~~>
{
   "tag":"Add",
   "contents":[
      {
         "tag":"Mult",
         "contents":[
            {
               "tag":"Mult",
               "contents":[
                  {
                     "tag":"Add",
                     "contents":[
                        {
                           "tag":"Lit",
                           "contents":1
                        },
                        {
                           "tag":"Lit",
                           "contents":5
                        }
                     ]
                  },
                  {
                     "tag":"Lit",
                     "contents":2
                  }
               ]
            },
            {
               "tag":"Lit",
               "contents":4
            }
         ]
      },
      {
         "tag":"Lit",
         "contents":-6
      }
   ]
}

-}
