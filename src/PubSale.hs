{--
   Copyright 2022 ₳DAO

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PubSale where

import           Prelude                (String, show, Show)
import           Control.Monad          hiding (fmap)
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.Maybe
import qualified Data.Map               as Map
import           Data.List              (delete)
-- import           Data.Ord
-- import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Credential
import           Ledger.Ada             as Ada hiding (divide)
import           Ledger.Constraints     as Constraints
import           Ledger.Index           as Index
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Contexts                   as Validation
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, NonEmpty(..) )
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions, ensureKnownCurrencies)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           GHC.Generics         (Generic)
import           Data.String          (IsString (..))
import           Data.Aeson           (ToJSON, FromJSON)

data SalePrice = TokensPerLovelace Integer | LovelacePerToken Integer
  deriving (Show, Generic)

PlutusTx.makeLift ''SalePrice

data Sale = Sale
  { saleTokenRef :: AssetClass,
    validToken   :: CurrencySymbol,
    salePrice    :: SalePrice,
    maxTokens    :: Integer
  }
  deriving (Show, Generic)

PlutusTx.makeLift ''Sale

data PubSaleDatum = SaleDatum Address | RequestDatum Address

PlutusTx.makeIsDataIndexed ''PubSaleDatum [ ('SaleDatum, 0)
                                          , ('RequestDatum, 1)
                                          ]
PlutusTx.makeLift ''PubSaleDatum

data SaleRedeemer
  = --
    Cancel
  |
    Batch
  |
    EndSale
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''SaleRedeemer
  [ ('Cancel, 0)
  , ('Batch, 1)
  , ('EndSale, 2)
  ]

PlutusTx.makeLift ''SaleRedeemer

{-# INLINABLE getDatum' #-}
getDatum' :: TxInfo -> TxOut -> Maybe PubSaleDatum
getDatum' txInfo o = do
  dh      <- txOutDatum o
  Datum d <- findDatum dh txInfo
  PlutusTx.fromBuiltinData d

{-# INLINABLE valueContainsAsset #-}
valueContainsAsset :: [AssetClass] -> Value -> Bool
valueContainsAsset as v = foldr (\a b -> (0 < (assetClassValueOf v a) || b)) False as

{-# INLINABLE valueContainsAsset' #-}
valueContainsAsset' :: [AssetClass] -> Value -> Bool
valueContainsAsset' as v = 0 < length (filter (\a -> 0 < (assetClassValueOf v a)) as)

-- At least one 'ins' contains at least one of 'assets'.
{-# INLINABLE whitelisted #-}
whitelisted :: [TxOut] -> [AssetClass] -> Bool
whitelisted ins assets = 0 < length (filter (valueContainsAsset' assets) (map txOutValue ins))

-- Return address of the script being validated.
{-# INLINABLE getOwnAddress #-}
getOwnAddress :: ScriptContext -> Address
getOwnAddress ctx = 
  let ownInput = findOwnInput ctx
      hash = ownHash ctx
      txInfo = scriptContextTxInfo ctx
  in case ownInput of
    Just txoInfo -> txOutAddress (txInInfoResolved txoInfo)
    Nothing      -> scriptHashAddress hash

-- Return the tokenName of the validator.
{-# INLINABLE ownTokenName #-}
ownTokenName :: ScriptContext -> TokenName
ownTokenName ctx =
  let hash = ownHash ctx
  in case hash of
    ValidatorHash h -> tokenName (fromBuiltin h)

{-# INLINABLE getInputsTo #-}
getInputsTo :: TxInfo -> Address -> [TxOut]
getInputsTo info address = map txInInfoResolved (filter (\a -> (txOutAddress $ txInInfoResolved a) == address) (txInfoInputs info))

{-# INLINABLE getOutputsTo #-}
getOutputsTo :: TxInfo -> Address -> [TxOut]
getOutputsTo info address = filter (\a -> (txOutAddress a) == address) (txInfoOutputs info)

{-# INLINABLE getOutputsByAsset #-}
getOutputsByAsset :: [TxOut] -> AssetClass -> [TxOut]
getOutputsByAsset outs asset = filter (\a -> 0 < (assetClassValueOf (txOutValue a) asset)) outs

{-# INLINABLE requestsMatch #-}
requestsMatch :: TxInfo -> TxOut -> Address -> Bool
requestsMatch info i d = case (getDatum' info i) of
  Nothing -> False
  Just pd -> case pd of
    SaleDatum _    -> False
    RequestDatum a -> a == d

{-# INLINABLE salesMatch #-}
salesMatch :: TxInfo -> TxOut -> Address -> Bool
salesMatch info i d = case (getDatum' info i) of
  Nothing -> False
  Just pd -> case pd of
    SaleDatum a    -> a == d
    RequestDatum _ -> False

{-# INLINABLE outFromAddress #-}
outFromAddress :: Address -> TxOut -> Bool
outFromAddress a o = (txOutAddress o) == a

-- sa is the script address
-- a is the address of the datum being validated
{-# INLINABLE getRequestInputs #-}
getRequestInputs :: Address -> Address -> TxInfo -> [TxOut]
getRequestInputs sa a info = [i | i <- filter (\i' -> requestsMatch info i' a) (filter (outFromAddress sa) (map txInInfoResolved (txInfoInputs info)))]

{-# INLINABLE getSaleInputs #-}
getSaleInputs :: Address -> Address -> TxInfo -> [TxOut]
getSaleInputs sa a info = [i | i <- filter (\i' -> salesMatch info i' a) (filter (outFromAddress sa) (map txInInfoResolved (txInfoInputs info)))]

{-# INLINABLE getSaleOutputs #-}
getSaleOutputs :: Address -> Address -> TxInfo -> [TxOut]
getSaleOutputs sa a info = [i | i <- filter (\i' -> salesMatch info i' a) (filter (outFromAddress sa) (txInfoOutputs info))]


{-# INLINABLE getValuesFromOuts #-}
getValuesFromOuts :: [TxOut] -> [Value]
getValuesFromOuts outs = map txOutValue outs

{-# INLINABLE sumValues #-}
sumValues :: [Value] -> Value
sumValues values = foldr (<>) mempty values

-- Return the amount of sale tokens given to the user.
{-# INLINABLE lockedToUser #-}
lockedToUser :: Address -> TxInfo -> Value
lockedToUser a info = sumValues [txOutValue o | o <- (filter (\o' -> a == (txOutAddress o')) (txInfoOutputs info))]

{-# INLINABLE getInputsWithToken #-}
getInputsWithToken :: TxInfo -> CurrencySymbol -> [TxOut]
getInputsWithToken info symbol =
  let ins = map txInInfoResolved (txInfoInputs info)
  in filter (\i -> 0 < length (filter (\(cs, tn, v) -> cs == symbol) (flattenValue (txOutValue i)))) ins

{-# INLINABLE getOutputsWithToken #-}
getOutputsWithToken :: TxInfo -> CurrencySymbol -> [TxOut]
getOutputsWithToken info symbol =
  let outs = txInfoOutputs info
  in filter (\i -> 0 < length (filter (\(cs, tn, v) -> cs == symbol) (flattenValue (txOutValue i)))) outs

{-# INLINABLE correctTokenName #-}
correctTokenName :: (CurrencySymbol, TokenName, Integer) -> TxOut -> Bool
correctTokenName (cs, tn, a) o =
  let ac = addressCredential (txOutAddress o)
  in case ac of
    PubKeyCredential pkh -> False
    ScriptCredential vh  -> case vh of
      ValidatorHash h -> (tokenName (fromBuiltin h)) == tn

data Saleing
instance Scripts.ValidatorTypes Saleing where
    type instance RedeemerType Saleing = SaleRedeemer
    type instance DatumType Saleing = PubSaleDatum

{-# INLINABLE saleScript #-}
saleScript :: Sale -> PubSaleDatum -> SaleRedeemer -> ScriptContext -> Bool
saleScript sale datum action ctx =
  let info = scriptContextTxInfo ctx
      tn = ownTokenName ctx
      ownAddress = getOwnAddress ctx
      ownInputs = getInputsTo info ownAddress
      ownOutputs = getOutputsTo info ownAddress
      validAsset = AssetClass ((validToken sale), tn)
      requestTokenOutputs = getOutputsByAsset (txInfoOutputs info) validAsset
      noValidityTokensLeave = length requestTokenOutputs == 0
  in noValidityTokensLeave && case action of
    Cancel  -> case datum of
      RequestDatum ad -> case (addressCredential ad) of
        PubKeyCredential pkh -> txSignedBy info pkh
        _                    -> False
      SaleDatum _     -> False
    Batch   -> case datum of
      RequestDatum ad ->
        let requestInputs = getRequestInputs ownAddress ad info
            validRequestInputs = getOutputsByAsset requestInputs validAsset
            requestInputsValues = getValuesFromOuts requestInputs
            requestInTotal = sumValues requestInputsValues
            requestInLovelace = valueOf requestInTotal adaSymbol adaToken
            lockedToUser' = lockedToUser ad info
            tokensToUser = assetClassValueOf lockedToUser' (saleTokenRef sale)
        in ((length requestInputs) == (length validRequestInputs)) &&
           (tokensToUser <= maxTokens sale) &&
           case (salePrice sale) of
             LovelacePerToken sp  -> (divide (requestInLovelace - 3000000) sp) <= tokensToUser
             TokensPerLovelace sp -> (requestInLovelace - 3000000) <= (divide tokensToUser sp)
      SaleDatum ad ->
        let saleInputs = getSaleInputs ownAddress ad info
            saleOutputs = getSaleOutputs ownAddress ad info
            saleInValues = getValuesFromOuts saleInputs
            saleOutValues = getValuesFromOuts saleOutputs
            saleInTotal = sumValues saleInValues
            saleOutTotal = sumValues saleOutValues
            saleInLovelace = valueOf saleInTotal adaSymbol adaToken
            saleOutLovelace = valueOf saleOutTotal adaSymbol adaToken
            saleInToken = assetClassValueOf saleInTotal (saleTokenRef sale)
            saleOutToken = assetClassValueOf saleOutTotal (saleTokenRef sale)
        in ((length saleInputs) == (length saleOutputs)) &&
           case (salePrice sale) of
             LovelacePerToken  sp -> (saleOutToken - saleInToken) <= (divide (saleOutLovelace - saleInLovelace) sp)
             TokensPerLovelace sp -> (divide (saleOutToken - saleInToken) sp) <= (saleOutLovelace - saleInLovelace)
    EndSale -> case datum of
      SaleDatum ad    -> case (addressCredential ad) of
        PubKeyCredential pkh -> txSignedBy info pkh
        _                    -> False
      RequestDatum _  -> False

{-# INLINABLE mkPolicy #-}
mkPolicy :: [AssetClass] -> BuiltinData -> ScriptContext -> Bool
mkPolicy assets _ ctx =
  let info = scriptContextTxInfo ctx
      ownPolicy = ownCurrencySymbol ctx
      flattenedMint = flattenValue (txInfoMint info)
      whitelist = whitelisted (map txInInfoResolved (txInfoInputs info)) assets
      inputsWithToken = getInputsWithToken info ownPolicy
      outputsWithToken = getOutputsWithToken info ownPolicy
      (cs, tn, a) = head flattenedMint
  in length flattenedMint == 1 && case a == -1 of
    True  -> True
    False -> case a == 1 of
      False -> False
      True  -> (length inputsWithToken == 0) &&
               whitelist &&
               correctTokenName (head flattenedMint) (head outputsWithToken)

policy :: [AssetClass] -> Scripts.MintingPolicy
policy assets = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode assets

curSymbol :: [AssetClass] -> CurrencySymbol
curSymbol assets = scriptCurrencySymbol $ policy assets

saleValidatorInstance :: Sale -> Scripts.TypedValidator Saleing
saleValidatorInstance sale = Scripts.mkTypedValidator @Saleing
    ($$(PlutusTx.compile [|| saleScript ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sale)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @PubSaleDatum @SaleRedeemer

saleValidatorHash :: Sale -> ValidatorHash
saleValidatorHash cur = Scripts.validatorHash (saleValidatorInstance cur)

saleValidatorScript :: Sale -> Validator
saleValidatorScript cur = Scripts.validatorScript (saleValidatorInstance cur)

saleValidatorAddress :: Sale -> Address
saleValidatorAddress cur = Ledger.scriptAddress (saleValidatorScript cur)
