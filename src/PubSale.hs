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

data Sale = Sale
  { saleTokenRef :: AssetClass,
    validToken   :: CurrencySymbol,
    salePrice    :: Integer,
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
valueContainsAsset as v = foldr (\a b -> 0 < (assetClassValueOf v a) || b) False as

-- At least one 'ins' contains at least one of 'assets'.
{-# INLINABLE whitelisted #-}
whitelisted :: [TxInInfo] -> [AssetClass] -> Bool
whitelisted ins assets = 0 < length (filter (valueContainsAsset assets) (map (txOutValue . txInInfoResolved) ins))

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

{-# INLINABLE resolveAll #-}
resolveAll :: [TxInInfo] -> [TxOut]
resolveAll ins = map txInInfoResolved ins

{-# INLINABLE getInputsTo #-}
getInputsTo :: TxInfo -> Address -> [TxOut]
getInputsTo info address = map txInInfoResolved (filter (\a -> (txOutAddress $ txInInfoResolved a) == address) (txInfoInputs info))

{-# INLINABLE getOutputsTo #-}
getOutputsTo :: TxInfo -> Address -> [TxOut]
getOutputsTo info address = filter (\a -> (txOutAddress a) == address) (txInfoOutputs info)

{-# INLINABLE getOutputsByAsset #-}
getOutputsByAsset :: [TxOut] -> AssetClass -> [TxOut]
getOutputsByAsset outs asset = filter (\a -> 0 < (assetClassValueOf (txOutValue a) asset)) outs

{-# INLINABLE datumIsRequest #-}
datumIsRequest :: Maybe PubSaleDatum -> Bool
datumIsRequest md = case md of
  Just d  -> case d of
    SaleDatum    _ -> False
    RequestDatum _ -> True
  Nothing -> False

{-# INLINABLE getRequestOutputs #-}
getRequestOutputs :: TxInfo -> [TxOut] -> [TxOut]
getRequestOutputs info ins = filter (\i -> datumIsRequest (getDatum' info i)) ins

{-# INLINABLE getValuesFromOuts #-}
getValuesFromOuts :: [TxOut] -> [Value]
getValuesFromOuts outs = map txOutValue outs

{-# INLINABLE sumValues #-}
sumValues :: [Value] -> Value
sumValues values = foldr (<>) mempty values

{-# INLINABLE twoAssets #-}
twoAssets :: TxOut -> Bool
twoAssets out = length (flattenValue (txOutValue out)) == 2

{-# valuesCompatible #-}
valuesCompatible :: Integer -> AssetClass -> Value -> Value -> Bool
valuesCompatible price saleToken requestValue paymentValue =
  let requestSaleTokens = assetClassValueOf requestValue saleToken
      requestAdaTokens = (valueOf requestValue adaSymbol adaToken) - 1000000
      outSaleTokens = assetClassValueOf paymentValue saleToken
      outAdaTokens = valueOf paymentValue adaSymbol adaToken
  in (outAdaTokens - requestAdaTokens) <= (divide (outSaleTokens - requestSaleTokens) price)

{-# INLINABLE lowerOutput #-}
lowerOutput :: AssetClass -> TxOut -> TxOut -> TxOut
lowerOutput saleToken o o' =
  let origSaleTokens = assetClassValueOf (txOutValue o) saleToken
      afterSaleTokens = assetClassValueOf (txOutValue o') saleToken
  in case (origSaleTokens < afterSaleTokens) of
    True  -> o
    False -> o'

{-# INLINABLE getBestOutput #-}
getBestOutput :: Integer -> AssetClass -> TxOut -> [TxOut] -> Maybe TxOut
getBestOutput price saleToken request outs =
  let -- indexedOuts = zip [1..] outs
      filteredOuts = filter (\o -> valuesCompatible price saleToken (txOutValue request) (txOutValue o)) outs
      lowestViableOut = foldr (\o o' -> lowerOutput saleToken o o') (head filteredOuts) filteredOuts
  in case (0 == length filteredOuts) of
    True  -> Nothing
    False -> Just lowestViableOut

{-# INLINABLE requestMet #-}
requestMet :: Integer -> AssetClass -> TxOut -> [TxOut] -> (Bool, [TxOut])
requestMet price saleToken request outs =
  let -- (bestIndex, bestOutput) = getBestOutput saleToken request outs
      bestOutput = getBestOutput price saleToken request outs
  in case bestOutput of
    Nothing -> (False, [])
    Just o  -> (True, (dropWhile (\o' -> o' == o) outs))

{-# INLINABLE allRequestsMet #-}
allRequestsMet :: Integer -> AssetClass -> [TxOut] -> [TxOut] -> Bool
allRequestsMet price saleToken requests txOuts =
  case (1 < (length requests)) of
    False ->
      let (b, _) = requestMet price saleToken (head requests) txOuts
      in b
    True  ->
      let (b, newOuts) = requestMet price saleToken (head requests) txOuts
      in case (0 < (length newOuts)) of
        False -> False
        True  -> b && allRequestsMet price saleToken (tail requests) newOuts

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
  let v = filter (\(cs', tn', a') -> cs' == cs && tn' == tn) (flattenValue (txOutValue o))
  in length v == 1

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
      requestInputs = getRequestOutputs info ownInputs
      saleInputs = getOutputsByAsset ownInputs (saleTokenRef sale)
      saleOutputs = getOutputsByAsset ownOutputs (saleTokenRef sale)
      requestInputsWithST = getOutputsByAsset requestInputs validAsset
      twoAssetRequestInputs = filter twoAssets requestInputsWithST
      validRequestInputs = filter (\o -> (valueOf (txOutValue o) adaSymbol adaToken) < (divide (maxTokens sale) (salePrice sale))) twoAssetRequestInputs
      requestTokenOutputs = getOutputsByAsset (txInfoOutputs info) validAsset
      noValidityTokensLeave = length requestTokenOutputs == 0
      saleInValues = getValuesFromOuts saleInputs
      saleOutValues = getValuesFromOuts saleOutputs
      requestInValues = getValuesFromOuts validRequestInputs
      requestInSum = sumValues requestInValues
      saleInTotal = sumValues saleInValues
      saleOutTotal = sumValues saleOutValues
      requestLovelace = valueOf requestInSum adaSymbol adaToken
      saleInLovelace = valueOf saleInTotal adaSymbol adaToken
      saleOutLovelace = valueOf saleOutTotal adaSymbol adaToken
      saleInToken = assetClassValueOf saleInTotal (saleTokenRef sale)
      saleOutToken = assetClassValueOf saleOutTotal (saleTokenRef sale)
  in noValidityTokensLeave && case action of
    Cancel  -> case datum of
      RequestDatum ad -> case (addressCredential ad) of
        PubKeyCredential pkh -> txSignedBy info pkh
        _                    -> False
      SaleDatum _     -> False
    Batch   ->
      ((length saleInputs) == (length saleOutputs)) &&
      ((length requestInputs) == (length validRequestInputs)) &&
      ((divide (saleOutToken - saleInToken) (salePrice sale)) <= (saleOutLovelace - saleInLovelace)) &&
      (saleOutLovelace - saleInLovelace) <= requestLovelace &&
      allRequestsMet (salePrice sale) (saleTokenRef sale) validRequestInputs (txInfoOutputs info)
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
      inputsWithAssets = whitelisted (txInfoInputs info) assets
      inputsWithToken = getInputsWithToken info ownPolicy
      outputsWithToken = getOutputsWithToken info ownPolicy
      (cs, tn, a) = head flattenedMint
  in length flattenedMint == 1 && case a == -1 of
    True  -> True
    False -> case a == 1 of
      False -> False
      True  -> (length inputsWithToken == 0) &&
               inputsWithAssets &&
               (length outputsWithToken == 1) &&
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
