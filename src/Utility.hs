{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPaymentPubKeyHash
    , wallet
    , companyPpkh
    , companyPpkhReal
    , mp
    , mpReal
    , mpMainnet 
    ) where

-- import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Ledger (unPaymentPubKeyHash, PaymentPubKeyHash, PaymentPubKeyHash(..))
import           Ledger.Crypto (PubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, knownWallet, mockWalletPaymentPubKeyHash)

import           Prelude hiding ((.))

import Market.Types  (MarketParams(..))

wallet :: Integer -> Wallet
wallet = knownWallet

-- companySpkh :: StakePubKeyHash
-- companySpkh = 
    
walletPaymentPubKeyHash :: Wallet -> PaymentPubKeyHash
walletPaymentPubKeyHash a = mockWalletPaymentPubKeyHash a

-- walletPubKeyHash :: Wallet -> PubKeyHash
-- walletPubKeyHash a = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash a

-- companyPkh :: PubKeyHash
-- companyPkh = walletPubKeyHash $ wallet 1

companyPpkh :: PaymentPubKeyHash
companyPpkh = walletPaymentPubKeyHash $ wallet 1

-- companyPkh :: PubKeyHash
-- companyPkh = unPaymentPubKeyHash companyPpkh


mp :: MarketParams
mp = MarketParams companyPpkh



companyPpkhReal :: PaymentPubKeyHash
companyPpkhReal = PaymentPubKeyHash "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347"


mpReal :: MarketParams
mpReal = MarketParams companyPpkhReal




companyPpkhMainnet :: PaymentPubKeyHash
companyPpkhMainnet = PaymentPubKeyHash "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347"


mpMainnet :: MarketParams
mpMainnet = MarketParams companyPpkhMainnet