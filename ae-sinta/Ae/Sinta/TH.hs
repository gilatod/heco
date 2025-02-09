{-# LANGUAGE TemplateHaskell #-}

module Ae.Sinta.TH where

import Ae.Sinta.Internal.Core
    ( morph,
      morph2,
      morph3,
      morph4,
      unbound,
      MorphBase,
      MorphBase2,
      MorphBase3,
      MorphBase4,
      Unbound )

import Language.Haskell.TH
    ( mkName,
      varT,
      Exp(VarE),
      Q,
      Pat(VarP),
      Dec(ValD, SigD),
      Body(NormalB), strTyLit, litT )

import Effectful ( type (:>), Eff )

-- * Unbound

makeUnbound :: String -> Q [Dec]
makeUnbound sym = do
    typeDec <- [t|
        (Unbound $symTypeLit $fT $rT :> $esT)
            => Eff $esT ($fT $rT)
        |]
    return
        [ SigD name typeDec
        , ValD (VarP name) (NormalB (VarE 'unbound)) []]
    where
        fT = varT (mkName "f")
        esT = varT (mkName "es")
        rT = varT (mkName "r")
        name = mkName sym
        symTypeLit = litT (strTyLit sym)

makeUnbounds :: [String] -> Q [Dec]
makeUnbounds = fmap concat . sequence . map makeUnbound

-- * Morph

makeMorph :: String -> Q [Dec]
makeMorph sym = do
    typeDec <- [t|
        (MorphBase $symTypeLit $aT $rT :> $esT)
            => Eff $esT $aT -> Eff $esT $rT
        |]
    return
        [ SigD name typeDec
        , ValD (VarP name) (NormalB (VarE 'morph)) [] ]
    where
        esT = varT (mkName "es")
        aT = varT (mkName "a")
        rT = varT (mkName "r")
        name = mkName sym
        symTypeLit = litT (strTyLit sym)

makeMorphs :: [String] -> Q [Dec]
makeMorphs = fmap concat . sequence . map makeMorph

makeMorph2 :: String -> Q [Dec]
makeMorph2 sym = do
    typeDec <- [t|
        (MorphBase2 $symTypeLit $aT $bT $rT :> $esT)
            => Eff $esT $aT -> Eff $esT $bT -> Eff $esT $rT
        |]
    return
        [ SigD name typeDec
        , ValD (VarP name) (NormalB (VarE 'morph2)) [] ]
    where
        esT = varT (mkName "es")
        aT = varT (mkName "a")
        bT = varT (mkName "b")
        rT = varT (mkName "r")
        name = mkName sym
        symTypeLit = litT (strTyLit sym)

makeMorphs2 :: [String] -> Q [Dec]
makeMorphs2 = fmap concat . sequence . map makeMorph2

makeMorph3 :: String -> Q [Dec]
makeMorph3 sym = do
    typeDec <- [t|
        (MorphBase3 $symTypeLit $aT $bT $cT $rT :> $esT)
            => Eff $esT $aT -> Eff $esT $bT -> Eff $esT $cT -> Eff $esT $rT
        |]
    return
        [ SigD name typeDec
        , ValD (VarP name) (NormalB (VarE 'morph3)) []]
    where
        esT = varT (mkName "es")
        aT = varT (mkName "a")
        bT = varT (mkName "b")
        cT = varT (mkName "c")
        rT = varT (mkName "r")
        name = mkName sym
        symTypeLit = litT (strTyLit sym)

makeMorphs3 :: [String] -> Q [Dec]
makeMorphs3 = fmap concat . sequence . map makeMorph3

makeMorph4 :: String -> Q [Dec]
makeMorph4 sym = do
    typeDec <- [t|
        (MorphBase4 $symTypeLit $aT $bT $cT $dT $rT :> $esT)
            => Eff $esT $aT -> Eff $esT $bT -> Eff $esT $cT -> Eff $esT $dT -> Eff $esT $rT
        |]
    return
        [ SigD name typeDec
        , ValD (VarP name) (NormalB (VarE 'morph4)) []]
    where
        esT = varT (mkName "es")
        aT = varT (mkName "a")
        bT = varT (mkName "b")
        cT = varT (mkName "c")
        dT = varT (mkName "d")
        rT = varT (mkName "r")
        name = mkName sym
        symTypeLit = litT (strTyLit sym)

makeMorphs4 :: [String] -> Q [Dec]
makeMorphs4 = fmap concat . sequence . map makeMorph4