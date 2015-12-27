{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Callback.Parser.Parser
import Data.Proxy


[callback|CALLBACK "gyration"
    EVERY 1
    NODES 2
    WHERE BELONGS(X 0, BEAD 0) AND BELONGS(X 1, BEAD 0)
    COMPUTE SUM (DIST(X 0, X 1)) + 1 * 0
|]

main = pure ()
