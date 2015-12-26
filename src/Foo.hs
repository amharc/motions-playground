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
    WHERE 1 == 1
    COMPUTE SUM (DIST(X 0, X 1))
|]

[callback|CALLBACK "test"
    EVERY 1
    NODES 2
    WHERE 1 == 1
    COMPUTE SUM 3
|]

main = pure ()
