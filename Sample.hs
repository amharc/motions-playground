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
    WHERE (BELONGS(X 0, BEAD 0) OR BELONGS (X 1, BEAD 1)) AND (BELONGS(X 1, BEAD 0) OR BELONGS(X 1, BEAD 1)) AND 2 + 2 == 4 AND NOT 1 > 2
    COMPUTE SUM (DIST(X 0, X 1)) + 1 * 0
|]

[callback|CALLBACK "test"
    EVERY 1
    NODES 0
    WHERE 1 == 1
    COMPUTE LIST 42
|]

[callback|CALLBACK "prod"
    EVERY 1
    NODES 1
    WHERE 1 == 1
    COMPUTE PRODUCT INT(DIST(X 0, X 0))
|]


main = pure ()
