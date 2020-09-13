module LSP.Network
  ( -- * TCP
    serve
  , connect
  , sendJSON
  , recvJSON

    -- * Process
  , runLangServer
  ) where

import           LSP.Network.Process
import           LSP.Network.TCP
