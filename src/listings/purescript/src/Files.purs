-- This example shows how the type system and Leffe can be used to
-- enforce closing of files, and reading/writing of files with correct
-- file modes.

module Files where

import Prelude

import Control.IxMonad (ibind, (:*>), (:>>=))
import Control.IxMonad.Leffe (addResource, getResource, ilift, removeResource, runLeffe)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import FSM (Add, Empty, Label, Remain, Remove, label)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.FS (FileDescriptor, FileFlags(W, R)) as FS
import Node.FS (BufferOffset, FS, BufferLength)
import Node.FS.Aff (fdClose, fdOpen, fdRead, fdWrite, stat) as FS
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)

foreign import kind FileMode

foreign import data Read :: FileMode
foreign import data Write :: FileMode

data FileMode (m :: FileMode) = FileMode

modeRead :: FileMode Read
modeRead = FileMode

modeWrite :: FileMode Write
modeWrite = FileMode

data File (mode :: FileMode) = File FilePath FS.FileDescriptor

-- start snippet openFile-signature
class FileOpen m (mode :: FileMode) where
  openFile
    :: forall label
     . Label label
    -> FilePath
    -> FileMode mode
    -> Add m label (File mode) Unit
-- end snippet openFile-signature

instance fileOpenRead :: MonadAff (fs :: FS | e) m => FileOpen m Read where
  openFile label path _ = do
    fd <- ilift $ liftAff $ FS.fdOpen path FS.R Nothing
    addResource label (File path fd)
    where
        bind = ibind

instance fileOpenWrite :: MonadAff (fs :: FS | e) m => FileOpen m Write where
  openFile label path _ = do
    fd <- ilift $ liftAff $ FS.fdOpen path FS.W Nothing
    addResource label (File path fd)
    where
        bind = ibind

-- start snippet stat-signature
stat
  :: forall m e label mode
   . MonadAff (fs :: FS | e) m
  => Label label
  -> Remain m label (File mode) Stats
-- end snippet stat-signature
stat label =
  getResource label
  :>>= \(File path _) -> ilift $ liftAff $ FS.stat path

-- start snippet readFile-signature
readFile
  :: forall m e l
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Label l
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Remain m l (File Read) Int
-- end snippet readFile-signature
readFile label buf offset length = do
  File path fd <- getResource label
  ilift $ liftAff $ FS.fdRead fd buf offset length Nothing
  where
    bind = ibind
    discard = ibind

writeFile
  :: forall m e l
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Label l
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Remain m l (File Write) Int
writeFile label contents offset length = do
  File path fd <- getResource label
  ilift $ liftAff (FS.fdWrite fd contents offset length Nothing)
  where
    bind = ibind
    discard = ibind

-- start snippet closeFile
class FileClose m (mode :: FileMode) where
  closeFile
    :: forall label
     . Label label
    -> Remove m label (File mode) Unit
-- end snippet closeFile

instance fileCloseRead :: MonadAff (fs :: FS | e) m => FileClose m Read where
  closeFile label =
    getResource label :>>= \(File _ fd) ->
    ilift (liftAff (FS.fdClose fd)) :*>
    removeResource label

instance fileCloseWrite :: MonadAff (fs :: FS | e) m => FileClose m Write where
  closeFile label =
    getResource label :>>= \(File _ fd) ->
    ilift (liftAff (FS.fdClose fd)) :*>
    removeResource label

-- start snippet labels
source :: Label "source"
source = label

target :: Label "target"
target = label
-- end snippet labels

-- start snippet copy-signature
copy
  :: forall m e
   . MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => FilePath
  -> FilePath
  -> Empty m Unit
-- end snippet copy-signature
-- start snippet copy-open
copy sourcePath targetPath = do
  openFile source sourcePath modeRead
  openFile target targetPath modeWrite
-- end snippet copy-open

-- start snippet copy-read-write
  Stats stats <- stat source
  let size = round stats.size
  buf <- ilift (liftEff (Buffer.create size))

  readFile source buf 0 size
  writeFile target buf 0 size
-- end snippet copy-read-write

-- start snippet copy-close
  closeFile source
  closeFile target
-- end snippet copy-close

  where
    bind = ibind
    discard = ibind

main
  :: forall e
   . Eff ( fs :: FS
         , buffer :: BUFFER
         , exception :: EXCEPTION
         | e
         ) Unit
-- silly main example
main = void (launchAff (runLeffe (copy "/tmp/in.txt" "/tmp/out.txt")))
