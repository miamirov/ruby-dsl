{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ruby.Interpreter
  ( Interpreter

  , interpret
  , interpret_
  ) where

import Data.IORef (IORef, readIORef, modifyIORef, newIORef)
import Data.Map (Map, (!?), insert, empty, fromList)

import Ruby.Dsl
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
import Control.Monad ((>=>), void, when)
import Control.Exception (throwIO, Exception)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import System.IO (openFile, IOMode(AppendMode, ReadMode), hClose, hGetLine, hPutStrLn)

data Interpreter a where
  InterpretLink    :: (IORef [Scope] -> IO Object) -> Interpreter Link
  InterpretCommand :: (IORef [Scope] -> IO (Either () Object)) -> Interpreter Command
  InterpretFile    :: (FilePath -> FilePath -> IO ()) -> Interpreter ()

interpret :: Interpreter () -> FilePath -> FilePath -> IO ()
interpret (InterpretFile io) = io

interpret_ :: Interpreter () -> IO ()
interpret_ (InterpretFile io) = io "" ""

interpretLink :: Interpreter Link -> IORef [Scope] -> IO Object
interpretLink (InterpretLink computeLink) = computeLink

interpretCommand :: Interpreter Command -> IORef [Scope] -> IO (Either () Object)
interpretCommand (InterpretCommand runCommand) = runCommand

data Scope = Scope
  { functions :: Map Name ([Object] -> IO Object)
  , variables :: Map Name Object
  }

data Object where
  Object :: Typeable t => RubyType t -> t -> Object

data RubyType t where
  RubyInt    :: RubyType Integer
  RubyFloat  :: RubyType Double
  RubyString :: RubyType String
  RubyBool   :: RubyType Bool
  RubyNil    :: RubyType ()

instance Show (RubyType t) where
  show :: RubyType t -> String
  show = \case
    RubyInt    -> "Integer"
    RubyFloat  -> "Float"
    RubyString -> "String"
    RubyBool   -> "Bool"
    RubyNil    -> "Nil"

newtype InterpretError
  = InterpretError String
  deriving (Eq, Exception)

instance Show InterpretError where
  show :: InterpretError -> String
  show (InterpretError msg) = "interpret error: " ++ msg

interpretError :: String  -> InterpretError
interpretError = InterpretError

instance Ruby Interpreter where
    file_
      :: [Interpreter Command]
      -> Interpreter ()
    file_ commandList = InterpretFile $ \inputFile outputFile -> do
      let scope = [ initScope inputFile outputFile ]
      stateRef <- newIORef scope
      void $ interpretCommandList commandList stateRef

    def_
      :: Name
      -> [Name]
      -> [Interpreter Command]
      -> Interpreter Command
    def_ fName argNames commandList =
      InterpretCommand $ \stateRef ->
        (Left <$>) $ modifyIORef stateRef $
          putFunction (func stateRef)
      where
        func :: IORef [Scope] -> [Object] -> IO Object
        func initialState argLinks = do
          checkCountOfArguments fName (length argNames) (length argLinks)
          withScope initialState $ \stateRef -> do
            modifyIORef stateRef $
              putVariables (zip argNames argLinks)
            mbResult <- interpretCommandList commandList stateRef
            case mbResult of
              Left () -> return $ Object RubyNil ()
              Right r -> return r

        putFunction :: ([Object] -> IO Object) -> [Scope] -> [Scope]
        putFunction _ [] = error "Impossible empty state"
        putFunction f (s:ss) =
          s { functions = insert fName f $ functions s } : ss

        putVariables :: [(Name, Object)] -> [Scope] -> [Scope]
        putVariables _ [] = error "Impossible empty state"
        putVariables ps (s:ss) =
          s { variables = putVariables_ ps $ variables s } : ss

        putVariables_ :: [(Name, Object)] -> Map Name Object -> Map Name Object
        putVariables_ [] s = s
        putVariables_ ((name, obj):ps) scope =
          insert name obj $ putVariables_ ps scope


    assoc_
      :: Name
      -> Interpreter Link
      -> Interpreter Command
    assoc_ name link = InterpretCommand $ \stateRef -> do
      obj <- interpretLink link stateRef
      Left <$> modifyIORef stateRef (putVariable obj)
      where
        putVariable :: Object -> [Scope] -> [Scope]
        putVariable _ [] = error "Impossible empty state"
        putVariable obj (s:ss) =
          s { variables = insert name obj $ variables s } : ss

    if_
      :: Interpreter Link
      -> [Interpreter Command]
      -> Maybe [Interpreter Command]
      -> Interpreter Command
    if_ conditionLink trueCommandList mbFalseCommandList =
      InterpretCommand $ \stateRef -> do
        condition <-
          interpretLink conditionLink stateRef >>= flip cast RubyBool
        if condition
        then
          interpretCommandList trueCommandList stateRef
        else
          case mbFalseCommandList of
            Nothing -> return $ Left ()
            Just falseCommandList ->
              interpretCommandList falseCommandList stateRef

    for_
      :: Name
      -> (Interpreter Link, Interpreter Link)
      -> [Interpreter Command]
      -> Interpreter Command
    for_ iName (fromLink, toLink) commandList =
      InterpretCommand $ \stateRef -> do
        from <- interpretLink fromLink stateRef >>= flip cast RubyInt
        to   <- interpretLink toLink   stateRef >>= flip cast RubyInt
        withScope stateRef $ runFor from to
      where
        runFor :: Integer -> Integer -> IORef [Scope] -> IO (Either () Object)
        runFor from to stateRef
          | from >= to = return $ Left ()
          | otherwise  = do
              _ <- interpretCommand (assoc_ iName $ int_ from) stateRef
              interpretCommandList commandList stateRef >>=
                \case
                  Left ()     -> runFor (succ from) to stateRef
                  r@(Right _) -> return r

    return_
      :: Interpreter Link
      -> Interpreter Command
    return_ link = InterpretCommand $ (Right <$>) . interpretLink link

    call_
      :: Name
      -> [Interpreter Link]
      -> Interpreter Link
    call_ name args = InterpretLink $ \stateRef -> do
      scope <- readIORef stateRef
      f <- findInScope scope ((!? name) . functions) $
             "function " ++ name ++ " not defined"
      args_ <- sequence (flip interpretLink stateRef <$> args)
      f args_

    onTop_
      :: Interpreter Link
      -> Interpreter Command
    onTop_ link = InterpretCommand $ (Left <$>) . void . interpretLink link

    read_
      :: Name
      -> Interpreter Link
    read_ name = InterpretLink $ \stateRef -> do
      scope <- readIORef stateRef
      findInScope scope ((!? name) . variables) $
        "varable " ++ name ++ " not defined"


    negate_ :: Interpreter Link -> Interpreter Link
    negate_ = interpretUnaryOperator $ \(Object vType value) ->
      case vType of
        RubyInt   -> return $ Object RubyInt   $ negate value
        RubyFloat -> return $ Object RubyFloat $ negate value
        _         -> noMethodException "negate" vType

    not_ :: Interpreter Link -> Interpreter Link
    not_ = interpretUnaryOperator $ unaryLogic not

    mult_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    mult_ = interpretBinaryOperator $ binaryNumeric "*" ((*), RubyInt) ((*), RubyFloat)

    div_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    div_ = interpretBinaryOperator $ binaryNumeric "/" (div, RubyInt) ((/), RubyFloat)

    plus_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    plus_ = interpretBinaryOperator binaryPlus

    minus_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    minus_ = interpretBinaryOperator $ binaryNumeric "-" ((-), RubyInt) ((-), RubyFloat)

    gt_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    gt_ = interpretBinaryOperator $ binaryCompare ">" (>)

    gte_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    gte_ = interpretBinaryOperator $ binaryCompare ">=" (>=)

    lt_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    lt_ = interpretBinaryOperator $ binaryCompare "<" (<)

    lte_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    lte_ = interpretBinaryOperator $ binaryCompare "<=" (<=)

    eq_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    eq_ = interpretBinaryOperator binaryEquals

    notEq_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    notEq_ left right = not_ $ eq_ left right

    and_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    and_ = interpretBinaryOperator $ binaryLogic (&&)

    or_ :: Interpreter Link -> Interpreter Link -> Interpreter Link
    or_ = interpretBinaryOperator $ binaryLogic (||)

    int_ :: Integer -> Interpreter Link
    int_ = interpretLiteral RubyInt

    float_ :: Double -> Interpreter Link
    float_ = interpretLiteral RubyFloat

    string_ :: String -> Interpreter Link
    string_ = interpretLiteral RubyString

    bool_ :: Bool -> Interpreter Link
    bool_ = interpretLiteral RubyBool

    nil_ :: () -> Interpreter Link
    nil_ = interpretLiteral RubyNil

interpretCommandList
  :: [Interpreter Command]
  -> IORef [Scope]
  -> IO (Either () Object)
interpretCommandList commandList =
  flip withScope $ interpretCommandList_ commandList
  where
    interpretCommandList_
      :: [Interpreter Command]
      -> IORef [Scope]
      -> IO (Either () Object)
    interpretCommandList_ [] _ = return $ Left ()
    interpretCommandList_ (command:commands) stateRef =
      interpretCommand command stateRef >>=
        \case
          Left ()     -> interpretCommandList_ commands stateRef
          r@(Right _) -> return r

emptyScope :: Scope
emptyScope = Scope empty empty

checkCountOfArguments :: Name -> Int -> Int -> IO ()
checkCountOfArguments fName expected actual =
  when (expected /= actual) $
    throwIO . interpretError $
      "Incorrect count of arguments on call function" ++
        fName ++ " (expected=" ++ show expected ++
        ", actual=" ++ show actual ++ ")"
  

initScope :: FilePath -> FilePath -> Scope
initScope inputFile outputFile = Scope
  { variables = empty
  , functions = fromList
      [ (,) "puts" $
          \objs -> do
            mapM_ puts_ objs
            return $ Object RubyNil ()
      , (,) "gets.chomp.to_i" $
          \args -> do
            checkCountOfArguments "gets.chomp.to_i" 0 (length args)
            line <- readLine_
            let mbInt = readMaybe @Integer line
            case mbInt of
              Nothing -> throwIO . interpretError $
                "Cannot convert string to Integer: '" ++ line ++ "'"
              Just i -> return $ Object RubyInt i
      , (,) "gets.chomp.to_f" $
          \args -> do
            checkCountOfArguments "gets.chomp.to_f" 0 (length args)
            line <- readLine_
            let mbInt = readMaybe @Double line
            case mbInt of
              Nothing -> throwIO . interpretError $
                "Cannot convert string to Float: '" ++ line ++ "'"
              Just i -> return $ Object RubyFloat i
      , (,) "gets.chomp" $
          \args -> do
            checkCountOfArguments "gets.chomp" 0 (length args)
            Object RubyString <$> readLine_
      ]
  }
  where
    puts_ :: Object -> IO ()
    puts_ (Object oType obj) =
      case oType of
        RubyInt    -> print_ $ show obj
        RubyFloat  -> print_ $ show obj
        RubyString -> print_ obj
        RubyBool   -> print_ $ show obj
        RubyNil    -> print_ $ show obj
    
    print_ :: String -> IO ()
    print_ value
      | null outputFile = print value
      | otherwise = do
          file <- openFile outputFile AppendMode
          hPutStrLn file value
          hClose file
    
    readLine_ :: IO String
    readLine_
      | null inputFile = getLine
      | otherwise = do
          file <- openFile inputFile ReadMode
          s <- hGetLine file
          hClose file
          return s

withScope :: IORef [Scope] -> (IORef [Scope] -> IO a) -> IO a
withScope stateRef scopedIO = do
  modifyIORef stateRef (emptyScope :)
  r <- scopedIO stateRef
  modifyIORef stateRef tail
  return r

findInScope :: [Scope] -> (Scope -> Maybe a) -> String -> IO a
findInScope scope looking errorMessage =
  case findInScope_ scope looking of
    Just a -> return a
    Nothing -> throwIO $ interpretError errorMessage

findInScope_ :: [Scope] -> (Scope -> Maybe a) -> Maybe a
findInScope_ scope looking
  | null scope = error "Illegal empty scope"
  | otherwise  = findInScope__ looking scope
  where
    findInScope__ :: (Scope -> Maybe a) -> [Scope] -> Maybe a
    findInScope__ _ [] = Nothing
    findInScope__ looking_ (s:ss) =
      case looking_ s of
        Nothing  -> findInScope__ looking_ ss
        Just obj -> Just obj

interpretLiteral :: Typeable a => RubyType a -> a -> Interpreter Link
interpretLiteral vType = InterpretLink . const . return . Object vType

interpretBinaryOperator
  :: (Object -> Object -> IO Object)
  -> Interpreter Link
  -> Interpreter Link
  -> Interpreter Link
interpretBinaryOperator operator (InterpretLink leftIO) (InterpretLink rightIO) =
  InterpretLink $ \stateRef -> do
    left <- leftIO stateRef
    right <- rightIO stateRef
    operator left right

interpretUnaryOperator
  :: (Object -> IO Object)
  -> Interpreter Link
  -> Interpreter Link
interpretUnaryOperator operator (InterpretLink valueIO) =
  InterpretLink $ valueIO >=> operator

binaryLogic
  :: (Bool -> Bool -> Bool)
  -> Object -> Object -> IO Object
binaryLogic operation leftO rightO = do
  left <- cast leftO RubyBool
  right <- cast rightO RubyBool
  return . Object RubyBool $ operation left right

unaryLogic
  :: (Bool -> Bool)
  -> Object -> IO Object
unaryLogic operation valueO = do
  value <- cast valueO RubyBool
  return . Object RubyBool $ operation value

binaryCompare
  :: Name
  -> (forall a. Ord a => a -> a -> Bool)
  -> Object -> Object -> IO Object
binaryCompare
  name
  comparator
  lObj@(Object lType _)
  rObj@(Object rType _)
  | isNumeric lType rType =
      binaryNumeric name (comparator, RubyBool) (comparator, RubyBool) lObj rObj
  | isString lType = do
      left  <- cast lObj RubyString
      right <- cast rObj RubyString
      return $ Object RubyBool $ comparator left right
  | otherwise = noMethodException name lType

binaryEquals :: Object -> Object -> IO Object
binaryEquals lObj@(Object lType _) rObj@(Object rType _)
  | same lType rType = binaryCompare "==" (==) lObj rObj
  | otherwise        = return $ Object RubyBool False

binaryPlus :: Object -> Object -> IO Object
binaryPlus lObj@(Object lType _) rObj
  | isString lType = do
      left  <- cast lObj RubyString
      right <- cast rObj RubyString
      return $ Object RubyString (left ++ right)
  | otherwise      =
      binaryNumeric "+" ((+), RubyInt) ((+), RubyFloat) lObj rObj

same :: (Typeable lt, Typeable rt) => RubyType lt -> RubyType rt -> Bool
same (_ :: RubyType lT) (_ :: RubyType rT) =
  isJust $ eqT @lT @rT

isNumeric :: RubyType lt -> RubyType rt -> Bool
isNumeric lType rType
  | isNumeric_ lType || isNumeric_ rType = True
  | otherwise                          = False
  where
    isNumeric_ :: RubyType t -> Bool
    isNumeric_ = \case
      RubyInt   -> True
      RubyFloat -> True
      _         -> False

isString :: RubyType t -> Bool
isString = \case
    RubyString -> True
    _          -> False

binaryNumeric
  :: (Typeable i, Typeable d)
  => Name
  -> (Integer -> Integer -> i, RubyType i)
  -> (Double -> Double -> d, RubyType d)
  -> Object -> Object -> IO Object
binaryNumeric
  name
  (onInt, rInt)
  (onFloat, rFloat)
  lObj@(Object lType left)
  rObj@(Object rType right)
    =
  case (lType, rType) of
    (RubyFloat, _) -> do
      right_ <- cast rObj RubyFloat
      return $ Object rFloat $ onFloat left right_
    (_, RubyFloat) -> do
      left_ <- cast lObj RubyFloat
      return $ Object rFloat $ onFloat left_ right
    (RubyInt, _) -> do
      right_ <- cast rObj RubyInt
      return $ Object rInt $ onInt left right_
    (_, RubyInt) -> do
      left_ <- cast rObj RubyInt
      return $ Object rInt $ onInt left_ right
    _ -> noMethodException name lType

noMethodException :: Name -> RubyType t -> IO a
noMethodException name tType =
  throwIO . interpretError $
    "No method '" ++ name ++ "' for " ++ show tType

cast :: Object -> RubyType t -> IO t
cast (Object (oType :: RubyType oT) obj) = \case
  RubyInt    -> noCast RubyInt
  RubyFloat  -> cast2float
  RubyString -> noCast RubyString
  RubyBool   -> cast2bool
  RubyNil    -> noCast RubyNil
  where
    cast2bool :: IO Bool
    cast2bool = return $
      case oType of
        RubyBool -> obj
        RubyNil  -> False
        _        -> True

    cast2float :: IO Double
    cast2float =
      case oType of
        RubyFloat -> return obj
        RubyInt   -> return $ fromInteger obj
        _         -> castException RubyFloat

    noCast :: Typeable t => RubyType t -> IO t
    noCast (tType :: RubyType tT) =
      case eqT @tT @oT of
        Just Refl -> return obj
        Nothing   -> castException tType

    castException :: RubyType t -> IO a
    castException tType =
      throwIO . interpretError $
        "cannot cast type " ++ show oType ++ " to the type" ++ show tType
