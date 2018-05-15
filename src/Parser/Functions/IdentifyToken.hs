module Parser.Functions.IdentifyToken where

import Data.Int (Int64)

import qualified Lexer.Types.Token as Tokens
import Lexer.Types.PositionWrapper

isKeywordAbs :: Tokens.WrappedToken -> Bool
isKeywordAbs (PosnWrapper _ (Tokens.Keyword Tokens.Abs)) = True
isKeywordAbs _ = False

isKeywordAccess :: Tokens.WrappedToken -> Bool
isKeywordAccess (PosnWrapper _ (Tokens.Keyword Tokens.Access)) = True
isKeywordAccess _ = False

isKeywordAfter :: Tokens.WrappedToken -> Bool
isKeywordAfter (PosnWrapper _ (Tokens.Keyword Tokens.After)) = True
isKeywordAfter _ = False

isKeywordAlias :: Tokens.WrappedToken -> Bool
isKeywordAlias (PosnWrapper _ (Tokens.Keyword Tokens.Alias)) = True
isKeywordAlias _ = False

isKeywordAll :: Tokens.WrappedToken -> Bool
isKeywordAll (PosnWrapper _ (Tokens.Keyword Tokens.All)) = True
isKeywordAll _ = False

isKeywordAnd :: Tokens.WrappedToken -> Bool
isKeywordAnd (PosnWrapper _ (Tokens.Keyword Tokens.And)) = True
isKeywordAnd _ = False

isKeywordArchitecture :: Tokens.WrappedToken -> Bool
isKeywordArchitecture (PosnWrapper _ (Tokens.Keyword Tokens.Architecture)) = True
isKeywordArchitecture _ = False

isKeywordArray :: Tokens.WrappedToken -> Bool
isKeywordArray (PosnWrapper _ (Tokens.Keyword Tokens.Array)) = True
isKeywordArray _ = False

isKeywordAssert :: Tokens.WrappedToken -> Bool
isKeywordAssert (PosnWrapper _ (Tokens.Keyword Tokens.Assert)) = True
isKeywordAssert _ = False

isKeywordAttribute :: Tokens.WrappedToken -> Bool
isKeywordAttribute (PosnWrapper _ (Tokens.Keyword Tokens.Attribute)) = True
isKeywordAttribute _ = False

isKeywordBegin :: Tokens.WrappedToken -> Bool
isKeywordBegin (PosnWrapper _ (Tokens.Keyword Tokens.Begin)) = True
isKeywordBegin _ = False

isKeywordBlock :: Tokens.WrappedToken -> Bool
isKeywordBlock (PosnWrapper _ (Tokens.Keyword Tokens.Block)) = True
isKeywordBlock _ = False

isKeywordBody :: Tokens.WrappedToken -> Bool
isKeywordBody (PosnWrapper _ (Tokens.Keyword Tokens.Body)) = True
isKeywordBody _ = False

isKeywordBuffer :: Tokens.WrappedToken -> Bool
isKeywordBuffer (PosnWrapper _ (Tokens.Keyword Tokens.Buffer)) = True
isKeywordBuffer _ = False

isKeywordBus :: Tokens.WrappedToken -> Bool
isKeywordBus (PosnWrapper _ (Tokens.Keyword Tokens.Bus)) = True
isKeywordBus _ = False

isKeywordCase :: Tokens.WrappedToken -> Bool
isKeywordCase (PosnWrapper _ (Tokens.Keyword Tokens.Case)) = True
isKeywordCase _ = False

isKeywordComponent :: Tokens.WrappedToken -> Bool
isKeywordComponent (PosnWrapper _ (Tokens.Keyword Tokens.Component)) = True
isKeywordComponent _ = False

isKeywordConfiguration :: Tokens.WrappedToken -> Bool
isKeywordConfiguration (PosnWrapper _ (Tokens.Keyword Tokens.Configuration)) = True
isKeywordConfiguration _ = False

isKeywordConstant :: Tokens.WrappedToken -> Bool
isKeywordConstant (PosnWrapper _ (Tokens.Keyword Tokens.Constant)) = True
isKeywordConstant _ = False

isKeywordDisconnect :: Tokens.WrappedToken -> Bool
isKeywordDisconnect (PosnWrapper _ (Tokens.Keyword Tokens.Disconnect)) = True
isKeywordDisconnect _ = False

isKeywordDownto :: Tokens.WrappedToken -> Bool
isKeywordDownto (PosnWrapper _ (Tokens.Keyword Tokens.Downto)) = True
isKeywordDownto _ = False

isKeywordElse :: Tokens.WrappedToken -> Bool
isKeywordElse (PosnWrapper _ (Tokens.Keyword Tokens.Else)) = True
isKeywordElse _ = False

isKeywordElsif :: Tokens.WrappedToken -> Bool
isKeywordElsif (PosnWrapper _ (Tokens.Keyword Tokens.Elsif)) = True
isKeywordElsif _ = False

isKeywordEnd :: Tokens.WrappedToken -> Bool
isKeywordEnd (PosnWrapper _ (Tokens.Keyword Tokens.End)) = True
isKeywordEnd _ = False

isKeywordEntity :: Tokens.WrappedToken -> Bool
isKeywordEntity (PosnWrapper _ (Tokens.Keyword Tokens.Entity)) = True
isKeywordEntity _ = False

isKeywordExit :: Tokens.WrappedToken -> Bool
isKeywordExit (PosnWrapper _ (Tokens.Keyword Tokens.Exit)) = True
isKeywordExit _ = False

isKeywordFile :: Tokens.WrappedToken -> Bool
isKeywordFile (PosnWrapper _ (Tokens.Keyword Tokens.File)) = True
isKeywordFile _ = False

isKeywordFor :: Tokens.WrappedToken -> Bool
isKeywordFor (PosnWrapper _ (Tokens.Keyword Tokens.For)) = True
isKeywordFor _ = False

isKeywordFunction :: Tokens.WrappedToken -> Bool
isKeywordFunction (PosnWrapper _ (Tokens.Keyword Tokens.Function)) = True
isKeywordFunction _ = False

isKeywordGenerate :: Tokens.WrappedToken -> Bool
isKeywordGenerate (PosnWrapper _ (Tokens.Keyword Tokens.Generate)) = True
isKeywordGenerate _ = False

isKeywordGeneric :: Tokens.WrappedToken -> Bool
isKeywordGeneric (PosnWrapper _ (Tokens.Keyword Tokens.Generic)) = True
isKeywordGeneric _ = False

isKeywordGuarded :: Tokens.WrappedToken -> Bool
isKeywordGuarded (PosnWrapper _ (Tokens.Keyword Tokens.Guarded)) = True
isKeywordGuarded _ = False

isKeywordIf :: Tokens.WrappedToken -> Bool
isKeywordIf (PosnWrapper _ (Tokens.Keyword Tokens.If)) = True
isKeywordIf _ = False

isKeywordIn :: Tokens.WrappedToken -> Bool
isKeywordIn (PosnWrapper _ (Tokens.Keyword Tokens.In)) = True
isKeywordIn _ = False

isKeywordInout :: Tokens.WrappedToken -> Bool
isKeywordInout (PosnWrapper _ (Tokens.Keyword Tokens.Inout)) = True
isKeywordInout _ = False

isKeywordIs :: Tokens.WrappedToken -> Bool
isKeywordIs (PosnWrapper _ (Tokens.Keyword Tokens.Is)) = True
isKeywordIs _ = False

isKeywordLabel :: Tokens.WrappedToken -> Bool
isKeywordLabel (PosnWrapper _ (Tokens.Keyword Tokens.Label)) = True
isKeywordLabel _ = False

isKeywordLibrary :: Tokens.WrappedToken -> Bool
isKeywordLibrary (PosnWrapper _ (Tokens.Keyword Tokens.Library)) = True
isKeywordLibrary _ = False

isKeywordLinkage :: Tokens.WrappedToken -> Bool
isKeywordLinkage (PosnWrapper _ (Tokens.Keyword Tokens.Linkage)) = True
isKeywordLinkage _ = False

isKeywordLoop :: Tokens.WrappedToken -> Bool
isKeywordLoop (PosnWrapper _ (Tokens.Keyword Tokens.Loop)) = True
isKeywordLoop _ = False

isKeywordMap :: Tokens.WrappedToken -> Bool
isKeywordMap (PosnWrapper _ (Tokens.Keyword Tokens.Map)) = True
isKeywordMap _ = False

isKeywordMod :: Tokens.WrappedToken -> Bool
isKeywordMod (PosnWrapper _ (Tokens.Keyword Tokens.Mod)) = True
isKeywordMod _ = False

isKeywordNand :: Tokens.WrappedToken -> Bool
isKeywordNand (PosnWrapper _ (Tokens.Keyword Tokens.Nand)) = True
isKeywordNand _ = False

isKeywordNew :: Tokens.WrappedToken -> Bool
isKeywordNew (PosnWrapper _ (Tokens.Keyword Tokens.New)) = True
isKeywordNew _ = False

isKeywordNext :: Tokens.WrappedToken -> Bool
isKeywordNext (PosnWrapper _ (Tokens.Keyword Tokens.Next)) = True
isKeywordNext _ = False

isKeywordNor :: Tokens.WrappedToken -> Bool
isKeywordNor (PosnWrapper _ (Tokens.Keyword Tokens.Nor)) = True
isKeywordNor _ = False

isKeywordNot :: Tokens.WrappedToken -> Bool
isKeywordNot (PosnWrapper _ (Tokens.Keyword Tokens.Not)) = True
isKeywordNot _ = False

isKeywordNull :: Tokens.WrappedToken -> Bool
isKeywordNull (PosnWrapper _ (Tokens.Keyword Tokens.Null)) = True
isKeywordNull _ = False

isKeywordOf :: Tokens.WrappedToken -> Bool
isKeywordOf (PosnWrapper _ (Tokens.Keyword Tokens.Of)) = True
isKeywordOf _ = False

isKeywordOn :: Tokens.WrappedToken -> Bool
isKeywordOn (PosnWrapper _ (Tokens.Keyword Tokens.On)) = True
isKeywordOn _ = False

isKeywordOpen :: Tokens.WrappedToken -> Bool
isKeywordOpen (PosnWrapper _ (Tokens.Keyword Tokens.Open)) = True
isKeywordOpen _ = False

isKeywordOr :: Tokens.WrappedToken -> Bool
isKeywordOr (PosnWrapper _ (Tokens.Keyword Tokens.Or)) = True
isKeywordOr _ = False

isKeywordOthers :: Tokens.WrappedToken -> Bool
isKeywordOthers (PosnWrapper _ (Tokens.Keyword Tokens.Others)) = True
isKeywordOthers _ = False

isKeywordOut :: Tokens.WrappedToken -> Bool
isKeywordOut (PosnWrapper _ (Tokens.Keyword Tokens.Out)) = True
isKeywordOut _ = False

isKeywordPackage :: Tokens.WrappedToken -> Bool
isKeywordPackage (PosnWrapper _ (Tokens.Keyword Tokens.Package)) = True
isKeywordPackage _ = False

isKeywordPort :: Tokens.WrappedToken -> Bool
isKeywordPort (PosnWrapper _ (Tokens.Keyword Tokens.Port)) = True
isKeywordPort _ = False

isKeywordProcedure :: Tokens.WrappedToken -> Bool
isKeywordProcedure (PosnWrapper _ (Tokens.Keyword Tokens.Procedure)) = True
isKeywordProcedure _ = False

isKeywordProcess :: Tokens.WrappedToken -> Bool
isKeywordProcess (PosnWrapper _ (Tokens.Keyword Tokens.Process)) = True
isKeywordProcess _ = False

isKeywordRange :: Tokens.WrappedToken -> Bool
isKeywordRange (PosnWrapper _ (Tokens.Keyword Tokens.Range)) = True
isKeywordRange _ = False

isKeywordRecord :: Tokens.WrappedToken -> Bool
isKeywordRecord (PosnWrapper _ (Tokens.Keyword Tokens.Record)) = True
isKeywordRecord _ = False

isKeywordRegister :: Tokens.WrappedToken -> Bool
isKeywordRegister (PosnWrapper _ (Tokens.Keyword Tokens.Register)) = True
isKeywordRegister _ = False

isKeywordRem :: Tokens.WrappedToken -> Bool
isKeywordRem (PosnWrapper _ (Tokens.Keyword Tokens.Rem)) = True
isKeywordRem _ = False

isKeywordReport :: Tokens.WrappedToken -> Bool
isKeywordReport (PosnWrapper _ (Tokens.Keyword Tokens.Report)) = True
isKeywordReport _ = False

isKeywordReturn :: Tokens.WrappedToken -> Bool
isKeywordReturn (PosnWrapper _ (Tokens.Keyword Tokens.Return)) = True
isKeywordReturn _ = False

isKeywordSelect :: Tokens.WrappedToken -> Bool
isKeywordSelect (PosnWrapper _ (Tokens.Keyword Tokens.Select)) = True
isKeywordSelect _ = False

isKeywordSeverity :: Tokens.WrappedToken -> Bool
isKeywordSeverity (PosnWrapper _ (Tokens.Keyword Tokens.Severity)) = True
isKeywordSeverity _ = False

isKeywordSignal :: Tokens.WrappedToken -> Bool
isKeywordSignal (PosnWrapper _ (Tokens.Keyword Tokens.Signal)) = True
isKeywordSignal _ = False

isKeywordSubtype :: Tokens.WrappedToken -> Bool
isKeywordSubtype (PosnWrapper _ (Tokens.Keyword Tokens.Subtype)) = True
isKeywordSubtype _ = False

isKeywordThen :: Tokens.WrappedToken -> Bool
isKeywordThen (PosnWrapper _ (Tokens.Keyword Tokens.Then)) = True
isKeywordThen _ = False

isKeywordTo :: Tokens.WrappedToken -> Bool
isKeywordTo (PosnWrapper _ (Tokens.Keyword Tokens.To)) = True
isKeywordTo _ = False

isKeywordTransport :: Tokens.WrappedToken -> Bool
isKeywordTransport (PosnWrapper _ (Tokens.Keyword Tokens.Transport)) = True
isKeywordTransport _ = False

isKeywordType :: Tokens.WrappedToken -> Bool
isKeywordType (PosnWrapper _ (Tokens.Keyword Tokens.Type)) = True
isKeywordType _ = False

isKeywordUnits :: Tokens.WrappedToken -> Bool
isKeywordUnits (PosnWrapper _ (Tokens.Keyword Tokens.Units)) = True
isKeywordUnits _ = False

isKeywordUntil :: Tokens.WrappedToken -> Bool
isKeywordUntil (PosnWrapper _ (Tokens.Keyword Tokens.Until)) = True
isKeywordUntil _ = False

isKeywordUse :: Tokens.WrappedToken -> Bool
isKeywordUse (PosnWrapper _ (Tokens.Keyword Tokens.Use)) = True
isKeywordUse _ = False

isKeywordVariable :: Tokens.WrappedToken -> Bool
isKeywordVariable (PosnWrapper _ (Tokens.Keyword Tokens.Variable)) = True
isKeywordVariable _ = False

isKeywordWait :: Tokens.WrappedToken -> Bool
isKeywordWait (PosnWrapper _ (Tokens.Keyword Tokens.Wait)) = True
isKeywordWait _ = False

isKeywordWhen :: Tokens.WrappedToken -> Bool
isKeywordWhen (PosnWrapper _ (Tokens.Keyword Tokens.When)) = True
isKeywordWhen _ = False

isKeywordWhile :: Tokens.WrappedToken -> Bool
isKeywordWhile (PosnWrapper _ (Tokens.Keyword Tokens.While)) = True
isKeywordWhile _ = False

isKeywordWith :: Tokens.WrappedToken -> Bool
isKeywordWith (PosnWrapper _ (Tokens.Keyword Tokens.With)) = True
isKeywordWith _ = False

isKeywordXor :: Tokens.WrappedToken -> Bool
isKeywordXor (PosnWrapper _ (Tokens.Keyword Tokens.Xor)) = True
isKeywordXor _ = False

isArrow :: Tokens.WrappedToken -> Bool
isArrow (PosnWrapper _ (Tokens.Operator Tokens.Arrow)) = True
isArrow _ = False

isDoubleStar :: Tokens.WrappedToken -> Bool
isDoubleStar (PosnWrapper _ (Tokens.Operator Tokens.DoubleStar)) = True
isDoubleStar _ = False

isVarAssign :: Tokens.WrappedToken -> Bool
isVarAssign (PosnWrapper _ (Tokens.Operator Tokens.VarAssign)) = True
isVarAssign _ = False

isInequality :: Tokens.WrappedToken -> Bool
isInequality (PosnWrapper _ (Tokens.Operator Tokens.Inequality)) = True
isInequality _ = False

isGreaterThanOrEqual :: Tokens.WrappedToken -> Bool
isGreaterThanOrEqual (PosnWrapper _ (Tokens.Operator Tokens.GreaterThanOrEqual)) = True
isGreaterThanOrEqual _ = False

isSignAssign :: Tokens.WrappedToken -> Bool
isSignAssign (PosnWrapper _ (Tokens.Operator Tokens.SignAssign)) = True
isSignAssign _ = False

isBox :: Tokens.WrappedToken -> Bool
isBox (PosnWrapper _ (Tokens.Operator Tokens.Box)) = True
isBox _ = False

isAmpersand :: Tokens.WrappedToken -> Bool
isAmpersand (PosnWrapper _ (Tokens.Operator Tokens.Ampersand)) = True
isAmpersand _ = False

isApostrophe :: Tokens.WrappedToken -> Bool
isApostrophe (PosnWrapper _ (Tokens.Operator Tokens.Apostrophe)) = True
isApostrophe _ = False

isLeftParen :: Tokens.WrappedToken -> Bool
isLeftParen (PosnWrapper _ (Tokens.Operator Tokens.LeftParen)) = True
isLeftParen _ = False

isRightParen :: Tokens.WrappedToken -> Bool
isRightParen (PosnWrapper _ (Tokens.Operator Tokens.RightParen)) = True
isRightParen _ = False

isStar :: Tokens.WrappedToken -> Bool
isStar (PosnWrapper _ (Tokens.Operator Tokens.Star)) = True
isStar _ = False

isPlus :: Tokens.WrappedToken -> Bool
isPlus (PosnWrapper _ (Tokens.Operator Tokens.Plus)) = True
isPlus _ = False

isComma :: Tokens.WrappedToken -> Bool
isComma (PosnWrapper _ (Tokens.Operator Tokens.Comma)) = True
isComma _ = False

isHyphen :: Tokens.WrappedToken -> Bool
isHyphen (PosnWrapper _ (Tokens.Operator Tokens.Hyphen)) = True
isHyphen _ = False

isPeriod :: Tokens.WrappedToken -> Bool
isPeriod (PosnWrapper _ (Tokens.Operator Tokens.Period)) = True
isPeriod _ = False

isSlash :: Tokens.WrappedToken -> Bool
isSlash (PosnWrapper _ (Tokens.Operator Tokens.Slash)) = True
isSlash _ = False

isColon :: Tokens.WrappedToken -> Bool
isColon (PosnWrapper _ (Tokens.Operator Tokens.Colon)) = True
isColon _ = False

isSemicolon :: Tokens.WrappedToken -> Bool
isSemicolon (PosnWrapper _ (Tokens.Operator Tokens.Semicolon)) = True
isSemicolon _ = False

isLessThan :: Tokens.WrappedToken -> Bool
isLessThan (PosnWrapper _ (Tokens.Operator Tokens.LessThan)) = True
isLessThan _ = False

isEqual :: Tokens.WrappedToken -> Bool
isEqual (PosnWrapper _ (Tokens.Operator Tokens.Equal)) = True
isEqual _ = False

isGreaterThan :: Tokens.WrappedToken -> Bool
isGreaterThan (PosnWrapper _ (Tokens.Operator Tokens.GreaterThan)) = True
isGreaterThan _ = False

isBar :: Tokens.WrappedToken -> Bool
isBar (PosnWrapper _ (Tokens.Operator Tokens.Bar)) = True
isBar _ = False

isIdentifier :: Tokens.WrappedToken -> Bool
isIdentifier (PosnWrapper _ (Tokens.Identifier _)) = True
isIdentifier _ = False

matchIdentifier :: Tokens.WrappedToken -> Maybe (PosnWrapper String)
matchIdentifier token =
   case unPos token of
      Tokens.Identifier iden -> Just $ PosnWrapper (getPos token) iden
      _ -> Nothing

isInteger :: Tokens.WrappedToken -> Bool
isInteger (PosnWrapper _ (Tokens.Literal (Tokens.Univ_Int _))) = True
isInteger _ = False

matchInteger :: Tokens.WrappedToken -> Maybe (PosnWrapper Int64)
matchInteger token =
   case unPos token of
      Tokens.Literal (Tokens.Univ_Int int) -> Just $ PosnWrapper (getPos token) int
      _ -> Nothing

isReal :: Tokens.WrappedToken -> Bool
isReal (PosnWrapper _ (Tokens.Literal (Tokens.Univ_Real _))) = True
isReal _ = False

isBitstr :: Tokens.WrappedToken -> Bool
isBitstr (PosnWrapper _ (Tokens.Literal (Tokens.BitStr _ _))) = True
isBitstr _ = False

isStr :: Tokens.WrappedToken -> Bool
isStr (PosnWrapper _ (Tokens.Literal (Tokens.Str _))) = True
isStr _ = False

isChar :: Tokens.WrappedToken -> Bool
isChar (PosnWrapper _ (Tokens.Literal (Tokens.Character _))) = True
isChar _ = False

matchChar :: Tokens.WrappedToken -> Maybe (PosnWrapper Char)
matchChar token =
   case unPos token of
      Tokens.Literal (Tokens.Character chr) -> Just $ PosnWrapper (getPos token) chr
      _ -> Nothing
