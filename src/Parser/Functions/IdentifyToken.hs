module Parser.Functions.IdentifyToken where

import qualified Lexer.Types.Token as Tokens
import Lexer.Types.PositionWrapper

isKeywordAbs :: PosnWrapper Tokens.Token -> Bool
isKeywordAbs (PosnWrapper _ (Tokens.Keyword Tokens.Abs)) = True
isKeywordAbs _ = False

isKeywordAccess :: PosnWrapper Tokens.Token -> Bool
isKeywordAccess (PosnWrapper _ (Tokens.Keyword Tokens.Access)) = True
isKeywordAccess _ = False

isKeywordAfter :: PosnWrapper Tokens.Token -> Bool
isKeywordAfter (PosnWrapper _ (Tokens.Keyword Tokens.After)) = True
isKeywordAfter _ = False

isKeywordAlias :: PosnWrapper Tokens.Token -> Bool
isKeywordAlias (PosnWrapper _ (Tokens.Keyword Tokens.Alias)) = True
isKeywordAlias _ = False

isKeywordAll :: PosnWrapper Tokens.Token -> Bool
isKeywordAll (PosnWrapper _ (Tokens.Keyword Tokens.All)) = True
isKeywordAll _ = False

isKeywordAnd :: PosnWrapper Tokens.Token -> Bool
isKeywordAnd (PosnWrapper _ (Tokens.Keyword Tokens.And)) = True
isKeywordAnd _ = False

isKeywordArchitecture :: PosnWrapper Tokens.Token -> Bool
isKeywordArchitecture (PosnWrapper _ (Tokens.Keyword Tokens.Architecture)) = True
isKeywordArchitecture _ = False

isKeywordArray :: PosnWrapper Tokens.Token -> Bool
isKeywordArray (PosnWrapper _ (Tokens.Keyword Tokens.Array)) = True
isKeywordArray _ = False

isKeywordAssert :: PosnWrapper Tokens.Token -> Bool
isKeywordAssert (PosnWrapper _ (Tokens.Keyword Tokens.Assert)) = True
isKeywordAssert _ = False

isKeywordAttribute :: PosnWrapper Tokens.Token -> Bool
isKeywordAttribute (PosnWrapper _ (Tokens.Keyword Tokens.Attribute)) = True
isKeywordAttribute _ = False

isKeywordBegin :: PosnWrapper Tokens.Token -> Bool
isKeywordBegin (PosnWrapper _ (Tokens.Keyword Tokens.Begin)) = True
isKeywordBegin _ = False

isKeywordBlock :: PosnWrapper Tokens.Token -> Bool
isKeywordBlock (PosnWrapper _ (Tokens.Keyword Tokens.Block)) = True
isKeywordBlock _ = False

isKeywordBody :: PosnWrapper Tokens.Token -> Bool
isKeywordBody (PosnWrapper _ (Tokens.Keyword Tokens.Body)) = True
isKeywordBody _ = False

isKeywordBuffer :: PosnWrapper Tokens.Token -> Bool
isKeywordBuffer (PosnWrapper _ (Tokens.Keyword Tokens.Buffer)) = True
isKeywordBuffer _ = False

isKeywordBus :: PosnWrapper Tokens.Token -> Bool
isKeywordBus (PosnWrapper _ (Tokens.Keyword Tokens.Bus)) = True
isKeywordBus _ = False

isKeywordCase :: PosnWrapper Tokens.Token -> Bool
isKeywordCase (PosnWrapper _ (Tokens.Keyword Tokens.Case)) = True
isKeywordCase _ = False

isKeywordComponent :: PosnWrapper Tokens.Token -> Bool
isKeywordComponent (PosnWrapper _ (Tokens.Keyword Tokens.Component)) = True
isKeywordComponent _ = False

isKeywordConfiguration :: PosnWrapper Tokens.Token -> Bool
isKeywordConfiguration (PosnWrapper _ (Tokens.Keyword Tokens.Configuration)) = True
isKeywordConfiguration _ = False

isKeywordConstant :: PosnWrapper Tokens.Token -> Bool
isKeywordConstant (PosnWrapper _ (Tokens.Keyword Tokens.Constant)) = True
isKeywordConstant _ = False

isKeywordDisconnect :: PosnWrapper Tokens.Token -> Bool
isKeywordDisconnect (PosnWrapper _ (Tokens.Keyword Tokens.Disconnect)) = True
isKeywordDisconnect _ = False

isKeywordDownto :: PosnWrapper Tokens.Token -> Bool
isKeywordDownto (PosnWrapper _ (Tokens.Keyword Tokens.Downto)) = True
isKeywordDownto _ = False

isKeywordElse :: PosnWrapper Tokens.Token -> Bool
isKeywordElse (PosnWrapper _ (Tokens.Keyword Tokens.Else)) = True
isKeywordElse _ = False

isKeywordElsif :: PosnWrapper Tokens.Token -> Bool
isKeywordElsif (PosnWrapper _ (Tokens.Keyword Tokens.Elsif)) = True
isKeywordElsif _ = False

isKeywordEnd :: PosnWrapper Tokens.Token -> Bool
isKeywordEnd (PosnWrapper _ (Tokens.Keyword Tokens.End)) = True
isKeywordEnd _ = False

isKeywordEntity :: PosnWrapper Tokens.Token -> Bool
isKeywordEntity (PosnWrapper _ (Tokens.Keyword Tokens.Entity)) = True
isKeywordEntity _ = False

isKeywordExit :: PosnWrapper Tokens.Token -> Bool
isKeywordExit (PosnWrapper _ (Tokens.Keyword Tokens.Exit)) = True
isKeywordExit _ = False

isKeywordFile :: PosnWrapper Tokens.Token -> Bool
isKeywordFile (PosnWrapper _ (Tokens.Keyword Tokens.File)) = True
isKeywordFile _ = False

isKeywordFor :: PosnWrapper Tokens.Token -> Bool
isKeywordFor (PosnWrapper _ (Tokens.Keyword Tokens.For)) = True
isKeywordFor _ = False

isKeywordFunction :: PosnWrapper Tokens.Token -> Bool
isKeywordFunction (PosnWrapper _ (Tokens.Keyword Tokens.Function)) = True
isKeywordFunction _ = False

isKeywordGenerate :: PosnWrapper Tokens.Token -> Bool
isKeywordGenerate (PosnWrapper _ (Tokens.Keyword Tokens.Generate)) = True
isKeywordGenerate _ = False

isKeywordGeneric :: PosnWrapper Tokens.Token -> Bool
isKeywordGeneric (PosnWrapper _ (Tokens.Keyword Tokens.Generic)) = True
isKeywordGeneric _ = False

isKeywordGuarded :: PosnWrapper Tokens.Token -> Bool
isKeywordGuarded (PosnWrapper _ (Tokens.Keyword Tokens.Guarded)) = True
isKeywordGuarded _ = False

isKeywordIf :: PosnWrapper Tokens.Token -> Bool
isKeywordIf (PosnWrapper _ (Tokens.Keyword Tokens.If)) = True
isKeywordIf _ = False

isKeywordIn :: PosnWrapper Tokens.Token -> Bool
isKeywordIn (PosnWrapper _ (Tokens.Keyword Tokens.In)) = True
isKeywordIn _ = False

isKeywordInout :: PosnWrapper Tokens.Token -> Bool
isKeywordInout (PosnWrapper _ (Tokens.Keyword Tokens.Inout)) = True
isKeywordInout _ = False

isKeywordIs :: PosnWrapper Tokens.Token -> Bool
isKeywordIs (PosnWrapper _ (Tokens.Keyword Tokens.Is)) = True
isKeywordIs _ = False

isKeywordLabel :: PosnWrapper Tokens.Token -> Bool
isKeywordLabel (PosnWrapper _ (Tokens.Keyword Tokens.Label)) = True
isKeywordLabel _ = False

isKeywordLibrary :: PosnWrapper Tokens.Token -> Bool
isKeywordLibrary (PosnWrapper _ (Tokens.Keyword Tokens.Library)) = True
isKeywordLibrary _ = False

isKeywordLinkage :: PosnWrapper Tokens.Token -> Bool
isKeywordLinkage (PosnWrapper _ (Tokens.Keyword Tokens.Linkage)) = True
isKeywordLinkage _ = False

isKeywordLoop :: PosnWrapper Tokens.Token -> Bool
isKeywordLoop (PosnWrapper _ (Tokens.Keyword Tokens.Loop)) = True
isKeywordLoop _ = False

isKeywordMap :: PosnWrapper Tokens.Token -> Bool
isKeywordMap (PosnWrapper _ (Tokens.Keyword Tokens.Map)) = True
isKeywordMap _ = False

isKeywordMod :: PosnWrapper Tokens.Token -> Bool
isKeywordMod (PosnWrapper _ (Tokens.Keyword Tokens.Mod)) = True
isKeywordMod _ = False

isKeywordNand :: PosnWrapper Tokens.Token -> Bool
isKeywordNand (PosnWrapper _ (Tokens.Keyword Tokens.Nand)) = True
isKeywordNand _ = False

isKeywordNew :: PosnWrapper Tokens.Token -> Bool
isKeywordNew (PosnWrapper _ (Tokens.Keyword Tokens.New)) = True
isKeywordNew _ = False

isKeywordNext :: PosnWrapper Tokens.Token -> Bool
isKeywordNext (PosnWrapper _ (Tokens.Keyword Tokens.Next)) = True
isKeywordNext _ = False

isKeywordNor :: PosnWrapper Tokens.Token -> Bool
isKeywordNor (PosnWrapper _ (Tokens.Keyword Tokens.Nor)) = True
isKeywordNor _ = False

isKeywordNot :: PosnWrapper Tokens.Token -> Bool
isKeywordNot (PosnWrapper _ (Tokens.Keyword Tokens.Not)) = True
isKeywordNot _ = False

isKeywordNull :: PosnWrapper Tokens.Token -> Bool
isKeywordNull (PosnWrapper _ (Tokens.Keyword Tokens.Null)) = True
isKeywordNull _ = False

isKeywordOf :: PosnWrapper Tokens.Token -> Bool
isKeywordOf (PosnWrapper _ (Tokens.Keyword Tokens.Of)) = True
isKeywordOf _ = False

isKeywordOn :: PosnWrapper Tokens.Token -> Bool
isKeywordOn (PosnWrapper _ (Tokens.Keyword Tokens.On)) = True
isKeywordOn _ = False

isKeywordOpen :: PosnWrapper Tokens.Token -> Bool
isKeywordOpen (PosnWrapper _ (Tokens.Keyword Tokens.Open)) = True
isKeywordOpen _ = False

isKeywordOr :: PosnWrapper Tokens.Token -> Bool
isKeywordOr (PosnWrapper _ (Tokens.Keyword Tokens.Or)) = True
isKeywordOr _ = False

isKeywordOthers :: PosnWrapper Tokens.Token -> Bool
isKeywordOthers (PosnWrapper _ (Tokens.Keyword Tokens.Others)) = True
isKeywordOthers _ = False

isKeywordOut :: PosnWrapper Tokens.Token -> Bool
isKeywordOut (PosnWrapper _ (Tokens.Keyword Tokens.Out)) = True
isKeywordOut _ = False

isKeywordPackage :: PosnWrapper Tokens.Token -> Bool
isKeywordPackage (PosnWrapper _ (Tokens.Keyword Tokens.Package)) = True
isKeywordPackage _ = False

isKeywordPort :: PosnWrapper Tokens.Token -> Bool
isKeywordPort (PosnWrapper _ (Tokens.Keyword Tokens.Port)) = True
isKeywordPort _ = False

isKeywordProcedure :: PosnWrapper Tokens.Token -> Bool
isKeywordProcedure (PosnWrapper _ (Tokens.Keyword Tokens.Procedure)) = True
isKeywordProcedure _ = False

isKeywordProcess :: PosnWrapper Tokens.Token -> Bool
isKeywordProcess (PosnWrapper _ (Tokens.Keyword Tokens.Process)) = True
isKeywordProcess _ = False

isKeywordRange :: PosnWrapper Tokens.Token -> Bool
isKeywordRange (PosnWrapper _ (Tokens.Keyword Tokens.Range)) = True
isKeywordRange _ = False

isKeywordRecord :: PosnWrapper Tokens.Token -> Bool
isKeywordRecord (PosnWrapper _ (Tokens.Keyword Tokens.Record)) = True
isKeywordRecord _ = False

isKeywordRegister :: PosnWrapper Tokens.Token -> Bool
isKeywordRegister (PosnWrapper _ (Tokens.Keyword Tokens.Register)) = True
isKeywordRegister _ = False

isKeywordRem :: PosnWrapper Tokens.Token -> Bool
isKeywordRem (PosnWrapper _ (Tokens.Keyword Tokens.Rem)) = True
isKeywordRem _ = False

isKeywordReport :: PosnWrapper Tokens.Token -> Bool
isKeywordReport (PosnWrapper _ (Tokens.Keyword Tokens.Report)) = True
isKeywordReport _ = False

isKeywordReturn :: PosnWrapper Tokens.Token -> Bool
isKeywordReturn (PosnWrapper _ (Tokens.Keyword Tokens.Return)) = True
isKeywordReturn _ = False

isKeywordSelect :: PosnWrapper Tokens.Token -> Bool
isKeywordSelect (PosnWrapper _ (Tokens.Keyword Tokens.Select)) = True
isKeywordSelect _ = False

isKeywordSeverity :: PosnWrapper Tokens.Token -> Bool
isKeywordSeverity (PosnWrapper _ (Tokens.Keyword Tokens.Severity)) = True
isKeywordSeverity _ = False

isKeywordSignal :: PosnWrapper Tokens.Token -> Bool
isKeywordSignal (PosnWrapper _ (Tokens.Keyword Tokens.Signal)) = True
isKeywordSignal _ = False

isKeywordSubtype :: PosnWrapper Tokens.Token -> Bool
isKeywordSubtype (PosnWrapper _ (Tokens.Keyword Tokens.Subtype)) = True
isKeywordSubtype _ = False

isKeywordThen :: PosnWrapper Tokens.Token -> Bool
isKeywordThen (PosnWrapper _ (Tokens.Keyword Tokens.Then)) = True
isKeywordThen _ = False

isKeywordTo :: PosnWrapper Tokens.Token -> Bool
isKeywordTo (PosnWrapper _ (Tokens.Keyword Tokens.To)) = True
isKeywordTo _ = False

isKeywordTransport :: PosnWrapper Tokens.Token -> Bool
isKeywordTransport (PosnWrapper _ (Tokens.Keyword Tokens.Transport)) = True
isKeywordTransport _ = False

isKeywordType :: PosnWrapper Tokens.Token -> Bool
isKeywordType (PosnWrapper _ (Tokens.Keyword Tokens.Type)) = True
isKeywordType _ = False

isKeywordUnits :: PosnWrapper Tokens.Token -> Bool
isKeywordUnits (PosnWrapper _ (Tokens.Keyword Tokens.Units)) = True
isKeywordUnits _ = False

isKeywordUntil :: PosnWrapper Tokens.Token -> Bool
isKeywordUntil (PosnWrapper _ (Tokens.Keyword Tokens.Until)) = True
isKeywordUntil _ = False

isKeywordUse :: PosnWrapper Tokens.Token -> Bool
isKeywordUse (PosnWrapper _ (Tokens.Keyword Tokens.Use)) = True
isKeywordUse _ = False

isKeywordVariable :: PosnWrapper Tokens.Token -> Bool
isKeywordVariable (PosnWrapper _ (Tokens.Keyword Tokens.Variable)) = True
isKeywordVariable _ = False

isKeywordWait :: PosnWrapper Tokens.Token -> Bool
isKeywordWait (PosnWrapper _ (Tokens.Keyword Tokens.Wait)) = True
isKeywordWait _ = False

isKeywordWhen :: PosnWrapper Tokens.Token -> Bool
isKeywordWhen (PosnWrapper _ (Tokens.Keyword Tokens.When)) = True
isKeywordWhen _ = False

isKeywordWhile :: PosnWrapper Tokens.Token -> Bool
isKeywordWhile (PosnWrapper _ (Tokens.Keyword Tokens.While)) = True
isKeywordWhile _ = False

isKeywordWith :: PosnWrapper Tokens.Token -> Bool
isKeywordWith (PosnWrapper _ (Tokens.Keyword Tokens.With)) = True
isKeywordWith _ = False

isKeywordXor :: PosnWrapper Tokens.Token -> Bool
isKeywordXor (PosnWrapper _ (Tokens.Keyword Tokens.Xor)) = True
isKeywordXor _ = False

isArrow :: PosnWrapper Tokens.Token -> Bool
isArrow (PosnWrapper _ (Tokens.Operator Tokens.Arrow)) = True
isArrow _ = False

isDoubleStar :: PosnWrapper Tokens.Token -> Bool
isDoubleStar (PosnWrapper _ (Tokens.Operator Tokens.DoubleStar)) = True
isDoubleStar _ = False

isVarAssign :: PosnWrapper Tokens.Token -> Bool
isVarAssign (PosnWrapper _ (Tokens.Operator Tokens.VarAssign)) = True
isVarAssign _ = False

isInequality :: PosnWrapper Tokens.Token -> Bool
isInequality (PosnWrapper _ (Tokens.Operator Tokens.Inequality)) = True
isInequality _ = False

isGreaterThanOrEqual :: PosnWrapper Tokens.Token -> Bool
isGreaterThanOrEqual (PosnWrapper _ (Tokens.Operator Tokens.GreaterThanOrEqual)) = True
isGreaterThanOrEqual _ = False

isSignAssign :: PosnWrapper Tokens.Token -> Bool
isSignAssign (PosnWrapper _ (Tokens.Operator Tokens.SignAssign)) = True
isSignAssign _ = False

isBox :: PosnWrapper Tokens.Token -> Bool
isBox (PosnWrapper _ (Tokens.Operator Tokens.Box)) = True
isBox _ = False

isAmpersand :: PosnWrapper Tokens.Token -> Bool
isAmpersand (PosnWrapper _ (Tokens.Operator Tokens.Ampersand)) = True
isAmpersand _ = False

isApostrophe :: PosnWrapper Tokens.Token -> Bool
isApostrophe (PosnWrapper _ (Tokens.Operator Tokens.Apostrophe)) = True
isApostrophe _ = False

isLeftParen :: PosnWrapper Tokens.Token -> Bool
isLeftParen (PosnWrapper _ (Tokens.Operator Tokens.LeftParen)) = True
isLeftParen _ = False

isRightParen :: PosnWrapper Tokens.Token -> Bool
isRightParen (PosnWrapper _ (Tokens.Operator Tokens.RightParen)) = True
isRightParen _ = False

isStar :: PosnWrapper Tokens.Token -> Bool
isStar (PosnWrapper _ (Tokens.Operator Tokens.Star)) = True
isStar _ = False

isPlus :: PosnWrapper Tokens.Token -> Bool
isPlus (PosnWrapper _ (Tokens.Operator Tokens.Plus)) = True
isPlus _ = False

isComma :: PosnWrapper Tokens.Token -> Bool
isComma (PosnWrapper _ (Tokens.Operator Tokens.Comma)) = True
isComma _ = False

isHyphen :: PosnWrapper Tokens.Token -> Bool
isHyphen (PosnWrapper _ (Tokens.Operator Tokens.Hyphen)) = True
isHyphen _ = False

isPeriod :: PosnWrapper Tokens.Token -> Bool
isPeriod (PosnWrapper _ (Tokens.Operator Tokens.Period)) = True
isPeriod _ = False

isSlash :: PosnWrapper Tokens.Token -> Bool
isSlash (PosnWrapper _ (Tokens.Operator Tokens.Slash)) = True
isSlash _ = False

isColon :: PosnWrapper Tokens.Token -> Bool
isColon (PosnWrapper _ (Tokens.Operator Tokens.Colon)) = True
isColon _ = False

isSemicolon :: PosnWrapper Tokens.Token -> Bool
isSemicolon (PosnWrapper _ (Tokens.Operator Tokens.Semicolon)) = True
isSemicolon _ = False

isLessThan :: PosnWrapper Tokens.Token -> Bool
isLessThan (PosnWrapper _ (Tokens.Operator Tokens.LessThan)) = True
isLessThan _ = False

isEqual :: PosnWrapper Tokens.Token -> Bool
isEqual (PosnWrapper _ (Tokens.Operator Tokens.Equal)) = True
isEqual _ = False

isGreaterThan :: PosnWrapper Tokens.Token -> Bool
isGreaterThan (PosnWrapper _ (Tokens.Operator Tokens.GreaterThan)) = True
isGreaterThan _ = False

isBar :: PosnWrapper Tokens.Token -> Bool
isBar (PosnWrapper _ (Tokens.Operator Tokens.Bar)) = True
isBar _ = False

isIdentifier :: PosnWrapper Tokens.Token -> Bool
isIdentifier (PosnWrapper _ (Tokens.Identifier _)) = True
isIdentifier _ = False

matchIdentifier :: PosnWrapper Tokens.Token -> Maybe (PosnWrapper String)
matchIdentifier token =
   case unPos token of
      Tokens.Identifier iden -> Just $ PosnWrapper (getPos token) iden
      _ -> Nothing

isInteger :: PosnWrapper Tokens.Token -> Bool
isInteger (PosnWrapper _ (Tokens.Literal (Tokens.Univ_Int _))) = True
isInteger _ = False

isReal :: PosnWrapper Tokens.Token -> Bool
isReal (PosnWrapper _ (Tokens.Literal (Tokens.Univ_Real _))) = True
isReal _ = False

isBitstr :: PosnWrapper Tokens.Token -> Bool
isBitstr (PosnWrapper _ (Tokens.Literal (Tokens.BitStr _ _))) = True
isBitstr _ = False

isStr :: PosnWrapper Tokens.Token -> Bool
isStr (PosnWrapper _ (Tokens.Literal (Tokens.Str _))) = True
isStr _ = False

isChar :: PosnWrapper Tokens.Token -> Bool
isChar (PosnWrapper _ (Tokens.Literal (Tokens.Character _))) = True
isChar _ = False
