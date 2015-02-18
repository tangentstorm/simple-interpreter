{$mode objfpc}
{Sadece boolean expression parse eden bir program}
unit uparse;
interface uses uast, utools;

  function ParseProgram : Node;
  function ParseBlock : Node;
  function ParseExpr: Node;

implementation

const TAB   = ^I;
const CR    = ^M;
const LF    = ^J;
const EOT   = ^D; // ascii end of transmission

var Look: char;
var Token: String;


procedure GetChar;
begin
   if Eof then Look := EOT
   else read(Look);
end;

procedure Error(s: string);
begin
   WriteLn;
   WriteLn(^G, 'Error: ', s, '.');
end;

procedure Abort(s: string);
begin
   Error(s);
   Halt;
end;

procedure Expected(s: string);
begin
   Abort(s + ' Expected');
end;

function IsAlpha(c: char): boolean;
begin
   IsAlpha := upcase(c) in ['A'..'Z'];
end;

function IsDigit(c: char): boolean;
begin
   IsDigit := c in ['0'..'9'];
end;

function IsAlNum(c: char): boolean;
begin
   IsAlNum := IsAlpha(c) or IsDigit(c);
end;

function IsAddop(c: char): boolean;
begin
   IsAddop := c in ['+', '-'];
end;

function IsMulop(c: char): boolean;
begin
   IsMulop := c in ['*', '/'];
end;

function IsWhite(c: char): boolean;
begin
   IsWhite := c in [' ', TAB];
end;

procedure SkipWhite;
begin
   while IsWhite(Look) do
      GetChar;
end;

procedure NewLine;
begin
    while Look in [CR, LF] do
    begin
        GetChar;
        SkipWhite;
    end;
end;

function GetName: string;
var TempStr: string;
begin
    TempStr := '';
    NewLine;
    if not IsAlpha(Look) then Expected('Name');

    while IsAlNum(Look) do
    begin
        TempStr := TempStr + UpCase(Look);
        GetChar;
    end;
    GetName := TempStr;
    SkipWhite;
end;


function GetNum: integer;
var Val: integer;
begin
    Val := 0;
    if not IsDigit(Look) then Expected('Integer');
    while IsDigit(Look) do
    begin
        Val := 10 * Val + Ord(Look) - Ord('0');
        GetChar;
    end;
    GetNum := Val;
    SkipWhite;
end;

procedure Match(x: char);
begin
   if Look = x then GetChar
   else Expected('''' + x + '''');
   SkipWhite;
end;

function IsOrop(c: char): boolean;
begin
   IsOrop := c in ['|', '~'];
end;

function IsRelop(c: char): boolean;
begin
   IsRelop := c in ['=', '#', '<', '>'];
end;

function ParseRelation: Node;
  var op : char;
  begin
   result := ParseExpr;
   if IsRelop(Look) then
     begin
       op := Look; GetChar;
       case op of
         // TODO : <=, >=, == using Look
	 '=' : result := NewBinOp(result, kEQ, ParseExpr);
	 '<' : result := NewBinOp(result, kLT, ParseExpr);
	 '>' : result := NewBinOp(result, kGT, ParseExpr);
	 '#' : result := NewBinOp(result, kNE, ParseExpr);
       end;
     end
    // TODO: else Expected('relation')
  end;

function BoolFactor: Node;
  begin
    if Look = '(' then
    begin
        Match('(');
        BoolFactor := ParseRelation;
        Match(')');
    end;
    if IsAlNum(Look) then
    begin {TODO: Bu kýsmý fix et}
        //TempStr := GetName;
        //if UpCase(TempStr) = 'TRUE'     then BoolFactor := true;
        //if UpCase(TempStr) = 'FALSE'    then BoolFactor := false;
        BoolFactor := ParseRelation;
    end;
  end;

function NotFactor: Node;
begin
    if Look = '!' then
    begin
        Match('!');
        result := NewUnOp(kNOT, BoolFactor);
    end else result := BoolFactor;

end;

function BoolTerm: Node;
  begin
    result := NotFactor;
    while Look = '&' do
    begin
        Match('&');
        BoolTerm := NewBinOp(result, kAND, NotFactor);
    end;
  end;

function BoolExpression: Node;
  var op : char;
  begin
    result := BoolTerm;
    while IsOrOp(Look) do
      op := Look; GetChar;
      begin
        case op of
            '|': result := NewBinOp(result, kOR,  BoolTerm);
            '~': result := NewBinOp(result, kXOR, BoolTerm);
        end;
      end;
    {
    Boolean expressioný tanýmlayan grameri burda yazalým.
    <b-expression> ::= <b-term> [<orop> <b-term>]*
    <b-term>       ::= <not-factor> [AND <not-factor>]*
    <not-factor>   ::= [NOT] <b-factor>
    <b-factor>     ::= <b-literal> | <b-variable> | <relation>
    <relation>     ::= | <expression> [<relop> <expression]
    <expression>   ::= <term> [<addop> <term>]*
    <term>         ::= <signed factor> [<mulop> factor]*
    <signed factor>::= [<addop>] <factor>
    <factor>       ::= <integer> | <variable> | (<b-expression>)
    }
  end;

function ParseFactor: Node;
  begin
    if Look = '(' then
      begin
	Match('(');
	BoolExpression;
	Match(')');
      end
    else if IsAlpha(Look) then result := NewVarExpr(GetName)
    else if IsDigit(Look) then result := NewIntExpr(GetNum)
    else if Look = '-' then result := NewUnOp(kNEG, ParseFactor);
  end;

function ParseTerm: Node;
  var op : char;
  begin
    result := ParseFactor;
    while IsMulop(Look) do
      begin
	op := Look; GetChar;
	case op of
	  '*' : result := NewBinOp(result, kMUL, ParseFactor);
	  '/' : result := NewBinOp(result, kDIV, ParseFactor);
	end;
      end;
  end;

function ParseExpr: Node;
  var op : char;
  begin
    result := ParseTerm;
    while IsAddop(Look) do
      begin
	op := Look; GetChar;
	case op of
	  '+' : result := NewBinOp(result, kADD, ParseTerm);
	  '-' : result := NewBinOp(result, kSUB, ParseTerm);
	end;
      end;
  end;


// keyword consumes a word and checks that it matches the expected string s.
function keyword(s:string; out tok:string) : boolean;
  begin
    tok := GetName;
    result := tok = s;
  end;

// -- statements ---

function ParseAssignStmt : Node;
  var id : string;
  begin
    id := token; match('=');
    result := NewAssignStmt(id, ParseExpr);
  end;

function ParseIfStmt : Node;
  var condition, thenPart, elsePart : Node;
  begin
    condition := nil; {TODO: }BoolExpression;
    thenPart := ParseBlock;
    elsePart := nil; //  TODO: parse 'ELSE'
    result := NewIfStmt(condition, thenPart, elsePart);
  end;

function ParseWriteStmt : Node;
  begin
    result := NewWriteStmt(ParseExpr);
  end;

function ParseStmt : Node;
  begin
    case Token of
      'IF'	: result := ParseIfStmt;
      'WRITE'	: result := ParseWriteStmt;
      else result := ParseAssignStmt;
    end;
    Token := GetName;
  end;

function ParseBlock : Node;
  begin
    Token := GetName;
    if Token = 'END' then
      result := NewEmptyStmt
    else
      begin
        result := ParseStmt;
        while (Token <> 'END') and (Token <> 'ENDIF') do
          result := NewBinOp(result, kSEQ, ParseStmt);
      end;
  end;


// -- top level parsing rules ---

function ParseProgram : Node;
  var decls, block : node;
  begin
    token := GetName;
    if token = 'BEGIN' then block := ParseBlock else Expected('BEGIN');
    result := NewProgram(decls, block);
  end;


// -- main entry point ---

procedure Init;
  begin
    Token := '';
    GetChar;
  end;

initialization
  Init;
end.
