{$mode objfpc}
{Sadece boolean expression parse eden bir program}
unit uparse;
interface uses uast;

  function Block : Node;

implementation

const TAB   = ^I;
const CR    = ^M;
const LF    = ^J;
const EOT   = ^D; // ascii end of transmission

var Look: char;
var Token: String;

function Expression: integer; Forward;

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

function Greater: integer;
begin
    Match('>');
    Greater := Expression;
end;

function Less: integer;
begin
    Match('<');
    Less := Expression;
end;

function NotEquals: integer;
begin
   Match('#');
   NotEquals := Expression;
end;

function Equals: integer;
begin
   Match('=');
   Equals := Expression;
end;

function Relation: boolean;
var TempNumber: integer;
begin
   TempNumber := Expression;
   if IsRelop(Look) then
   begin
      case Look of
       '=': Relation := TempNumber  =    Equals;
       '<': Relation := TempNumber  <    Less;
       '>': Relation := TempNumber  >    Greater;
       '#': Relation := TempNumber  <>   NotEquals;
      end;
   end;
end;

function BoolFactor: boolean;
begin
    if Look = '(' then
    begin
        Match('(');
        BoolFactor := Relation;
        Match(')');
    end;
    if IsAlNum(Look) then
    begin {TODO: Bu kýsmý fix et}
        //TempStr := GetName;
        //if UpCase(TempStr) = 'TRUE'     then BoolFactor := true;
        //if UpCase(TempStr) = 'FALSE'    then BoolFactor := false;
        BoolFactor := Relation;
    end;
end;

function NotFactor: boolean;
begin
    if Look = '!' then
    begin
        Match('!');
        NotFactor := Not BoolFactor;
    end else NotFactor := BoolFactor;

end;

function BoolTerm: boolean;
begin
    BoolTerm := NotFactor;
    while Look = '&' do
    begin
        Match('&');
        BoolTerm := BoolTerm and NotFactor;
    end;
end;

function BoolOr: boolean;
begin
   Match('|');
   BoolOr := BoolTerm;
end;

procedure BoolXor;
begin
   Match('~');
   BoolTerm;
end;

function BoolExpression: boolean;
begin
    BoolExpression := BoolTerm;
    while IsOrOp(Look) do
    begin
        case Look of
            '|': BoolExpression := BoolExpression or BoolOr;
            //'~': BoolXor;
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

function Factor: integer;
begin
    if Look = '(' then
    begin
        Match('(');
        BoolExpression;
        Match(')');
    end;

    if IsAlpha(Look) then Token  := GetName;
    if IsDigit(Look) then Factor := GetNum;
end;

function NegFactor: integer;
begin
   Match('-');
   if IsDigit(Look) then
      NegFactor := -GetNum
   else
   begin
      NegFactor := Factor;
   end;
end;

function Multiply: integer;
begin
   Match('*');
   Multiply := Factor;
end;

function Divide: integer;
begin
   Match('/');
   Divide := Factor;
end;

function Term: integer;
begin
    Term := Factor;
    while IsMulop(Look) do
    begin
        case Look of
            '*': Term := Term * Multiply;
            '/': Term := Term div Divide; {Bu satýr gidip aþaðýdaki gelecek. Þimdilik bunla idare edelim}
            //'/': Term := Term / Divide;
        end;
    end;
end;

function Add: integer;
begin
   Match('+');
   Add := Term;
end;

function Subtract: integer;
begin
   Match('-');
   Subtract := Term;
end;

function Expression: integer;
begin
   Expression := Term;
   while IsAddop(Look) do
   begin
      case Look of
       '+': Expression := Expression + Add;
       '-': Expression := Expression - Subtract;
      end;
   end;
end;

function ParseExpr : Node;
  begin
    result := NewIntExpr(Expression);
  end;

function Assignment : Node;
begin
    Writeln('Assignment');
    result := NewAssignStmt();
end;

// keyword consumes a token.
function keyword(s:string; out tok:string) : boolean;
  begin
    tok := GetName;
    result := tok = s;
  end;


function DoIf : Node;
var condition, thenPart, elsePart : Node;
begin
    condition := nil; {TODO: }BoolExpression;
    thenPart := Block;
    elsePart := nil; //  TODO: parse 'ELSE'
    result := NewIfStmt(condition, thenPart, elsePart);
end;

function ParseWriteStmt : Node;
begin
  result := NewWriteStmt(ParseExpr);
end;

function Block : Node;
begin
    // TODO: compose a Block to hold the statements.
    Token := GetName;
    while (Token <> 'END') and (Token <> 'ENDIF') do
    begin
      case Token of
	'IF'	: result := DoIf;
	'WRITE'	: result := ParseWriteStmt;
	else result := Assignment;
      end;
      Token := GetName;
    end;
end;

procedure Init;
begin
    Token := '';
    GetChar;
end;

begin
  Init;
end.
