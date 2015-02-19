{$mode objfpc}
{Sadece boolean expression parse eden bir program}
unit uparse;
interface uses uast, utools, sysutils;

  function ParseProgram : Node;
  function ParseBlock : Node;
  function ParseExpr: Node;
  var doTrace : boolean = false; // for debugging. enable with -t

implementation

const TAB   = ^I;
const CR    = ^M;
const LF    = ^J;
const EOT   = ^D; // ascii end of transmission

var Look: char;
var Token: String;

procedure trace(s : string); inline;
  begin if doTrace then Writeln(s)
  end;

procedure GetChar;
  begin
    if Eof then Look := EOT
    else read(Look);
    Trace('"' + Look + '"');
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
   IsWhite := c in [' ', TAB,CR,LF];
end;

procedure SkipWhite;
  begin
    while IsWhite(Look) do GetChar
  end;


function GetName: string;
  begin
    SkipWhite;
    Token := '';
    if not IsAlpha(Look) then
      begin
	Write('Saw "', Look ,'",but'); Expected('Name');
      end;
    while IsAlNum(Look) do
      begin
        Token := Token + UpCase(Look);
        GetChar;
      end;
    result := Token;
    SkipWhite;
    Trace('Token='+Token);
  end;


function GetNum: integer;
  var Val: integer;
  begin
    Trace('+GetNum');
    Val := 0;
    if not IsDigit(Look) then Expected('Integer');
    while IsDigit(Look) do
      begin
        Val := 10 * Val + Ord(Look) - Ord('0');
        GetChar;
      end;
    result := Val;
    Trace('-GetNum->' + IntToStr(result));
  end;


procedure Match(x: char);
  begin
    if Look = x then GetChar
    else Expected('''' + x + '''');
    SkipWhite;
  end;

procedure Keyword(s : string);
  var i : integer;
  begin
    Trace('Keyword('+s+')');
    SkipWhite; Token := '';
    if length(s) > 0 then for i := 1 to length(s) do
      begin
	Token += UpCase(Look);
	if UpCase(s[i]) <> UpCase(Look) then
	  Abort('Expected "'+ s +'" but saw: "'+ Token +'"');
	GetChar;
      end;
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
      end;
    // TODO: else Expected('relation')
  end;

function BoolFactor: Node;
  begin
    SkipWhite;
    if Look = '(' then
      begin
        Match('('); result := ParseRelation; Match(')');
      end
    else if IsAlNum(Look) then
      begin {TODO: Bu kýsmý fix et}
        //TempStr := GetName;
        //if UpCase(TempStr) = 'TRUE'     then BoolFactor := true;
        //if UpCase(TempStr) = 'FALSE'    then BoolFactor := false;
	result := ParseRelation;
      end;
  end;

function NotFactor: Node;
  begin
    if Look = '!' then
      begin
        Match('!');
        result := NewUnOp(kNOT, BoolFactor)
      end
    else result := BoolFactor
  end;

function BoolTerm: Node;
  begin
    Trace('+BoolTerm');
    result := NotFactor;
    while Look = '&' do
      begin
        Match('&');
        BoolTerm := NewBinOp(result, kAND, NotFactor);
      end;
    Trace('-BoolTerm');
  end;

function ParseBoolExpr: Node;
  var op : char;
  begin
    Trace('+BoolExpr');
    result := BoolTerm;
    Trace('isorp('+Look+')?');
    while IsOrOp(Look) do
      trace('YES!');
      op := Look; //GetChar;
      begin
        case op of
            '|': result := NewBinOp(result, kOR,  BoolTerm);
            '~': result := NewBinOp(result, kXOR, BoolTerm);
        end;
      end;
    Trace('-BoolExpr');
  end;


function ParseFactor: Node;
  begin
    Trace('+Factor');
    SkipWhite;
    if Look = '(' then
      begin
	Match('(');
	ParseBoolExpr;
	Match(')');
      end
    else if IsAlpha(Look) then result := NewVarExpr(GetName)
    else if IsDigit(Look) then result := NewIntExpr(GetNum)
    else if Look = '-' then result := NewUnOp(kNEG, ParseFactor);
    Trace('-Factor');
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
    result := ParseTerm;  SkipWhite;
    while IsAddop(Look) do
      begin
	op := Look; GetChar;
	case op of
	  '+' : result := NewBinOp(result, kADD, ParseTerm);
	  '-' : result := NewBinOp(result, kSUB, ParseTerm);
	end;
      end;
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
    condition := ParseBoolExpr;
    thenPart := ParseBlock;
    elsePart := nil; //  TODO: parse 'ELSE'
    result := NewIfStmt(condition, thenPart, elsePart);
  end;

function ParseWhileStmt : Node;
  var cond, body : Node;
  begin
    trace('ParseWhileStmt.cond');
    cond := ParseBoolExpr;
    trace('ParseWhileStmt.body');
    keyword('DO');
    body := ParseBlock;
    result := NewWhileStmt(cond,body);
  end;

function ParseWriteStmt : Node;
  begin result := NewWriteStmt(ParseExpr);
  end;

function ParseStmt : Node;
  begin trace('ParseStmt:' + Token);
    case Token of
      'IF'    : result := ParseIfStmt;
      'WHILE' : result := ParseWhileStmt;
      'WRITE' : result := ParseWriteStmt;
      else result := ParseAssignStmt;
    end;
    GetName;
  end;



// -- top level parsing rules ---

function ParseBlock : Node;
  function AtEndToken:boolean;
    begin result := (token = 'END') or (token='ENDIF') or (token='ENDWHILE')
    end;
  begin
    trace('ParseBlock');
    GetName;
    if AtEndToken then result := NewEmptyStmt
    else
      begin
        result := ParseStmt;
        while not AtEndToken do result := NewBinOp(result, kSEQ, ParseStmt);
      end;
  end;

function ParseProgram : Node;
  var decls, block : node;
  begin
    if GetName = 'BEGIN' then block := ParseBlock else Expected('BEGIN');
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
