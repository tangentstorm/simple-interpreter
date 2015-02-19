{$mode objfpc}
{Sadece boolean expression parse eden bir program}
unit uparse;
interface uses uast, utools, sysutils;

  function Prog : Node;
  function Block(EndTokens : array of string) : Node;
  function Expr: Node;
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
    if Eof then Look := EOT else read(Look);
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
    IsMulop := c in ['*', '/', '%'];
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

function Relation: Node;
  var op : char;
  begin
    result := Expr;
    if IsRelop(Look) then
      begin
	op := Look; GetChar;
	case op of
	  // TODO : <=, >=, == using Look
	  '=' : result := NewBinOp(result, kEQ, Expr);
	  '<' : result := NewBinOp(result, kLT, Expr);
	  '>' : result := NewBinOp(result, kGT, Expr);
	  '#' : result := NewBinOp(result, kNE, Expr);
	end;
      end;
    // TODO: else Expected('relation')
  end;

function BoolFactor: Node;
  begin
    SkipWhite;
    if Look = '(' then
      begin
        Match('('); result := Relation; Match(')');
      end
    else if IsAlNum(Look) then
      begin {TODO: Bu kýsmý fix et}
        //TempStr := GetName;
        //if UpCase(TempStr) = 'TRUE'     then BoolFactor := true;
        //if UpCase(TempStr) = 'FALSE'    then BoolFactor := false;
	result := Relation;
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

function BoolExpr: Node;
  var op : char;
  begin
    Trace('+BoolExpr');
    result := BoolTerm;
    while IsOrOp(Look) do
      op := Look; //GetChar;
      begin
        case op of
            '|': result := NewBinOp(result, kOR,  BoolTerm);
            '~': result := NewBinOp(result, kXOR, BoolTerm);
        end;
      end;
    Trace('-BoolExpr');
  end;


function Factor: Node;
  begin
    Trace('+Factor');
    SkipWhite;
    if Look = '(' then
      begin
	Match('(');
	result := Expr;
	Match(')');
      end
    else if IsAlpha(Look) then result := NewVarExpr(GetName)
    else if IsDigit(Look) then result := NewIntExpr(GetNum)
    else if Look = '-' then
      begin
	match('-');
	result := NewUnOp(kNEG, Expr);
      end;
    Trace('-Factor');
  end;

function Term: Node;
  var op : char;
  begin
    Trace('+Term');
    result := Factor;
    while IsMulop(Look) do
      begin
	op := Look; GetChar;
	case op of
	  '*' : result := NewBinOp(result, kMUL, Factor);
	  '/' : result := NewBinOp(result, kDIV, Factor);
	  '%' : result := NewBinOp(result, kMOD, Factor);
	end;
      end;
    Trace('-Term');
  end;


function Expr: Node;
  var op : char;
  begin
    Trace('+Expr');
    result := Term;  SkipWhite;
    while IsAddop(Look) do
      begin
	op := Look; GetChar;
	case op of
	  '+' : result := NewBinOp(result, kADD, Term);
	  '-' : result := NewBinOp(result, kSUB, Term);
	end;
      end;
    Trace('-Expr');
  end;


// -- statements ---

function AssignStmt : Node;
  var id : string;
  begin
    id := token; match('=');
    result := NewAssignStmt(id, Expr);
  end;

function IfStmt : Node;
  var condition, thenPart, elsePart : Node;
  begin
    condition := BoolExpr;
    keyword('THEN');
    thenPart := Block(['ELSE', 'ENDIF']);
    if token = 'ELSE' then elsePart := Block(['ENDIF'])
    else elsePart := EmptyStmt;
    if token = 'ENDIF' then {ok} else expected('ENDIF');
    result := NewIfStmt(condition, thenPart, elsePart);
  end;

function WhileStmt : Node;
  var cond, body : Node;
  begin
    trace('WhileStmt.cond');
    cond := BoolExpr;
    trace('WhileStmt.body');
    keyword('DO');
    body := Block(['ENDWHILE']);
    result := NewWhileStmt(cond,body);
  end;

function WriteStmt : Node;
  begin result := NewWriteStmt(Expr);
  end;

function Stmt : Node;
  begin trace('Stmt:' + Token);
    case Token of
      'IF'    : result := IfStmt;
      'WHILE' : result := WhileStmt;
      'WRITE' : result := WriteStmt;
      else result := AssignStmt;
    end;
    GetName;
  end;



// -- top level parsing rules ---

function Block(EndTokens : array Of string) : Node;
  function AtEndToken:boolean;
    var i:integer;
    begin
      result := false;
      for i := low(EndTokens) to High(EndTokens) do
	result := result or (token = EndTokens[i]);
    end;
  begin
    trace('Block');
    GetName;
    if AtEndToken then result := EmptyStmt
    else
      begin
        result := Stmt;
        while not AtEndToken do result := NewBinOp(result, kSEQ, Stmt);
      end;
  end;

function Prog : Node;
  var aBlock : node;
  begin
    if GetName = 'BEGIN' then aBlock := Block(['END'])
    else Expected('BEGIN');
    result := NewProgram(aBlock);
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
