{$mode objfpc}
unit uAST; // abstract syntax tree for simple interpreter
interface uses utools;
  type
    Node      = ^NodeData;
    NodeList  = array of Node;
    NodeKind  = ( kINT, kBOOL, kSTR, kVAR,
                  kNEG, kNOT,
                  kADD, kSUB, kMUL, kDIV, kMOD,
                  kXOR, kAND, kOR,
                  kLT, KGT, kEQ, kNE, kLE, KGE,
                  kSEQ, // sequences of statements
                  kOK, kWRITE, kIF, kWHILE, kASSIGN, kBLOCK, kPROG );
    UnOp      = kNEG .. kNOT; // unary operators (1 argument)
    BinOp     = kADD .. kSEQ; // binary operators (2 arguments)
    DataKind  = kINT .. kVAR;
    NodeData  = record
		  nid : cardinal;
		case kind: NodeKind of
                  kINT   : ( int : integer );
                  kNEG, kNOT : ( arg : Node );
                  kADD..kSEQ : ( arg0, arg1 : Node );
                  kVAR   : ( id : string );
                  kWRITE : ( expr : Node );
                  kIF    : ( condition, thenPart, elsePart : Node );
                  kWHILE : ( whileCond, whileBody : Node );
                  kASSIGN: ( assignId : string; assignVal : Node );
                  kPROG  : ( block : Node );
                end;
  const kBinChars : array[BinOp] of string =
          ('+','-','*','/','%','~','&','|','<','>',' = ','#','≤','≥',';');

  function NewIntExpr(int : Integer) : Node;
  function NewVarExpr(id : string) : Node;
  function NewBinOp(x : Node; op: BinOp; y : Node) : Node;
  function NewUnOp(op : UnOp; y : Node) : Node;
  function NewIfStmt(condition, thenPart, elsePart : Node) : Node;
  function NewWriteStmt(expr : Node) : Node;
  function NewWhileStmt(cond, body : Node) : Node;
  function NewAssignStmt(id : string; val : Node) : Node;
  function NewProgram(block : Node) : Node;
  function NewEmptyStmt : Node;

  function Nodes(ns: array of node): NodeList;
  function Children(n:Node): NodeList; // mostly for uviz
  procedure DumpNode(n:Node; depth:integer=0); // for debugging



implementation

  var nodecount : cardinal = 0;

  procedure New(var n : Node; kind: NodeKind);
    begin System.New(n); n^.kind := kind; n^.nid := nodecount; inc(nodecount)
    end;


  function NewIntExpr(int : Integer) : Node;
    begin New(result, kINT); result^.int := int;
    end;

  function NewVarExpr(id : string) : Node;
    begin New(result, kVAR); result^.id := id;
    end;

  function NewBinOp( x : Node; op: BinOp; y : Node ) : Node;
    begin New(result, op); result^.arg0 := x; result^.arg1 := y;
    end;

  function NewUnOp( op : UnOp; y : Node ) : Node;
    begin New(result, op); result^.arg := y;
    end;

  function NewIfStmt(condition, thenPart, elsePart: Node) : Node;
    begin New(result, kIF); result^.condition := condition;
          result^.thenPart := thenPart; result^.elsePart := elsePart;
    end;

  function NewEmptyStmt : Node;
    begin New(result, kOK);
    end;

  function NewWriteStmt( expr : Node ) : Node;
    begin New(result, kWRITE); result^.expr := expr;
    end;

  function NewWhileStmt(cond, body : Node) : Node;
    begin
      New(result, kWHILE);
      result^.whileCond := cond; result^.whileBody := body;
    end;

  function NewAssignStmt(id : string; val : Node) : Node;
    begin New(result, kASSIGN); result^.assignId := id; result^.assignVal := val;
    end;

  function NewProgram(block : Node) : Node;
    begin New(result, kPROG); result^.block := block;
    end;

  function Nodes(ns: array of node): NodeList;
    var i: integer;
    begin
      SetLength(result, length(ns));
      if length(ns) > 0 then
        for i := 0 to High(ns) do result[i] := ns[i];
    end;

  function Children(n:Node): NodeList;
    begin
      if (n^.kind) in [Low(BinOp)..High(BinOp)] then
        result := Nodes([n^.arg0, n^.arg1])
      else case n^.kind of
        kNEG : result := Nodes([n^.arg]);
        kWRITE : result := Nodes([n^.expr]);
        kASSIGN: result := Nodes([n^.assignVal]);
	kWHILE : result := Nodes([n^.whileCond, n^.whileBody]);
	kIF : result := Nodes([n^.condition, n^.thenPart, n^.elsePart]);
        kPROG : result := Nodes([n^.block]);
        otherwise result := Nodes([]);
      end;
    end;


  procedure DumpNode(n : Node; depth:integer=0);
    procedure indent; var j: integer;
      begin if depth>0 then for j:=0 to depth do write(' ')
      end;
    begin
      if n = nil then write('<NIL>')
      else if not Assigned(n) then write('<BAD>')
      else if (n^.kind) in [kADD..kGE] then
	begin
	  write('('); DumpNode(n^.arg0);
	  write(' ', kBinChars[n^.kind], ' ');
	  DumpNode(n^.arg1); write(')');
	end
      else case n^.kind of
        kOK : write('OK');
        kINT : write(n^.int);
        kVAR : write(n^.id);
        kNEG : begin write('-'); DumpNode(n^.arg); end;
        kWRITE : begin
                   indent; write('[write '); DumpNode(n^.expr); writeln(']')
                 end;
        kASSIGN: begin
                   indent; write('[', n^.assignId, ' := ');
                   DumpNode(n^.assignVal); writeln(']')
                 end;
        kSEQ : begin DumpNode(n^.arg0, depth); DumpNode(n^.arg1, depth); end;
	kWHILE : begin indent;
                   write('[while '); DumpNode(n^.whileCond); writeln(' do ');
                   DumpNode(n^.whileBody, depth+1); writeln(']');
                 end;
	kIF : begin
                indent; write('[if '); DumpNode(n^.condition); writeln;
                indent; write(' then '); DumpNode(n^.thenPart, depth+1);
                indent; write(' else '); DumpNode(n^.elsePart, depth+1);
                writeln(']');
              end;
        kPROG : DumpNode(n^.block, depth);
        otherwise writeln('<UNKNOWN: ', n^.kind ,'>');
      end;
    end;

end.
