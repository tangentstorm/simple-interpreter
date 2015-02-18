{$mode objfpc}
unit uAST; // abstract syntax tree for simple interpreter
interface uses utools;

  type
    Node      = ^NodeData;
    NodeKind  = ( kINT, kBOOL, kSTR, kVAR,
                  kNEG, kNOT,
                  kADD, kSUB, kMUL, kDIV,
                  kXOR, kAND, kOR,
                  kLT, KGT, kEQ, kNE, kLE, KGE,
                  kSEQ, kOK,  // sequences of statements, empty statement
                  kWRITE, kIF, kASSIGN, kBLOCK, kPROG );
    UnOp      = kNEG .. kNOT; // unary operators (1 argument)
    BinOp     = kADD .. kSEQ; // binary operators (2 arguments)
    DataKind  = kINT .. kVAR;
    NodeData  = record case kind : NodeKind of
                  kOK    : ();
                  kINT   : ( int : integer );
                  kNEG, kNOT : ( arg : Node );
                  kADD..kSEQ : ( arg0, arg1 : Node );
                  kVAR   : ( id : string );
                  kWRITE : ( expr : Node );
                  kASSIGN: ( assignId : string; assignVal : Node );
                  kPROG  : ( vars, block : Node );
                end;

  function NewIntExpr(int : Integer) : Node;
  function NewVarExpr(id : string) : Node;

  function NewBinOp( x : Node; op: BinOp; y : Node ) : Node;
  function NewUnOp( op : UnOp; y : Node ) : Node;

  function NewIfStmt(condition, thenPart, elsePart : Node) : Node;
  function NewWriteStmt( expr : Node ) : Node;
  function NewAssignStmt(id : string; val : Node) : Node;
  function NewEmptyStmt : Node;

  function NewProgram(vars, block : Node) : Node;

  procedure DumpNode(n:Node; depth:integer=0); // for debugging


implementation

  procedure New(var n : Node; kind: NodeKind);
    begin System.New(n); n^.kind := kind;
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
    begin result := nil end;

  function NewWriteStmt( expr : Node ) : Node;
    begin New(result, kWRITE); result^.expr := expr;
    end;

  function NewAssignStmt(id : string; val : Node) : Node;
    begin New(result, kASSIGN); result^.assignId := id; result^.assignVal := val;
    end;

  function NewEmptyStmt : Node;
    begin New(result, kOK);
    end;

  function NewProgram(vars, block : Node) : Node;
    begin New(result, kPROG); result^.vars := vars; result^.block := block;
    end;

  procedure DumpNode(n : Node; depth:integer=0);
    procedure indent; var j: integer;
      begin if depth>0 then for j:=0 to depth do write(' ')
      end;
    begin
      if n = nil then write('<NIL>')
      else if not Assigned(n) then write('<BAD>')
      else case n^.kind of
        kINT   : write(n^.int);
        kVAR   : write(n^.id);
        kWRITE : begin
                   indent; write('[write '); DumpNode(n^.expr); writeln(']')
                 end;
        kASSIGN: begin
                   indent; write('[', n^.assignId, ' := ');
                   DumpNode(n^.assignVal); writeln(']')
                 end;
        kSEQ : begin DumpNode(n^.arg0); DumpNode(n^.arg1); end;
        kPROG : begin
                  indent; writeln('[prog ');
                  DumpNode(n^.vars, depth+1); DumpNode(n^.block, depth+1);
                  writeln(']');
                end;
        otherwise writeln('<UNKNOWN: ', n^.kind ,'>');
      end;
    end;

end.
