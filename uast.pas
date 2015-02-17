{$mode objfpc}
unit uAST; // abstract syntax tree for simple interpreter
interface uses utools;

  type
    Node      = ^NodeData;
    NodeKind  = ( kINT, kBOOL, kSTR, kVAR,
		  kWRITE, kIF, kASSIGN, kBLOCK, kVARS, kPROG );
    DataKind  = kINT .. kVAR;
    NodeData  = record case kind : NodeKind of
		  kINT   : ( int : integer );
		  kVAR   : ( id : string );
		  kWRITE : ( expr : Node );
                  kVARS  : ( names : strings );
                  kPROG  : ( vars, block : Node );
		end;

  function NewIntExpr(int : Integer) : Node;
  function NewVarExpr(id : string) : Node;

  function NewIfStmt(condition, thenPart, elsePart : Node) : Node;
  function NewWriteStmt( expr : Node ) : Node;
  function NewAssignStmt : Node;

  function NewVarDecls(names : strings) : Node;
  function NewProgram(vars, block : Node) : Node;

implementation

  procedure New(var n : Node; kind: NodeKind);
    begin
      System.New(n); n^.kind := kind;
    end;

  function NewIntExpr(int : Integer) : Node;
    begin
      New(result, kINT); result^.int := int;
    end;

  function NewVarExpr(id : string) : Node;
    begin
      New(result, kVAR); result^.id := id;
    end;

  function NewIfStmt(condition, thenPart, elsePart: Node) : Node;
    begin result := nil end;

  function NewWriteStmt( expr : Node ) : Node;
    begin
      New(result, kWRITE); result^.expr := expr;
    end;

  function NewAssignStmt : Node;
    begin result := nil
    end;

  function NewVarDecls(names : strings) : Node;
    begin
      New(result, kVARS); result^.names := names;
    end;

  function NewProgram(vars, block : Node) : Node;
    begin
      New(result, kPROG); result^.vars := vars; result^.block := block;
    end;

end.
