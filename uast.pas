{$mode objfpc}
unit uAST; // abstract syntax tree for simple interpreter
interface

  type
    Node      = ^NodeData;
    NodeKind  = ( kINT, kBOOL, kSTR, kVAR,
		  kWRITE, kIF, kASSIGN, kBLOCK );
    DataKind  = kINT .. kVAR;
    NodeData  = record case kind : NodeKind of
		  kINT   : ( int : integer );
		  kVAR   : ( id : string );
		  kWRITE : ( expr : Node );
		end;

  function NewIntExpr(int : Integer) : Node;
  function NewVarExpr(id : string) : Node;

  function NewIfStmt(condition, thenPart, elsePart : Node) : Node;
  function NewWriteStmt( expr : Node ) : Node;
  function NewAssignStmt : Node;

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
    begin result := nil end;

end.
