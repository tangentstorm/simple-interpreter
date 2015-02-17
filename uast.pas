{$mode delphi}
unit uAST; // abstract syntax tree for simple interpreter
interface

  type
    Node	= ^NodeData;
    NodeKind	= ( kWRITE, kINT, kIF, kASSIGN, kBLOCK );
    NodeData	= record case kind : NodeKind of
		    kINT   : ( int  : integer );
		    kWRITE : ( expr : Node );
		  end;

  function NewIntExpr(int : Integer) : Node;
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

  function NewIfStmt(condition, thenPart, elsePart: Node) : Node;
    begin result := nil end;

  function NewWriteStmt( expr : Node ) : Node;
    begin
      New(result, kWRITE); result^.expr := expr;
    end;

  function NewAssignStmt : Node;
    begin result := nil end;

end.
