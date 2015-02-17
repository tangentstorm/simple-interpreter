{$mode delphi}
unit uAST; // abstract syntax tree for simple interpreter
interface

  type
    Node	= ^NodeData;
    NodeKind	= ( kWRITE, kINT, kIF, kASSIGN, kBLOCK );
    NodeData	= record case kind : NodeKind of
		    kINT   : ( vInt  : integer );
		  end;

  function NewIfStmt(condition, thenPart, elsePart : Node) : Node;
  function NewWriteStmt : Node;
  function NewAssignStmt : Node;

implementation

  function NewIfStmt(condition, thenPart, elsePart: Node) : Node;
    begin result := nil end;

  function NewWriteStmt : Node;
    begin result := nil end;

  function NewAssignStmt : Node;
    begin result := nil end;

end.
