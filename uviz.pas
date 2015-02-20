// For generating a graphviz-compatible diagram for an AST.
{$mode objfpc}
unit uviz;
interface uses uast;

  procedure WriteDOT(n : Node);
  procedure EmitNode(n : Node);
  function Caption(n : Node) : string;

implementation

  function Caption(n : Node) : string;
    begin
      if n^.kind in [low(BinOp)..High(BinOp)] then
	result := kBinChars[n^.kind]
      else if n^.kind = kINT then WriteStr(result, n^.int)
      else if n^.kind = kVAR then result := n^.id
      else if n^.kind = kASSIGN then result := n^.assignId + ' :='
      else begin
	WriteStr(caption, n^.kind); // eg kWRITE
	Delete(caption, 1, 1);      // strip off the 'k'
	result := lowercase(caption);
      end
    end;

  procedure EmitNode(n : Node);
    var kid : Node;
    begin
      Write('  n', n^.nid, '[label="', Caption(n),'"');
      if n^.kind in [Low(DataKind)..High(DataKind)] then
	WriteLn(' shape="square"]')
      else WriteLn(']');
      // two loops through children just to make the DOT code look nicer.
      for kid in Children(n) do WriteLn('    n', n^.nid, ' -> n', kid^.nid);
      for kid in Children(n) do EmitNode(kid);
    end;

 procedure WriteDOT(n : Node);
   begin
     WriteLn('digraph {');
     EmitNode(n);
     WriteLn('}');
   end;

begin
end.
