// simple interpreter
{$mode objfpc}
program simp;
uses uparse, uast, ueval;

var ast : Node;
begin
  ast := Block;
  eval(ast);
  // TODO: ast.Free;
end.
