// simple interpreter
{$mode objfpc}
program simp;
uses uparse, uast, ueval;

var ast : Node;
begin
  ast := ParseProgram;
  eval(ast);
  // TODO: ast.Free;
end.
