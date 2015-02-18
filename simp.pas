// simple interpreter
{$mode objfpc}
program simp;
uses uparse, uast, uenv, ueval;

var root : Node;
begin
  root := ParseProgram;
  if (ParamCount > 0) and (ParamStr(1) = '-d') then DumpNode(root)
  else eval(root, EmptyEnv);
  // TODO: ast.Free;
end.
