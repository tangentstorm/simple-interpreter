// simple interpreter
{$mode objfpc}
program simp;
uses uparse, uast, uenv, ueval;

var root : Node; dump : boolean = false; i : integer;
begin
  if (ParamCount > 0) then for i := 1 to ParamCount do
    case ParamStr(i) of
      '-d' : dump := true;
      '-t' : uparse.doTrace := true;
      else {ok}
    end;
  root := uparse.Prog;
  if dump then DumpNode(root) else eval(root, EmptyEnv);
  // TODO: free memory for ast, environement
end.
