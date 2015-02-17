{$mode objfpc}
unit ueval;
interface uses uast, uenv, variants;

  function eval(n : Node; e : Env) : variant;
  function eval(n : Node) : variant;

implementation

function eval(n : Node; e : Env) : variant;
  begin
    result := null;
    case n^.kind of
      kWRITE : Writeln(eval(n^.expr));
      kINT   : result := n^.int;
      kVAR   : result := Lookup(e, n^.id);
    end;
  end;

function eval(n : Node) : variant;
  begin
    result := eval(n, EmptyEnv);
  end;

end.
