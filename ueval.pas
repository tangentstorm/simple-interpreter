{$mode objfpc}
unit ueval;
interface uses uast, uenv, variants;

  function eval(n : Node; e : Env) : variant;

implementation

function eval(n : Node; e : Env) : variant;
  begin
    result := null;
    case n^.kind of
      kWRITE : Writeln(eval(n^.expr, e));
      kINT   : result := n^.int;
      kVAR   : result := Lookup(e, n^.id);
      kPROG  : result := eval(n^.block, e);
    end;
  end;

end.
