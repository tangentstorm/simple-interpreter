{$mode objfpc}
unit ueval;
interface uses uast, uenv, variants;

  function eval(n : Node; var e : Env) : variant;

implementation

function eval(n : Node; var e : Env) : variant;
  begin
    // WriteLn('evaluating node: ', n^.kind); DumpNode(n);
    result := null;
    case n^.kind of
      kWRITE : Writeln(eval(n^.expr, e));
      kINT   : result := n^.int;
      kVAR   : result := GetVar(e, n^.id);
      kPROG  : result := eval(n^.block, e);
      kSEQ   : begin
                 result := eval(n^.arg0, e);
                 result := eval(n^.arg1, e);
               end;
      kASSIGN: e := SetVar(e, n^.assignId, eval(n^.assignVal, e));
    end;
  end;

end.
