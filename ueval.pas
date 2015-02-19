{$mode objfpc}
unit ueval;
interface uses uast, uenv, variants;

  function eval(n : Node; var e : Env) : variant;

implementation

function eval(n : Node; var e : Env) : variant;
  begin
    // WriteLn('evaluating node: ', n^.kind); DumpNode(n);
    result := null;
    if n^.kind <> kOK then case n^.kind of
      kWRITE : Writeln(eval(n^.expr, e));
      kINT   : result := n^.int;
      // arithmetic operators
      kNEG   : result := -eval(n^.arg, e);
      kADD   : result := eval(n^.arg0, e) + eval(n^.arg1, e);
      kSUB   : result := eval(n^.arg0, e) - eval(n^.arg1, e);
      kMUL   : result := eval(n^.arg0, e) * eval(n^.arg1, e);
      kDIV   : result := eval(n^.arg0, e) div eval(n^.arg1, e);
      kMOD   : result := eval(n^.arg0, e) mod eval(n^.arg1, e);
      // bitwise operators
      kXOR   : result := eval(n^.arg0, e) xor eval(n^.arg1, e);
      kAND   : result := eval(n^.arg0, e) and eval(n^.arg1, e);
      kOR    : result := eval(n^.arg0, e) or eval(n^.arg1, e);
      // boolean operators
      kLT    : result := Ord(eval(n^.arg0, e) < eval(n^.arg1, e));
      kGT    : result := Ord(eval(n^.arg0, e) > eval(n^.arg1, e));
      kEQ    : result := Ord(eval(n^.arg0, e) = eval(n^.arg1, e));
      kNE    : result := Ord(eval(n^.arg0, e) <> eval(n^.arg1, e));
      kLE    : result := Ord(eval(n^.arg0, e) <= eval(n^.arg1, e));
      kGE    : result := Ord(eval(n^.arg0, e) >= eval(n^.arg1, e));
      // variables and control flow:
      kVAR   : result := GetVar(e, n^.id);
      kPROG  : result := eval(n^.block, e);
      kSEQ   : begin
                 result := eval(n^.arg0, e);
                 result := eval(n^.arg1, e);
               end;
      kASSIGN: e := SetVar(e, n^.assignId, eval(n^.assignVal, e));
      kWHILE : while eval(n^.whileCond, e) = 1 do eval(n^.whileBody, e);
      kIF : if eval(n^.condition, e) = 1 then eval(n^.thenPart, e)
	    else eval(n^.elsePart, e);
    end;
  end;


initialization
end.
