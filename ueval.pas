{$mode objfpc}
unit ueval;
interface uses uast, variants;

function eval(n : Node) : variant;

implementation

function eval(n : Node) : variant;
  begin
    result := null;
    case n.kind of
      kWRITE : Writeln(eval(n.expr));
      kINT   : result := n.int;
    end;
  end;

end.
