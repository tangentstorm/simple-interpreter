// simple interpreter
{$mode objfpc}
program simp;
uses uparse, uast, uenv, ueval, uviz;

var root : Node; i : integer; help, dump, graph, interp : boolean;
begin
  help:=false; dump:=false; graph:=false; interp := false;
  if (ParamCount > 0) then for i := 1 to ParamCount do
    case ParamStr(i) of
      '-d' : dump := true;
      '-g' : graph := true;
      '-h' : help := true;
      '-i' : interp := true;
      '-t' : uparse.doTrace := true;
      else {ok}
    end;

  // only allow one mode:
  case ord(graph) + ord(dump) + ord(uparse.doTrace) + ord(help) + ord(interp) of
    0 : interp := true;
    1 : {ok};
  otherwise
    WriteLn('ERROR: flags are mutually exclusive.');
    Writeln; help := true;
  end;

  if help then
    begin
      Writeln('Usage: ./simp [-d|-g|-h|-i|-t] < source.si');
      Writeln;
      Writeln(' -d : dump AST in text format.');
      Writeln(' -g : dump the AST in `dot` format for graphviz.');
      Writeln(' -h : show this help message');
      Writeln(' -i : interpret source code (default).');
      Writeln(' -t : trace execution in the parser.');
      Writeln;
      Writeln('Note: these options are mutually exclusive.');
    end
  else
    root := uparse.Prog;
    if dump then DumpNode(root)
    else if graph then uviz.WriteDOT(root)
    else eval(root, EmptyEnv);
  // TODO: free memory for AST, environement
end.
