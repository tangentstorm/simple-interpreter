// interpreter environment (nested scopes)
{$mode objfpc}
unit uenv;
interface uses uAst, variants, sysutils;

type
  Binding = record
    id    : string;
    kind  : uAst.DataKind;
    value : variant;
  end;
  Env = ^EnvData;
  EnvData = record
    next : Env;
    defs : array of Binding;
  end;

const
  EmptyEnv : Env = nil;

function LookUp(e : Env; id: string) : variant;

implementation

function LookUp(e : Env; id: string) : variant;
  begin
    result := null;
    //  TODO: search through environment.
    if result = null then
      begin
        writeln('Undefined Identifier: ', id); halt(1);
      end;
  end;

end.
