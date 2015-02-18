// interpreter environment (nested scopes)
{$mode objfpc}
unit uenv;
interface uses uAst, variants, sysutils;

type
  Env = ^EnvData;
  EnvData = record
    next  : Env;
    id    : string;
    value : variant;
  end;

const
  EmptyEnv : Env = nil;

function GetVar(e : Env; id: string) : variant;
function SetVar(e : Env; id: string; val : variant) : Env;

procedure DumpEnv(e : Env); // for debugging.

implementation

function Find(e : Env; id: string; out binding : Env ) : boolean;
  begin
    result := false;
    while (e <> nil) and (not result) do
      if id = e^.id then
        begin
          result := true; binding := e
        end
      else e := e^.next
  end;

procedure DumpEnv(e : Env);
  begin
    while e <> nil do
      begin
        writeln(e^.id, ' -> ', e^.value);
        e := e^.next;
      end
  end;


function GetVar(e : Env; id: string) : variant;
  var f : Env;
  begin
    if Find(e, id, f) then result := f^.value
    else
      begin
        writeln('Undefined Identifier: ', id); halt(1)
      end;
  end;

function SetVar(e : Env; id: string; val : variant) : Env;
  var f : Env;
  begin
    if Find(e, id, f) then
      begin
        f^.value := val; result := e;
      end
    else
      begin
        new(result);
        result^.id := id;
        result^.next := e;
        result^.value := val;
      end;
  end;

initialization
end.
