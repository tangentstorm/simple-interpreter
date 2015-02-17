// misc helper types and routines
{$mode objfpc}
unit utools;
interface

  // reinventing the wheel to avoid using objects. :/
  type
    strings	= ^StringsData;
    StringsData	= array of string;

  procedure append( var strs : strings; const s : string);

implementation

  procedure append( var strs : strings; const s : string);
    begin
      SetLength(strs^, Length(strs^)+1);
      strs^[High(strs^)] := s;
    end;

end.
