unit UnitHeathcliffHelpers;

{$MODE Delphi}

interface

{.$DEFINE snl}

{$IFDEF snl}
uses
   snlUIVista,
   snlUIWindows7;

type
   THeathcliffForm = TWindows7Form;
{$ELSE}
uses
   Forms;
   
type
   THeathcliffForm = TForm;
{$ENDIF}

implementation

end.
