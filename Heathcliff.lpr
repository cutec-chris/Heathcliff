// *****************************************************************************
// Copyright: © 1999,2000,2010 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      Heathcliff.dpr
// Compiler:  Delphi, FreePascal
// Purpose:   Advanced registry reading and writing
// Authors:   Patrick M. Kolla (pk)
// *****************************************************************************
// Dependencies:
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2010-03-11  pk   --  Added this header (from Code Test Browser)
// *****************************************************************************

program Heathcliff;

{$MODE Delphi}

uses
   Forms, Interfaces,
   FormUnitStatus in 'FormUnitStatus.pas' {FormStatus},
   FormUnitColors in 'FormUnitColors.pas' {FormColors},
   FormUnitDebug in 'FormUnitDebug.pas' {FormDebug},
   FormUnitDelays in 'FormUnitDelays.pas' {FormDelays},
   FormUnitHelpLines in 'FormUnitHelpLines.pas' {FormHelpLines},
   FormUnitImport in 'FormUnitImport.pas' {fImport},
   FormUnitLicense in 'FormUnitLicense.pas' {FormLicense},
   FormUnitMain in 'FormUnitMain.pas' {FormMain},
   FormUnitPad in 'FormUnitPad.pas' {FormSketchpad},
   FormUnitPick in 'FormUnitPick.pas' {FormPickImage},
   FormUnitPreview in 'FormUnitPreview.pas' {FormPreview},
   UnitHeathcliffHelpers in 'UnitHeathcliffHelpers.pas';

{$R *.res}

begin
   Application.Initialize;
   Application.Title := 'Heathcliff for Catweazle LC1 - Yoghurt Mixer';
   Application.CreateForm(TFormMain, FormMain);
   Application.CreateForm(TFormStatus, FormStatus);
   Application.CreateForm(TFormColors, FormColors);
   Application.CreateForm(TFormDebug, FormDebug);
   Application.CreateForm(TFormDelays, FormDelays);
   Application.CreateForm(TFormHelpLines, FormHelpLines);
   Application.CreateForm(TFormImport, FormImport);
   Application.CreateForm(TFormLicense, FormLicense);
   Application.CreateForm(TFormSketchpad, FormSketchpad);
   Application.CreateForm(TFormPickImage, FormPickImage);
   Application.CreateForm(TFormPreview, FormPreview);
   Application.Run;
end.

