{*------------------------------------------------------------------------------
  Form to set up hardware specific delays
  @Author    Patrick M. Kolla
  @Version   2009-06-24   Patrick M. Kolla	Now open source
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 1999,2000,2009-10 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      FormUnitDelays.pas
// Compiler:  Borland Delphi 2006
// Purpose:   Form to set up hardware specific delays
// Author:    Patrick M. Kolla (pk)
// *****************************************************************************
//  This file is part of Heathcliff for Catweazle LC1 - Yoghurt Mixer.
//
//  Heathcliff is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Heathcliff is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Heathcliff.  If not, see <http://www.gnu.org/licenses/>.
// *****************************************************************************

unit FormUnitDelays;

{$MODE Delphi}

interface

uses
   LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, Buttons,
   UnitHeathcliffHelpers;

type
   TFormDelays = class(THeathcliffForm)
      bnCancel: TBitBtn;
      bnHelp: TBitBtn;
      bnOK: TBitBtn;
      editStep10: TEdit;
      editStep40: TEdit;
      editStep5: TEdit;
      editStepEdges: TEdit;
      groupSteps: TGroupBox;
      st10Degree: TStaticText;
      st40Degree: TStaticText;
      st5Degree: TStaticText;
      stMax: TStaticText;
   private
   public
      function Execute(var Step5, Step10, Step40, StepMax: word): boolean;
   end;

var
   FormDelays: TFormDelays;

implementation

{$R *.lfm}

{ TFormDelays }

function TFormDelays.Execute(var Step5, Step10, Step40, StepMax: word): boolean;
var sStep: string;
    i, iErrorCode: integer;
begin
   editStep5.Text := IntToStr(Step5);
   editStep10.Text := IntToStr(Step10);
   editStep40.Text := IntToStr(Step40);
   editStepEdges.Text := IntToStr(StepMax);
   Result := ShowModal=mrOK;
   if Result then begin
      sStep := editStep5.Text;
      Val(sStep, i, iErrorCode);
      if iErrorCode=0
       then Step5 := i;
      sStep := editStep10.Text;
      Val(sStep, i, iErrorCode);
      if iErrorCode=0
       then Step10 := i;
      sStep := editStep40.Text;
      Val(sStep, i, iErrorCode);
      if iErrorCode=0
       then Step40 := i;
      sStep := editStepEdges.Text;
      Val(sStep, i, iErrorCode);
      if iErrorCode=0
       then StepMax := i;

   end;
end;

end.
