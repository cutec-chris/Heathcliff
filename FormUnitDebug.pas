{*------------------------------------------------------------------------------
  Form for debugging (showing waveform, for example)
  @Author    Patrick M. Kolla
  @Version   2009-06-24   Patrick M. Kolla	Now open source
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      FormUnitDebug.pas
// Compiler:  Borland Delphi 2006
// Purpose:   Form for debugging (showing waveform, for example)
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

unit FormUnitDebug;

{$MODE Delphi}

interface

uses
   LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, ExtCtrls, Menus,
   UnitHeathcliffHelpers;

type
   TFormDebug = class(TForm)
      img: TImage;
      N16xZoom1: TMenuItem;
      N1xZoom1: TMenuItem;
      N2xZoom1: TMenuItem;
      N32xZoom1: TMenuItem;
      N4xZoom1: TMenuItem;
      N64xZoom1: TMenuItem;
      N8xZoom1: TMenuItem;
      pmZoom: TPopupMenu;
      Step11: TMenuItem;
      Step161: TMenuItem;
      Step21: TMenuItem;
      Step321: TMenuItem;
      Step41: TMenuItem;
      Step641: TMenuItem;
      Step81: TMenuItem;
      procedure N1xZoom1Click(Sender: TObject);
      procedure N2xZoom1Click(Sender: TObject);
      procedure N4xZoom1Click(Sender: TObject);
      procedure N8xZoom1Click(Sender: TObject);
      procedure N16xZoom1Click(Sender: TObject);
      procedure N32xZoom1Click(Sender: TObject);
      procedure N64xZoom1Click(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure Step11Click(Sender: TObject);
      procedure Step21Click(Sender: TObject);
      procedure Step41Click(Sender: TObject);
      procedure Step81Click(Sender: TObject);
      procedure Step161Click(Sender: TObject);
      procedure Step321Click(Sender: TObject);
      procedure Step641Click(Sender: TObject);
   private
   public
      WaveScale: Integer;
      WaveSteps: Integer;
   end;

var
   FormDebug: TFormDebug;

implementation

uses
   FormUnitMain;

{$R *.lfm}

procedure TFormDebug.N1xZoom1Click(Sender: TObject);
begin
   WaveScale := 1;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N2xZoom1Click(Sender: TObject);
begin
   WaveScale := 2;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N4xZoom1Click(Sender: TObject);
begin
   WaveScale := 4;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N8xZoom1Click(Sender: TObject);
begin
   WaveScale := 8;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N16xZoom1Click(Sender: TObject);
begin
   WaveScale := 16;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N32xZoom1Click(Sender: TObject);
begin
   WaveScale := 32;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.N64xZoom1Click(Sender: TObject);
begin
   WaveScale := 64;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.FormCreate(Sender: TObject);
begin
   WaveScale := 1;
   WaveSteps := 1;
end;

procedure TFormDebug.Step11Click(Sender: TObject);
begin
   WaveSteps := 1;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step21Click(Sender: TObject);
begin
   WaveSteps := 2;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step41Click(Sender: TObject);
begin
   WaveSteps := 4;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step81Click(Sender: TObject);
begin
   WaveSteps := 8;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step161Click(Sender: TObject);
begin
   WaveSteps := 16;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step321Click(Sender: TObject);
begin
   WaveSteps := 32;
   FormMain.aFramePreviewExecute(nil);
end;

procedure TFormDebug.Step641Click(Sender: TObject);
begin
   WaveSteps := 64;
   FormMain.aFramePreviewExecute(nil);
end;

end.