{*------------------------------------------------------------------------------
  Form that allows picking a part of a image for background
  @Author    Patrick M. Kolla
  @Version   2009-06-24   Patrick M. Kolla	Now open source
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      FormUnitPick.pas
// Compiler:  Borland Delphi 2006
// Purpose:   Form that allows picking a part of a image for background
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

unit FormUnitPick;

{$MODE Delphi}

interface

uses
   LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   ExtCtrls, ComCtrls,
   UnitHeathcliffHelpers;

type
   TFormPickImage = class(TForm)
      img: TImage;
      sbar: TStatusBar;
      procedure FormActivate(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure imgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   private
   public
      DataRect: TRect;
      DataImage: TBitmap;
      procedure Redraw;
   end;

var
   FormPickImage: TFormPickImage;

implementation

{$R *.lfm}

procedure TFormPickImage.Redraw;
var rContent: TRect;
begin
   if (DataRect.Left>-1) and (DataRect.Right>-1) then begin
      with img.canvas do begin
         Brush.Color := clRed;
         rContent := Rect(0,0,DataImage.Width,DataImage.Height);
         CopyRect(rContent,DataImage.Canvas,rContent);
         //FrameRect(myrect);
         DrawFocusRect(DataRect);
      end;
   end;
end;

procedure TFormPickImage.FormCreate(Sender: TObject);
begin
   DataRect.left := -1;
   DataRect.right := -1;
end;

procedure TFormPickImage.imgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then begin
      DataRect.left := x;
      DataRect.top := y;
   end else if mbRight = Button then begin
      DataRect.right := x;
      DataRect.bottom := y;
   end;
   ReDraw;
end;

procedure TFormPickImage.FormActivate(Sender: TObject);
begin
   Redraw;
end;

end.