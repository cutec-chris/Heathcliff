{*------------------------------------------------------------------------------
  Form for displaying the applications license
  @Author    Patrick M. Kolla
  @Version   2009-06-24   Patrick M. Kolla	Now open source
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      FormUnitLicense.pas
// Compiler:  Borland Delphi 2006
// Purpose:   Form for displaying the applications license
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

unit FormUnitLicense;

{$MODE Delphi}

interface

uses
   LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, Buttons, ExtCtrls, ComCtrls,
   UnitHeathcliffHelpers;

type
   TFormLicense = class(THeathcliffForm)
      bnCancel: TBitBtn;
      bnOK: TBitBtn;
      mGPL: TMemo;
   private
   public
   end;

var
  FormLicense: TFormLicense;

implementation

uses
   FormUnitMain;

{$R *.lfm}

end.
