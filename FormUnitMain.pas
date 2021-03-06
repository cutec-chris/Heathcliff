{*------------------------------------------------------------------------------
  Main form organizing all GUI stuff
  @Author    Patrick M. Kolla
  @Version   2009-06-24   Patrick M. Kolla	Now open source
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
// License:   GNU Public License v3
// Project:   Heathcliff
// File:      FormUnitMain.pas
// Compiler:  Borland Delphi 2006
// Purpose:   Main form organizing all GUI stuff
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

unit FormUnitMain;

{$MODE Delphi}

interface

uses
   LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, ExtCtrls, ComCtrls, ToolWin, Buttons, Spin, Menus, ImgList,
   ActnList, Registry, ClipBrd, FileUtil, Math, uLaserFrames,
   UnitHeathcliffHelpers, FormUnitHelpLines;

const
   myc_norm = 0;
   myc_real = 1;
   myc_back = 2;
   myc_sel = 3;
   myc_link = 4;
   mytc_thumb = 0;
   myoc_bg = 0;
   myoc_help = 1;
   myoc_rotcenter = 2;
   mytlc_lines = 0;
   mytlc_areas = 1;
   mytlc_text = 2;
   mytlc_back = 3;
   verinfo = 'Release 9 (Build 42)';
   LeftRulerWidth = 32;
   TopRulerHeight = 20;
   crMovePoint = 1;
   crAddPoint = 2;
   crDelPoint = 3;
   crZoom = 4;
   crPointType = 5;
   crAnimFrame = 6;
   crMoveRotFrame = 7;
   crSetPoints = 8;
   effect_slide = 0;
   effect_morph = 1;
   effect_plode = 2;
   effect_xflip = 3;
   effect_yflip = 4;
   effect_dflip = 5;
   effect_rotate = 6;
   effect_drain = 7;

type
   atp = array of TPoint;
   atb = array of boolean;
   TStates = (sNone, sMove, sAdd,sDel, sAnim, sZoom, sPointType, sMoveRotateFrame, sAuxPoints, sEquilateral);

   TimingArray = array[0..3] of word;

   TUndoData = record
      Op: Tstates;
      Pos: integer;
      x,y: byte;
      Point: TSmallPoint;
   end;

   { TFormMain }

   TFormMain = class(TForm)
    aAddFrame: TAction;
    aCheckAllLinks: TAction;
    aCheckFrameLinks: TAction;
    aChooseImgPart: TAction;
    aCopyFrameToClipboard: TAction;
    aCreateWavefile: TAction;
    aCutFrameToClipboard: TAction;
    aDeleteFrame: TAction;
    aFrameDelay: TAction;
    aFrameEffectParam: TAction;
    aFrameMorph: TAction;
    aFramePreview: TAction;
    aHelpContents: TAction;
    aImportFrame: TAction;
    aImportShow: TAction;
    aLoadBackImg: TAction;
    aNewFile: TAction;
    aOpenFile: TAction;
    aPasteFrameFromClipboard: TAction;
    aRenameFrame: TAction;
    aSaveAsFile: TAction;
    aSaveFile: TAction;
    aShowPreview: TAction;
    Checkframelinks1: TMenuItem;
    dockBottom: TPanel;
    dockFiles1: TPanel;
    dockTools: TPanel;
    ilOptions: TImageList;
    ilTools: TImageList;
    ilWork: TImageList;
    imgTimeLine: TImageList;
    iTimeline: TImage;
    lbThumbs: TListBox;
    miAbout: TMenuItem;
    miAddFrame: TMenuItem;
    miBackImg: TMenuItem;
    miCenterFrame: TMenuItem;
    miCheckLinks: TMenuItem;
    miChoosePart: TMenuItem;
    miCircles1: TMenuItem;
    miCircles2: TMenuItem;
    miCircles3: TMenuItem;
    miCircles4: TMenuItem;
    miCircles5: TMenuItem;
    miCircles6: TMenuItem;
    miCircles7: TMenuItem;
    miCircles8: TMenuItem;
    miCirclesize: TMenuItem;
    miCloseloop: TMenuItem;
    miColor0: TMenuItem;
    miColor1: TMenuItem;
    miCopyFrame: TMenuItem;
    miCreatePreview: TMenuItem;
    miCutFrame: TMenuItem;
    miDelays: TMenuItem;
    miDelFrame: TMenuItem;
    miDisplayPanel: TMenuItem;
    miEdit: TMenuItem;
    miEffectDFlip: TMenuItem;
    miEffectDrain: TMenuItem;
    miEffectMorph: TMenuItem;
    miEffectParam: TMenuItem;
    miEffectPlode: TMenuItem;
    miEffectRotate: TMenuItem;
    miEffectSlide: TMenuItem;
    miEffectXFlip: TMenuItem;
    miEffectYFlip: TMenuItem;
    miEquilateral: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    miFilePanel: TMenuItem;
    miFlipX: TMenuItem;
    miFlipY: TMenuItem;
    miFrame: TMenuItem;
    miFrameAddFrame: TMenuItem;
    miFramecolors: TMenuItem;
    miFrameCopyFrame: TMenuItem;
    miFrameCutFrame: TMenuItem;
    miFrameDelay: TMenuItem;
    miFrameDelFrame: TMenuItem;
    miFrameFlipD: TMenuItem;
    miFrameFlipX2: TMenuItem;
    miFrameFlipY: TMenuItem;
    miFrameFrameDelay: TMenuItem;
    miFrameFrameMorph: TMenuItem;
    miFrameImport: TMenuItem;
    miFrameMorph: TMenuItem;
    miFramePasteFrame: TMenuItem;
    miFrameRenFrame: TMenuItem;
    miFullImg: TMenuItem;
    miGrid: TMenuItem;
    miGrid128: TMenuItem;
    miGrid16: TMenuItem;
    miGrid32: TMenuItem;
    miGrid4: TMenuItem;
    miGrid64: TMenuItem;
    miGrid8: TMenuItem;
    miGridCustom: TMenuItem;
    miHelp: TMenuItem;
    miHelpContents: TMenuItem;
    miHelpLines: TMenuItem;
    miImport: TMenuItem;
    miImportFrame: TMenuItem;
    miImportShow: TMenuItem;
    miLastFile0: TMenuItem;
    miLastFile1: TMenuItem;
    miLastFile2: TMenuItem;
    miLastFile3: TMenuItem;
    miLastFile4: TMenuItem;
    miLicense: TMenuItem;
    miLinkOverlay: TMenuItem;
    miLoad: TMenuItem;
    miMakeWave: TMenuItem;
    miMousePosPanel: TMenuItem;
    miMoveNoRedraw: TMenuItem;
    miNew: TMenuItem;
    miNoImg: TMenuItem;
    miOpenAgain: TMenuItem;
    miOpt2xZoom: TMenuItem;
    miOpt3xZoom: TMenuItem;
    miPartImg: TMenuItem;
    miPasteFrame: TMenuItem;
    miPlayPanel: TMenuItem;
    miPointFramePanel: TMenuItem;
    miProjectImportShow: TMenuItem;
    miProjekt: TMenuItem;
    miRenFrame: TMenuItem;
    miRenum: TMenuItem;
    miRotateFrame: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miScale: TMenuItem;
    miSetRotPoint: TMenuItem;
    miSettings: TMenuItem;
    miShowBackframe: TMenuItem;
    miShowFrameWavePreview: TMenuItem;
    miShowLinks: TMenuItem;
    miShowNoOfPoints: TMenuItem;
    miShowPoints: TMenuItem;
    miShowReal: TMenuItem;
    miShowRuler: TMenuItem;
    miSnapGrid: TMenuItem;
    miSnapHelp: TMenuItem;
    miTimeline: TMenuItem;
    miToolAuxPoints: TMenuItem;
    miToolLink: TMenuItem;
    miToolMoveRotate: TMenuItem;
    miToolPanel: TMenuItem;
    miToolSharpen: TMenuItem;
    miUndo: TMenuItem;
    miUseGrid: TMenuItem;
    miWindow: TMenuItem;
    miZoom: TMenuItem;
    miZoom1: TMenuItem;
    miZoom2: TMenuItem;
    miZoom3: TMenuItem;
    miZoom4: TMenuItem;
    miZoom5: TMenuItem;
    miZoom6: TMenuItem;
    miZoom7: TMenuItem;
    miZoom8: TMenuItem;
    //mpPreview: TMediaPlayer;
    myal: TActionList;
    menuMain: TMainMenu;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    odBitmap: TOpenDialog;
    odLC1: TOpenDialog;
    panelLeft: TPanel;
    pmBackImg: TPopupMenu;
    pmCircles: TPopupMenu;
    pmColor: TPopupMenu;
    pmEffect: TPopupMenu;
    pmFrame: TPopupMenu;
    pmGeo: TPopupMenu;
    pmGrid: TPopupMenu;
    pmiChoosePart: TMenuItem;
    pmiLoadBackImg: TMenuItem;
    pmLastFiles: TPopupMenu;
    pmPoints: TPopupMenu;
    pmZoom: TPopupMenu;
    sbBlank: TToolButton;
    sbCloseLoop: TToolButton;
    sbColor: TToolButton;
    sbEffect: TToolButton;
    sbFlipX: TToolButton;
    sbFlipY: TToolButton;
    sbFrame: TStatusBar;
    sbFullImg: TToolButton;
    sbLock: TToolButton;
    sbNoImg: TToolButton;
    sbPartImg: TToolButton;
    sbRuler: TToolButton;
    sbSharpen: TToolButton;
    sbShowBackframe: TToolButton;
    sbShowGrid: TToolButton;
    sbShowLinks: TToolButton;
    sbShowNumPoints: TToolButton;
    sbShowPoints: TToolButton;
    sbShowReal: TToolButton;
    sbSnapGrid: TToolButton;
    sbSnapHelp: TToolButton;
    sdLC1: TSaveDialog;
    tbAdd: TToolButton;
    tbCompile: TToolButton;
    tbDel: TToolButton;
    tbGeo: TToolButton;
    tbHelp: TToolButton;
    tbImportFrame: TToolButton;
    tbImportShow: TToolButton;
    tbLive: TToolButton;
    tbmiFrameEffectParam: TMenuItem;
    tbMove: TToolButton;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbPlay: TToolButton;
    tbPointTools: TToolButton;
    tbRepeat: TToolButton;
    tbSave: TToolButton;
    tbShowPreview: TToolButton;
    tbStop: TToolButton;
    tbTimeline: TToolBar;
    tbTimelineLeft: TToolButton;
    tbTimelineRight: TToolButton;
    tbTimelineZoomIn: TToolButton;
    tbTimelineZoomOut: TToolButton;
    tbZoom: TToolButton;
    toolbarDisplay: TToolBar;
    toolbarFiles: TToolBar;
    toolbarPlay: TToolBar;
    toolbarPointsFrames: TToolBar;
    ToolbarSep971: TToolButton;
    ToolbarSep973: TToolButton;
    ToolbarSep976: TToolButton;
    ToolbarSep977: TToolButton;
    ToolbarSep978: TToolButton;
    toolbarTools: TToolBar;
    toolwinTimeline: TPanel;
    procedure aAddFrameExecute(Sender: TObject);
    procedure aCheckAllLinksExecute(Sender: TObject);
    procedure aCheckFrameLinksExecute(Sender: TObject);
    procedure aChooseImgPartExecute(Sender: TObject);
    procedure aCopyFrameToClipboardExecute(Sender: TObject);
    procedure aCreateWavefileExecute(Sender: TObject);
    procedure aCutFrameToClipboardExecute(Sender: TObject);
    procedure aDeleteFrameExecute(Sender: TObject);
    procedure aFrameDelayExecute(Sender: TObject);
    procedure aFrameEffectParamExecute(Sender: TObject);
    procedure aFrameMorphExecute(Sender: TObject);
    procedure aFramePreviewExecute(Sender: TObject);
    procedure aHelpContentsExecute(Sender: TObject);
    procedure aImportFrameExecute(Sender: TObject);
    procedure aImportShowExecute(Sender: TObject);
    procedure aLoadBackImgExecute(Sender: TObject);
    procedure aNewFileExecute(Sender: TObject);
    procedure aOpenFileExecute(Sender: TObject);
    procedure aPasteFrameFromClipboardExecute(Sender: TObject);
    procedure aRenameFrameExecute(Sender: TObject);
    procedure aSaveAsFileExecute(Sender: TObject);
    procedure aSaveFileExecute(Sender: TObject);
    procedure aShowPreviewExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure iThumbsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbThumbsClick(Sender: TObject);
    procedure lbThumbsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbThumbsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbThumbsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbThumbsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbTimelineMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure miAboutClick(Sender: TObject);
    procedure miCenterFrameClick(Sender: TObject);
    procedure miCircles1Click(Sender: TObject);
    procedure miCircles2Click(Sender: TObject);
    procedure miCircles3Click(Sender: TObject);
    procedure miCircles4Click(Sender: TObject);
    procedure miCircles5Click(Sender: TObject);
    procedure miCircles6Click(Sender: TObject);
    procedure miCircles7Click(Sender: TObject);
    procedure miCircles8Click(Sender: TObject);
    procedure miCirclesizeClick(Sender: TObject);
    procedure miCloseloopClick(Sender: TObject);
    procedure miColor0Click(Sender: TObject);
    procedure miColor1Click(Sender: TObject);
    procedure miDelaysClick(Sender: TObject);
    procedure miDisplayPanelClick(Sender: TObject);
    procedure miEffectDFlipClick(Sender: TObject);
    procedure miEffectDrainClick(Sender: TObject);
    procedure miEffectMorphClick(Sender: TObject);
    procedure miEffectParamClick(Sender: TObject);
    procedure miEffectPlodeClick(Sender: TObject);
    procedure miEffectRotateClick(Sender: TObject);
    procedure miEffectSlideClick(Sender: TObject);
    procedure miEffectXFlipClick(Sender: TObject);
    procedure miEffectYFlipClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miFilePanelClick(Sender: TObject);
    procedure miFlipXClick(Sender: TObject);
    procedure miFlipYClick(Sender: TObject);
    procedure miFramecolorsClick(Sender: TObject);
    procedure miFrameFlipDClick(Sender: TObject);
    procedure miFrameFlipX2Click(Sender: TObject);
    procedure miFrameFlipYClick(Sender: TObject);
    procedure miFullImgClick(Sender: TObject);
    procedure miGrid128Click(Sender: TObject);
    procedure miGrid16Click(Sender: TObject);
    procedure miGrid32Click(Sender: TObject);
    procedure miGrid4Click(Sender: TObject);
    procedure miGrid64Click(Sender: TObject);
    procedure miGrid8Click(Sender: TObject);
    procedure miGridClick(Sender: TObject);
    procedure miGridCustomClick(Sender: TObject);
    procedure miHelpLinesClick(Sender: TObject);
    procedure miLastfile0Click(Sender: TObject);
    procedure miLastfile1Click(Sender: TObject);
    procedure miLastfile2Click(Sender: TObject);
    procedure miLastfile3Click(Sender: TObject);
    procedure milastfile4Click(Sender: TObject);
    procedure miLicenseClick(Sender: TObject);
    procedure miLinkOverlayClick(Sender: TObject);
    procedure miMousePosPanelClick(Sender: TObject);
    procedure miMoveNoRedrawClick(Sender: TObject);
    procedure miNoImgClick(Sender: TObject);
    procedure miOpenAgainClick(Sender: TObject);
    procedure miOpt2xZoomClick(Sender: TObject);
    procedure miOpt3xZoomClick(Sender: TObject);
    procedure miPartImgClick(Sender: TObject);
    procedure miPlayPanelClick(Sender: TObject);
    procedure miPointFramePanelClick(Sender: TObject);
    procedure miRenumClick(Sender: TObject);
    procedure miRotateFrameClick(Sender: TObject);
    procedure miScaleClick(Sender: TObject);
    procedure miSetRotPointClick(Sender: TObject);
    procedure miShowBackframeClick(Sender: TObject);
    procedure miShowLinksClick(Sender: TObject);
    procedure miShowNoOfPointsClick(Sender: TObject);
    procedure miShowPointsClick(Sender: TObject);
    procedure miShowRealClick(Sender: TObject);
    procedure miShowRulerClick(Sender: TObject);
    procedure miSnapGridClick(Sender: TObject);
    procedure miSnapHelpClick(Sender: TObject);
    procedure miTimelineClick(Sender: TObject);
    procedure miToolAuxPointsClick(Sender: TObject);
    procedure miToolLinkClick(Sender: TObject);
    procedure miToolMoveRotateClick(Sender: TObject);
    procedure miToolPanelClick(Sender: TObject);
    procedure miToolSharpenClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miUseGridClick(Sender: TObject);
    procedure miZoom1Click(Sender: TObject);
    procedure miZoom2Click(Sender: TObject);
    procedure miZoom3Click(Sender: TObject);
    procedure miZoom4Click(Sender: TObject);
    procedure miZoom5Click(Sender: TObject);
    procedure miZoom6Click(Sender: TObject);
    procedure miZoom7Click(Sender: TObject);
    procedure miZoom8Click(Sender: TObject);
    procedure miZoomClick(Sender: TObject);
    procedure mpPreviewNotify(Sender: TObject);
    procedure pmiChoosePartClick(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbAnimClick(Sender: TObject);
    procedure sbBlankClick(Sender: TObject);
    procedure sbCloseLoopClick(Sender: TObject);
    procedure sbColorClick(Sender: TObject);
    procedure sbDelClick(Sender: TObject);
    procedure sbEffectClick(Sender: TObject);
    procedure sbFlipXClick(Sender: TObject);
    procedure sbFlipYClick(Sender: TObject);
    procedure sbFullImgClick(Sender: TObject);
    procedure sbLockClick(Sender: TObject);
    procedure sbMoveClick(Sender: TObject);
    procedure sbNoImgClick(Sender: TObject);
    procedure sbPartImgClick(Sender: TObject);
    procedure sbPointTypeClick(Sender: TObject);
    procedure sbRulerClick(Sender: TObject);
    procedure sbSharpenClick(Sender: TObject);
    procedure sbShowBackframeClick(Sender: TObject);
    procedure sbShowGridClick(Sender: TObject);
    procedure sbShowLinksClick(Sender: TObject);
    procedure sbShowNumPointsClick(Sender: TObject);
    procedure sbShowPointsClick(Sender: TObject);
    procedure sbShowRealClick(Sender: TObject);
    procedure sbSnapGridClick(Sender: TObject);
    procedure sbSnapHelpClick(Sender: TObject);
    procedure tbAddClick(Sender: TObject);
    procedure tbDelClick(Sender: TObject);
    procedure tbGeoClick(Sender: TObject);
    procedure tbLiveClick(Sender: TObject);
    procedure tbMoveClick(Sender: TObject);
    procedure tbPlayClick(Sender: TObject);
    procedure tbPointToolsClick(Sender: TObject);
    procedure tbRepeatClick(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
    procedure tbTimelineLeftClick(Sender: TObject);
    procedure tbTimelineRightClick(Sender: TObject);
    procedure tbTimeLineZoomInClick(Sender: TObject);
    procedure tbTimeLineZoomOutClick(Sender: TObject);
    procedure tbZoomClick(Sender: TObject);
    procedure TimeLineRedraw;
    procedure toolbarDisplayClose(Sender: TObject);
    procedure toolbarFilesClose(Sender: TObject);
    procedure toolbarPlayClose(Sender: TObject);
    procedure toolbarPointsFramesClose(Sender: TObject);
    procedure toolbarToolsClose(Sender: TObject);
    procedure toolwinTimelineClose(Sender: TObject);
  private
    { Private-Deklarationen }
    lastpoint,topoint: TPoint;
    procedure WritePoints(var ms: TStream; x,y: longint; color,blank: boolean);
    procedure WritePointArray(var ms: TStream; a: atp; b: atb; color: boolean; var ws: longint; var lp: TPoint);
    procedure FrameToArray(f: TLaserFrame; var mya: atp; var myb: atb; var framed: longint);
  public
    { Public-Deklarationen }
    oldmovepos: TPoint;
    allframes: array of TLaserFrame;
    FFile: TLaserFrames;
    msLivePreview: TMemoryStream;
    Undo: TUndoData;
    RotStart,RotEnd: TPoint;
    ClipBoardFrame: TLaserFrame;
    CircleSize,ZoomFactor: Byte;
    SelectedPoint: Integer;
    EquilateralSides: Integer;
    EquilateralCenter: TPoint;
    EquilateralStart: TPoint;
    EquilateralFinished: boolean;
    OldWorkState,WorkState: Tstates;
    GridWidth: integer;
    DontDraw,Drawing,FileChanged,LicenseAgree: boolean;
    MyColorNames: array[0..1] of string;
    MyColors: array[0..1,0..4] of TColor;
    MyTextColors: array[0..0] of TColor;
    MyOtherColors: array[0..2] of TColor;
    MyTimeLineColors: array[0..3] of TColor;
    MyIcons: array[0..0] of HIcon;
    MyTimes: TimingArray;

    procedure ReDraw;
    procedure DrawGrid(cv: TCanvas; clGrid: TColor);
    procedure DrawRulers;
    procedure DrawHelpLines(f: TLaserFrame; cv: TCanvas; clLines: TColor);
    procedure DrawThumb(f: TLaserFrame; cv: TCanvas; offx, offy, divider: word; clNorm: TColor);
    procedure DrawFrame(f: TLaserFrame; cv: TCanvas; drawreal, drawover: boolean; clNorm,clSel,clReal,clLines: TColor);
    procedure DrawLinks(fThis,fOld: TLaserFrame; cv: TCanvas; clLink: TColor);
    procedure RenumberList;
    procedure Equilateral(center,start: TPoint);
    function CurrentFrame: Integer;
    procedure SaveToFile(fn: string; var y: TLaserFrames);
    procedure LoadFromFile(fn: string; var y: TLaserFrames; realload: boolean);
    procedure AddUsedFile(fn: string);
    procedure SetColorsFromDialog;
    function CheckFrameLinks(fi: integer; var s: string): boolean;
    procedure ReCreatePreview;
    procedure InsertFrame(f: TLaserFrame; pos: integer);
    procedure DeleteFrame(pos: integer; var f: TLaserFrame);
    procedure RotateFrame(f: TLaserFrame; degrees: real);
  end;

   TRegistryHelper = class helper for TRegistry
      function GetValueDefault(const ValueName: string; Default: boolean): boolean; overload;
      function GetValueDefault(const ValueName: string; Default: string): string; overload;
      function GetValueDefault(const ValueName: string; Default: cardinal): cardinal; overload;
      procedure SetValue(const ValueName: string; Data: boolean); overload;
      procedure SetValue(const ValueName: string; Data: string); overload;
      procedure SetValue(const ValueName: string; Data: cardinal); overload;
   end;

var
  FormMain: TFormMain;

function arg(x,y: integer): real;

implementation

uses
   FormUnitPick,
   FormUnitPad,
   FormUnitDebug,
   FormUnitStatus,
   FormUnitImport,
   FormUnitColors,
   FormUnitPreview,
   FormUnitDelays,
   FormUnitLicense;

{$R *.lfm}

function arg(x,y: integer): real;
var ret: real;
begin
   if (x>0) and (y>=0) then
      ret := ArcTan(y/x)
   else if (x<0) then
      ret := Pi+ArcTan(y/x)
   else if (x>0) and (y<0) then
      ret := 2*Pi+ArcTan(y/x)
   else if (x=0) and (y>0) then
      ret := Pi / 2
   else if (x=0) and (y<0) then
      ret := 3*Pi / 2
   else ret := 0;
   arg := ret;
end;

procedure CreateWaveHeader(var ms: TStream; framesamples,datasize: longint);
var l,l2: longint;
    w: word;
begin
   ms.Seek(0,soFromBeginning);
                ms.Write('RIFF',4);
   l2 := (4*datasize*framesamples);
   l := 36+l2;
                ms.Write(l,4);          // RIFFsize = 36 + block(=4) * samples
                ms.Write('WAVEfmt ',8);
   l := 16;     ms.Write(l,4);
   w := 1;      ms.Write(w,2);          // Coding: 1=PCM 2=MuLaw
   w := 2;      ms.Write(w,2);          // Channels: 1=Mono 2=Stereo
   l := 44100;  ms.Write(l,4);          // Sampling rate
   l := 176400; ms.Write(l,4);          // average bps: res/8*rate*channels
   w := 4;      ms.Write(w,2);          // block alignment
   w := 16;     ms.Write(w,2);          // resolution = bits per sample
                ms.Write('data',4);
                ms.Write(l2,4);         // RIFFsize = 36 + block(=4) * samples
end;

function TRegistryHelper.GetValueDefault(const ValueName: string; Default: boolean): boolean;
begin
   try
      Result := Self.ReadBool(ValueName);
   except
      Result := Default;
   end;
end;

function TRegistryHelper.GetValueDefault(const ValueName: string; Default: string): string;
begin
   try
      Result := Self.ReadString(ValueName);
   except
      Result := Default;
   end;
end;

function TRegistryHelper.GetValueDefault(const ValueName: string; Default: cardinal): cardinal;
begin
   try
      Result := Self.ReadInteger(ValueName);
   except
      Result := Default;
   end;
end;

procedure TRegistryHelper.SetValue(const ValueName: string; Data: boolean);
begin
   Self.WriteBool(ValueName, Data);
end;

procedure TRegistryHelper.SetValue(const ValueName: string; Data: string);
begin
   Self.WriteString(ValueName, Data);
end;

procedure TRegistryHelper.SetValue(const ValueName: string; Data: cardinal);
begin
   Self.WriteInteger(ValueName, Data);
end;

procedure TFormMain.WritePoints(var ms: TStream; x,y: longint; color,blank: boolean);
var l,ax,ay: longint;
    bx,by: byte;
begin
   ax := x; while ax>255 do Dec(ax,256); while ax<0 do Inc(ax,256);
   ay := y; while ay>255 do Dec(ay,256); while ay<0 do Inc(ay,256);
   //lastpoint.x := ax;
   //lastpoint.y := ay;
   if miFlipY.Checked then bx := 255 - ax else bx := ax;
   if miFlipX.Checked then by := ay else by := 255 - ay;
   l := (bx shl 8) + (by shl 24);
   if color then Inc(l);
   if blank then Inc(l,2);
   ms.Write(l,4);
end;

procedure TFormMain.WritePointArray(var ms: TStream; a: atp; b: atb; color: boolean; var ws: longint; var lp: TPoint);
var i: longint;
begin
   if Length(a)>0 then begin
      for i := 0 to Pred(Length(a)) do begin
         WritePoints(ms,TPoint(a[i]).x+128,TPoint(a[i]).y+128,color,b[i]);
         Inc(ws);
      end;
      lp := a[Pred(Length(a))];
   end;
end;

procedure TFormMain.FrameToArray(f: TLaserFrame; var mya: atp; var myb: atb; var framed: longint);
var i,j: integer;
    p,np,pp: TSmallPoint;
    dx,dy: integer;
    ax,ay: word;
    tx,ty,t,tsub: word;
    wp1,wp2: TPoint;
    mywinkel: real;
begin
   for i := 0 to Pred(f.Points.Count) do begin
      p := f.Points[i];
      if (i<(Pred(f.Points.count)))
       then np := f.Points[i+1]
        else np := f.Points[0];
      if (i>0)
       then pp := f.Points[Pred(i)]
        else pp := f.Points[Pred(f.Points.count)];
      dx := Integer(np.x) - Integer(p.x);
      dy := Integer(np.y) - Integer(p.y);
      if Abs(dx) < 32 then tx := MyTimes[0] else if Abs(dx) < 64 then tx := MyTimes[1] else tx := MyTimes[2];
      if Abs(dy) < 32 then ty := MyTimes[0] else if Abs(dy) < 64 then ty := MyTimes[1] else ty := MyTimes[2];
      if tx > ty then t := tx else t := ty;
      if ((p.bits and 1)=1) then begin // vorher nach naechster schleife
         wp1.x := Integer(p.x)-Integer(pp.x);
         wp1.y := Integer(p.y)-Integer(pp.y);
         wp2.x := Integer(np.x)-Integer(p.x);
         wp2.y := Integer(np.y)-Integer(p.y);
         //mywinkel := Abs((wp1.x*wp2.x)+(wp1.y*wp2.y))/(Sqrt(Sqr(wp1.x)+Sqr(wp1.y))*Sqrt(Sqr(wp2.x)+Sqr(wp2.y)));
         mywinkel := Abs(arg(wp2.x,wp2.y)-arg(wp1.x,wp1.y));
         tsub := Round(MyTimes[3]*2*(mywinkel/Pi));
         Inc(framed,tsub);
         SetLength(mya,framed);
         SetLength(myb,framed);
         for j := 0 to Pred(tsub) do begin
            TPoint(mya[framed-tsub+j]).x := p.x; // vorher np
            TPoint(mya[framed-tsub+j]).y := p.y; // vorher np
            myb[framed-tsub+j] := (p.bits and 2)=2;
         end;
      end;
      Inc(framed,t);
      SetLength(mya,framed);
      SetLength(myb,framed);
      for j := 0 to Pred(t) do begin
         ax := p.x + Round(dx*j/t);
         ay := p.y + Round(dy*j/t);
         TPoint(mya[framed-t+j]).x := ax;
         TPoint(mya[framed-t+j]).y := ay;
         myb[framed-t+j] := (p.bits and 2)=2;
      end;
   end; // end framework
end;

procedure TFormMain.AddUsedFile(fn: string);
var s: string;
begin
   if (Pos(fn,miLastfile0.caption)=0)
   and (Pos(fn,miLastfile1.caption)=0)
   and (Pos(fn,miLastfile2.caption)=0)
   and (Pos(fn,miLastfile3.caption)=0)
   and (Pos(fn,miLastfile4.caption)=0) then begin
      miLastfile4.Caption := miLastfile3.Caption;
      s := miLastfile4.Caption; s[2] := '4'; miLastFile4.Caption := s;
      miLastfile4.Visible := miLastfile3.Visible;
      miLastfile3.Caption := miLastfile2.Caption;
      s := miLastfile3.Caption; s[2] := '3'; miLastFile3.Caption := s;
      miLastfile3.Visible := miLastfile2.Visible;
      miLastfile2.Caption := miLastfile1.Caption;
      s := miLastfile2.Caption; s[2] := '2'; miLastFile2.Caption := s;
      miLastfile2.Visible := miLastfile1.Visible;
      miLastfile1.Caption := miLastfile0.Caption;
      s := miLastfile1.Caption; s[2] := '1'; miLastFile1.Caption := s;
      miLastfile1.Visible := miLastfile0.Visible;
      miLastfile0.Caption := '&0 '+fn;
      miLastfile1.Visible := true;
   end;
end;

function TFormMain.CurrentFrame: Integer;
begin
   if FormSketchpad<>nil then
      Result := StrToInt(FormSketchpad.panelFrameSwitcher.Caption)
   else Result := 0;
end;

procedure TFormMain.DrawGrid(cv: TCanvas; clGrid: TColor);
var ps: array[0..1] of TPoint;
    psz: array[0..1] of TPoint;
    i,j: integer;
begin
   i := GridWidth;
   ps[0].x := 0;
   ps[1].x := 256;
   while i<255 do begin
      ps[0].y := i;
      ps[1].y := i;
      for j := 0 to 1 do begin
         if miFlipY.Checked
          then psz[j].x := (256*ZoomFactor)-(ps[j].x*ZoomFactor)-FormSketchpad.sbX.Position
           else psz[j].x := (ps[j].x*ZoomFactor)-FormSketchpad.sbX.Position;
         if miFlipX.Checked
          then psz[j].y := (256*ZoomFactor)-(ps[j].y*ZoomFactor)-FormSketchpad.sbY.Position
           else psz[j].y := (ps[j].y*ZoomFactor)-FormSketchpad.sbY.Position;
      end;
      with cv do begin
         pen.color := clGrid;
         pen.width := 1;
         pen.style := psDot;
         MoveTo(psz[0].x,psz[0].y);
         LineTo(psz[1].x,psz[1].y);
         pen.style := psSolid;
      end;
      Inc(i,GridWidth);
   end;
   i := GridWidth;
   ps[0].y := 0;
   ps[1].y := 256;
   while i<255 do begin
      ps[0].x := i;
      ps[1].x := i;
      for j := 0 to 1 do begin
         if miFlipY.Checked then
          psz[j].x := (256*ZoomFactor)-(ps[j].x*ZoomFactor)-FormSketchpad.sbX.Position
           else psz[j].x := (ps[j].x*ZoomFactor)-FormSketchpad.sbX.Position;
         if miFlipX.checked then
          psz[j].y := (256*ZoomFactor)-(ps[j].y*ZoomFactor)-FormSketchpad.sbY.Position
           else psz[j].y := (ps[j].y*ZoomFactor)-FormSketchpad.sbY.Position;
      end;
      with cv do begin
         pen.color := clGrid;
         pen.width := 1;
         pen.style := psDot;
         MoveTo(psz[0].x,psz[0].y);
         LineTo(psz[1].x,psz[1].y);
         pen.style := psSolid;
      end;
      Inc(i,GridWidth);
   end;
end;

procedure TFormMain.DrawRulers;
var i: integer;
    txw,txh,gw: word;
    x,y: word;
    s: string;
    lw,lh,tw,th: word;
begin
   tw := FormSketchpad.iTopRuler.Width;
   th := FormSketchpad.iTopRuler.Height;
   lw := FormSketchpad.iLeftRuler.Width;
   lh := FormSketchpad.iLeftRuler.Height;
   with FormSketchpad.iTopRuler.canvas do begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0,0,tw,th));
      Brush.Color := clWhite;
      FillRect(Rect({lw+}4,3,tw-4,th-4));
      FillRect(Rect(4,3,lw-4,th));
      Pen.Width := 1;
      Pen.Color := clDkGray;
      MoveTo(tw-1,0); LineTo(tw-1,th-1); LineTo(lw-2,th-1);
      MoveTo(tw-5,2); LineTo({lw+}2,2); LineTo({lw+}2,th{-3});
      Pen.Color := clWhite;
      MoveTo(0,th-1); LineTo(0,0); LineTo(tw-2,0);
      Font.Color := clBlack;
   end;
   with FormSketchpad.iLeftRuler.Canvas do begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0,0,lw,lh));
      Brush.Color := clWhite;
      //FillRect(Rect(4,4,lw-4,lh-4));
      FillRect(Rect(4,0,lw-4,lh-4));
      Pen.Width := 1;
      Pen.Color := clDkGray;
      //MoveTo(lw-1,0); LineTo(lw-1,lh-1); LineTo(0,lh-1);
      MoveTo(2,0); LineTo(2,lh-2);
      //MoveTo(lw-4,4); LineTo(2,4); LineTo(2,lh-4);
      Pen.Color := clWhite;
      MoveTo(0,0); LineTo(0,lh-1);
      Font.Color := clBlack;
   end;
   gw := GridWidth;
   while (gw*ZoomFactor)<FormSketchpad.iTopRuler.Canvas.TextWidth('-250')
      do gw := gw * 2;
   i := gw;
   while i<255 do begin
      with FormSketchpad.iTopRuler.canvas do begin
         x := (i*ZoomFactor-FormSketchpad.sbX.Position)+lw;
         if x>lw+7 then begin MoveTo(x,3); LineTo(x,th-4); end;
         if miFlipY.Checked then s := IntToStr(256-i) else s := IntToStr(i);
         txw := TextWidth(s);
         if ((x-(txw div 2))>(lw-6)) and ((x+(txw div 2))+6<tw)
           then TextOut(x-(txw div 2),3,s);
      end;
      with FormSketchpad.iLeftRuler.canvas do begin
         y := (i*ZoomFactor-FormSketchpad.sbY.Position)+1;
         MoveTo(4,y); LineTo(lw-4,y);
         if miFlipX.Checked then s := IntToStr(256-i) else s := IntToStr(i);
         txw := TextWidth(s);
         txh := TextHeight(s);
         if ((y-(txh div 2))>0) and ((y+(txh div 2))+6<lh)
           then TextOut((lw div 2)-(txw div 2),y-(txh div 2),s);
      end;
      Inc(i,gw);
   end;
end;

procedure TFormMain.DrawHelpLines(f: TLaserFrame; cv: TCanvas; clLines: TColor);
var i: integer;
    x,y: word;
    b: byte;
    p1,p2,wp1,wp2: TPoint;
    lw,th: word;
begin
   if f<>nil then begin
      th := FormSketchpad.iTopRuler.Height;
      lw := FormSketchpad.iLeftRuler.Width;
      with cv do begin
         Pen.Width := 1;
         Pen.Style := psDot;
         Pen.Color := clLines;
      end;
      if Length(f.HelpLines.x)>0
       then for i := 0 to Pred(Length(f.HelpLines.x)) do begin
         b := f.HelpLines.x[i];
         x := (b*ZoomFactor-FormSketchpad.sbX.Position);
         with cv do begin
            MoveTo(x,0); LineTo(x,256*ZoomFactor);
         end;
         with FormSketchpad.iTopRuler.Canvas do begin
            Brush.Color := clLines;
            Pen.Color := clBlack;
            Polygon([Point(x-5+lw,th div 2),Point(x+5+lw,th div 2),Point(x+lw,th)]);
         end;
      end;
      if Length(f.HelpLines.y)>0
       then for i := 0 to Pred(Length(f.HelpLines.y)) do begin
         b := f.HelpLines.y[i];
         y := (b*ZoomFactor-FormSketchpad.sbY.Position);
         with cv do begin
            MoveTo(0,y); LineTo(256*ZoomFactor,y);
         end;
         with FormSketchpad.iLeftRuler.Canvas do begin
            Brush.Color := clLines;
            Pen.Color := clBlack;
            Polygon([Point(lw div 2,y+5),Point(lw div 2,y-5),Point(lw,y)]);
         end;
      end;
      if Length(f.HelpLines.d[0])>0
       then for i := 0 to Pred(Length(f.HelpLines.d[0])) do begin
         p1 := f.HelpLines.d[0,i];
         p2 := f.HelpLines.d[1,i];
         wp1.x := (p1.x*ZoomFactor-FormSketchpad.sbX.Position);
         wp1.y := (p1.y*ZoomFactor-FormSketchpad.sbY.Position);
         wp2.x := (p2.x*ZoomFactor-FormSketchpad.sbX.Position);
         wp2.y := (p2.y*ZoomFactor-FormSketchpad.sbY.Position);
         with cv do begin
            MoveTo(wp1.x,wp1.y); LineTo(wp2.x,wp2.y);
         end;
      end;
   end;
end;

procedure TFormMain.DrawThumb(f: TLaserFrame; cv: TCanvas; offx, offy, divider: word; clNorm: TColor);
var ps: array of TPoint;
    i: integer;
    iPointCount: integer;
    myp: TSmallPoint;
begin
   iPointCount := f.Points.count;
   SetLength(ps,iPointCount);
   for i := 0 to Pred(iPointCount) do begin
      myp := f.Points[i];
      if miFlipY.Checked then
       ps[i].x := (255)-myp.x
        else ps[i].x := myp.x;
      if miFlipX.checked then
       ps[i].y := (255)-myp.y
        else ps[i].y := myp.y;
      ps[i].x := ps[i].x div divider + offx;
      ps[i].y := ps[i].y div divider + offy;
   end;
   with cv do begin
      Pen.Color := clNorm;
      Pen.Width := 1;
      Font.Color := clNorm;
      Polyline(ps);
      if Length(ps)>0 then begin
         MoveTo(ps[Pred(Length(ps))].x,ps[Pred(Length(ps))].y);
         LineTo(ps[0].x,ps[0].y);
      end;
   end; // with
end;

procedure TFormMain.DrawFrame(f: TLaserFrame; cv: TCanvas; drawreal, drawover: boolean; clNorm,clSel,clReal,clLines: TColor);
var ps: array of TPoint;
    i: integer;
    iPointCount: integer;
    myp: TSmallPoint;
    p1,p2: TPoint;
    cl: TColor;
    circlefactor: integer;
    x,y: integer;
begin
   iPointCount := f.Points.count;
   circlefactor := 2; //ZoomFactor;
   with cv do begin
      x := (f.RotCenter.x*ZoomFactor-FormSketchpad.sbX.Position);
      y := (f.RotCenter.y*ZoomFactor-FormSketchpad.sbY.Position);
      Pen.Width := 2; Pen.Color := MyOtherColors[myoc_rotcenter]; Pen.Style := psSolid;
      MoveTo(x-5,y-5); LineTo(x+6,y+6);
      MoveTo(x-5,y+6); LineTo(x+6,y-5);
      Pen.Width := 1;
      x := (f.AuxCenter.x*ZoomFactor-FormSketchpad.sbX.Position);
      y := (f.AuxCenter.y*ZoomFactor-FormSketchpad.sbY.Position);
      MoveTo(x-5,y-5); LineTo(x+6,y+6);
      MoveTo(x-5,y+6); LineTo(x+6,y-5);
      Pen.Style := psDot;
      MoveTo((f.RotCenter.x*ZoomFactor-FormSketchpad.sbX.Position),(f.RotCenter.y*ZoomFactor-FormSketchpad.sbY.Position));
      LineTo((f.AuxCenter.x*ZoomFactor-FormSketchpad.sbX.Position),(f.AuxCenter.y*ZoomFactor-FormSketchpad.sbY.Position));
      Pen.Color := clLines; //clAqua; //###
   end;
   SetLength(ps,iPointCount);
   for i := 0 to Pred(iPointCount) do begin
      myp := f.Points[i];
      if miFlipY.Checked then
       ps[i].x := (256*ZoomFactor)-(myp.x*ZoomFactor)-FormSketchpad.sbX.Position
        else ps[i].x := (myp.x*ZoomFactor)-FormSketchpad.sbX.Position;
      if miFlipX.checked then
       ps[i].y := (256*ZoomFactor)-(myp.y*ZoomFactor)-FormSketchpad.sbY.Position
        else ps[i].y := (myp.y*ZoomFactor)-FormSketchpad.sbY.Position;
   end;
   if Length(ps)>0 then with cv do begin
      // echtes signal: keine scharfen ecken!
      if Length(ps)>1 then begin
         Pen.Width := 2; Pen.Style := psDashDotDot; Pen.Color := clReal;
         if drawreal then for i := 1 to Length(ps)-2 do begin
            p1.x := (ps[i].x + ps[i-1].x) div 2;
            p1.y := (ps[i].y + ps[i-1].y) div 2;
            p2.x := (ps[i+1].x + ps[i].x) div 2;
            p2.y := (ps[i+1].y + ps[i].y) div 2;
            if ((TSmallPoint(f.Points[i]).bits and 1)=0) then
              PolyBezier([p1,ps[i],ps[i],p2]);
            if Length(ps)>2 then begin
               if ((TSmallPoint(f.Points[0]).bits and 1)=0) then begin
                  p1.x := (ps[0].x + ps[Length(ps)-1].x) div 2;
                  p1.y := (ps[0].y + ps[Length(ps)-1].y) div 2;
                  p2.x := (ps[1].x + ps[0].x) div 2;
                  p2.y := (ps[1].y + ps[0].y) div 2;
                  PolyBezier([p1,ps[0],ps[0],p2]);
               end;
               if ((TSmallPoint(f.Points[Length(ps)-1]).bits and 1)=0) then begin
                  p1.x := (ps[Length(ps)-1].x + ps[Length(ps)-2].x) div 2;
                  p1.y := (ps[Length(ps)-1].y + ps[Length(ps)-2].y) div 2;
                  p2.x := (ps[0].x + ps[Length(ps)-1].x) div 2;
                  p2.y := (ps[0].y + ps[Length(ps)-1].y) div 2;
                  PolyBezier([p1,ps[Length(ps)-1],ps[Length(ps)-1],p2]);
               end;
            end;
         end;
         Pen.Color := clNorm;
         Font.Color := clNorm;
         for i := 0 to Length(ps)-2 do begin
            if ((TSmallPoint(f.Points[i]).bits and 2)=0)
              then begin Pen.Width := 2; Pen.Style := psSolid; end
              else begin Pen.Width := 1; Pen.Style := psDot; end;
            MoveTo(ps[i].x,ps[i].y);
            LineTo(ps[i+1].x,ps[i+1].y);
         end;
      end;
      if (miCloseLoop.Checked) and (Length(ps)>2) then begin
         if ((TSmallPoint(f.Points[Length(ps)-1]).bits and 2)=0)
           then begin Pen.Width := 2; Pen.Style := psSolid; end
           else begin Pen.Width := 1; Pen.Style := psDot; end;
         MoveTo(ps[Length(ps)-1].x,ps[Length(ps)-1].y);
         LineTo(ps[0].x,ps[0].y);
      end;
      if miShowPoints.Checked then begin
         for i := 0 to iPointCount-1 do begin
            myp := f.Points[i];
            Pen.Width := 2;
            Pen.Style := psSolid;
            if (myp.p=1) or ((i=SelectedPoint) and (not FormSketchpad.multiselect)) then begin
               Pen.Color := clSel;
               Font.Color := clSel;
            end else begin
               Pen.Color := clNorm;
               Font.Color := clNorm;
            end;
            cl := MyColors[(f.Bits and 1),myc_link];
            if myp.overlay and drawover then Brush.Color := cl;
            if ((myp.bits and 1)=0) then
            Ellipse(ps[i].x-(CircleSize*CircleFactor),
                    ps[i].y-(CircleSize*CircleFactor),
                    ps[i].x+(CircleSize*CircleFactor),
                    ps[i].y+(CircleSize*CircleFactor))
            else Rectangle(ps[i].x-(CircleSize*CircleFactor),
                           ps[i].y-(CircleSize*CircleFactor),
                           ps[i].x+(CircleSize*CircleFactor),
                           ps[i].y+(CircleSize*CircleFactor));
            Brush.Color := clBlack;
            if miShowNoOfPoints.Checked then
            TextOut(ps[i].x+(CircleSize*CircleFactor)+2,
                    ps[i].y-(CircleSize*CircleFactor),
                    myp.Caption);
         end;
      end;
   end;
   //###
   If not EquilateralFinished then begin
      cl := clFuchsia;
      if miFlipY.Checked then
       p1.x := (256*ZoomFactor)-(EquilateralCenter.x*ZoomFactor)-FormSketchpad.sbX.Position
        else p1.x := (EquilateralCenter.x*ZoomFactor)-FormSketchpad.sbX.Position;
      if miFlipX.checked then
       p1.y := (256*ZoomFactor)-(EquilateralCenter.y*ZoomFactor)-FormSketchpad.sbY.Position
        else p1.y := (EquilateralCenter.y*ZoomFactor)-FormSketchpad.sbY.Position;
      with cv do begin
        Pen.Color := cl;
        Pen.Style := psDot;
        Pen.Width := 1;
        Ellipse(p1.x-(CircleSize*CircleFactor),
                p1.y-(CircleSize*CircleFactor),
                p1.x+(CircleSize*CircleFactor),
                p1.y+(CircleSize*CircleFactor));
      end;
   end;
end;

procedure TFormMain.DrawLinks(fThis,fOld: TLaserFrame; cv: TCanvas; clLink: TColor);
var pThis,pOld: TSmallPoint;
    i,j: integer;
    pfrom,pto: TPoint;
    dx,dy: integer;
    circlefactor: integer;
begin
   circlefactor := 2; //ZoomFactor;
   for i := 0 to Pred(fOld.Points.Count) do begin
      pOld := fOld.Points[i];
      if miFlipY.Checked then
       pfrom.x := 256*ZoomFactor-pOld.x*ZoomFactor-FormSketchpad.sbX.Position
        else pfrom.x := pOld.x*ZoomFactor-FormSketchpad.sbX.Position;
      if miFlipX.checked then
       pfrom.y := 256*ZoomFactor-pOld.y*ZoomFactor-FormSketchpad.sbY.Position
        else pfrom.y := pOld.y*ZoomFactor-FormSketchpad.sbY.Position;
      for j := 0 to Pred(fThis.Points.count) do begin
         if fThis.Links[j,i] then begin
            pThis := fThis.Points[j];
            if miFlipY.Checked then
             pto.x := 256*ZoomFactor-pThis.x*ZoomFactor-FormSketchpad.sbX.Position
              else pto.x := pThis.x*ZoomFactor-FormSketchpad.sbX.Position;
            if miFlipX.checked then
             pto.y := 256*ZoomFactor-pThis.y*ZoomFactor-FormSketchpad.sbY.Position
              else pto.y := pThis.y*ZoomFactor-FormSketchpad.sbY.Position;
            with cv do begin
               pen.color := clLink;
               pen.width := 1;
               if j<>SelectedPoint then pen.style := psDash;
               dx := Abs(pfrom.x - pto.x);
               dy := Abs(pfrom.y - pto.y);
               if (dx<(2*CircleSize*CircleFactor)) and (dy<(2*CircleSize*CircleFactor)) then
                  pThis.overlay := true else pThis.overlay := false;
               MoveTo(pfrom.x,pfrom.y);
               LineTo(pto.x,pto.y);
               pen.style := psSolid;
            end; // with cv
         end; // if links=true
      end; // for j
   end; // for i
end;

procedure TFormMain.ReDraw;
var mycopyrect: TRect;
    myf: TLaserFrame;
    c,c2,c3: TColor;
begin
   if (FFile <> nil) and (FFile.Count>0) and (not DontDraw) then begin
      Drawing := true;
      myf := FFile.Frames[currentframe];
      if miShowRuler.Checked then begin
         FormSketchpad.iTopRuler.Height := TopRulerHeight;
         FormSketchpad.iLeftRuler.Width := LeftRulerWidth;
         DrawRulers;
      end else begin
         FormSketchpad.iTopRuler.Height := 0;
         FormSketchpad.iLeftRuler.Width := 0;
      end;
      with FormSketchpad.pad.canvas do begin
         if FormSketchpad.pad.Width<256*ZoomFactor then FormSketchpad.sbX.Max := 256*ZoomFactor-FormSketchpad.pad.Width else FormSketchpad.sbX.Max := 0;
         if FormSketchpad.pad.Height<256*ZoomFactor then FormSketchpad.sbY.Max := 256*ZoomFactor-FormSketchpad.pad.Height else FormSketchpad.sbY.Max := 0;
         Brush.Color := clBtnFace;
         FillRect(Rect(0,256*ZoomFactor,FormSketchpad.pad.ClientRect.Right,FormSketchpad.pad.ClientRect.Bottom));
         FillRect(Rect(256*ZoomFactor,0,FormSketchpad.pad.ClientRect.Right,FormSketchpad.pad.ClientRect.Bottom));
         Brush.Color := MyOtherColors[myoc_bg];
         FillRect(Rect(0,0,256*ZoomFactor,256*ZoomFactor));
         if (myf.Bitmap<>nil) and (not miNoImg.Checked) then begin
            if not myf.Bitmap.Empty then begin
               if miFullImg.Checked then mycopyrect := Rect(0,0,myf.Bitmap.Width,myf.Bitmap.Height)
               else if miPartImg.Checked then begin
                  mycopyrect := myf.ImgRect;
               end else begin
                  mycopyrect := Rect(0,0,256,256);
               end;
               FormSketchpad.pad.Canvas.CopyRect(Rect(0-FormSketchpad.sbX.Position,
                                                   0-FormSketchpad.sbY.Position,
                                                   256*ZoomFactor-FormSketchpad.sbX.Position,
                                                   256*ZoomFactor-FormSketchpad.sbY.Position),
                                              myf.Bitmap.Canvas,mycopyrect);
            end;
         end;
         if miUseGrid.Checked then DrawGrid(FormSketchpad.pad.canvas,clDkGray);
         if miSnapHelp.Checked then DrawHelpLines(myf,FormSketchpad.pad.canvas,MyOtherColors[myoc_help]);
         if (currentframe>0) and (miShowBackframe.Checked) then begin
            if (FFile.Frames[Pred(currentframe)].Bits and 1)=0 then begin
               c := MyColors[0,myc_back];
               c2 := MyColors[0,myc_real];
               //c3 := MyColors[0,myc_link];
            end else begin
               c := MyColors[1,myc_back];
               c2 := MyColors[1,myc_real];
               //c3 := MyColors[1,myc_link];
            end;
            DrawFrame(FFile.frames[Pred(currentframe)], FormSketchpad.pad.canvas,false,false,c,c,c2,MyOtherColors[myoc_help]);
            //if miShowLinks.Checked and (CurrentFrame>0) then DrawLinks(myf,FFile.frames[currentframe-1],FormSketchpad.pad.canvas,c3);
            //### nach den nächsten frame setzen
         end;
         if ((myf.Bits and 1)=0) then begin
            c := MyColors[0,myc_norm];
            c2 := MyColors[0,myc_sel];
            c3 := MyColors[0,myc_real];
         end else begin
            c := MyColors[1,myc_norm];
            c2 := MyColors[1,myc_sel];
            c3 := MyColors[1,myc_real];
         end;
         DrawFrame(myf,FormSketchpad.pad.canvas,miShowReal.Checked,true,c,c2,c3,MyOtherColors[myoc_help]);
         //### test von oben
         if (currentframe>0) and (miShowBackframe.Checked) and (miShowLinks.Checked) then begin
            if ((FFile.frames[Pred(currentframe)].Bits and 1)=0) then
               c3 := MyColors[0,myc_link] else c3 := MyColors[1,myc_link];
            DrawLinks(myf,FFile.frames[Pred(currentframe)],FormSketchpad.pad.canvas,c3);
         end;
      end;
      Drawing := false;
   end;
   TimeLineRedraw;
end;

procedure TFormMain.RenumberList;
var i: integer;
    myp: TSmallPoint;
    myf: TLaserFrame;
begin
   myf := FFile.Frames[currentframe];
   for i := 0 to Pred(myf.Points.Count) do begin
      myp := myf.Points[i];
      myp.Caption := IntToStr(i);
   end;
   Redraw;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var i: integer;
    s: string;
    reg: TRegistry;
begin
   FFile := TLaserFrames.Create;
   msLivePreview := TMemoryStream.Create;
   Undo.Op := sNone;
   ZoomFactor := 2;
   CircleSize := 3;
   oldmovepos.x := -1;
   oldmovepos.y := -1;
   workstate := sMove;
   oldworkstate := sNone;
   GridWidth := 16;
   miGrid16.Checked := true;
   Dontdraw := false; Drawing := false;
   FileChanged := false;
   EquilateralSides := 6;
   EquilateralCenter.x := 128; EquilateralCenter.y := 128;
   EquilateralFinished := true;
   {$IFNDEF WINDOWS}
   reg := TRegIniFile.Create('settings.ini');
   {$else}
   reg := TRegistry.Create;
   {$endif}
   reg.RootKey := HKEY_CURRENT_USER;
   try
      if reg.OpenKey('\SOFTWARE\PepiMK Software\Heathcliff', true) then begin
         try
            with reg do begin
               LicenseAgree := GetValueDefault('LicenseAgree', false);
               odLC1.InitialDir := GetValueDefault('OpenDir', ExtractFilePath(ParamStr(0)));
               sdLC1.InitialDir := GetValueDefault('SaveDir', ExtractFilePath(ParamStr(0)));
               miShowPoints.Checked := GetValueDefault('ShowPoints', true);
               sbShowPoints.Down := miShowPoints.Checked;
               miShowNoOfPoints.Checked := GetValueDefault('ShowNumbers',true);
               sbShowNumPoints.Down := miShowNoOfPoints.Checked;
               miShowBackframe.Checked := GetValueDefault('ShowBackframe',true);
               sbShowBackFrame.Down := miShowBackframe.Checked;
               miShowLinks.Checked := GetValueDefault('ShowLinks',true);
               sbShowLinks.Down := miShowLinks.Checked;
               miShowReal.Checked := GetValueDefault('ShowRound',false);
               sbShowReal.Down := miShowReal.Checked;
               miUseGrid.Checked := GetValueDefault('ShowGrid',true);
               sbShowGrid.Down := miUseGrid.Checked;
               miSnapGrid.Checked := GetValueDefault('SnapGrid',true);
               sbSnapGrid.Down := miSnapGrid.Checked;
               miSnapHelp.Checked := GetValueDefault('SnapHelp',false);
               sbSnapHelp.Down := miSnapHelp.Checked;
               miShowRuler.Checked := GetValueDefault('ShowRuler',false);
               sbRuler.Down := miShowRuler.Checked;
               miFlipX.Checked := GetValueDefault('FlipX',false);
               sbFlipX.Down := miFlipX.Checked;
               miFlipY.Checked := GetValueDefault('FlipY',false);
               sbFlipY.Down := miFlipY.Checked;
               miCloseLoop.Checked := GetValueDefault('CloseLoop',true);
               sbCloseLoop.Down := miCloseLoop.Checked;
               tbRepeat.Down := GetValueDefault('RepeatPlay',true);
               tbLive.Down := GetValueDefault('LivePreview',true);
               case GetValueDefault('Image',0) of
                  0: sbNoImg.Down := true;
                  1: sbPartImg.Down := true;
                  else sbFullImg.Down := true;
               end;
               i := GetValueDefault('Zoom',2);
               case i of
                  1 : miZoom1.Checked := true;
                  2 : miZoom2.Checked := true;
                  3 : miZoom3.Checked := true;
                  4 : miZoom4.Checked := true;
                  5 : miZoom5.Checked := true;
                  6 : miZoom6.Checked := true;
                  7 : miZoom7.Checked := true;
                  8 : miZoom8.Checked := true;
                  else begin
                     miZoom2.Checked := true;
                     i := 2;
                  end;
               end;
               ZoomFactor := i;
               i := GetValueDefault('Grid',16);
               case i of
                  4 : miGrid4.Checked := true;
                  8 : miGrid8.Checked := true;
                  16 : miGrid16.Checked := true;
                  32 : miGrid32.Checked := true;
                  64 : miGrid64.Checked := true;
                  128 : miGrid128.Checked := true;
                  else begin
                     miGridCustom.Visible := true;
                     miGridCustom.Checked := true;
                     miGridCustom.Caption := '&'+IntToStr(i);
                  end;
               end;
               GridWidth := i;
               i := GetValueDefault('Circles',3);
               case i of
                  1 : miCircles1.Checked := true;
                  2 : miCircles2.Checked := true;
                  3 : miCircles3.Checked := true;
                  4 : miCircles4.Checked := true;
                  5 : miCircles5.Checked := true;
                  6 : miCircles6.Checked := true;
                  7 : miCircles7.Checked := true;
                  8 : miCircles8.Checked := true;
                  else begin
                     miCircles3.Checked := true;
                     i := 3;
                  end;
               end;
               CircleSize := i;
               FormMain.Left := GetValueDefault('WinLeft',0);
               FormMain.Top := GetValueDefault('WinTop',0);
               FormMain.Width := GetValueDefault('WinWidth',662);
               FormMain.Height := GetValueDefault('WinHeight',579);
               s := GetValueDefault('LastFile0','');
               if FileExistsUTF8(s) { *Converted from FileExists* } then begin
                  miLastFile0.Caption := '&0 '+s;
                  miLastFile0.Visible := true;
               end else miLastFile0.Visible := false;
               s := GetValueDefault('LastFile1','');
               if FileExistsUTF8(s) { *Converted from FileExists* } then begin
                  miLastFile1.Caption := '&1 '+s;
                  miLastFile1.Visible := true;
               end else miLastFile1.Visible := false;
               s := GetValueDefault('LastFile2','');
               if FileExistsUTF8(s) { *Converted from FileExists* } then begin
                  miLastFile2.Caption := '&2 '+s;
                  miLastFile2.Visible := true;
               end else miLastFile2.Visible := false;
               s := GetValueDefault('LastFile3','');
               if FileExistsUTF8(s) { *Converted from FileExists* } then begin
                  miLastFile3.Caption := '&3 '+s;
                  miLastFile3.Visible := true;
               end else miLastFile3.Visible := false;
               s := GetValueDefault('LastFile4','');
               if FileExistsUTF8(s) { *Converted from FileExists* } then begin
                  miLastFile4.Caption := '&4 '+s;
                  miLastFile4.Visible := true;
               end else miLastFile4.Visible := false;
               // benutzerdefinierte farben
            end;
         finally
            reg.CloseKey;
         end;
      end;
      if reg.OpenKey('\SOFTWARE\PepiMK Software\Heathcliff\Colors', true) then begin
         try
            with reg do begin
               MyColors[0,0] := GetValueDefault('Norm0',clRed);
               MyColors[1,0] := GetValueDefault('Norm1',clBlue);
               MyColors[0,1] := GetValueDefault('Real0',$004080FF);
               MyColors[1,1] := GetValueDefault('Real1',$00FF8080);
               MyColors[0,2] := GetValueDefault('Back0',clMaroon);
               MyColors[1,2] := GetValueDefault('Back1',$00A00000);
               MyColors[0,3] := GetValueDefault('Sel0',clGreen);
               MyColors[1,3] := GetValueDefault('Sel1',clGreen);
               MyColors[0,4] := GetValueDefault('Link0',clYellow);
               MyColors[1,4] := GetValueDefault('Link1',clYellow);
               MyColorNames[0] := GetValueDefault('Name0','&Red');
               MyColorNames[1] := GetValueDefault('Name1','&Blue');
               miColor0.Caption := MyColorNames[0];
               miColor1.Caption := MyColorNames[1];
               MyTextColors[0] := GetValueDefault('ThumbText',clLime);
               MyOtherColors[myoc_bg] := GetValueDefault('Background',clBlack);
               MyOtherColors[myoc_help] := GetValueDefault('HelpLines',clLime);
               MyOtherColors[myoc_rotcenter] := GetValueDefault('Rotation',clAqua);
               MyTimeLineColors[mytlc_lines] := GetValueDefault('TimelineLines',clRed);
               MyTimeLineColors[mytlc_areas] := GetValueDefault('TimelineAreas',$00404080);
               MyTimeLineColors[mytlc_text] := GetValueDefault('TimelineText',clLime);
               MyTimeLineColors[mytlc_back] := GetValueDefault('TimelineBackground',$00004000);
               MyTimes[0] := GetValueDefault('Time5Degree',30);
               MyTimes[1] := GetValueDefault('Time10Degree',55);
               MyTimes[2] := GetValueDefault('Time40Degree',66);
               MyTimes[3] := GetValueDefault('TimeEdges',30);
               //RegLoadToolbarPositionsEx(Self,HKEY_CURRENT_USER,'\SOFTWARE\PepiMK Software\Heathcliff\Panels');
            end;
         finally
            reg.CloseKey;
         end;
      end;
   finally
      FreeAndNil(reg);
   end;
   //Screen.Cursors[crMovePoint] := LoadCursor(hInstance,'MOVEPOINT');
   //Screen.Cursors[crAddPoint] := LoadCursor(hInstance,'ADDPOINT');
   //Screen.Cursors[crDelPoint] := LoadCursor(hInstance,'DELPOINT');
   //Screen.Cursors[crZoom] := LoadCursor(hInstance,'ZOOM');
   //Screen.Cursors[crPointType] := LoadCursor(hInstance,'POINTTYPE'); //Screen.Cursors[crCross];
   //Screen.Cursors[crAnimFrame] := LoadCursor(hInstance,'ANIM'); //Screen.Cursors[crCross];
   //Screen.Cursors[crMoveRotFrame] := Screen.Cursors[crSize]; //LoadCursor(hInstance,'ANIM'); //Screen.Cursors[crCross];
   //Screen.Cursors[crSetPoints] := Screen.Cursors[crCross]; //LoadCursor(hInstance,'ANIM'); //Screen.Cursors[crCross];
   //MyIcons[0] := LoadIcon(hInstance,'FULLICON');
   //Application.Icon.Handle := MyIcons[0];
   //FormMain.Icon.Handle := MyIcons[0];
end;

procedure TFormMain.LoadFromFile(fn: string; var y: TLaserFrames; realload: boolean);
var i: integer;
    yNew: TLaserFrames;
    fFrame: TLaserFrame;
begin
   Dontdraw := true;
   yNew := TLaserFrames.Create;
   repeat until not Drawing;

   if realload then begin
      //panelPoint.Caption := 'Point: -';
      //panelPointLink.Caption := 'Link: -';
      //panelPointX.Caption := 'X: -';
      //panelPointY.Caption := 'Y: -';
      FormSketchpad.panelFrameSwitcher.Caption := '0';
      FormSketchpad.Caption := fn;
      FormSketchpad.Parent:=Self;
      FormSketchpad.Align:=alClient;
      lbThumbs.Items.Clear;
   end;

   yNew.LoadFromFile(fn);

   if realload then begin
      {li := lvFrames.Items.Add;
      if Length(myf.framename)>0 then li.caption := myf.framename
      else li.caption := 'noname';
      li.subitems.add(IntToStr(myf.delay));
      li.subitems.add(IntToStr(myf.morph));}
      for i := 0 to Pred(yNew.Count)
       do lbThumbs.Items.Add(TLaserFrame(yNew.Frames[i]).FrameName);
   end;
   if yNew.count>0 then begin
      fFrame := yNew.frames[0];
      if realload then begin
         FormSketchpad.sbFrames.Max := Pred(yNew.count);
         miBackImg.Caption := 'Background image: '+fFrame.ImgName;
         if FileExistsUTF8(fFrame.ImgName) { *Converted from FileExists* } then begin
            miFullImg.Enabled := true;
            miPartImg.Enabled := true;
            miChoosePart.Enabled := true;
            sbPartImg.Enabled := true;
            sbFullImg.Enabled := true;
         end;
      end;
   end else begin
      fFrame := yNew.Add;
   end;
   y := nil;
   y := yNew;
   if realload then begin
      if lbThumbs.Items.Count>0
       then lbThumbs.ItemIndex := 0;
      FormSketchpad.sbFramesChange(nil);
   end;
   Dontdraw := false;
   if FileExistsUTF8(fn+'.wav') { *Converted from FileExists* } then begin
      tbPlay.Enabled := true;
      tbShowPreview.Enabled := true;
   end else begin
      tbPlay.Enabled := false;
      tbShowPreview.Enabled := false;
   end;
end;

procedure TFormMain.SaveToFile(fn: string; var y: TLaserFrames);
var //f: file of TPoint;
    fLC1File: file;
    iFrame,j,l: integer;
    bData: byte;
    wFrameCount: word;
    fFrame: TLaserFrame;
    pPoint: TSmallPoint;
    sMagic: array[1..4] of AnsiChar;
    aPoint: array[1..11] of byte;
    wSize: word;
    mThis, mOld, cThis, cOld, wWord: word;
begin
   Dontdraw := true;
   wFrameCount := y.count;
   FileMode := 2;
   AssignFile(fLC1File,fn);
   ReWrite(fLC1File,1);
   sMagic := 'LC1Y';
   BlockWrite(fLC1File,sMagic,4);
   bData := 6; // Dateiversion
   BlockWrite(fLC1File,bData,1);
   for iFrame := 0 to Pred(wFrameCount) do begin
      fFrame := y.frames[iFrame];
      // frame name
      wSize := Length(fFrame.FrameName);
      BlockWrite(fLC1File, wSize, 2);
      for j := 0 to Pred(Length(fFrame.FrameName)) do begin
         bData := Ord(fFrame.FrameName[j+1]);
         BlockWrite(fLC1File, bData, 1);
      end;
      // frame delay
      BlockWrite(fLC1File,fFrame.Delay,2);
      // frame morph time
      BlockWrite(fLC1File,fFrame.Morph,2);
      // effect type
      BlockWrite(fLC1File,fFrame.Effect,2);
      // effect param
      BlockWrite(fLC1File,fFrame.EffectParam,2);
      // rotcenter & auxcenter
      BlockWrite(fLC1File,fFrame.RotCenter.x,1);
      BlockWrite(fLC1File,fFrame.RotCenter.y,1);
      BlockWrite(fLC1File,fFrame.AuxCenter.x,1);
      BlockWrite(fLC1File,fFrame.AuxCenter.y,1);
      // bits for some stuff
      BlockWrite(fLC1File,fFrame.Bits,2);
      // image name
      wSize := Length(fFrame.ImgName);
      BlockWrite(fLC1File,wSize,2);
      for j := 0 to Pred(Length(fFrame.ImgName)) do begin
         bData := Ord(fFrame.ImgName[j+1]);
         BlockWrite(fLC1File,bData,1);
      end;
      // image size
      BlockWrite(fLC1File,fFrame.ImgRect,sizeof(TRect));
      // helplines
      wSize := Length(fFrame.HelpLines.x);
      BlockWrite(fLC1File,wSize,2);
      for j := 0 to Pred(Length(fFrame.HelpLines.x)) do begin
         BlockWrite(fLC1File,fFrame.HelpLines.x[j],1);
      end;
      wSize := Length(fFrame.HelpLines.y);
      BlockWrite(fLC1File,wSize,2);
      for j := 0 to Pred(Length(fFrame.HelpLines.y)) do begin
         BlockWrite(fLC1File,fFrame.HelpLines.y[j],1);
      end;
      wSize := Length(fFrame.HelpLines.d[0]);
      BlockWrite(fLC1File,wSize,2);
      for j := 0 to Pred(Length(fFrame.HelpLines.d[0])) do begin
         BlockWrite(fLC1File,fFrame.HelpLines.d[0,j].x,1);
         BlockWrite(fLC1File,fFrame.HelpLines.d[0,j].y,1);
         BlockWrite(fLC1File,fFrame.HelpLines.d[1,j].x,1);
         BlockWrite(fLC1File,fFrame.HelpLines.d[1,j].y,1);
      end;
      // points
      wSize := fFrame.Points.count;
      BlockWrite(fLC1File,wSize,2);
      for j := 0 to Pred(fFrame.Points.Count) do begin
         pPoint := fFrame.Points[j];
         for l := 1 to 5 do begin
            aPoint[l] := Ord(pPoint.Caption[l]);
         end;
         aPoint[6] := Byte(pPoint.x);
         aPoint[7] := Byte(pPoint.y);
         aPoint[8] := Byte(Hi(pPoint.p));
         aPoint[9] := Byte(Lo(pPoint.p));
         aPoint[10] := Byte(Hi(pPoint.bits));
         aPoint[11] := Byte(Lo(pPoint.bits));
         BlockWrite(fLC1File,aPoint,11);
      end;
      // link-matrix
      mThis := fFrame.Points.count;
      if iFrame>0 then begin
         mOld := y.frames[Pred(iFrame)].Points.count
      end else begin
         mOld := 0;
      end;
      SetLength(fFrame.links,mThis,mOld);
      BlockWrite(fLC1File,mThis,2);
      BlockWrite(fLC1File,mOld,2);
      if (mThis>0) and (mOld>0) then begin
         for cOld := 0 to Pred(mOld) do begin
            cThis := 0;
            wWord := 0;
            while cThis<mThis do begin
               if fFrame.links[cThis,cOld] then begin
                  l := (1 shl (cThis mod 16));
               end else begin
                  l := 0;
               end;
               wWord := wWord or l;
               if ((cThis mod 16)=15) or ((cThis+1)>=mThis) then begin
                  BlockWrite(fLC1File,wWord,2);
               end;
               Inc(cThis);
            end; // while
         end; // for
      end; // if matrix > 0
   end;
   CloseFile(fLC1File);
   FormSketchpad.Caption := fn;
   MessageDlg('File was saved.',mtConfirmation,[mbOK],0);
   Dontdraw := false;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
   Redraw;
end;

procedure TFormMain.miMoveNoRedrawClick(Sender: TObject);
begin
   miMoveNoRedraw.Checked := not miMoveNoRedraw.Checked;
   Redraw;
end;

procedure TFormMain.miCirclesizeClick(Sender: TObject);
var s: string;
    i,ec: integer;
begin
   s := InputBox('Enter size of circles','new size',IntToStr(CircleSize));
   Val(s,i,ec);
   if ec=0 then Circlesize := i;
   case CircleSize of
      1 : miCircles1.Checked := true;
      2 : miCircles2.Checked := true;
      3 : miCircles3.Checked := true;
      4 : miCircles4.Checked := true;
      5 : miCircles5.Checked := true;
      6 : miCircles6.Checked := true;
      7 : miCircles7.Checked := true;
      8 : miCircles8.Checked := true;
      else begin
         miCircles3.Checked := true;
         CircleSize := 3;
      end;
   end;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;
end;

procedure TFormMain.miZoomClick(Sender: TObject);
var s: string;
    i,ec: integer;
begin
   s := InputBox('Enter zoom factor','new factor',IntToStr(ZoomFactor));
   Val(s,i,ec);
   if ec=0 then ZoomFactor := i;
   case ZoomFactor of
      1 : miZoom1.Checked := true;
      2 : miZoom2.Checked := true;
      3 : miZoom3.Checked := true;
      4 : miZoom4.Checked := true;
      5 : miZoom5.Checked := true;
      6 : miZoom6.Checked := true;
      7 : miZoom7.Checked := true;
      8 : miZoom8.Checked := true;
      else begin
         miZoom2.Checked := true;
         ZoomFactor := 2;
      end;
   end;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TFormMain.sbMoveClick(Sender: TObject);
begin
   workstate := sMove;
end;

procedure TFormMain.sbAddClick(Sender: TObject);
begin
   workstate := sAdd;
end;

procedure TFormMain.sbDelClick(Sender: TObject);
begin
   workstate := sDel;
end;

procedure TFormMain.sbAnimClick(Sender: TObject);
begin
   workstate := sAnim;
end;

procedure TFormMain.sbPointTypeClick(Sender: TObject);
begin
   workstate := sPointType;
end;

procedure TFormMain.miGridClick(Sender: TObject);
var s: string;
    i,ec: integer;
begin
   s := InputBox('Enter grid width','new width',IntToStr(GridWidth));
   Val(s,i,ec);
   if ec=0 then GridWidth := i;
   case GridWidth of
      4 : miGrid4.Checked := true;
      8 : miGrid4.Checked := true;
      16 : miGrid4.Checked := true;
      32 : miGrid4.Checked := true;
      64 : miGrid4.Checked := true;
      128 : miGrid4.Checked := true;
      else begin
         miGridCustom.Visible := true;
         miGridCustom.Checked := true;
         miGridCustom.Caption := '&'+IntToStr(GridWidth);
      end;
   end;
   miGrid.Caption := 'Grid: '+IntToStr(GridWidth)+'...';
   Redraw;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var reg: TRegistry;
begin
   reg := TRegistry.Create;
   reg.RootKey := HKEY_CURRENT_USER;
   try
      if reg.OpenKey('\SOFTWARE\PepiMK Software\Heathcliff', true)
      then with reg do begin
         SetValue('OpenDir',odLC1.InitialDir);
         SetValue('SaveDir',sdLC1.InitialDir);
         SetValue('ShowPoints',sbShowPoints.Down);
         SetValue('ShowNumbers',sbShowNumPoints.Down);
         SetValue('ShowBackframe',sbShowBackframe.Down);
         SetValue('ShowLinks',sbShowLinks.Down);
         SetValue('ShowRound',sbShowReal.Down);
         SetValue('ShowGrid',sbShowGrid.Down);
         SetValue('SnapGrid',sbSnapGrid.Down);
         SetValue('SnapHelp',sbSnapHelp.Down);
         SetValue('ShowRuler',sbRuler.Down);
         SetValue('FlipX',sbFlipX.Down);
         SetValue('FlipY',sbFlipY.Down);
         SetValue('CloseLoop',sbCloseLoop.Down);
         SetValue('RepeatPlay',tbRepeat.Down);
         SetValue('LivePreview',tbLive.Down);
         if sbNoImg.Down
          then SetValue('Image',0)
           else if sbPartImg.Down
            then SetValue('Image',1)
             else if sbFullImg.Down
              then SetValue('Image',2);
         SetValue('Zoom',ZoomFactor);
         SetValue('Grid',GridWidth);
         SetValue('Circles',CircleSize);
         SetValue('WinLeft',FormMain.Left);
         SetValue('WinTop',FormMain.Top);
         SetValue('WinWidth',FormMain.Width);
         SetValue('WinHeight',FormMain.Height);
         SetValue('Lastfile0',Copy(miLastfile0.Caption,4,Length(miLastfile0.Caption)-3));
         SetValue('Lastfile1',Copy(miLastfile1.Caption,4,Length(miLastfile1.Caption)-3));
         SetValue('Lastfile2',Copy(miLastfile2.Caption,4,Length(miLastfile2.Caption)-3));
         SetValue('Lastfile3',Copy(miLastfile3.Caption,4,Length(miLastfile3.Caption)-3));
         SetValue('Lastfile4',Copy(miLastfile4.Caption,4,Length(miLastfile4.Caption)-3));
         CloseKey;
      end;
      if reg.OpenKey('\SOFTWARE\PepiMK Software\Heathcliff\Colors', true)
      then with reg do begin
         SetValue('Norm0',MyColors[0,0]);
         SetValue('Norm1',MyColors[1,0]);
         SetValue('Real0',MyColors[0,1]);
         SetValue('Real1',MyColors[1,1]);
         SetValue('Back0',MyColors[0,2]);
         SetValue('Back1',MyColors[1,2]);
         SetValue('Sel0',MyColors[0,3]);
         SetValue('Sel1',MyColors[1,3]);
         SetValue('Link0',MyColors[0,4]);
         SetValue('Link1',MyColors[1,4]);
         SetValue('Name0',MyColorNames[0]);
         SetValue('Name1',MyColorNames[1]);
         SetValue('ThumbText',MyTextColors[0]);
         SetValue('Background',MyOtherColors[myoc_bg]);
         SetValue('HelpLines',MyOtherColors[myoc_help]);
         SetValue('Rotation',MyOtherColors[myoc_rotcenter]);
         SetValue('TimelineLines',MyTimeLineColors[mytlc_lines]);
         SetValue('TimelineAreas',MyTimeLineColors[mytlc_areas]);
         SetValue('TimelineText',MyTimeLineColors[mytlc_text]);
         SetValue('TimelineBackground',MyTimeLineColors[mytlc_back]);
         SetValue('Time5Degree',MyTimes[0]);
         SetValue('Time10Degree',MyTimes[1]);
         SetValue('Time40Degree',MyTimes[2]);
         SetValue('TimeEdges',MyTimes[3]);
         CloseKey;
      end;
      //RegSaveToolbarPositionsEx(Self, HKEY_CURRENT_USER, '\SOFTWARE\PepiMK Software\Heathcliff\Panels');
   finally
      FreeAndNil(reg);
   end;
end;

procedure TFormMain.miOpt2xZoomClick(Sender: TObject);
var divx,divy: integer;
begin
   {fMain.Width := 247 + (2*256);
   fMain.Height := 67 + (2*256);
   if sbRuler.Down then fMain.Width := fMain.Width + LeftRulerWidth;
   if sbRuler.Down then fMain.Height := fMain.Height + TopRulerHeight;}
   divx := 2*256-FormSketchpad.Pad.Width;
   FormMain.Width := FormMain.Width + divx;
   divy := 2*256-FormSketchpad.Pad.Height;
   FormMain.Height := FormMain.Height + divy;
   ZoomFactor := 2;
   miZoom3.Checked := true;
   miZoom.Caption := '&Zoom: 2x...';
   Redraw;
end;

procedure TFormMain.miRenumClick(Sender: TObject);
begin
   RenumberList;
end;

procedure TFormMain.iThumbsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sel: word;
begin
   sel := y div 33;
   if sel < FFile.count then begin
      if sel <= FormSketchpad.sbFrames.Max then begin
         FormSketchpad.sbFrames.Position := sel;
         FormSketchpad.sbFramesChange(nil);
      end;
   end;
end;

procedure TFormMain.miLastfile0Click(Sender: TObject);
begin
   LoadFromFile(Copy(miLastfile0.Caption,4,Length(miLastfile0.Caption)-3),FFile,true);
   Redraw;
end;

procedure TFormMain.miLastfile1Click(Sender: TObject);
begin
   LoadFromFile(Copy(miLastfile1.Caption,4,Length(miLastfile1.Caption)-3),FFile,true);
   Redraw;
end;

procedure TFormMain.miLastfile2Click(Sender: TObject);
begin
   LoadFromFile(Copy(miLastfile2.Caption,4,Length(miLastfile2.Caption)-3),FFile,true);
   Redraw;
end;

procedure TFormMain.miLastfile3Click(Sender: TObject);
begin
   LoadFromFile(Copy(miLastfile3.Caption,4,Length(miLastfile3.Caption)-3),FFile,true);
   Redraw;
end;

procedure TFormMain.milastfile4Click(Sender: TObject);
begin
   LoadFromFile(Copy(miLastfile4.Caption,4,Length(miLastfile4.Caption)-3),FFile,true);
   Redraw;
end;

procedure TFormMain.lbThumbsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var c: TColor;
    s,s2: string;
    //p1,ps,p2: TPoint;
begin
   if FFile<>nil then if index<FFile.count then with (Control as TListBox).Canvas do begin
      Brush.Color := MyOtherColors[myoc_bg];
      FillRect(Rect);
      Pen.Color := clGreen;
      Pen.Width := 1;
      if (odSelected in State) then begin
         Pen.Style := psDot;
         Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
      end;
      Pen.Style := psSolid;
      //MoveTo(Rect.Left,Rect.Bottom-1); LineTo(Rect.Right,Rect.Bottom-1);
      if ((FFile.frames[index].Bits and 1)=0) then
        c := MyColors[0,myc_norm] else c := MyColors[1,myc_norm];
      FormMain.DrawThumb(FFile.frames[index],(Control as TListBox).Canvas,
                      Rect.Left,Rect.Top+18,4,c);
      // sperrsymbol
      //..
      if (FFile.frames[index].Bits and 2)=2 then begin
         Font.Color := clRed;
         s := 'Locked';
         TextOut(Rect.Right-2-TextWidth(s),Rect.Top+2,s);
      end;
      // und texte
      Font.Color := MyTextColors[mytc_thumb];
      Pen.Color := clGreen;
      TextOut(Rect.Left+2,Rect.Top+2,FFile.frames[index].FrameName);
      TextOut(Rect.Left+66,Rect.Top+20,'Delay:');
      s := IntToStr(FFile.frames[index].Delay);
      TextOut(Rect.Right-6-TextWidth(s),Rect.Top+20,s);
      TextOut(Rect.Left+66,Rect.Top+34,'Morph:');
      s := IntToStr(FFile.frames[index].Morph);
      TextOut(Rect.Right-6-TextWidth(s),Rect.Top+34,s);
      TextOut(Rect.Left+66,Rect.Top+48,'Effect:');
      case FFile.frames[index].Effect of
         effect_slide    : s := 'Slide';
         effect_morph    : s := 'Morph';
         effect_plode    : s := '-plode';
         effect_xflip    : s := 'X-Flip';
         effect_yflip    : s := 'Y-Flip';
         effect_dflip    : s := 'D-Flip';
         effect_rotate   : s := 'Rotate';
         effect_drain    : s := 'Drain';
      else s := '?';
      end;
      TextOut(Rect.Right-6-TextWidth(s),Rect.Top+48,s);
      //TextOut(Rect.Left+66,Rect.Top+62,'Color:');
      //if ((Tframe(FFile.frames[index]).bits and 1)=0) then s := MyColorNames[0]
      //else s := MyColorNames[1];
      //if Pos('&',s)>0 then Delete(s,Pos('&',s),1);
      //TextOut(Rect.Right-6-TextWidth(s),Rect.Top+62,s);
      if FFile.frames[index].Effect in [effect_rotate,effect_drain,effect_xflip,effect_yflip,effect_dflip,effect_plode] then begin
         case FFile.frames[index].Effect of
            effect_rotate,effect_drain: begin s := 'Rotation:'; s2 := IntToStr(FFile.frames[index].EffectParam)+'°'; end;
            effect_xflip,effect_yflip,effect_dflip,effect_plode: begin s := 'Rotation:'; if FFile.frames[index].EffectParam = 1 then s2 := 'Trig' else s2 := 'Lin'; end;
            else begin s := 'Param:'; s2 := IntToStr(FFile.frames[index].EffectParam)+'°'; end;
         end;
         TextOut(Rect.Left+66,Rect.Top+62,s);
         TextOut(Rect.Right-6-TextWidth(s2),Rect.Top+62,s2);
      end;
   end;
end;

procedure TFormMain.lbThumbsClick(Sender: TObject);
begin
   if (lbThumbs.ItemIndex>-1) and (lbThumbs.ItemIndex<FFile.count) then begin
      FormSketchpad.sbFrames.Position := lbThumbs.ItemIndex;
      FormSketchpad.sbFramesChange(nil);
   end;
end;

procedure TFormMain.miGrid4Click(Sender: TObject);
begin
   GridWidth := 4;
   miGrid.Caption := 'Grid: 4';
   miGrid4.Checked := true;
   Redraw;
end;

procedure TFormMain.miGrid8Click(Sender: TObject);
begin
   GridWidth := 8;
   miGrid.Caption := 'Grid: 8';
   miGrid8.Checked := true;
   Redraw;
end;

procedure TFormMain.miGrid16Click(Sender: TObject);
begin
   GridWidth := 16;
   miGrid.Caption := 'Grid: 16';
   miGrid16.Checked := true;
   Redraw;
end;

procedure TFormMain.miGrid32Click(Sender: TObject);
begin
   GridWidth := 32;
   miGrid.Caption := 'Grid: 32';
   miGrid32.Checked := true;
   Redraw;
end;

procedure TFormMain.miGrid64Click(Sender: TObject);
begin
   GridWidth := 64;
   miGrid.Caption := 'Grid: 64';
   miGrid64.Checked := true;
   Redraw;
end;

procedure TFormMain.miGrid128Click(Sender: TObject);
begin
   GridWidth := 128;
   miGrid.Caption := 'Grid: 128';
   miGrid128.Checked := true;
   Redraw;
end;

procedure TFormMain.miGridCustomClick(Sender: TObject);
var s: string;
    i,ec: integer;
begin
   s := Copy(miGridCustom.Caption, 2, Pred(Length(miGridCustom.Caption)));
   Val(s,i,ec);
   if ec=0 then begin
      GridWidth := i;
      miGrid.Caption := 'Grid: '+s;
      miGridCustom.Checked := true;
   end;
   Redraw;
end;

procedure TFormMain.miZoom1Click(Sender: TObject);
begin
   ZoomFactor := 1;
   miZoom1.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom2Click(Sender: TObject);
begin
   ZoomFactor := 2;
   miZoom2.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom3Click(Sender: TObject);
begin
   ZoomFactor := 3;
   miZoom3.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom4Click(Sender: TObject);
begin
   ZoomFactor := 4;
   miZoom4.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom5Click(Sender: TObject);
begin
   ZoomFactor := 5;
   miZoom5.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom6Click(Sender: TObject);
begin
   ZoomFactor := 6;
   miZoom6.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom7Click(Sender: TObject);
begin
   ZoomFactor := 7;
   miZoom7.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miZoom8Click(Sender: TObject);
begin
   ZoomFactor := 8;
   miZoom8.Checked := true;
   miZoom.Caption := '&Zoom: '+IntToStr(ZoomFactor)+'x...';
   Redraw;

end;

procedure TFormMain.miCircles1Click(Sender: TObject);
begin
   CircleSize := 1;
   miCircles1.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;
end;

procedure TFormMain.miCircles2Click(Sender: TObject);
begin
   CircleSize := 2;
   miCircles2.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles3Click(Sender: TObject);
begin
   CircleSize := 3;
   miCircles3.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles4Click(Sender: TObject);
begin
   CircleSize := 4;
   miCircles4.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles5Click(Sender: TObject);
begin
   CircleSize := 5;
   miCircles5.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles6Click(Sender: TObject);
begin
   CircleSize := 6;
   miCircles6.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles7Click(Sender: TObject);
begin
   CircleSize := 7;
   miCircles7.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miCircles8Click(Sender: TObject);
begin
   CircleSize := 8;
   miCircles8.Checked := true;
   miCirclesize.Caption := 'Circlesize: '+IntToStr(CircleSize)+'...';
   Redraw;

end;

procedure TFormMain.miColor0Click(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   if (myf.Bits and 1)=1 then Dec(myf.Bits);
   //if (myf.bits and 1)=0 then Inc(myf.bits);
   lbThumbs.Refresh;
   miColor0.Checked := true;
   Redraw;
end;

procedure TFormMain.miColor1Click(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   //if (myf.bits and 1)=1 then Dec(myf.bits);
   if (myf.Bits and 1)=0 then Inc(myf.Bits);
   lbThumbs.Refresh;
   miColor1.Checked := true;
   Redraw;
end;

procedure TFormMain.miEffectSlideClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_slide;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.miEffectMorphClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_morph;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.miEffectPlodeClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_plode;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.miEffectXFlipClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_xflip;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.miEffectYFlipClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_yflip;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.lbThumbsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var was: integer;
begin
   if (Button=mbLeft) then begin
      was := lbThumbs.ItemAtPos(Point(x,y),false);
      if (was>-1) and (was<FFile.count) then begin
         if (FFile.frames[was].Bits and 2)=0 then lbThumbs.BeginDrag(false,5);
      end;
   end;
end;

procedure TFormMain.lbThumbsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var wo: integer;
begin
   wo := lbThumbs.ItemAtPos(Point(x,y),false);
   if (wo > -1) and (wo<>lbThumbs.ItemIndex)
   and (not ((lbThumbs.ItemIndex=Pred(lbThumbs.Items.Count))
   and (wo=lbThumbs.Items.Count))) then begin
      Accept := (Source Is TListBox);
   end else Accept := false;
end;

procedure TFormMain.lbThumbsDragDrop(Sender, Source: TObject; X, Y: Integer);
var iTargetFrame, iSelectedFrame, neupos: integer;
    myf,mypf,mynf: TLaserFrame;
    doit: boolean;
begin
   if (Sender is TListBox) and (Source Is TListBox) then begin
      iTargetFrame := lbThumbs.ItemAtPos(Point(x,y),false);
      iSelectedFrame := lbThumbs.ItemIndex;
      if (iTargetFrame>-1) and (iSelectedFrame>-1) then begin
         //FormSketchpad.Caption := 'Dragging '+IntToStr(was)+' B4: '+IntToStr(wo);
         myf := FFile.frames[iSelectedFrame];
         if myf.Effect=1 then
         doit := (MessageDlg('Do you really want to move this frame? The link matrix will be corrupted after this!',
                  mtWarning,[mbYes,mbNo],0)=mrYes)
         else doit := true;
         if doit then begin
            if (iSelectedFrame<(Pred(FFile.Count))) then begin
               mynf := FFile.frames[iSelectedFrame+1];
               if iSelectedFrame>0 then begin
                  mypf := FFile.frames[Pred(iSelectedFrame)];
                  SetLength(mynf.links,mynf.Points.count,mypf.Points.count);
               end else begin
                  SetLength(mynf.links,mynf.Points.count,0);
               end;
            end;
            FFile.Delete(iSelectedFrame);
            if iTargetFrame>FFile.count then begin
               neupos := FFile.Add(myf);
            end else begin
               FFile.Insert(iTargetFrame, myf);
               neupos := iTargetFrame;
            end;
            if neupos<(Pred(FFile.count)) then begin
               mynf := FFile.frames[neupos+1];
               SetLength(mynf.links,mynf.Points.count,myf.Points.count);
            end;
            if neupos>0 then begin
               mypf := FFile.frames[Pred(neupos)];
               SetLength(myf.links,myf.Points.count,mypf.Points.count);
            end else begin
               SetLength(myf.links,myf.Points.count,0);
            end;
            lbThumbs.ItemIndex := neupos;
         end;
      end;
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.miShowRealClick(Sender: TObject);
begin
   miShowReal.Checked := not miShowReal.Checked;
   sbShowReal.Down := miShowReal.Checked;
   Redraw;
end;

procedure TFormMain.SetColorsFromDialog;
begin
   MyColors[0,myc_norm] := FormColors.imgNorm0.Tag;
   MyColors[1,myc_norm] := FormColors.imgNorm1.Tag;
   MyColors[0,myc_real] := FormColors.imgReal0.Tag;
   MyColors[1,myc_real] := FormColors.imgReal1.Tag;
   MyColors[0,myc_back] := FormColors.imgBack0.Tag;
   MyColors[1,myc_back] := FormColors.imgBack1.Tag;
   MyColors[0,myc_sel] := FormColors.imgSel0.Tag;
   MyColors[1,myc_sel] := FormColors.imgSel1.Tag;
   MyColors[0,myc_link] := FormColors.imgLink0.Tag;
   MyColors[1,myc_link] := FormColors.imgLink1.Tag;
   MyColorNames[0] := FormColors.editColor0.Text;
   MyColorNames[1] := FormColors.editColor1.Text;
   miColor0.Caption := MyColorNames[0];
   miColor1.Caption := MyColorNames[1];
   MyTextColors[mytc_thumb] := FormColors.imgThumbText.Tag;
   MyOtherColors[myoc_bg] := FormColors.imgBackground.Tag;
   MyOtherColors[myoc_help] := FormColors.imgHelpLines.Tag;
   MyOtherColors[myoc_rotcenter] := FormColors.imgRotCenter.Tag;
   MyTimelineColors[mytlc_lines] := FormColors.imgTimelineLines.Tag;
   MyTimelineColors[mytlc_areas] := FormColors.imgTimelineAreas.Tag;
   MyTimelineColors[mytlc_text] := FormColors.imgTimelineText.Tag;
   MyTimelineColors[mytlc_back] := FormColors.imgTimelineBack.Tag;
end;

procedure TFormMain.miFramecolorsClick(Sender: TObject);
procedure FillCanvas(c: TImage; cl: TColor);
begin
   with c.canvas do begin
      Brush.Color := cl;
      FillRect(ClipRect);
      c.Tag := cl;
   end;
end;

begin
   with FormColors do begin
      FillCanvas(imgNorm0,MyColors[0,myc_norm]);
      FillCanvas(imgNorm1,MyColors[1,myc_norm]);
      FillCanvas(imgReal0,MyColors[0,myc_real]);
      FillCanvas(imgReal1,MyColors[1,myc_real]);
      FillCanvas(imgBack0,MyColors[0,myc_back]);
      FillCanvas(imgBack1,MyColors[1,myc_back]);
      FillCanvas(imgSel0,MyColors[0,myc_sel]);
      FillCanvas(imgSel1,MyColors[1,myc_sel]);
      FillCanvas(imgLink0,MyColors[0,myc_link]);
      FillCanvas(imgLink1,MyColors[1,myc_link]);
      editColor0.Text := MyColorNames[0];
      editColor1.Text := MyColorNames[1];
      FillCanvas(imgThumbText,MyTextColors[mytc_thumb]);
      FillCanvas(imgBackground,MyOtherColors[myoc_bg]);
      FillCanvas(imgHelpLines,MyOtherColors[myoc_help]);
      FillCanvas(imgRotCenter,MyOtherColors[myoc_rotcenter]);
      FillCanvas(imgTimelineLines,MyTimelineColors[mytlc_lines]);
      FillCanvas(imgTimelineAreas,MyTimelineColors[mytlc_areas]);
      FillCanvas(imgTimelineText,MyTimelineColors[mytlc_text]);
      FillCanvas(imgTimelineBack,MyTimelineColors[mytlc_back]);
   end;
   if FormColors.ShowModal=mrOK then begin
      SetColorsFromDialog;
      Redraw;
      lbThumbs.Refresh;
   end;
end;

procedure TFormMain.miAboutClick(Sender: TObject);
begin
   MessageDlg('Info about Heathcliff: '+#13#10#13#10+verinfo+#13#10#13#10+'This one goes out to the one i love...',mtInformation,[mbOK],0);
end;


procedure TFormMain.aNewFileExecute(Sender: TObject);
var myf: TLaserFrame;
    doit: boolean;
begin
   if not filechanged then doit := true else
   doit := (MessageDlg('Do you really want to discard any changes made to this file?',
                 mtConfirmation,[mbYes,mbNo],0)=mrYes);
   if doit then begin
      FFile.Clear;
      myf := TLaserFrame.Create;
      FormSketchpad.sbFrames.Position := 0;
      FormSketchpad.sbFrames.Max := 0;
      FormSketchpad.panelFrameSwitcher.Caption := '0';
      FormSketchpad.Caption := 'new file';
      FormSketchpad.Caption := 'new file';
      FormSketchpad.Parent:=Self;
      FormSketchpad.Align:=alClient;
      FFile.Filename := '';
      FFile.Add(myf);
      lbThumbs.Items.Clear;
      lbThumbs.Items.Add('');
   end;
   Redraw;
end;

procedure TFormMain.aOpenFileExecute(Sender: TObject);
var doit: boolean;
begin
   if not filechanged then doit := true else
   doit := (MessageDlg('Do you really want to discard any changes made to this file?',
                 mtConfirmation,[mbYes,mbNo],0)=mrYes);
   odLC1.Title := 'Open Yoghurt file...';
   odLC1.Filename := '';
   if doit then if odLC1.Execute then begin
      odLC1.InitialDir := ExtractFilePath(odLC1.Filename);
      LoadFromFile(odLC1.Filename,FFile,true);
      AddUsedFile(odLC1.Filename);
      FileChanged := false;
      Redraw;
   end;
end;

procedure TFormMain.aSaveFileExecute(Sender: TObject);
begin
   if FileExistsUTF8(FFile.Filename) { *Converted from FileExists* } then SaveToFile(FFile.Filename,FFile)
   else aSaveAsFileExecute(Sender);
   FileChanged := false;
end;

procedure TFormMain.aSaveAsFileExecute(Sender: TObject);
begin
   sdLC1.Filename := FFile.Filename;
   if sdLC1.Execute then begin
      sdLC1.InitialDir := ExtractFilePath(sdLC1.Filename);
      SaveToFile(sdLC1.filename,FFile);
      AddUsedFile(sdLC1.Filename);
      FFile.Filename := sdLC1.filename;
      FileChanged := false;
   end;
end;

procedure TFormMain.InsertFrame(f: TLaserFrame; pos: integer);
var myf, mynf: TLaserFrame;
begin
   myf := f;
   mynf := FormMain.FFile.frames[pos];
   if pos>0 then
      SetLength(myf.links,myf.Points.count,TLaserFrame(FFile.frames[Pred(pos)]).Points.count)
   else
      SetLength(myf.links,myf.Points.count,0);
   SetLength(mynf.links,mynf.Points.count,myf.Points.count);
   FFile.Insert(pos,myf);
   lbThumbs.Items.Insert(CurrentFrame,FFile.frames[Pred(FFile.Count)].FrameName);
   FormSketchpad.sbFrames.Max := FormSketchpad.sbFrames.Max + 1;
   FormSketchpad.sbFramesChange(nil);
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.DeleteFrame(pos: integer; var f: TLaserFrame);
var mynf,mypf: TLaserFrame;
begin
   filechanged := true;
   if FFile<>nil then if FFile.Count>0 then begin
      if FFile.Count>1 then begin
         if (pos<Pred(FFile.Count)) then begin
            mynf := FFile.frames[pos+1];
            if (pos=0) then begin
               SetLength(mynf.links,mynf.Points.count,0);
            end else begin
               mypf := FFile.frames[Pred(pos)];
               SetLength(mynf.links,mynf.Points.count,mypf.Points.count);
            end;
         end;
         f := FFile.frames[pos];
         FFile.Delete(pos);
         lbThumbs.Items.Delete(pos);
         if pos>0 then Dec(pos);
         FormSketchpad.panelFrameSwitcher.Caption := IntToStr(pos);
         FormSketchpad.sbFrames.Max := Pred(FFile.Count);
         FormSketchpad.sbFramesChange(nil);
      end else MessageDlg('You can''t delete the one and only frame!',mtError,[mbOK],0);
   end;
end;

procedure TFormMain.aDeleteFrameExecute(Sender: TObject);
var myf: TLaserFrame;
begin
   DeleteFrame(CurrentFrame,myf);
end;

procedure TFormMain.aImportFrameExecute(Sender: TObject);
begin
   FormImport.lb.Items.Clear;
   if FormImport.ShowModal=mrOK
   then if FormImport.lb.ItemIndex>-1 then begin
      InsertFrame(FormImport.FFile.frames[FormImport.lb.ItemIndex],CurrentFrame);
      FreeAndNil(FormImport.FFile);
   end;
end;

procedure TFormMain.aImportShowExecute(Sender: TObject);
var myf,mynf: TLaserFrame;
    y2: TLaserFrames;
    i: integer;
begin
   odLC1.Title := 'Import Yoghurt file...';
   if odLC1.Execute then begin
      if FileExistsUTF8(odLC1.Filename) { *Converted from FileExists* } then begin
         LoadFromFile(odLC1.Filename,y2,false);
         if y2<>nil then begin
            if y2.Count>0
            then for i := Pred(y2.Count) downto 0 do begin
               myf := y2.frames[i];
               if CurrentFrame>0 then
                  SetLength(myf.links,myf.Points.count,TLaserFrame(FFile.frames[Pred(CurrentFrame)]).Points.count)
               else
                  SetLength(myf.links,myf.Points.count,0);
               mynf := FFile.frames[CurrentFrame];
               FFile.Insert(CurrentFrame,myf);
               SetLength(mynf.links,mynf.Points.count,myf.Points.count);
               lbThumbs.Items.Insert(CurrentFrame,myf.FrameName);
               FormSketchpad.sbFrames.Max := FormSketchpad.sbFrames.Max + 1;
            end;
            FreeAndNil(y2);
         end;
      end;
   end;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.aHelpContentsExecute(Sender: TObject);
begin
  //Application.HelpJump('HelpContents');
end;

procedure TFormMain.sbShowNumPointsClick(Sender: TObject);
begin
   miShowNoOfPoints.Checked := not miShowNoOfPoints.Checked;
   sbShowNumPoints.Down := miShowNoOfPoints.Checked;
   Redraw;

end;

procedure TFormMain.sbShowLinksClick(Sender: TObject);
begin
   miShowLinks.Checked := not miShowLinks.Checked;
   sbShowLinks.Down := miShowLinks.Checked;
   Redraw;

end;

procedure TFormMain.sbShowRealClick(Sender: TObject);
begin
   miShowReal.Checked := not miShowReal.Checked;
   sbShowReal.Down := miShowReal.Checked;
   Redraw;

end;

procedure TFormMain.sbCloseLoopClick(Sender: TObject);
begin
   miCloseloop.Checked := not miCloseloop.Checked;
   sbCloseLoop.Down := miCloseLoop.Checked;
   Redraw;

end;

procedure TFormMain.sbShowGridClick(Sender: TObject);
begin
   miUseGrid.Checked := not miUseGrid.Checked;
   sbShowGrid.Down := miUseGrid.Checked;
   Redraw;

end;

procedure TFormMain.sbSnapGridClick(Sender: TObject);
begin
   miSnapGrid.Checked := not miSnapGrid.Checked;
   sbSnapGrid.Down := miSnapGrid.Checked;
end;

procedure TFormMain.sbNoImgClick(Sender: TObject);
begin
   miNoImg.Checked := true;
   sbNoImg.Down := miNoImg.Checked;
   Redraw;

end;

procedure TFormMain.sbPartImgClick(Sender: TObject);
begin
   miPartImg.Checked := true;
   sbPartImg.Down := miPartImg.Checked;
   Redraw;

end;

procedure TFormMain.sbFullImgClick(Sender: TObject);
begin
   miFullImg.Checked := true;
   sbFullImg.Down := miFullImg.Checked;
   Redraw;

end;

procedure TFormMain.sbFlipYClick(Sender: TObject);
begin
   miFlipY.Checked := not miFlipY.Checked;
   sbFlipY.Down := miFlipY.Checked;
   Redraw;

end;

procedure TFormMain.sbFlipXClick(Sender: TObject);
begin
   miFlipX.Checked := not miFlipX.Checked;
   sbFlipX.Down := miFlipX.Checked;
   Redraw;

end;

procedure TFormMain.sbShowPointsClick(Sender: TObject);
begin
   miShowPoints.Checked := not miShowPoints.Checked;
   sbShowPoints.Down := miShowPoints.Checked;
   Redraw;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if FileChanged then begin
      CanClose := (MessageDlg('Do you really want to discard any changes made to this file?',
                 mtConfirmation,[mbYes,mbNo],0)=mrYes);
   end else CanClose := true;
end;

procedure TFormMain.sbEffectClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   Inc(myf.Effect);
   if myf.Effect>7 then myf.Effect := 0;
   case myf.Effect of
      effect_slide  : miEffectSlide.Checked := true;
      effect_morph  : miEffectMorph.Checked := true;
      effect_plode  : miEffectPlode.Checked := true;
      effect_xflip  : miEffectXFlip.Checked := true;
      effect_yflip  : miEffectYFlip.Checked := true;
      effect_dflip  : miEffectDFlip.Checked := true;
      effect_rotate : miEffectRotate.Checked := true;
      effect_drain  : miEffectDrain.Checked := true;
   end;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.aAddFrameExecute(Sender: TObject);
var myf: TLaserFrame;
begin
   myf := FFile.Add;
   lbThumbs.Items.Add('');
   FormSketchpad.sbFrames.Max := Pred(FFile.Count);
end;

procedure TFormMain.aRenameFrameExecute(Sender: TObject);
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      lbThumbs.Items[CurrentFrame] := InputBox('Enter new framename','new name 4 frame',lbThumbs.Items[CurrentFrame]);
      TLaserFrame(FFile.frames[CurrentFrame]).FrameName := lbThumbs.Items[CurrentFrame];
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.aFrameDelayExecute(Sender: TObject);
var myf: TLaserFrame;
    s: string;
    i,ec: integer;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      s := InputBox('Enter new framedelay in ms','new delay',IntToStr(myf.Delay));
      Val(s,i,ec);
      if ec=0 then myf.Delay := i;
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.aFrameMorphExecute(Sender: TObject);
var myf: TLaserFrame;
    s: string;
    i,ec: integer;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      s := InputBox('Enter new morphtime in ms','new morphtime',IntToStr(myf.Morph));
      Val(s,i,ec);
      if ec=0 then myf.Morph := i;
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.tbMoveClick(Sender: TObject);
begin
   workstate := sMove;
   FormSketchpad.pad.Cursor := crMovePoint;
end;

procedure TFormMain.tbAddClick(Sender: TObject);
begin
   workstate := sAdd;
   FormSketchpad.pad.Cursor := crAddPoint;
end;

procedure TFormMain.tbDelClick(Sender: TObject);
begin
   workstate := sDel;
   FormSketchpad.pad.Cursor := crDelPoint;
end;

procedure TFormMain.tbZoomClick(Sender: TObject);
begin
   workstate := sZoom;
   FormSketchpad.pad.Cursor := crZoom;
end;

procedure TFormMain.toolbarFilesClose(Sender: TObject);
begin
   miFilePanel.Checked := false;
end;

procedure TFormMain.toolbarToolsClose(Sender: TObject);
begin
   miToolPanel.Checked := false;
end;

procedure TFormMain.toolbarPointsFramesClose(Sender: TObject);
begin
   miPointFramePanel.Checked := false;
end;

procedure TFormMain.toolbarDisplayClose(Sender: TObject);
begin
   miDisplayPanel.Checked := false;
end;

procedure TFormMain.miFilePanelClick(Sender: TObject);
begin
   toolbarFiles.Visible := not toolbarFiles.Visible;
   miFilePanel.Checked := toolbarFiles.Visible;
end;

procedure TFormMain.miPlayPanelClick(Sender: TObject);
begin
   toolbarPlay.Visible := not toolbarPlay.Visible;
   miPlayPanel.Checked := toolbarPlay.Visible;
end;

procedure TFormMain.miToolPanelClick(Sender: TObject);
begin
   toolbarTools.Visible := not toolbarTools.Visible;
   miToolPanel.Checked := toolbarTools.Visible;

end;

procedure TFormMain.miPointFramePanelClick(Sender: TObject);
begin
   toolbarPointsFrames.Visible := not toolbarPointsFrames.Visible;
   miPointFramePanel.Checked := toolbarPointsFrames.Visible;

end;

procedure TFormMain.miDisplayPanelClick(Sender: TObject);
begin
   toolbarDisplay.Visible := not toolbarDisplay.Visible;
   miDisplayPanel.Checked := toolbarDisplay.Visible;

end;

procedure TFormMain.miMousePosPanelClick(Sender: TObject);
begin
   sbFrame.Visible := not sbFrame.Visible;
   miMousePosPanel.Checked := sbFrame.Visible;
end;

procedure TFormMain.miLinkOverlayClick(Sender: TObject);
var iCurrent,iPrevious,f: integer;
    fCurrent,fPrevious: TLaserFrame;
    myp,mypp: TSmallPoint;
    dx,dy: integer;
begin
   f := 0;
   if FFile<>nil then if FFile.Count>1 then if CurrentFrame>0 then begin
      fCurrent := FFile.frames[CurrentFrame];
      fPrevious := FFile.frames[Pred(CurrentFrame)];
      for iCurrent := 0 to Pred(fCurrent.Points.count) do begin
         myp := fCurrent.Points[iCurrent];
         for iPrevious := 0 to Pred(fPrevious.Points.count) do begin
            mypp := fPrevious.Points[iPrevious];
            dx := Abs(myp.x-mypp.x);
            dy := Abs(myp.y-mypp.y);
            if (dx<(2*Circlesize)) and (dy<(2*CircleSize)) then begin
               Inc(f);
               fCurrent.links[iCurrent,iPrevious] := true;
               myp.overlay := true;
            end;
         end; // for j mypf
      end; // for i myf
      if f>0 then MessageDlg(IntToStr(f)+' overlapping points found!',
         mtInformation,[mbOK],0);
   end; // wenn alles ok
   Redraw;
end;

procedure TFormMain.aFramePreviewExecute(Sender: TObject);
var myf: TLaserFrame;
    myp,mynp: TSmallPoint;
    i,j: integer;
    dx,dy: integer;
    tx,ty,t: word;
    ax,ay: word;
    oldax,olday: word;
    framedauer: word;
    wavedata: atp;
begin
   myf := FFile.frames[Currentframe];
   framedauer := 0;
   SetLength(wavedata,0);
   with FormDebug.img.canvas do begin
      Brush.Color := clBlack;
      FillRect(Cliprect);
      Pen.Color := clGreen;
      MoveTo(0,255);
      LineTo(FormDebug.img.width,255);
      Pen.Color := clRed;
   end;
   ax := 127;
   ay := 127;
   for i := 0 to Pred(myf.Points.count) do begin
      myp := myf.Points[i];
      if (i<(Pred(myf.Points.count))) then mynp := myf.Points[i+1] else mynp := myf.Points[0];
      dx := Integer(mynp.x) - Integer(myp.x);
      dy := Integer(mynp.y) - Integer(myp.y);
      if Abs(dx) < 32 then tx := 30 else if Abs(dx) < 64 then tx := 55 else tx := 66;
      if Abs(dy) < 32 then ty := 30 else if Abs(dy) < 64 then ty := 55 else ty := 66;
      if tx > ty then t := tx else t := ty;
      Inc(framedauer,t);
      SetLength(wavedata,framedauer);
      FormDebug.img.width := framedauer;
      j := 0;
      while (j<t) do begin
         oldax := ax;
         olday := ay;
         ax := myp.x + Round(dx*j/t);
         ay := myp.y + Round(dy*j/t);
         TPoint(wavedata[framedauer-t+j]).x := ax;
         TPoint(wavedata[framedauer-t+j]).y := ay;
         with FormDebug.img.canvas do begin
            MoveTo(FormDebug.WaveScale*((framedauer-t+j-1) div FormDebug.WaveSteps),oldax);
            LineTo(FormDebug.WaveScale*((framedauer-t+j)   div FormDebug.WaveSteps),ax);
            MoveTo(FormDebug.WaveScale*((framedauer-t+j-1) div FormDebug.WaveSteps),olday+256);
            LineTo(FormDebug.WaveScale*((framedauer-t+j)   div FormDebug.WaveSteps),ay+256);
         end;
         //Inc(j,FormDebug.WaveSteps);
         Inc(j);
      end;
   end;
   if framedauer>0 then
      FormDebug.Caption := 'waveform preview -- duration: '+IntToStr(framedauer)+' samples -- rate: '+IntToStr(Round(44100/framedauer))+' Hz -- zoom: '+IntToStr(FormDebug.WaveScale)+'/'+IntToStr(FormDebug.WaveSteps)
   else
      FormDebug.Caption := 'waveform preview -- no wave available...';
   FormDebug.Show;
end;

procedure TFormMain.aCreateWavefileExecute(Sender: TObject);
var myf,mynf,mypf,frameDummy: TLaserFrame;
    myp,mypp,dummyp: TSmallPoint;
    j,framecounter: integer;
    bx,by: integer;
    iFrameDuration, iMorphDuration: longint;
    iFrameSamples, iMorphSamples: longint;
    iFullTime, iTimeDone: longint;
    s: string;
    wavedata, olddata, tempdata, morphdata: atp;
    waveblank, oldblank, tempblank, morphblank: atb;
    ms: TMemoryStream;
    fs: TFileStream;
    calc,winkel,radius,pw: real;
    writtensamples: longint;
procedure Stetigkeit(fromp,top: TPoint; color: boolean; blank: boolean);
var j: integer;
    dx,dy: integer;
    ax,ay: word;
    tx,ty,t: word;
    a: atp;
    b: atb;
begin
   dx := Integer(top.x) - Integer(fromp.x);
   dy := Integer(top.y) - Integer(fromp.y);
   if Abs(dx) < 32 then tx := MyTimes[0] else if Abs(dx) < 64 then tx := MyTimes[1] else tx := MyTimes[2];
   if Abs(dy) < 32 then ty := MyTimes[0] else if Abs(dy) < 64 then ty := MyTimes[1] else ty := MyTimes[2];
   if tx > ty then t := tx else t := ty;
   SetLength(a,t);
   SetLength(b,t);
   for j := 0 to Pred(t) do begin
      ax := fromp.x + Round(dx*j/t);
      ay := fromp.y + Round(dy*j/t);
      TPoint(a[j]).x := ax;
      TPoint(a[j]).y := ay;
      b[j] := blank;
   end;
   WritePointArray(TStream(ms),a,b,color,writtensamples,lastpoint);
end;
procedure DoEffect(Frame1, Frame2: TLaserFrame; forw: boolean);
var j,i: integer;
    subcolor: boolean;
    subcalc: real;
    a,b,c,v,dp: TPoint;
begin
   // ### effect: x-flip,y-flip,plode
   if (Frame1.Effect in [effect_plode,effect_xflip,effect_yflip]) then begin
      topoint.x := Frame1.RotCenter.x; topoint.y := Frame1.RotCenter.y;
      bx := -1; by := -1;
      // ### stetig: lastpoint -> topoint
      Stetigkeit(lastpoint,topoint,((Frame1.Bits and 1)=1),true);
      FormStatus.pgMorph.Max := iMorphSamples;
      for j := 0 to Pred(iMorphSamples div 2) do begin
         for i := 0 to Pred(Length(wavedata)) do begin
            calc := (j+((i+1)/Length(wavedata))) / (iMorphSamples div 2);
            if Frame1.EffectParam = 0 then begin
               if not forw then calc := 1 - calc;
            end else begin
               if forw then calc := sin(calc*Pi/2) else calc := cos(calc*Pi/2);
            end;
            if Frame1.Effect in [effect_yflip,effect_plode] then bx := Round((TPoint(wavedata[i]).x-Frame1.RotCenter.x)*calc+Frame1.RotCenter.x) else bx := TPoint(wavedata[i]).x;
            if Frame1.Effect in [effect_xflip,effect_plode] then by := Round((TPoint(wavedata[i]).y-Frame1.RotCenter.y)*calc+Frame1.RotCenter.y) else by := TPoint(wavedata[i]).y;
            subcolor := ((Frame2.Bits and 1)=1);
            WritePoints(TStream(ms),bx+128,by+128,subcolor,false); //##
            Inc(writtensamples);
         end;
         if forw then FormStatus.pgMorph.Position := j
         else FormStatus.pgMorph.Position := (FormStatus.pgMorph.Max+j);
         FormStatus.pgTotal.Position := iTimeDone + (Frame1.Morph*j div iMorphSamples);
         FormStatus.Repaint;
      end;
   end else if (Frame1.Effect in [effect_dflip]) then begin
      a := Frame1.RotCenter;
      b.x := Frame1.RotCenter.x-Frame1.AuxCenter.x;
      b.y := Frame1.RotCenter.y-Frame1.AuxCenter.y;
      // topoint errechnen
      if Length(wavedata)>0 then begin
         calc := 0;
         if forw then calc := 1 - calc;
         c := wavedata[0];
         subcalc := (((c.x-a.x)*b.x)+((c.y-a.y)*b.y)) / (Sqr(b.x)+Sqr(b.y));
         v.x := a.x + Round(subcalc*b.x);
         v.y := a.y + Round(subcalc*b.y);
         dp.x := v.x - c.x;
         dp.y := v.y - c.y;
         c.x := c.x + Round(calc*dp.x);
         c.y := c.y + Round(calc*dp.y);
         if c.x>255 then c.x := 255; if c.x<0 then c.x := 0;
         if c.y>255 then c.y := 255; if c.y<0 then c.y := 0;
         bx := c.x; by := c.y;
         topoint := c;
      end;
      // ### stetig: lastpoint -> topoint
      Stetigkeit(lastpoint,topoint,((Frame1.Bits and 1)=1),true);
      FormStatus.pgMorph.Max := iMorphSamples;
      for j := 0 to Pred(iMorphSamples div 2) do begin
         for i := 0 to Pred(Length(wavedata)) do begin
            calc := (j+((i+1)/Length(wavedata))) / (iMorphSamples div 2);
            if forw then calc := 1 - calc;
            c := wavedata[i];
            subcalc := (((c.x-a.x)*b.x)+((c.y-a.y)*b.y)) / (Sqr(b.x)+Sqr(b.y));
            v.x := a.x + Round(subcalc*b.x);
            v.y := a.y + Round(subcalc*b.y);
            dp.x := v.x - c.x;
            dp.y := v.y - c.y;
            c.x := c.x + Round(calc*dp.x);
            c.y := c.y + Round(calc*dp.y);
            if c.x>255 then c.x := 255; if c.x<0 then c.x := 0;
            if c.y>255 then c.y := 255; if c.y<0 then c.y := 0;
            bx := c.x; by := c.y;
            subcolor := ((Frame2.Bits and 1)=1);
            WritePoints(TStream(ms),bx+128,by+128,subcolor,false); //##
            Inc(writtensamples);
         end;
         if forw then FormStatus.pgMorph.Position := j
         else FormStatus.pgMorph.Position := (FormStatus.pgMorph.Max+j);
         FormStatus.pgTotal.Position := iTimeDone + (Frame1.Morph*j div iMorphSamples);
         FormStatus.Repaint;
      end;
   end else if (Frame1.Effect in [effect_rotate, effect_drain]) then begin
   // ### effect: rotate
      bx := -1;
      by := -1;
      if Length(wavedata)>0 then begin
         if Frame1.Effect=effect_drain then begin
            topoint.x := Frame1.RotCenter.x;
            topoint.y := Frame1.RotCenter.y;
         end else begin
            winkel := Frame1.EffectParam*Pi/180;
            topoint.x := (TPoint(wavedata[0]).x-Frame1.RotCenter.x);
            topoint.y := (TPoint(wavedata[0]).y-Frame1.RotCenter.y);
            pw := arg(topoint.x,topoint.y)+winkel;
            radius := Sqrt(Sqr(topoint.x)+Sqr(topoint.y));
            topoint.x := Round(radius*Cos(pw))+Frame1.RotCenter.x;
            if topoint.x<0 then topoint.x := 0; if topoint.x>255 then topoint.x := 255;
            topoint.y := Round(radius*Sin(pw))+Frame1.RotCenter.y;
            if topoint.y<0 then topoint.y := 0; if topoint.y>255 then topoint.y := 255;
         end;
         Stetigkeit(lastpoint,topoint,((Frame1.Bits and 1)=1),true);
      end;
      FormStatus.pgMorph.Max := iMorphSamples;
      for j := 0 to Pred(iMorphSamples div 2) do begin
         for i := 0 to Pred(Length(wavedata)) do begin
            calc := 1 - ((j+((i+1)/Length(wavedata))) / (iMorphSamples div 2));
            subcalc := (j+((i+1)/Length(wavedata))) / (iMorphSamples div 2);
            if not forw then calc := 1 - calc;
            if not forw then subcalc := 1 - subcalc;
            winkel := Frame1.EffectParam*Pi*calc/180;
            if not forw then winkel := - winkel;
            bx := (TPoint(wavedata[i]).x-Frame1.RotCenter.x);
            by := (TPoint(wavedata[i]).y-Frame1.RotCenter.y);
            pw := arg(bx,by)+winkel;
            radius := Sqrt(Sqr(bx)+Sqr(by));
            bx := Round(radius*Cos(pw))+Frame1.RotCenter.x;
            by := Round(radius*Sin(pw))+Frame1.RotCenter.y;
            if Frame1.Effect=effect_drain then begin
               bx := Round((bx-Frame1.RotCenter.x)*subcalc)+Frame1.RotCenter.x;
               by := Round((by-Frame1.RotCenter.y)*subcalc)+Frame1.RotCenter.y;
            end;
            //if bx<0 then bx := 0; if bx>255 then bx := 255;
            //if by<0 then by := 0; if by>255 then by := 255;
            if bx<1 then bx := 1; if bx>254 then bx := 254;
            if by<1 then by := 1; if by>254 then by := 254;
            //### sollte nicht so sein, aber was solls
            subcolor := ((Frame2.Bits and 1)=1);
            WritePoints(TStream(ms),bx+128,by+128,subcolor,false);
            Inc(writtensamples);
         end;
         if Frame1.Effect=effect_drain then FormStatus.pgMorph.Position := j
         else begin
            if forw then FormStatus.pgMorph.Position := j
            else FormStatus.pgMorph.Position := (FormStatus.pgMorph.Max+j);
         end;
         FormStatus.pgTotal.Position := iTimeDone + (Frame1.Morph*j div iMorphSamples);
         FormStatus.Repaint;
      end;
   end;
   // ### special effect: morph last frame into this!!! ################
   if (Frame1.Effect=effect_morph) and (framecounter>0) and forw then begin
      mypf := FFile.frames[Pred(framecounter)];
      // ### stetig: lastpoint -> topoint
      topoint.x := TSmallPoint(mypf.Points[0]).x;
      topoint.y := TSmallPoint(mypf.Points[0]).y;
      Stetigkeit(lastpoint,topoint,((Frame1.Bits and 1)=1),true);
      iMorphDuration := 0;
      iMorphSamples := Frame1.Morph*44100 div 1000;
      FormStatus.pgMorph.Max := iMorphSamples;
      dummyp := nil;
      while (iMorphDuration<iMorphSamples) do begin
         frameDummy := TLaserFrame.Create;
         try
            for i := 0 to Pred(mypf.Points.count) do begin
               calc := iMorphDuration / iMorphSamples;
               if calc>1 then calc := 1;
               for j := 0 to Pred(myf.Points.count) do begin
                  if Frame1.links[j,i] then begin
                     myp := Frame1.Points[j];
                     mypp := mypf.Points[i];
                     dummyp := TSmallPoint.Create;
                     dummyp.x := mypp.x + (Round((myp.x - mypp.x) * calc));
                     dummyp.y := mypp.y + (Round((myp.y - mypp.y) * calc));
                     dummyp.bits := mypp.bits;
                     frameDummy.Points.add(dummyp);
                  end;
               end;
            end;
            FrameToArray(frameDummy,morphdata,morphblank,iMorphDuration);
         finally
            FreeAndNil(frameDummy);
         end;
         if iMorphDuration<iMorphSamples then FormStatus.pgMorph.Position := iMorphDuration;
         FormStatus.pgTotal.Position := iTimeDone + Round(Frame1.Morph*calc);
         FormStatus.Repaint;
      end;
      if dummyp<>nil then begin
         lastpoint.x := dummyp.x;
         lastpoint.y := dummyp.y;
      end;
      WritePointArray(TStream(ms),morphdata,morphblank,((mypf.Bits and 1)=1),writtensamples,lastpoint);
   end;
   Inc(iTimeDone,Frame1.Morph div 2);
   if bx>-1 then lastpoint.x := bx;
   if by>-1 then lastpoint.y := by;
end;

//### Main MakeWave ##########################################################
begin
   //mpPreview.Close;
   iFrameDuration := 0;
   FormStatus.Show;
   SetLength(wavedata,0);  SetLength(waveblank,0);
   SetLength(olddata,0);   SetLength(oldblank,0);
   SetLength(tempdata,0);  SetLength(tempblank,0);
   // ### main part ###########################################################
   iFrameDuration := 0;
   // Create Wave Header
   s := Copy(ExtractFileName(FFile.Filename),1,Length(FFile.Filename)-Length(ExtractFileExt(FFile.Filename)));
   s := ExtractFilePath(FFile.Filename)+s+'.WAV';
   if FileExistsUTF8(s) { *Converted from FileExists* } then DeleteFileUTF8(s); { *Converted from DeleteFile* }
   fs := TFileStream.Create(s,fmCreate);
   ms := TMemoryStream.Create;
   try
      iFrameSamples := 1;
      CreateWaveHeader(TStream(fs),iFrameSamples,Length(wavedata));
      // run frame
      iFullTime := 0;
      iTimeDone := 0;
      for framecounter := 0 to Pred(FFile.Count) do begin
         Inc(iFullTime,TLaserFrame(FFile.frames[framecounter]).Delay);
         Inc(iFullTime,TLaserFrame(FFile.frames[framecounter]).Morph);
      end;
      FormStatus.pgTotal.Max := iFullTime;
      FormStatus.pgTotal.Position := 0;
      myf := FFile.frames[0];
      // #### frame-loop ######################################################
      topoint.x := 128; topoint.y := 128;
      lastpoint := topoint;
      //lastpoint.x := 128; lastpoint.y := 128;
      for framecounter := 0 to Pred(FFile.Count) do begin
         myf := FFile.frames[framecounter];
         if myf.Points.count>0 then begin
            FormStatus.labelFrame.Caption := myf.FrameName + '   ('+IntToStr(framecounter+1)+'/'+IntToStr(FFile.Count)+')';
            FormStatus.Repaint;
            iFrameDuration := 0;
            SetLength(wavedata,0); SetLength(waveblank,0);
            // ### create pointarray for frame ###################################
            FrameToArray(myf,wavedata,waveblank,iFrameDuration);
            iFrameSamples := Round(44100*myf.Delay/(1000*iFrameDuration));
            iMorphSamples := Round(44100*myf.Morph/(1000*iFrameDuration));
            FormStatus.pgFrame.Max := iFrameSamples;
            FormStatus.pgMorph.Max := iMorphSamples;
            FormStatus.pgFrame.Position := 0;
            FormStatus.pgMorph.Position := 0;
            // ### run the effects before this frame #############################
            DoEffect(myf,myf,true);
            // ### write this frame away #########################################
            topoint.x := wavedata[0].x;
            topoint.y := wavedata[0].y;
            //### stetig: lastpoint -> topoint
            Stetigkeit(lastpoint,topoint,((myf.Bits and 1)=1),true);
            for j := 0 to Pred(iFrameSamples) do begin
               WritePointArray(TStream(ms),wavedata,waveblank,((myf.Bits and 1)=1),writtensamples,lastpoint);
               FormStatus.pgFrame.Position := j;
               FormStatus.pgTotal.Position := iTimeDone + (myf.Delay*j div iFrameSamples);
               FormStatus.Repaint;
            end;
            lastpoint.x := wavedata[Pred(Length(wavedata))].x;
            lastpoint.y := wavedata[Pred(Length(wavedata))].y;
            Inc(iTimeDone,myf.Delay);
            // ### write effects-part of next frame ##############################
            if framecounter < Pred(FFile.Count) then begin
               mynf := FFile.frames[framecounter+1];
               iMorphSamples := Round(44100*mynf.Morph/(1000*iFrameDuration));
               case mynf.Effect of
                  effect_xflip,effect_yflip,effect_plode,effect_drain: begin
                     topoint.x := mynf.RotCenter.x;
                     topoint.y := mynf.RotCenter.y;
                  end;
               else begin
                     topoint.x := TPoint(wavedata[0]).x;
                     topoint.y := TPoint(wavedata[0]).y;
                  end;
               end;
               bx := -1;
               by := -1;
               //### stetig: lastpoint -> topoint
               Stetigkeit(lastpoint,topoint,((mynf.Bits and 1)=1),true);
               DoEffect(mynf,myf,false);
               ms.Seek(0,soFromBeginning);
               fs.CopyFrom(ms,0);
               ms.Clear;
            end;
            // end writing
            FormStatus.pgTotal.Position := iTimeDone;
            FormStatus.Repaint;
            tempdata := olddata;  tempblank := oldblank;
            olddata := wavedata;  oldblank := waveblank;
            wavedata := tempdata; waveblank := tempblank;
         end; // if points>0
      end; // for frames
      topoint.x := 128;
      topoint.y := 128;
      Stetigkeit(lastpoint,topoint,((myf.Bits and 1)=1),true);
      fs.CopyFrom(ms,0);
      // groessen korrigieren:
      CreateWaveHeader(TStream(fs),writtensamples,1);
   finally
      FreeAndNil(fs);
      FreeAndNil(ms);
   end;
   FormStatus.Hide;
   tbPlay.Enabled := true;
   tbShowPreview.Enabled := true;
end;


function TFormMain.CheckFrameLinks(fi: integer; var s: string): boolean;
var iPrevious,iCurrent,f: integer;
    fCurrent,fPrevious: TLaserFrame;
begin
   f := 0;
   if FFile<>nil then if FFile.Count>1 then if fi>0 then begin
      fCurrent := FFile.frames[fi];
      fPrevious := FFile.frames[Pred(fi)];
      for iPrevious := 0 to Pred(fPrevious.Points.Count) do begin
         f := 0;
         for iCurrent := 0 to Pred(fCurrent.Points.count) do begin
            if fCurrent.links[iCurrent,iPrevious]
             then Inc(f);
         end;
         if f=0 then begin
            s := s + 'Frame '+IntToStr(fi)+' has no link from point '+IntToStr(iPrevious)+'!'#13#10;
            //Inc(ff);
         end;
      end;
   end;
   Result := (f=0);
end;

procedure TFormMain.aCheckFrameLinksExecute(Sender: TObject);
var s: string;
begin
   s := '';
   CheckFrameLinks(CurrentFrame,s);
   if s = ''
    then MessageDlg('Everything''s fine!',mtInformation,[mbOK],0)
     else MessageDlg(s,mtWarning,[mbOK],0);
end;

procedure TFormMain.aCheckAllLinksExecute(Sender: TObject);
var i: integer;
    s: string;
begin
   s := '';
   if FFile<>nil then if FFile.Count>1 then begin
      for i := 1 to Pred(FFile.Count) do
        CheckFrameLinks(i,s);
   end;
   if s = ''
    then MessageDlg('Everything''s fine!',mtInformation,[mbOK],0)
     else MessageDlg(s,mtWarning,[mbOK],0);
end;

procedure TFormMain.sbRulerClick(Sender: TObject);
begin
   miShowRuler.Checked := not miShowRuler.Checked;
   sbRuler.Down := miShowRuler.Checked;
   Redraw;
   if sbRuler.Down then begin
      Width := Width + LeftRulerWidth;
      Height := Height + TopRulerHeight;
   end else begin
      Width := Width - LeftRulerWidth;
      Height := Height - TopRulerHeight;
   end;
end;

procedure TFormMain.miShowPointsClick(Sender: TObject);
begin
   miShowPoints.Checked := not miShowPoints.Checked;
   sbShowPoints.Down := miShowPoints.Checked;
   Redraw;
end;

procedure TFormMain.miShowNoOfPointsClick(Sender: TObject);
begin
   miShowNoOfPoints.Checked := not miShowNoOfPoints.Checked;
   sbShowNumPoints.Down := miShowNoOfPoints.Checked;
   Redraw;
end;

procedure TFormMain.miShowBackframeClick(Sender: TObject);
begin
   miShowBackframe.Checked := not miShowBackframe.Checked;
   sbShowBackFrame.Down := miShowBackframe.Checked;
   Redraw;
end;

procedure TFormMain.miShowLinksClick(Sender: TObject);
begin
   miShowLinks.Checked := not miShowLinks.Checked;
   sbShowLinks.Down := miShowLinks.Checked;
   Redraw;
end;

procedure TFormMain.miUseGridClick(Sender: TObject);
begin
   miUseGrid.Checked := not miUseGrid.Checked;
   sbShowGrid.Down := miUseGrid.Checked;
   Redraw;
end;

procedure TFormMain.miSnapGridClick(Sender: TObject);
begin
   miSnapGrid.Checked := not miSnapGrid.Checked;
   sbSnapGrid.Down := miSnapGrid.Checked;
   Redraw;
end;

procedure TFormMain.miFlipXClick(Sender: TObject);
begin
   miFlipX.Checked := not miFlipX.Checked;
   sbFlipX.Down := miFlipX.Checked;
   Redraw;
end;

procedure TFormMain.miFlipYClick(Sender: TObject);
begin
   miFlipY.Checked := not miFlipY.Checked;
   sbFlipY.Down := miFlipY.Checked;
   Redraw;
end;

procedure TFormMain.miPartImgClick(Sender: TObject);
begin
   miPartImg.Checked := true;
   sbPartImg.Down := true;
   Redraw;
end;

procedure TFormMain.miFullImgClick(Sender: TObject);
begin
   miFullImg.Checked := true;
   sbFullImg.Down := true;
   Redraw;
end;

procedure TFormMain.miNoImgClick(Sender: TObject);
begin
   miNoImg.Checked := true;
   sbNoImg.Down := true;
   Redraw;

end;

procedure TFormMain.miCloseloopClick(Sender: TObject);
begin
   miCloseLoop.Checked := not miCloseLoop.Checked;
   sbCloseLoop.Down := miCloseLoop.Checked;
   Redraw;
end;

procedure TFormMain.miShowRulerClick(Sender: TObject);
begin
   miShowRuler.Checked := not miShowRuler.Checked;
   sbRuler.Down := miShowRuler.Checked;
   ReDraw;
end;

procedure TFormMain.miOpenAgainClick(Sender: TObject);
begin
   pmLastFiles.Popup(Mouse.CursorPos.x,Mouse.CursorPos.y);
end;

procedure TFormMain.miOpt3xZoomClick(Sender: TObject);
begin
   Width := 247 + (3*256);
   Height := 67 + (3*256);
   if sbRuler.Down then Width := Width + LeftRulerWidth;
   if sbRuler.Down then Height := Height + TopRulerHeight;
   ZoomFactor := 3;
   miZoom3.Checked := true;
   miZoom.Caption := '&Zoom: 3x...';
   Redraw;
end;

procedure TFormMain.aLoadBackImgExecute(Sender: TObject);
var myf: TLaserFrame;
begin
   myf := FFile.frames[Currentframe];
   if odBitmap.Execute then begin
      if FileExistsUTF8(odBitmap.Filename) { *Converted from FileExists* } then begin
         if myf.Bitmap=nil then myf.Bitmap := TBitmap.Create;
         myf.Bitmap.LoadFromFile(odBitmap.Filename);
         aLoadBackImg.Caption := 'Background image: '+ExtractFileName(odBitmap.Filename);
         myf := FFile.frames[currentframe];
         myf.ImgName := odBitmap.filename;
         ReDraw;
         miFullImg.Enabled := true;
         miPartImg.Enabled := true;
         miChoosePart.Enabled := true;
         sbPartImg.Enabled := true;
         sbFullImg.Enabled := true;
      end;
   end;
end;

procedure TFormMain.pmiChoosePartClick(Sender: TObject);
var myf: TLaserFrame;
begin
   myf := FFile.frames[Currentframe];
   if myf.Bitmap<>nil
    then if not myf.Bitmap.Empty
     then with FormPickImage do begin
      Img.Picture := TPicture(myf.Bitmap);
      DataImage := myf.Bitmap;
      DataRect := TLaserFrame(FFile.frames[currentframe]).ImgRect;
      ShowModal;
      TLaserFrame(FFile.frames[currentframe]).ImgRect := DataRect;
      miPartImg.Checked := true;
   end;
   sbPartImgClick(Sender);
end;

procedure TFormMain.aChooseImgPartExecute(Sender: TObject);
var myf: TLaserFrame;
begin
   myf := FFile.frames[Currentframe];
   if myf.Bitmap<>nil
    then if not myf.Bitmap.Empty
     then with FormPickImage do begin
      Img.Picture := TPicture(myf.Bitmap);
      DataImage := myf.Bitmap;
      DataRect := TLaserFrame(FFile.frames[currentframe]).ImgRect;
      ShowModal;
      TLaserFrame(FFile.frames[currentframe]).ImgRect := DataRect;
      miPartImg.Checked := true;
   end;
   sbPartImgClick(Sender);
end;

procedure TFormMain.sbSnapHelpClick(Sender: TObject);
begin
   miSnapHelp.Checked := not miSnapHelp.Checked;
   sbSnapHelp.Down := miSnapHelp.Checked;
   Redraw;
end;

procedure TFormMain.miSnapHelpClick(Sender: TObject);
begin
   miSnapHelp.Checked := not miSnapHelp.Checked;
   sbSnapHelp.Down := miSnapHelp.Checked;
   Redraw;
end;

procedure TFormMain.miHelpLinesClick(Sender: TObject);
begin
   if not Assigned(FFile)
    then Exit;
   if FFile.Count=0
    then Exit;
   FormHelpLines.Execute(TLaserFrame(FFile.Frames[CurrentFrame]).HelpLines);
end;

procedure TFormMain.tbPlayClick(Sender: TObject);
begin
   if FFile<>nil then begin
      if FileExistsUTF8(FFile.Filename+'.WAV') { *Converted from FileExists* } then begin
         {mpPreview.Filename := FFile.Filename+'.WAV';
         mpPreview.Notify := false;
         mpPreview.Open;
         mpPreview.Notify := true;
         mpPreview.Play;
         }
         tbPlay.Enabled := false;
         tbStop.Enabled := true;
      end else MessageDlg('You need to compile this show first.',mtError,[mbOK],0);
   end;
end;

procedure TFormMain.tbStopClick(Sender: TObject);
begin
   tbPlay.Enabled := true;
   tbStop.Enabled := false;
   {
   if not (mpPreview.Mode in [mpNotReady,mpStopped]) then begin
      mpPreview.Notify := false;
      mpPreview.Close;
   end;
   }
end;

procedure TFormMain.mpPreviewNotify(Sender: TObject);
begin
   {
   case mpPreview.Mode of
      mpPlaying: begin
         tbPlay.Enabled := false;
         tbStop.Enabled := true;
      end;
      else begin
         if (tbRepeat.Down) and (mpPreview.Mode in [mpStopped,mpPaused,mpOpen,mpSeeking]) then begin
            mpPreview.Notify := true;
            mpPreview.Play;
         end else begin
            tbPlay.Enabled := true;
            tbStop.Enabled := false;
         end;
      end;
   end;
   }
end;

procedure TFormMain.toolbarPlayClose(Sender: TObject);
begin
   miPlayPanel.Checked := false;
end;

procedure TFormMain.miEffectRotateClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_rotate;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.ReCreatePreview;
var mya: atp;
    myb: atb;
    framedauer: longint;
    written: longint;
    myf: TLaserFrame;
    //ms: TMemoryStream;
    p: TPoint;
    ok: boolean;
begin
   ok := true;
   if tbLive.Down then begin
      myf := FFile.frames[CurrentFrame];
      SetLength(mya,0); SetLength(myb,0);
      if myf.Points.count>2 then begin
         //PlaySound(nil, 0, SND_MEMORY  or SND_ASYNC or SND_LOOP);
         msLivePreview.Clear;
         framedauer := 0; written := 0;
         FrameToArray(myf,mya,myb,framedauer);
         CreateWaveHeader(TStream(msLivePreview),framedauer,1);
         WritePointArray(TStream(msLivePreview),mya,myb,false,written,p);
         //ok := PlaySound(msLivePreview.Memory, 0, SND_MEMORY  or SND_ASYNC or SND_LOOP)
      end else;// PlaySound(nil, 0, SND_MEMORY  or SND_ASYNC or SND_LOOP);
   end else;// PlaySound(nil, 0, SND_MEMORY  or SND_ASYNC or SND_LOOP);
   if not ok then begin
      tbLive.Down := false;
      MessageDlg('Sorry, sound can''t be played.',mtError,[mbOK],0);
   end;
end;

procedure TFormMain.tbLiveClick(Sender: TObject);
begin
   tbLive.Down := not tbLive.Down;
   ReCreatePreview;
end;

procedure TFormMain.miFrameFlipYClick(Sender: TObject);
var myf: TLaserFrame;
    myp: TSmallPoint;
    i: integer;
begin
   myf := FFile.frames[CurrentFrame];
   for i := 0 to Pred(myf.Points.count) do begin
      myp := myf.Points[i];
      myp.x := 255 - myp.x;
   end;
   Redraw;
end;

procedure TFormMain.miFrameFlipX2Click(Sender: TObject);
var myf: TLaserFrame;
    myp: TSmallPoint;
    i: integer;
begin
   myf := FFile.frames[CurrentFrame];
   for i := 0 to Pred(myf.Points.count) do begin
      myp := myf.Points[i];
      myp.y := 255 - myp.y;
   end;
   Redraw;
end;

procedure TFormMain.miEffectParamClick(Sender: TObject);
var myf: TLaserFrame;
    s: string;
    i,ec: integer;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      s := InputBox('Enter new effect parameter','new effect parameter',IntToStr(myf.EffectParam));
      Val(s,i,ec);
      if ec=0 then begin
         if i<0 then i := 0;
         myf.EffectParam := i;
      end;
      case myf.Effect of
         effect_rotate,effect_dflip: begin
            while myf.EffectParam>359 do Dec(myf.EffectParam,360);
         end;
      end;
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.miEffectDFlipClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_dflip;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.aFrameEffectParamExecute(Sender: TObject);
var myf: TLaserFrame;
    s,s2: string;
    i,ec: integer;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      if myf.Effect in [effect_xflip,effect_yflip,effect_dflip,effect_plode] then
         s2 := 'Enter 0 for linear, 1 for trigonometric effect progression'
         else s2 := 'Enter new effect parameter';
      s := InputBox(s2,'new effect parameter',IntToStr(myf.EffectParam));
      Val(s,i,ec);
      if ec=0 then begin
         //if i<0 then i := 0;
         myf.EffectParam := i;
      end;
      case myf.Effect of
         effect_rotate,effect_dflip: begin
            while myf.EffectParam>359 do Dec(myf.EffectParam,360);
            while myf.EffectParam<-359 do Inc(myf.EffectParam,360);
         end;
         effect_xflip,effect_yflip,effect_plode: begin
            if (not (myf.EffectParam in [0,1])) then myf.EffectParam := 0;
         end;
      end;
   end;
   lbThumbs.Refresh;
end;

procedure TFormMain.miDelaysClick(Sender: TObject);
begin
   FormDelays.Execute(MyTimes[0], MyTimes[1], MyTimes[2], MyTimes[3]);
end;

procedure TFormMain.miUndoClick(Sender: TObject);
var myf: TLaserFrame;
begin
   myf := FFile.frames[CurrentFrame];
   case Undo.Op of
      sAdd: begin
         myf.Points.delete(Undo.Pos);
      end;
      sMove: begin
         TSmallPoint(myf.Points[Undo.Pos]).x := Undo.x;
         TSmallPoint(myf.Points[Undo.Pos]).y := Undo.y;
      end;
      sDel: begin
         myf.Points.insert(Undo.Pos,Undo.Point);
      end;
   end;
   Redraw;
   lbThumbs.Refresh;
end;

procedure TFormMain.aShowPreviewExecute(Sender: TObject);
begin
   if FileExistsUTF8(FFile.Filename+'.WAV') { *Converted from FileExists* } then begin
      with FormPreview do begin
         Caption := 'Preview loading...';
         DataStream := TMemoryStream.Create;
         DataStream.LoadFromFile(FFile.Filename+'.WAV');
         Caption := 'Preview loaded...';
         tbPreviewPos.Min := 40;
         tbPreviewPos.Max := DataStream.Size;
         tbPreviewPos.Position := 40;
         ShowModal;
         Caption := 'Preview ended...';
      end;
   end else begin
      MessageDlg('Hey, you need to compile first!',mtInformation,[mbOK],0);
      tbShowPreview.Enabled := false;
   end;
end;

procedure TFormMain.RotateFrame(f: TLaserFrame; degrees: real);
var myp: TSmallPoint;
    i: integer;
    radius,pw: real;
    bx,by: integer;
begin
   for i := 0 to Pred(f.Points.Count) do begin
      myp := f.Points[i];
      bx := (Integer(myp.x)-f.AuxCenter.X);
      by := (Integer(myp.y)-f.AuxCenter.Y);
      pw := arg(bx,by);
      radius := Sqrt(Sqr(bx)+Sqr(by));
      pw := pw + degrees;
      bx := Round(radius*Cos(pw))+f.RotCenter.X;
      if bx<0 then bx := 0; if bx>255 then bx := 255;
      myp.x := bx;
      by := Round(radius*Sin(pw))+f.RotCenter.Y;
      if by<0 then by := 0; if by>255 then by := 255;
      myp.y := by;
   end;
end;

procedure TFormMain.sbShowBackframeClick(Sender: TObject);
begin
   miShowBackframe.Checked := not miShowBackframe.Checked;
   sbShowBackframe.Down := miShowBackframe.Checked;
   Redraw;

end;

procedure TFormMain.miRotateFrameClick(Sender: TObject);
var myf: TLaserFrame;
    s: string;
    w,ec: integer;
    winkel: real;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      s := InputBox('Enter rotation in degrees','rotate frame','0');
      Val(s,w,ec);
      winkel := -w/180*Pi;
      if ec=0 then begin
         RotateFrame(myf,winkel);
      end;
   end;
   Redraw;
   lbThumbs.Refresh;
end;

procedure TFormMain.miSetRotPointClick(Sender: TObject);
var myf: TLaserFrame;
    sx,sy: string;
    wx,wy,ecx,ecy: integer;
begin
   if (CurrentFrame>-1) and (CurrentFrame<FFile.Count) then begin
      myf := TLaserFrame(FFile.frames[CurrentFrame]);
      sx := InputBox('Enter new effect center point (x)','effect center',IntToStr(myf.RotCenter.x));
      sy := InputBox('Enter new effect center point (y)','effect center',IntToStr(myf.RotCenter.y));
      Val(sx,wx,ecx); if wx<0 then ecx := 1; if wx>255 then ecx := 1;
      Val(sy,wy,ecy); if wy<0 then ecy := 1; if wy>255 then ecy := 1;
      if (ecx=0) and (ecy=0) then begin
         myf.RotCenter.x := wx;
         myf.RotCenter.y := wy;
      end;
      sx := InputBox('Enter new auxiliary center point (x)','auxiliary center',IntToStr(myf.AuxCenter.x));
      sy := InputBox('Enter new auxiliary center point (y)','auxiliary center',IntToStr(myf.AuxCenter.y));
      Val(sx,wx,ecx); if wx<0 then ecx := 1; if wx>255 then ecx := 1;
      Val(sy,wy,ecy); if wy<0 then ecy := 1; if wy>255 then ecy := 1;
      if (ecx=0) and (ecy=0) then begin
         myf.AuxCenter.x := wx;
         myf.AuxCenter.y := wy;
      end;
   end;
   ReDraw;
   lbThumbs.Refresh;
end;

procedure TFormMain.aCopyFrameToClipboardExecute(Sender: TObject);
var myf: TLaserFrame;
begin
   ClipBoardFrame := TLaserFrame.Create;
   myf := FFile.frames[CurrentFrame];
   ClipBoardFrame.Assign(myf);
   aPasteFrameFromClipboard.Enabled := true;
end;

procedure TFormMain.aPasteFrameFromClipboardExecute(Sender: TObject);
var newf: TLaserFrame;
begin
   if ClipBoardFrame<>nil then begin
      newf := TLaserFrame.Create;
      newf.Assign(ClipBoardFrame);
      InsertFrame(newf,CurrentFrame);
   end;
end;

procedure TFormMain.aCutFrameToClipboardExecute(Sender: TObject);
begin
   DeleteFrame(CurrentFrame,ClipBoardFrame);
end;

procedure TFormMain.miToolSharpenClick(Sender: TObject);
begin
   tbPointTools.ImageIndex := miToolSharpen.ImageIndex;
   tbPointTools.Hint := miToolSharpen.Hint;
   tbPointToolsClick(Sender);
end;

procedure TFormMain.miToolLinkClick(Sender: TObject);
begin
   tbPointTools.ImageIndex := miToolLink.ImageIndex;
   tbPointTools.Hint := miToolLink.Hint;
   tbPointToolsClick(Sender);
end;

procedure TFormMain.miToolMoveRotateClick(Sender: TObject);
begin
   tbPointTools.ImageIndex := miToolMoveRotate.ImageIndex;
   tbPointTools.Hint := miToolMoveRotate.Hint;
   tbPointToolsClick(Sender);
end;

procedure TFormMain.tbPointToolsClick(Sender: TObject);
begin
   case tbPointTools.ImageIndex of
      6: begin
         workstate := sMoveRotateFrame;
         FormSketchpad.pad.Cursor := crMoveRotFrame;
      end;
      5: begin
         workstate := sAnim;
         FormSketchpad.pad.Cursor := crAnimFrame;
      end;
      4: begin
         workstate := sPointType;
         FormSketchpad.pad.Cursor := crPointType;
      end;
      7: begin
         workstate := sAuxPoints;
         FormSketchpad.pad.Cursor := crSetPoints;
      end;
   end;
   tbPointTools.Down := true;
end;

procedure TFormMain.sbSharpenClick(Sender: TObject);
var myf: TLaserFrame;
    myp: TSmallPoint;
    i: integer;
begin
   FileChanged := true;
   myf := FFile.frames[currentframe];
   if FormSketchpad.MultiSelect then begin
      for i := 0 to Pred(myf.Points.Count) do begin
         myp := myf.Points[i];
         if ((myp.p and 1)>0) then if ((myp.bits and 1)=0) then Inc(myp.bits) else Dec(myp.bits);
      end;
   end else begin
      if (SelectedPoint>-1) and (SelectedPoint<myf.Points.count) then begin
         myp := myf.Points[SelectedPoint];
         if ((myp.bits and 1)=0) then Inc(myp.bits) else Dec(myp.bits);
      end;
   end;
   Redraw;
end;

procedure TFormMain.sbBlankClick(Sender: TObject);
var myf: TLaserFrame;
    myp: TSmallPoint;
    i: integer;
begin
   FileChanged := true;
   myf := FFile.frames[currentframe];
   if FormSketchpad.MultiSelect then begin
      for i := 0 to Pred(myf.Points.Count) do begin
         myp := myf.Points[i];
         if ((myp.p and 1)>0) then if ((myp.bits and 2)=0) then Inc(myp.bits,2) else Dec(myp.bits,2);
      end;
   end else begin
   if (SelectedPoint>-1) and (SelectedPoint<myf.Points.count) then begin
      myp := myf.Points[SelectedPoint];
      if ((myp.bits and 2)=0) then Inc(myp.bits,2) else Dec(myp.bits,2);
   end;
   end;
   Redraw;
end;

procedure TFormMain.sbColorClick(Sender: TObject);
begin
   FileChanged := true;
   if miColor0.Checked then begin
      miColor1.Checked := true;
      miColor1Click(Sender);
   end else begin
      miColor0.Checked := true;
      miColor0Click(Sender);
   end;

end;

procedure TFormMain.sbLockClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   if (myf.Bits and 2)=0 then Inc(myf.Bits,2) else Dec(myf.Bits,2);
   sbLock.Down := ((myf.Bits and 2)=2);
   lbThumbs.Refresh;
   Redraw;

end;

procedure TFormMain.miEffectDrainClick(Sender: TObject);
var myf: TLaserFrame;
begin
   FileChanged := true;
   myf := FFile.frames[Currentframe];
   myf.Effect := effect_drain;
   lbThumbs.Refresh;
   Redraw;
end;

procedure TFormMain.miToolAuxPointsClick(Sender: TObject);
begin
   tbPointTools.ImageIndex := miToolAuxPoints.ImageIndex;
   tbPointTools.Hint := miToolAuxPoints.Hint;
   tbPointToolsClick(Sender);
end;

procedure TFormMain.miCenterFrameClick(Sender: TObject);
var point1,point2, dp, erg: TPoint;
    iPoint: integer;
    fCurrentFrame: TLaserFrame;
    spCurrentPoint: TSmallPoint;
begin
   fCurrentFrame := FFile.frames[CurrentFrame];
   if fCurrentFrame<>nil then if fCurrentFrame.Points.count>0 then begin
      FileChanged := true;
      point1 := fCurrentFrame.AuxCenter;
      point2 := fCurrentFrame.AuxCenter;
      for iPoint := 0 to Pred(fCurrentFrame.Points.Count) do begin
         spCurrentPoint := fCurrentFrame.Points[iPoint];
         if spCurrentPoint.x<point1.x then point1.x := spCurrentPoint.x;
         if spCurrentPoint.x>point2.x then point2.x := spCurrentPoint.x;
         if spCurrentPoint.y<point1.y then point1.y := spCurrentPoint.y;
         if spCurrentPoint.y>point2.y then point2.y := spCurrentPoint.y;
      end;
      dp.x := fCurrentFrame.RotCenter.x-((point2.x+point1.x) div 2);
      dp.y := fCurrentFrame.RotCenter.y-((point2.y+point1.y) div 2);
      for iPoint := 0 to Pred(fCurrentFrame.Points.Count) do begin
         spCurrentPoint := fCurrentFrame.Points[iPoint];
         erg.x := spCurrentPoint.x + dp.x;
         if erg.x>255 then erg.x := 255; if erg.x<0 then erg.x := 0;
         spCurrentPoint.x := erg.x;
         erg.y := spCurrentPoint.y + dp.y;
         if erg.y>255 then erg.y := 255; if erg.y<0 then erg.y := 0;
         spCurrentPoint.y := erg.y;
      end;
      FormMain.Redraw;
      lbThumbs.Refresh;
   end;
end;

procedure TFormMain.miFrameFlipDClick(Sender: TObject);
var myf: TLaserFrame;
    myp: TSmallPoint;
    a,b,c,v,dp: TPoint;
    i: integer;
    br: real;
begin
   myf := FFile.frames[CurrentFrame];
   a := myf.RotCenter;
   b.x := myf.RotCenter.x-myf.AuxCenter.x;
   b.y := myf.RotCenter.y-myf.AuxCenter.y;
   for i := 0 to Pred(myf.Points.Count) do begin
      myp := myf.Points[i];
      c.x := myp.x; c.y := myp.y;
      br := (((c.x-a.x)*b.x)+((c.y-a.y)*b.y)) / (Sqr(b.x)+Sqr(b.y));
      v.x := a.x + Round(br*b.x);
      v.y := a.y + Round(br*b.y);
      dp.x := v.x - c.x;
      dp.y := v.y - c.y;
      c.x := c.x + (2*dp.x);
      c.y := c.y + (2*dp.y);
      if c.x>255 then c.x := 255; if c.x<0 then c.x := 0;
      if c.y>255 then c.y := 255; if c.y<0 then c.y := 0;
      myp.x := c.x;
      myp.y := c.y;
   end;
   Redraw;
   lbThumbs.Refresh;
end;

procedure TFormMain.lbTimelineMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var myf: TLaserFrame;
begin
   if (Index>-1) and (Index<FFile.Count) then begin
      myf := FFile.frames[Index];
      Height := (myf.Delay+myf.Morph) div 10;
   end else Height := 20;
end;

procedure TFormMain.miScaleClick(Sender: TObject);
var p,ap: TPoint;
    i: integer;
    myf: TLaserFrame;
    myp: TSmallPoint;
    s: string;
    ec: integer;
    r: real;
begin
   myf := FFile.frames[CurrentFrame];
   if myf<>nil then if myf.Points.count>0 then begin
      s := InputBox('Enter scale factor (1=100%)','scale frame','1');
      Val(s,r,ec);
      if ec=0 then begin
         FileChanged := true;
         ap := myf.AuxCenter;
         for i := 0 to Pred(myf.Points.Count) do begin
            myp := myf.Points[i];
            p.x := myp.x;
            p.y := myp.y;
            p.x := Round((p.x - ap.x) * r) + ap.x;
            if p.x>255 then p.x := 255; if p.x<0 then p.x := 0;
            p.y := Round((p.y - ap.y) * r) + ap.y;
            if p.y>255 then p.y := 255; if p.y<0 then p.y := 0;
            myp.x := p.x;
            myp.y := p.y;
         end;
         FormMain.Redraw;
         lbThumbs.Refresh;
      end;
   end;
end;

procedure TFormMain.miLicenseClick(Sender: TObject);
begin
   LicenseAgree := false;
   Redraw;
end;

procedure TFormMain.Equilateral(center,start: TPoint);
var fCurrent,fPrevious: TLaserFrame;
    sides: integer;
    newp: TSmallPoint;
    radius,alpha,beta: Real;
    i,ec: integer;
    x,y: integer;
    s: string;
begin
   if (FFile<>nil) then if (FFile.Count>0) and (currentframe<FFile.Count) then begin
      fCurrent := FFile.Frames[currentframe];
      // abfragen
      sides := EquilateralSides;
      s := IntToStr(sides);
      if InputQuery('Equilateral','Please enter number of sides:',s) then begin
         fCurrent.Points.Clear;
         Val(s,sides,ec);
         if ec=0 then EquilateralSides := sides;
         sides := EquilateralSides;
         // nun calc
         //alpha = arccos( (xs-xc)/R )
         alpha := ArcTan2(start.y-center.y,start.x-center.x);
         //alpha := 0;
         radius := Sqrt(Sqr(center.x-start.x)+Sqr(center.y-start.y));
         beta := 2*Pi / sides; // Schritt zwischen Winkeln
         for i := 0 to Pred(sides) do begin
            x := center.x + Round(radius * cos(beta*i+alpha));
            if x<0 then x := 0; if x>255 then x:=255;
            y := center.y + Round(radius * sin(beta*i+alpha));
            if y<0 then y := 0; if y>255 then y:=255;
            newp := TSmallPoint.Create;
            newp.X := x;
            newp.Y := y;
            fCurrent.Points.Add(newp);
         end;
      end;
      RenumberList;
      if CurrentFrame>0 then begin
         fPrevious := FFile.frames[Pred(CurrentFrame)];
         SetLength(fCurrent.links,fCurrent.Points.count,fPrevious.Points.count);
      end;
   end;
   FormMain.Redraw;
end;

procedure TFormMain.tbGeoClick(Sender: TObject);
begin
   case tbGeo.ImageIndex of
      8: begin
         workstate := sEquilateral;
         FormSketchpad.pad.Cursor := crMovePoint;
      end;
   end;
   tbGeo.Down := true;
end;

procedure TFormMain.TimeLineRedraw;
var iFrame: integer;
    iPosition0,iPosition1,iPosition2,iPosition3: longint;
    frameCurrent: TLaserFrame;
    h: integer;
    divider, iFirst, hiddentime: integer;
    iMaxTime: longint;
    nulloffs: integer;
    s: string;
    r: real;
    aPoints: array[1..6] of TPoint;
begin
   if FFile<>nil then if FFile.Count>0 then begin
      with iTimeline.Canvas do begin
         Brush.Color := MyTimelineColors[mytlc_back];
         FillRect(ClipRect);
      end;
      iPosition0 := 0;
      h := iTimeline.Height;
      divider := tbTimeLineZoomIn.Tag;
      iFirst := tbTimelineLeft.Tag;
      if iFirst>=FFile.Count
       then iFirst := Pred(FFile.Count);
      iMaxTime := 0;
      nulloffs := 14;
      hiddentime := 0;
      for iFrame := 0 to Pred(iFirst) do begin
         Inc(hiddentime,TLaserFrame(FFile.frames[0]).Morph);
         Inc(hiddentime,TLaserFrame(FFile.frames[0]).Delay);
      end;
      Inc(iMaxTime,Hiddentime);
      for iFrame := iFirst to Pred(FFile.Count) do begin
         frameCurrent := FFile.frames[iFrame];
         with iTimeline.Canvas do begin
            Pen.Style := psSolid;
            Pen.Width := 2;
            Pen.Color := MyColors[(frameCurrent.Bits and 1),myc_norm];
            //Pen.Color := MyTimelineColors[mytlc_lines];
            iPosition1 := iPosition0 + (frameCurrent.Morph div (2*divider));
            iPosition2 := iPosition1 + (frameCurrent.Delay div divider);
            iPosition3 := iPosition2 + (frameCurrent.Morph div (2*divider));
            Inc(iMaxTime, frameCurrent.Delay + frameCurrent.Morph);
            aPoints[1].x := iPosition0;
            aPoints[1].y := h;
            aPoints[2].x := iPosition1;
            aPoints[2].y := h;
            aPoints[3].x := iPosition2;
            aPoints[3].y := h;
            aPoints[4].x := iPosition3;
            aPoints[4].y := h;
            aPoints[5].x := iPosition1;
            aPoints[5].y := nulloffs;
            aPoints[6].x := iPosition2;
            aPoints[6].y := nulloffs;
            Brush.Color := MyColors[(frameCurrent.Bits and 1), myc_back];
            //Brush.Color := MyTimelineColors[mytlc_areas]; //clMaroon;
            Brush.Style := bsBDiagonal;
            Polygon([aPoints[1], aPoints[2], aPoints[5]]);
            Brush.Style := bsDiagCross;
            Polygon([aPoints[2], aPoints[3], aPoints[6], aPoints[5]]);
            Brush.Style := bsFDiagonal;
            Polygon([aPoints[3], aPoints[4], aPoints[6]]);
            Brush.Color := MyTimelineColors[mytlc_back]; //$00004000;
            Pen.Color := clLime;
            TextOut(iPosition1+2,nulloffs+2,'#'+IntToStr(iFrame));
            s := frameCurrent.FrameName;
            while (TextWidth(s)>(iPosition3-iPosition1-4)) and (Length(s)>5)
               do s := Copy(s, 1, Pred(Length(s)));
            TextOut(iPosition1+2,nulloffs+16,s);
            iPosition0 := iPosition3;
         end;
      end;
      iFrame := hiddentime;
      while iFrame < iMaxTime do begin
         Inc(iFrame, 500);
         with iTimeline.Canvas do begin
            Brush.Color := MyTimelineColors[mytlc_back];
            Brush.Style := bsSolid;
            Font.Color := MyTimelineColors[mytlc_text];
            //r := i / 1000;
            //Str(r:4:1,s);
            r := iFrame;
            Str(r:7:0,s);
            //TextOut(((i-hiddentime) div divider)-(TextWidth(s) div 2),0,s);
            TextOut(((iFrame-hiddentime) div divider)-(TextWidth(s))+3,0,s);
            Pen.Color := MyTimelineColors[mytlc_text];
            Pen.Style := psDot;
            Pen.Width := 1;
            MoveTo((iFrame-hiddentime) div divider,nulloffs);
            //MoveTo((i-hiddentime) div divider,0);
            LineTo((iFrame-hiddentime) div divider,h);
         end;
      end;
   end;
end;

procedure TFormMain.tbTimeLineZoomInClick(Sender: TObject);
begin
   if tbTimeLineZoomIn.Tag>1 then tbTimeLineZoomIn.Tag := tbTimeLineZoomIn.Tag div 2;
   TimeLineRedraw;
end;

procedure TFormMain.tbTimeLineZoomOutClick(Sender: TObject);
begin
   tbTimeLineZoomIn.Tag := tbTimeLineZoomIn.Tag * 2;
   TimeLineRedraw;
end;

procedure TFormMain.miTimelineClick(Sender: TObject);
begin
   toolwinTimeline.Visible := not toolwinTimeline.Visible;
   miTimeline.Checked := toolwinTimeline.Visible;
end;

procedure TFormMain.tbTimelineLeftClick(Sender: TObject);
begin
   if tbTimelineLeft.Tag>0 then tbTimelineLeft.Tag := tbTimelineLeft.Tag - 1;
   TimeLineRedraw;
end;

procedure TFormMain.tbTimelineRightClick(Sender: TObject);
begin
   if tbTimelineLeft.Tag<Pred(FFile.Count)
    then tbTimelineLeft.Tag := tbTimelineLeft.Tag + 1;
   TimeLineRedraw;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
   //iTimeline.Align := alNone;
   //iTimeline.Align := alClient;
   iTimeline.Width := FormMain.Width;
end;

procedure TFormMain.toolwinTimelineClose(Sender: TObject);
begin
   miTimeline.Checked := false;
end;

procedure TFormMain.tbRepeatClick(Sender: TObject);
begin
   tbRepeat.Down := not tbRepeat.Down;
end;

end.

