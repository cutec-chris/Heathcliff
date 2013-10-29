[Setup]
VersionInfoVersion=0.1
VersionInfoCompany=Patrick Michael Kolla
VersionInfoDescription=Lasershow Editor Setup
VersionInfoCopyright=1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
VersionInfoProductName=Heathcliff for Catweazle LC1 - Yoghurt Mixer
VersionInfoProductVersion=1.1
AppCopyright=1999,2000,2009-2010 Patrick Michael Kolla. All rights reserved.
AppName=Heathcliff for Catweazle LC1 - Yoghurt Mixer
AppVerName=1.1
OutputBaseFilename=heathcliff-setup
InfoBeforeFile=H:\Projekte\Spaﬂprogrammierung\Multimedia\Heathcliff\version.txt
LicenseFile=H:\Projekte\Spaﬂprogrammierung\Multimedia\Heathcliff\gpl.txt
DefaultDirName={pf}\Heathcliff\
DefaultGroupName=Heathcliff
ShowLanguageDialog=no
AppPublisher=Patrick Michael Kolla
AppPublisherURL=https://launchpad.net/heathcliff
AppSupportURL=https://launchpad.net/heathcliff
AppUpdatesURL=https://launchpad.net/heathcliff
AppVersion=1.1
UninstallDisplayName=Heathcliff for Catweazle LC1 - Yoghurt Mixer
UninstallDisplayIcon={app}\Heathcliff.exe

[Files]
Source: ..\bin\Heathcliff.exe; DestDir: {app}
Source: ..\bin\Heathcliff.hlp; DestDir: {app}
Source: ..\demos\Guinness Harfe.LC1; DestDir: {app}\Demos\
Source: ..\demos\Morphing Geometrische Figuren.LC1; DestDir: {app}\Demos\
Source: ..\demos\Rotierendes Dreieck.LC1; DestDir: {app}\Demos\
Source: ..\demos\Wandernde Dreieckspitze.LC1; DestDir: {app}\Demos\
Source: ..\gpl.txt; DestDir: {app}
