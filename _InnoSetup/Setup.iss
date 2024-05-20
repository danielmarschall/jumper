; (De)Coder Script für InnoSetup
; Fehler bei Uninstallation: ReadOnly, Anwendung in Benutzung

[Setup]
AppName=PegSolitaire
AppVerName=PegSolitaire
AppVersion=1.0
AppCopyright=© Copyright 2009 - 2024 ViaThinkSoft
AppPublisher=ViaThinkSoft
AppPublisherURL=https://www.viathinksoft.de/
AppSupportURL=https://www.daniel-marschall.de/
AppUpdatesURL=https://www.viathinksoft.de/
DefaultDirName={autopf}\PegSolitaire
DefaultGroupName=PegSolitaire
UninstallDisplayIcon={app}\PegSolitaire.exe
VersionInfoCompany=ViaThinkSoft
VersionInfoCopyright=© Copyright 2009 - 2024 ViaThinkSoft
VersionInfoDescription=PegSolitaire Setup
VersionInfoTextVersion=1.0.0.0
VersionInfoVersion=1.0
OutputDir=.
OutputBaseFilename=PegSolitaireSetup
; Configure Sign Tool in InnoSetup at "Tools => Configure Sign Tools" (adjust the path to your SVN repository location)
; Name    = sign_single   
; Command = "C:\SVN\...\sign_single.bat" $f
SignTool=sign_single
SignedUninstaller=yes

[Languages]
Name: de; MessagesFile: "compiler:Languages\German.isl"

[Files]
; Allgemein
Source: "..\PegSolitaire.exe"; DestDir: "{app}"; Flags: ignoreversion signonce
Source: "..\PegSolitaire.deu"; DestDir: "{app}"; Flags: ignoreversion signonce
Source: "..\Boards\*.brd"; DestDir: "{app}\Boards"; Flags: ignoreversion
Source: "..\Boards\*.txt"; DestDir: "{app}\Boards"; Flags: ignoreversion
Source: "..\Journal\Info.txt"; DestDir: "{app}\Journal"; Flags: ignoreversion

;[Folders]
;Name: "{group}\Webseiten"; Languages: de

[Icons]
; Allgemein
Name: "{group}\Peg Solitaire"; Filename: "{app}\PegSolitaire.exe"
; Deutsch
;Name: "{group}\Deinstallieren"; Filename: "{uninstallexe}"
;Name: "{group}\Webseiten\Daniel Marschall"; Filename: "https://www.daniel-marschall.de/"
;Name: "{group}\Webseiten\ViaThinkSoft"; Filename: "https://www.viathinksoft.de/"
;Name: "{group}\Webseiten\Projektseite auf ViaThinkSoft"; Filename: "https://www.viathinksoft.de/projects/jumper"

[Run]
Filename: "{app}\PegSolitaire.exe"; Description: "PegSolitaire"; Flags: nowait postinstall skipifsilent

[Code]
function InitializeSetup(): Boolean;
begin
  if CheckForMutexes('PegSolitaireSetup')=false then
  begin
    Createmutex('PegSolitaireSetup');
    Result := true;
  end
  else
  begin
    Result := False;
  end;
end;

