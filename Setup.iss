; (De)Coder Script für InnoSetup
; Fehler bei Uninstallation: ReadOnly, Anwendung in Benutzung

[Setup]
AppName=PegSolitaire
AppVerName=PegSolitaire
AppVersion=1.0
AppCopyright=© Copyright 2009 - 2018 ViaThinkSoft.
AppPublisher=ViaThinkSoft
AppPublisherURL=http://www.viathinksoft.de/
AppSupportURL=http://www.daniel-marschall.de/
AppUpdatesURL=http://www.viathinksoft.de/
DefaultDirName={pf}\PegSolitaire
DefaultGroupName=PegSolitaire
UninstallDisplayIcon={app}\PegSolitaire.exe
VersionInfoCompany=ViaThinkSoft
VersionInfoCopyright=© Copyright 2009 - 2018 ViaThinkSoft.
VersionInfoDescription=PegSolitaire Setup
VersionInfoTextVersion=1.0.0.0
VersionInfoVersion=1.
OutputBaseFilename=PegSolitaireSetup
Compression=zip/9

[Languages]
Name: de; MessagesFile: "compiler:Languages\German.isl"

[Files]
; Allgemein
Source: "PegSolitaire.exe"; DestDir: "{app}"
Source: "PegSolitaire.deu"; DestDir: "{app}"
Source: "Boards\*.brd"; DestDir: "{app}\Boards"; Flags: ignoreversion
Source: "Boards\*.txt"; DestDir: "{app}\Boards"; Flags: ignoreversion
Source: "Journal\Info.txt"; DestDir: "{app}\Journal"; Flags: ignoreversion

[Folders]
Name: "{group}\Webseiten"; Languages: de

[Icons]
; Allgemein
Name: "{group}\PegSolitaire"; Filename: "{app}\PegSolitaire.exe"
; Deutsch
Name: "{group}\Deinstallieren"; Filename: "{uninstallexe}"
Name: "{group}\Webseiten\Daniel Marschall"; Filename: "https://www.daniel-marschall.de/"
Name: "{group}\Webseiten\ViaThinkSoft"; Filename: "https://www.viathinksoft.de/"
Name: "{group}\Webseiten\Projektseite auf ViaThinkSoft"; Filename: "https://www.viathinksoft.de/projects/jumper"

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

