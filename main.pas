unit Main;

{$mode objfpc}{$H+}

interface

uses
{$ifdef Windows}
 Windows,
{$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, FileUtil, LazFileUtils, StrUtils, LCLType, PairSplitter;

type

  { TMainForm }

  TMainForm = class(TForm)
    DisplayPanel: TPanel;
    FileListBox: TListBox;
    FileNameDataLabel: TLabel;
    FileNameLabel: TLabel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    PicBox: TImage;
    SaveButton: TButton;
    CancelButton: TButton;
    WhenBox: TEdit;
    WhenLabel: TLabel;
    WhereBox: TEdit;
    WhereLabel: TLabel;
    WhoBox: TEdit;
    WhoLabel: TLabel;
    procedure ShowEditBoxes();
    procedure HideEditBoxes();
    procedure FormActivate(Sender: TObject);
    procedure FileListBoxClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure WhenBoxEnter(Sender: TObject);
    procedure WhereBoxEnter(Sender: TObject);
    procedure WhoBoxEnter(Sender: TObject);
  private

  public

  end;

var
  MainForm   : TMainForm;
  longfname  : string;    { full name including path }
  fname      : string;    { short name excluding path }
  key        : string;    { longfname changed to .txt }
  fullpath   : string;


implementation

{$R *.lfm}

{ TMainForm }
Procedure TMainForm.ShowEditBoxes();
begin
    WhenBox.Visible:=True;
    WhenLabel.Visible:=True;
    WhoBox.Visible:=True;
    WhoLabel.Visible:=True;
    WhereBox.Visible:=True;
    WhereLabel.Visible:=True;
    FileNameDataLabel.Visible:=True;
end;

Procedure TMainForm.HideEditBoxes();
begin
    WhenBox.Visible:=False;
    WhenLabel.Visible:=False;
    WhoBox.Visible:=False;
    WhoLabel.Visible:=False;
    WhereBox.Visible:=False;
    WhereLabel.Visible:=False;
    FileNameDataLabel.Visible:=False;
end;

Procedure ShowComments(filename:string);
var f : textfile;
    when, who, where : string;
begin
  (*$i- *)                          (* Turn off to use ioresult *)
  assignfile(f,filename);
  reset(f);
  (*$i+ *)                          (* Turn on error trap *)
  if ioresult=0 then                (* file found - no errors *)
  begin
    readln(f,when);
    readln(f,who);
    readln(f,where);
    mainform.whenbox.Text:=when;
    mainform.whobox.Text:=who;
    mainform.wherebox.Text:=where;
    close(f);
  end;
end;

Procedure SaveEdits(filename:string);
var f       : textfile;
begin
  assignfile(f,filename);
  rewrite(f);
  writeln(f, mainform.whenbox.Text);
  writeln(f, mainform.whobox.Text);
  writeln(f, mainform.wherebox.Text);
  close(f);
end;

function GetBit(Value: QWord; Index: Byte): Boolean;
begin
  Result := ((Value shr Index) and 1) = 1;
end;

function WindowsDriveType( dt : UINT ): String;
begin
    case dt of
        0: Result:='Unknown';
        1: Result:='Invalid';
        2: Result:='Removbl';
        3: Result:='Fixed';
        4: Result:='Network';
        5: Result:='CD-ROM';
        6: Result:='RAMDISK';
    end;
end;

function GetDriveLetters(): TStringList;
{$ifdef Windows}
var dword     : QWord;
    letter    : Char;
    i         : Byte = 0;
    shortname : String;
    dr        : Packed array[0..3] of char;
    dt        : UINT;
{$endif}
begin
    Result:=TStringList.Create();
    {$ifdef Windows}
        ZeroMemory(@dr, sizeof(dr));
        lstrcpy(dr, 'A:\');

        dword:=GetLogicalDrives();

        for letter:='A' to 'Z' do
        begin
            if GetBit(dword,i)=True then
            begin
                { get drive type }
                dr[0]:=char(ord('A')+i);
                dt:=GetDriveType(dr);
                shortname:='(DRIVE) '+letter+':\ ('+WindowsDriveType(dt)+')';
                Result.Add( shortname );
            end;
            inc(i);
        end;
    {$endif}
end;

{ update FileListBox with directory list of current directory }
Procedure ListDirectory();
var
    list, templist : TStringList;
    st, shortname  : String;
begin
    fullpath:=GetCurrentDir();

    { list drives }
    list := GetDriveLetters();
    if (list.Count > 0) then
    begin
        { add spacer }
        list.Add('----');
    end;

    { add PARENT (..) to list of directories and drives }
    list.Add( 'PARENT (..)' );

    { list directories }
    templist := FindAllDirectories(fullpath, false {don't search in subdirectory});
    templist.Sort;
    { strip full path off and prepend "(DIR) " }
    for st in templist do
    begin
        shortname:=ExtractFileName(st);
        shortname:='(DIR) '+shortname;
        list.Add( shortname );
    end;
    templist.Free;

    { list files }
    templist:=TStringList.Create();
    templist := FindAllFiles(fullpath, '*.jpg', false {don't search in subdirectory});
    templist.Sort;
    if (templist.Count > 0) then
    begin
        { add spacer }
        {list.Add('----'+IntToStr(templist.Count));}
        list.Add('----');
    end;
    { strip full path off }
    for st in templist do
    begin
        shortname:=ExtractFileName(st);
        list.Add( shortname );
    end;
    templist.Free;

    mainform.FileListBox.Items := list;
    list.Free;

    { update the filelabel with the path }
    MainForm.FileNameLabel.Caption:=fullpath;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Mainform.Caption:='LPIX v5.6 - by Wayne Lockie Nov 22, 2020 (https://github.com/bjlockie/pix)';
  ListDirectory();
end;

procedure ChangeDir( dirname : String );
begin
    try
        ChDir( dirname );
    except
        ShowMessage('Can''t change to '''+dirname+'''');
    end;
end;

procedure TMainForm.FileListBoxClick(Sender: TObject);
var
  fnamekey        : Integer;
  parentDirPath   : String;
  Reply, BoxStyle : Integer;
begin
    if SaveButton.visible=true then
    begin
        { maybe changes have been made }
        BoxStyle := MB_ICONQUESTION + MB_YESNO;
        Reply := Application.MessageBox('Save changes?                ', 'Save changes', BoxStyle);
        if Reply = IDYES then
        begin
            SaveEdits(key);
        end
        else
        begin
            ShowComments(key);
        end;
    end;

    SaveButton.visible:=False;
    CancelButton.visible:=False;

    { selected item, short filname (no path) }
    fnamekey:=FileListBox.ItemIndex;
    if fnamekey > -1 then
    begin
        fname:=FileListBox.Items[fnamekey];

        if AnsiStartsStr( '----', fname ) then
        begin
            { do nothing on spacer }
        end
        else
        begin
            fullpath:=GetCurrentDir();

            if AnsiStartsStr( '(DRIVE) ', fname ) then
            begin
                { extract drive letter (eg. C:\) from fname }
                fname:=MidStr(fname,9,3);
                {Reply := Application.MessageBox('Save changes?                ', PChar(fname), BoxStyle);}
                {$ifdef Windows}
                ChangeDir(fname);
                ListDirectory();
                { hide all the edit boxes }
                HideEditBoxes();
                {$endif}
            end
            else if AnsiStartsStr( 'PARENT (..)', fname ) then
            begin
                parentDirPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fullpath));
                { change to parent directory }
                ChangeDir( parentDirPath );

                ListDirectory();

                { hide all the edit boxes }
                HideEditBoxes();
            end
            else if AnsiStartsStr( '(DIR) ', fname ) then
            begin
                { remove "(DIR) " from fname }
                fname:=RightStr(fname,Length(fname)-6);

                { long filename (including path) }
                longfname:=AppendPathDelim(fullpath)+fname;

                { a directory has been selected }
                ChangeDir( longfname );

                ListDirectory();

                { hide all the edit boxes }
                HideEditBoxes();
            end
            else
            begin
                { show all the edit boxes }
                ShowEditBoxes();

                { long filename (including path) }
                longfname:=AppendPathDelim(fullpath)+fname;

                PicBox.Picture.LoadFromFile(longfname);
                WhenBox.text:='';
                WhoBox.text:='';
                WhereBox.text:='';
                key:=copy(longfname,1,length(longfname)-4)+'.txt';

                { update the filelabel with the path }
                FileNameLabel.Caption:=longfname;

                { update the data filename label }
                FileNameDataLabel.Caption:='['+key+']';

                ShowComments(key);
            end;
        end;
    end;
end;

procedure TMainForm.WhenBoxEnter(Sender: TObject);
begin
    SaveButton.visible:=true;
    CancelButton.visible:=true;
end;

procedure TMainForm.WhereBoxEnter(Sender: TObject);
begin
  SaveButton.visible:=true;
  CancelButton.visible:=true;
end;

procedure TMainForm.WhoBoxEnter(Sender: TObject);
begin
  SaveButton.visible:=true;
  CancelButton.visible:=true;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
(* Save edited comments *)
begin
    SaveEdits(key);
    SaveButton.Visible:=False;
    CancelButton.Visible:=False;
end;

procedure TMainForm.CancelButtonClick(Sender: TObject);
(* cancel edited comments *)
begin
    SaveButton.Visible:=False;
    CancelButton.Visible:=False;
end;


end.

