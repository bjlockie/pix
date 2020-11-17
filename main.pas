unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, FileUtil, LazFileUtils, StrUtils, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    DisplayPanel: TPanel;
    SaveButton: TButton;
    FileListBox: TListBox;
    WhenBox: TEdit;
    WhoBox: TEdit;
    WhereBox: TEdit;
    FileDataNameLabel: TLabel;
    WhenLabel: TLabel;
    WhoLabel: TLabel;
    WhereLabel: TLabel;
    PicBox: TImage;
    FileLabel: TLabel;
    procedure FileDataNameLabelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FileListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure WhenLabelClick(Sender: TObject);
    procedure DisplayPanelClick(Sender: TObject);
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


{ update FileListBox with directory list of current directory }
Procedure ListDirectory();
var
    list, flist, dirlist : TStringList;
    st, shortname        : String;
begin
    fullpath:=GetCurrentDir();

    dirlist:=TStringList.Create();
    list := FindAllDirectories(fullpath, false {don't search in subdirectory});
    { strip full path off and prepend "(DIR) " }
    for st in list do
    begin
        shortname:=ExtractFileName(st);
        shortname:='(DIR) '+shortname;
        dirlist.Add( shortname );
    end;
    list.Free;
    dirlist.Sort;

    { prepend PARENT (..) to list of directories }
    dirlist.Insert( 0, 'PARENT (..)' );

    flist:=TStringList.Create();
    list := FindAllFiles(fullpath, '*.jpg', false {don't search in subdirectory});
    { strip full path off }
    for st in list do
    begin
        shortname:=ExtractFileName(st);
        flist.Add( shortname );
    end;
    list.Free;
    flist.Sort;

    for st in flist do
    begin
        dirlist.Add( st )
    end;
    flist.Free;

    mainform.FileListBox.Items := dirlist;
    dirlist.Free;

    { update the filelabel }
    mainform.filelabel.Caption:=fullpath;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Mainform.Caption:='LPIX v5 - by Wayne Lockie Nov 13, 2020';
  SaveButton.visible:=false;
  FileLabel.visible:=false;
  FileDataNameLabel.visible:=false;
  ListDirectory();
end;

procedure TMainForm.FileDataNameLabelClick(Sender: TObject);
begin

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
         Reply := Application.MessageBox('Save changes and continue?"', 'MessageBoxDemo', BoxStyle);
         if Reply = IDYES then
         begin
              SaveEdits(key);
              SaveButton.visible:=false;
         end;
    end;

    { selected item, short filname (no path) }
    fnamekey:=FileListBox.ItemIndex;
    if fnamekey > -1 then
    begin
        fname:=FileListBox.Items[fnamekey];

        fullpath:=GetCurrentDir();

        if AnsiStartsStr( 'PARENT (..)', fname ) then
        begin
            parentDirPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fullpath));
            { change to parent directory }
            ChDir( parentDirPath );

            ListDirectory();
        end
        else if AnsiStartsStr( '(DIR) ', fname ) then
        begin
            { remove "(DIR) " from fname }
            fname:=RightStr(fname,Length(fname)-6);

            { long filename (including path) }
            longfname:=AppendPathDelim(fullpath)+fname;

            { a directory has been selected }
            ChDir( longfname );

            ListDirectory();
        end
        else
        begin
            { long filename (including path) }
            longfname:=AppendPathDelim(fullpath)+fname;

            FileDataNameLabel.Caption:=longfname;

            picbox.Picture.LoadFromFile(longfname);
            WhenBox.text:='';
            Whobox.text:='';
            WhereBox.text:='';
            key:=copy(longfname,1,length(longfname)-4)+'.txt';
            FileDataNameLabel.caption:='['+key+']';
            WhenBox.Visible :=true;
            WhoBox.visible:=True;
            Wherebox.visible:=true;
            ShowComments(key);
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

procedure TMainForm.WhenLabelClick(Sender: TObject);
begin

end;

procedure TMainForm.DisplayPanelClick(Sender: TObject);
begin

end;

procedure TMainForm.WhenBoxEnter(Sender: TObject);
begin
  SaveButton.visible:=true;
end;

procedure TMainForm.WhereBoxEnter(Sender: TObject);
begin
  SaveButton.visible:=true;
end;

procedure TMainForm.WhoBoxEnter(Sender: TObject);
begin
  SaveButton.visible:=true;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
(* Save edited comments *)
begin
  SaveEdits(key);
  SaveButton.visible:=false;
end;


end.

