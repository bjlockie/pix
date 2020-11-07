unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, FileUtil, LazFileUtils, StrUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    FileListBox: TListBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    WhenBox: TEdit;
    WhoBox: TEdit;
    WhereBox: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LoadPanel: TPanel;
    SavePanel: TPanel;
    PicBox: TImage;
    FileLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FileListBoxClick(Sender: TObject);
    procedure LoadPanelClick(Sender: TObject);
    procedure SavePanelClick(Sender: TObject);
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

procedure TMainForm.LoadPanelClick(Sender: TObject);
var
    list, shortlist : TStringList;
    st, shortname   : String;
begin
  WhenBox.Visible :=false;
  SavePanel.visible:=false;
  if SelectDirectoryDialog.Execute then
  begin
    savepanel.visible:=false;
    fullpath:=copy(SelectDirectoryDialog.FileName,1,240);

    { update FileListBox with list of pictures }
    begin
        shortlist:=TStringList.Create();
        list := FindAllDirectories(fullpath, false {don't search in subdirectory});
        { strip full path off and add "(DIR) " }
        for st in list do
          begin
            shortname:=ExtractFileName(st);
            shortname:='(DIR) '+shortname;
            shortlist.Add( shortname );
          end;

        list := FindAllFiles(fullpath, '*.jpg', false {don't search in subdirectory});
        { strip full path off }
        for st in list do
          begin
            shortname:=ExtractFileName(st);
            shortlist.Add( shortname );
          end;

        FileListBox.Items := shortlist;
        list.Free;
        shortlist.Free;
    end;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Mainform.Caption:='LPIX v4 - by Wayne Lockie Nov 7, 2020';
  SavePanel.visible:=false;
end;

procedure TMainForm.FileListBoxClick(Sender: TObject);
var
  fnamekey : Integer;
begin
  { selected item, short filname (no path) }
  fnamekey:=FileListBox.ItemIndex;
  fname:=FileListBox.Items[fnamekey];

  { long filename (including path) }
  longfname:=AppendPathDelim(fullpath)+fname;

  filelabel.Caption:=longfname;

  if AnsiStartsStr( '(DIR) ', fname ) then
  begin
      { a directory has been selected }
      ChDir( longfname );
  end
  else
  begin
      picbox.Picture.LoadFromFile(longfname);
      WhenBox.text:='';
      Whobox.text:='';
      WhereBox.text:='';
      key:=copy(longfname,1,length(longfname)-4)+'.txt';
      label1.caption:='['+key+']';
      WhenBox.Visible :=true;
      WhoBox.visible:=True;
      Wherebox.visible:=true;
      ShowComments(key);
  end;
end;


procedure TMainForm.SavePanelClick(Sender: TObject);
(* Save edited comments *)
begin
  SaveEdits(key);
  SavePanel.visible:=false;
end;

procedure TMainForm.WhenBoxEnter(Sender: TObject);
begin
  SavePanel.visible:=true;
end;

procedure TMainForm.WhereBoxEnter(Sender: TObject);
begin
  SavePanel.visible:=true;
end;

procedure TMainForm.WhoBoxEnter(Sender: TObject);
begin
  SavePanel.visible:=true;
end;



end.

