unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, FileUtil, LazFileUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TreeView_FileList: TTreeView;
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
    procedure TreeView_FileListExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeView_FileListSelectionChanged(Sender: TObject);
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

procedure TMainForm.TreeView_FileListExpanded(Sender: TObject; Node: TTreeNode);
var
    list : TStringList;
    st, shortname   : String;

    { update FileListBox with list of pictures }
begin
    list := FindAllFiles(fullpath, '*.jpg', false {don't search in subdirectory});
    { strip full path off }
    for st in list do
    begin
        shortname:=ExtractFileName(st);
    end;

   list.Free;
end;

procedure TMainForm.TreeView_FileListSelectionChanged(Sender: TObject);
var
  aNode   : TTreeNode;
  fnamekey : Integer;
begin
  if Sender is TTreeView then //as the selectionchanged is raised from Treeview1 the
                                               //sender will always be a TTreeView but in case
                                               //of multiple treeviews attached to the same event this
                                               //way you set the caption to the controls selected node that raised the event.
     fname := TTreeView(Sender).Selected.Text;
   else // not really going to happen ever but here is a no thrills stupid code that is closely coupled with treeview1 not recommended to be used.
     fname := "";//

  { long filename (including path) }
  longfname:=AppendPathDelim(fullpath)+fname;

  filelabel.Caption:=longfname;
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
    shortname:=ExtractFileName(fullpath);

    TreeView_FileList.Items.Clear;

    // populate TTreeView :

    // init the tree - add a root node :
    TreeView_FileList.TopItem:= TreeView_FileList.Items.Add( NIL, shortname );
    TreeView_FileList.TopItem.HasChildren:= TRUE;
//    TreeView_FileList.Images:= ImageList;

  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  st, shortname   : String;
  list    : TStringList;
  aNode   : TTreeNode;

  begin
  Mainform.Caption:='LPIX - by Wayne Lockie June 22, 2020';
  SavePanel.visible:=false;

begin
      list := FindAllFiles(fullpath, '*.jpg', false {don't search in subdirectory});
      { strip full path off }
      for st in list do
        begin
          shortname:=ExtractFileName(st);

          // add child nodes + expand tree :
           aNode:= TreeView_FileList.Items.AddChild( TreeView_FileList.TopItem, shortname );
           aNode.ImageIndex:= 0;
           aNode.SelectedIndex:= 0;
       end;

      list.Free;
  end;
end;

procedure TMainForm.FileListBoxClick(Sender: TObject);
var
  aNode   : TTreeNode;
  fnamekey : Integer;
begin
  { selected item, short filname (no path) }
  fname:=TreeView_FileList.SelectedNode.Text;
  anode:=TreeView_FileList.Items[fnamekey];

  { long filename (including path) }
  longfname:=AppendPathDelim(fullpath)+fname;

  filelabel.Caption:=longfname;
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

