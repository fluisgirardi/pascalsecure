unit security.manager.level.mgntdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, ActnList, security.manager.mgntdlg;

type
  TsecureUsrLvlMgnt = class(TCustomUsrMgnt)
    ChangePass: TAction;
    addUser: TAction;
    delUser: TAction;
    DisableUser: TAction;
    ListView1: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
  private

  public

  end;

var
  secureUsrLvlMgnt: TsecureUsrLvlMgnt;

implementation

{$R *.lfm}

end.

