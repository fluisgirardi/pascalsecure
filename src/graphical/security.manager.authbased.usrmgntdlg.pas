unit security.manager.authbased.usrmgntdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls;

type
  TAuthBasedUsrMgnt = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListView1: TListView;
    SecurePageControl1: TPageControl;
    SecureTabSheet1: TTabSheet;
    ToolBar1: TToolBar;
  private

  public

  end;

var
  AuthBasedUsrMgnt: TAuthBasedUsrMgnt;

implementation

{$R *.lfm}

end.

