unit security.manager.level.mgntdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel;

type
  TsecureUsrLvlMgnt = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListView1: TListView;
  private

  public

  end;

var
  secureUsrLvlMgnt: TsecureUsrLvlMgnt;

implementation

{$R *.lfm}

end.

