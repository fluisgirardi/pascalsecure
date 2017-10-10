unit security.manager.mgntdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ActnList;

type
  TCustomUsrMgnt = class(TForm)
    ActionList1: TActionList;
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
  private

  public

  end;

var
  CustomUsrMgnt: TCustomUsrMgnt;

implementation

{$R *.lfm}

end.

