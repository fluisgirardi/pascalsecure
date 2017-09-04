unit security.manager.changepassdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type
  TsecureChangeUsrPass = class(TForm)
    ButtonPanel1: TButtonPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private

  public

  end;

var
  secureChangeUsrPass: TsecureChangeUsrPass;

implementation

{$R *.lfm}

end.

