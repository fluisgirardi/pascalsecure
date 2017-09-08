unit security.manager.level.addusrdlg;

{$mode objfpc}{$H+}

interface

uses
  security.manager.addusrdlg, StdCtrls, ButtonPanel;

type
  TsecureLevelAddUser = class(TsecureAddUser)
    Label5: TLabel;
    secureUserLevel: TSpinEdit;
  private

  public

  end;

var
  secureLevelAddUser: TsecureLevelAddUser;

implementation

{$R *.lfm}

end.

