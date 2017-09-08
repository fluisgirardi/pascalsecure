unit security.manager.addusrdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ComCtrls, Spin;

type
  TsecureLevelAddUser = class(TForm)
    ButtonPanel1: TButtonPanel;
    UserEnabled: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    usrPassword: TEdit;
    usrConfirmPassword: TEdit;
    usrLogin: TEdit;
    Label1: TLabel;
    usrFullName: TEdit;
  private

  public

  end;

var
  secureLevelAddUser: TsecureLevelAddUser;

implementation

{$R *.lfm}

end.

