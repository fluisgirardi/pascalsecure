unit security.manager.graphical_user_management;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls, ComCtrls,
  sysutils, math, strutils,
  security.manager.schema,
  security.manager.custom_usrmgnt_interface,
  security.manager.basic_user_management;

type

  TSecureFocusedControl = (fcUserName, fcPassword);

  { TCustomFrmLogin }

  TSecureCustomFrmLogin = Class(TCustomForm)
  private
    FFocusedControl: TSecureFocusedControl;
    FOnCancelClick: TNotifyEvent;
    FOnOKClick: TNotifyEvent;
    function GetUserLogin: String; virtual; abstract;
    function GetUserPassword: String; virtual; abstract;
    procedure SetFocusedControl(AValue: TSecureFocusedControl); virtual; abstract;
    procedure SetUserLogin(AValue: String); virtual; abstract;
    procedure SetUserPassword(AValue: String); virtual; abstract;
  public
    procedure DisableEntry; virtual; abstract;
    procedure EnableEntry; virtual; abstract;
    property FocusedControl:TSecureFocusedControl read FFocusedControl write SetFocusedControl;
    property UserLogin:String read GetUserLogin write SetUserLogin;
    property UserPassword:String read GetUserPassword write SetUserPassword;
    property OnOKClick:TNotifyEvent read FOnOKClick write FOnOKClick;
    property OnCancelClick:TNotifyEvent read FOnCancelClick write FOnCancelClick;
  end;

  TSecureCustomFrmLoginClass = class of TSecureCustomFrmLogin;

  { TFrmLogin }

  TSecureFrmLogin = Class(TSecureCustomFrmLogin)
  private
    lblLogin,
    lblPassword:TLabel;
    edtLogin,
    edtPassword:TEdit;
    btnButtons:TButtonPanel;
    procedure CancelClicked(Sender: TObject);
    function GetUserLogin: String; override;
    function GetUserPassword: String; override;
    procedure OKClicked(Sender: TObject);
    procedure SetUserLogin(AValue: String); override;
    procedure SetUserPassword(AValue: String); override;
    procedure SetFocusedControl(AValue: TSecureFocusedControl); override;
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure DisableEntry; override;
    procedure EnableEntry; override;
    property FocusedControl:TSecureFocusedControl read FFocusedControl write FFocusedControl;
    property UserLogin:String read GetUserLogin write SetUserLogin;
    property UserPassword:String read GetUserPassword write SetUserPassword;
  end;

  { TGraphicalUsrMgntInterface }

  TGraphicalUsrMgntInterface = class(TCustomUsrMgntInterface)
  private
    procedure LevelAddUserClick(Sender: TObject);
    procedure LevelBlockUserClick(Sender: TObject;
      const aUser: TUserWithLevelAccess);
    procedure LevelChangeUserClick(Sender: TObject;
      const aUser: TUserWithLevelAccess);
    procedure LevelChangeUserPassClick(Sender: TObject;
      const aUser: TUserWithLevelAccess);
  protected
    frmLogin:TSecureCustomFrmLogin;
    FCanCloseLogin:Boolean;
    FCurrentUserSchema:TUsrMgntSchema;
    procedure CanCloseLogin(Sender: TObject; var CanClose: boolean);
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    function  GetLoginClass:TSecureCustomFrmLoginClass; virtual;
  public
    function  Login(out aLogin, aPass:UTF8String):Boolean; override;
    function  Login:Boolean; override;
    function  CanLogout:Boolean; override;
    procedure UserManagement(aSchema:TUsrMgntSchema); override;
    procedure FreezeUserLogin; override;
    procedure UnfreezeUserLogin; override;
    procedure ProcessMessages; override;
    function  LoginVisibleBetweenRetries:Boolean; override;
  end;

ResourceString
  strFrmLoginCaption   = 'PascalSecure Login';
  strUserLogin         = '&Login';
  strUserPass          = '&Password';
  SSpecialLoginCaption = 'Enter a user that can access the "%s" token.';
  strYes               = 'Yes';
  strNo                = 'No';

implementation

uses security.manager.controls_manager,
     security.exceptions,
     security.manager.level.mgntdlg,
     security.manager.level.addusrdlg, Dialogs;

{ TFrmLogin }

function TSecureFrmLogin.GetUserLogin: String;
begin
  Result:=edtLogin.Text;
end;

procedure TSecureFrmLogin.CancelClicked(Sender: TObject);
begin
  if Assigned(FOnCancelClick) then
    FOnCancelClick(Sender)
end;

function TSecureFrmLogin.GetUserPassword: String;
begin
  Result:=edtPassword.Text;
end;

procedure TSecureFrmLogin.OKClicked(Sender: TObject);
begin
  if Assigned(FOnOKClick) then
    FOnOKClick(Sender);
end;

procedure TSecureFrmLogin.SetUserLogin(AValue: String);
begin
  edtLogin.Text:=AValue;
end;

procedure TSecureFrmLogin.SetUserPassword(AValue: String);
begin
  edtPassword.Text:=AValue;
end;

procedure TSecureFrmLogin.SetFocusedControl(AValue: TSecureFocusedControl);
begin
  if CanFocus then
    case AValue of
      fcUserName: if edtLogin.Enabled and edtLogin.CanFocus then edtLogin.SetFocus;
      fcPassword: if edtPassword.Enabled and  edtPassword.CanFocus then edtPassword.SetFocus;
    end;

  FFocusedControl:=AValue;
end;

procedure TSecureFrmLogin.DoShow;
begin
  inherited DoShow;
  SetFocusedControl(FFocusedControl);
end;

constructor TSecureFrmLogin.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner,Num);
  BorderStyle:=bsDialog;
  SetBounds(0,0,300,150);
  Position:=poScreenCenter;
  FormStyle:=fsSystemStayOnTop;

  lblLogin:=TLabel.Create(Self);
  lblLogin.Caption:=strUserLogin;
  lblLogin.SetBounds(8,8,58,14);
  lblLogin.Parent:=Self;

  edtLogin:=TEdit.Create(Self);
  edtLogin.Name:='PASCALSECURITY_FRMLOGIN_edtLogin';
  edtLogin.Text:='';
  edtLogin.SetBounds(8,24, 290, 86);
  edtLogin.Parent:=Self;

  lblLogin.FocusControl:=edtLogin;

  lblPassword:=TLabel.Create(Self);
  lblPassword.Caption:=strUserPass;
  lblPassword.SetBounds(8,56,54,14);
  lblPassword.Parent:=Self;

  edtPassword:=TEdit.Create(Self);
  edtPassword.Name:='PASCALSECURITY_FRMLOGIN_edtPassword';
  edtPassword.PasswordChar:='*';
  edtPassword.SetBounds(8,72, 290, 86);
  edtPassword.Text:='';
  edtPassword.Parent:=Self;

  lblPassword.FocusControl:=edtPassword;

  btnButtons:=TButtonPanel.Create(Self);
  btnButtons.ShowButtons:=[pbOK, pbCancel];
  btnButtons.OKButton.OnClick:=@OKClicked;
  btnButtons.CancelButton.OnClick:=@CancelClicked;
  btnButtons.Parent:=Self;
end;

procedure TSecureFrmLogin.EnableEntry;
begin
  edtPassword.Enabled:=true;
  edtLogin.Enabled:=true;
  btnButtons.Enabled:=true;
  DoShow;
end;

procedure TSecureFrmLogin.DisableEntry;
begin
  edtPassword.Enabled:=false;
  edtLogin.Enabled:=false;
  btnButtons.Enabled:=false;
end;

{ TGraphicalUsrMgntInterface }

procedure TGraphicalUsrMgntInterface.OKClick(Sender: TObject);
var
  aUID:Integer;
begin
  FCanCloseLogin:=false;
  if not Assigned(frmLogin) then exit;
  FCanCloseLogin:=GetControlSecurityManager.Login(frmLogin.GetUserLogin,frmLogin.GetUserPassword, aUID);
  if not FCanCloseLogin then begin
    frmLogin.SetUserPassword('');
    frmLogin.FocusedControl:=fcPassword;
    frmLogin.DoShow;
  end;
end;

function TGraphicalUsrMgntInterface.GetLoginClass: TSecureCustomFrmLoginClass;
begin
  Result:=TSecureFrmLogin;
end;

procedure TGraphicalUsrMgntInterface.LevelAddUserClick(Sender: TObject);
begin

end;

procedure TGraphicalUsrMgntInterface.LevelBlockUserClick(Sender: TObject;
  const aUser: TUserWithLevelAccess);
begin

end;

procedure TGraphicalUsrMgntInterface.LevelChangeUserClick(Sender: TObject;
  const aUser: TUserWithLevelAccess);
begin

end;

procedure TGraphicalUsrMgntInterface.LevelChangeUserPassClick(Sender: TObject;
  const aUser: TUserWithLevelAccess);
begin

end;

procedure TGraphicalUsrMgntInterface.CanCloseLogin(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=FCanCloseLogin;
end;

procedure TGraphicalUsrMgntInterface.CancelClick(Sender: TObject);
begin
  FCanCloseLogin:=true;
end;

function TGraphicalUsrMgntInterface.Login(out aLogin, aPass: UTF8String
  ): Boolean;
var
  afrmLogin: TSecureCustomFrmLogin;
begin
  afrmLogin:=GetLoginClass.CreateNew(Application);
  try
    Result:=afrmLogin.ShowModal=mrOK;
    if Result then begin
      aLogin:=frmLogin.UserLogin;
      aPass :=frmLogin.UserPassword;
    end;
  finally
    FreeAndNil(afrmLogin);
  end;
end;

function TGraphicalUsrMgntInterface.Login: Boolean;
begin
  if Assigned(frmLogin) then begin
    Result:=false;
    frmLogin.ShowOnTop;
    exit;
  end;

  FCanCloseLogin:=false;

  frmLogin:=GetLoginClass.CreateNew(nil);
  try
    frmLogin.Caption:=strFrmLoginCaption;
    frmLogin.OnCloseQuery:=@CanCloseLogin;
    frmLogin.OnOKClick:=@OKClick;
    frmLogin.OnCancelClick:=@CancelClick;
    frmLogin.UserLogin:='';
    frmLogin.FocusedControl:=fcUserName;
    Result:=frmLogin.ShowModal=mrOK;
  finally
    FreeAndNil(frmLogin);
  end;
end;

function TGraphicalUsrMgntInterface.CanLogout: Boolean;
begin
  Result:=true;
end;

procedure TGraphicalUsrMgntInterface.UserManagement(aSchema: TUsrMgntSchema);
var
  lvlfrm:TsecureUsrLvlMgnt;
  lvlSchema: TUsrLevelMgntSchema;
  i: Integer;
  usr: TUserWithLevelAccess;
  item: TListItem;
  lvlLength: Integer;

  function RepeatString(aStr:String; count:Integer):String;
  var
    c: Integer;
  begin
    Result:='';
    for c:=1 to count do
      Result:=Result+aStr;
  end;

begin
  //allows one user management...
  if (not Assigned(FCurrentUserSchema)) and Assigned(aSchema) then begin
    FCurrentUserSchema:=aSchema;
    try
      if aSchema is TUsrLevelMgntSchema then begin
        lvlSchema:=TUsrLevelMgntSchema(aSchema);
        lvlfrm:=TsecureUsrLvlMgnt.Create(Self);
        lvlfrm.OnAddUserClick:=@LevelAddUserClick;
        lvlfrm.OnBlockUserClick:=@LevelBlockUserClick;
        lvlfrm.OnChangeUserClick:=@LevelChangeUserClick;
        lvlfrm.OnChangeUsrPassClick:=@LevelChangeUserPassClick;
        try

          lvlLength := Max(Length(IntToStr(lvlSchema.MinLevel)), Length(IntToStr(lvlSchema.MaxLevel)));

          for i:=0 to lvlSchema.UserList.Count-1 do begin
            usr:=lvlSchema.UserList.KeyData[lvlSchema.UserList.Keys[i]];
            item:=lvlfrm.ListView1.Items.add;
            item.Caption:=usr.Login;
            item.SubItems.Add(usr.UserDescription);
            item.SubItems.Add(RightStr(RepeatString('0',lvlLength)+inttostr(usr.UserLevel),lvlLength));
            item.SubItems.Add(ifthen(usr.UserBlocked, strYes, strNo));
            item.Data:=usr;
          end;
          lvlfrm.ShowModal;
        finally
          FreeAndNil(lvlfrm);
        end;
        exit;
      end;

      if aSchema is TUsrAuthSchema then begin

        exit;
      end;

      if aSchema is TUsrGroupAuthSchema then begin

        exit;
      end;

      if aSchema is TGroupAuthSchema then begin

        exit;
      end;

      //unknown schema class...
      raise EUnknownUserMgntSchema.Create;
    finally
      FCurrentUserSchema:=nil;
    end;
  end else begin
    if not Assigned(aSchema) then
      raise ENilUserSchema.Create;

    //TODO: Second user management session exception...
  end;

end;

procedure TGraphicalUsrMgntInterface.FreezeUserLogin;
begin
  if Assigned(frmLogin) then frmLogin.DisableEntry;
end;

procedure TGraphicalUsrMgntInterface.UnfreezeUserLogin;
begin
  if Assigned(frmLogin) then frmLogin.EnableEntry;
end;

procedure TGraphicalUsrMgntInterface.ProcessMessages;
begin
  Application.ProcessMessages;
  CheckSynchronize(1);
  Sleep(1);
end;

function TGraphicalUsrMgntInterface.LoginVisibleBetweenRetries: Boolean;
begin
  Result:=true;
end;

end.

