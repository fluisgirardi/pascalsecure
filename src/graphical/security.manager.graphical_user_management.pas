unit security.manager.graphical_user_management;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls,
  sysutils,
  security.manager.schema,
  security.manager.custom_usrmgnt_interface,
  security.manager.basic_user_management;

type

  TFocusedControl = (fcUserName, fcPassword);

  { TCustomFrmLogin }

  TCustomFrmLogin = Class(TCustomForm)
  private
    FFocusedControl: TFocusedControl;
    FOnCancelClick: TNotifyEvent;
    FOnOKClick: TNotifyEvent;
    function GetUserLogin: String; virtual; abstract;
    function GetUserPassword: String; virtual; abstract;
    procedure SetFocusedControl(AValue: TFocusedControl); virtual; abstract;
    procedure SetUserLogin(AValue: String); virtual; abstract;
    procedure SetUserPassword(AValue: String); virtual; abstract;
  public
    procedure DisableEntry; virtual; abstract;
    procedure EnableEntry; virtual; abstract;
    property FocusedControl:TFocusedControl read FFocusedControl write SetFocusedControl;
    property UserLogin:String read GetUserLogin write SetUserLogin;
    property UserPassword:String read GetUserPassword write SetUserPassword;
    property OnOKClick:TNotifyEvent read FOnOKClick write FOnOKClick;
    property OnCancelClick:TNotifyEvent read FOnCancelClick write FOnCancelClick;
  end;

  TCustomFrmLoginClass = class of TCustomFrmLogin;

  { TFrmLogin }

  TFrmLogin = Class(TCustomFrmLogin)
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
    procedure SetFocusedControl(AValue: TFocusedControl); override;
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure DisableEntry; override;
    procedure EnableEntry; override;
    property FocusedControl:TFocusedControl read FFocusedControl write FFocusedControl;
    property UserLogin:String read GetUserLogin write SetUserLogin;
    property UserPassword:String read GetUserPassword write SetUserPassword;
  end;

  { TGraphicalUsrMgntInterface }

  TGraphicalUsrMgntInterface = class(TCustomUsrMgntInterface)
  protected
    frmLogin:TCustomFrmLogin;
    FCanCloseLogin:Boolean;
    procedure CanCloseLogin(Sender: TObject; var CanClose: boolean);
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    function  GetLoginClass:TCustomFrmLoginClass; virtual;
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

implementation

uses security.manager.controls_manager;

{ TFrmLogin }

function TFrmLogin.GetUserLogin: String;
begin
  Result:=edtLogin.Text;
end;

procedure TFrmLogin.CancelClicked(Sender: TObject);
begin
  if Assigned(FOnCancelClick) then
    FOnCancelClick(Sender)
end;

function TFrmLogin.GetUserPassword: String;
begin
  Result:=edtPassword.Text;
end;

procedure TFrmLogin.OKClicked(Sender: TObject);
begin
  if Assigned(FOnOKClick) then
    FOnOKClick(Sender);
end;

procedure TFrmLogin.SetUserLogin(AValue: String);
begin
  edtLogin.Text:=AValue;
end;

procedure TFrmLogin.SetUserPassword(AValue: String);
begin
  edtPassword.Text:=AValue;
end;

procedure TFrmLogin.SetFocusedControl(AValue: TFocusedControl);
begin
  if CanFocus then
    case AValue of
      fcUserName: if edtLogin.Enabled and edtLogin.CanFocus then edtLogin.SetFocus;
      fcPassword: if edtPassword.Enabled and  edtPassword.CanFocus then edtPassword.SetFocus;
    end;

  FFocusedControl:=AValue;
end;

procedure TFrmLogin.DoShow;
begin
  inherited DoShow;
  SetFocusedControl(FFocusedControl);
end;

constructor TFrmLogin.CreateNew(AOwner: TComponent; Num: Integer);
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

procedure TFrmLogin.EnableEntry;
begin
  edtPassword.Enabled:=true;
  edtLogin.Enabled:=true;
  btnButtons.Enabled:=true;
  DoShow;
end;

procedure TFrmLogin.DisableEntry;
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

function TGraphicalUsrMgntInterface.GetLoginClass: TCustomFrmLoginClass;
begin
  Result:=TFrmLogin;
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
  afrmLogin: TCustomFrmLogin;
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
begin
  raise Exception.Create('Not implemented yet!');
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

