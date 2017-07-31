unit security.actions.login_logout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.actions.authorized_by_user_management,
  security.manager.basic_user_management;

type

  { TpSCADALogin_LogoutAction }

  TLogin_LogoutAction = class(TAuthByUserMgntAction)
  private
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FWithUserLoggedInImageIndex,
    FWithoutUserLoggedInImageIndex:LongInt;
    FWithUserLoggedInCaption,
    FWithoutUserLoggedInCaption,
    FWithUserLoggedInHint,
    FWithoutUserLoggedInHint:String;
    function GetCurrentCaption: String;
    function GetCurrentHintMessage: String;
    function GetCurrentImageIndex: LongInt;
    procedure SetWithUserLoggedInCaption(const AValue: String);
    procedure SetWithUserLoggedInHint(const AValue: String);
    procedure SetWithUserLoggedInImageIndex(const AValue: LongInt);
    procedure SetWithoutUserLoggedInCaption(const AValue: String);
    procedure SetWithoutUserLoggedInHint(const AValue: String);
    procedure SetWithoutUserLoggedInImageIndex(const AValue: LongInt);
    procedure UpdateMyState;
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  published
    property Caption:String read GetCurrentCaption;
    property Hint:String read GetCurrentHintMessage;
    property ImageIndex:LongInt read GetCurrentImageIndex;
    property WithUserLoggedInCaption:String        read FWithUserLoggedInCaption    write SetWithUserLoggedInCaption;
    property WithUserLoggedInHint:String           read FWithUserLoggedInHint       write SetWithUserLoggedInHint;
    property WithUserLoggedInImageIndex:LongInt    read FWithUserLoggedInImageIndex write SetWithUserLoggedInImageIndex;

    property WithoutUserLoggedInCaption:String     read FWithoutUserLoggedInCaption    write SetWithoutUserLoggedInCaption;
    property WithoutUserLoggedInHint:String        read FWithoutUserLoggedInHint       write SetWithoutUserLoggedInHint;
    property WithoutUserLoggedInImageIndex:LongInt read FWithoutUserLoggedInImageIndex write SetWithoutUserLoggedInImageIndex;
    property BeforeLogin:TNotifyEvent              read FBeforeLogin                   write FBeforeLogin;
    property AfterLogin:TNotifyEvent               read FAfterLogin                    write FAfterLogin;
  end;

implementation

{ TLogin_LogoutAction }

function TLogin_LogoutAction.GetCurrentCaption: String;
begin
  Result:=inherited Caption;
end;

function TLogin_LogoutAction.GetCurrentHintMessage: String;
begin
  Result:=inherited Hint;
end;

function TLogin_LogoutAction.GetCurrentImageIndex: LongInt;
begin
  Result:=inherited ImageIndex;
end;

procedure TLogin_LogoutAction.SetWithUserLoggedInCaption(
  const AValue: String);
begin
  if FWithUserLoggedInCaption=AValue then exit;
  FWithUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.SetWithUserLoggedInHint(const AValue: String
  );
begin
  if FWithUserLoggedInHint=AValue then exit;
  FWithUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.SetWithUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithUserLoggedInImageIndex=AValue then exit;
  FWithUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.SetWithoutUserLoggedInCaption(
  const AValue: String);
begin
  if FWithoutUserLoggedInCaption=AValue then exit;
  FWithoutUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.SetWithoutUserLoggedInHint(
  const AValue: String);
begin
  if FWithoutUserLoggedInHint=AValue then exit;
  FWithoutUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.SetWithoutUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithoutUserLoggedInImageIndex=AValue then exit;
  FWithoutUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TLogin_LogoutAction.UpdateMyState;
begin
  if GetControlSecurityManager.UserManagement<>nil then
    if TBasicUserManagement(GetControlSecurityManager.UserManagement).UserLogged then begin
      inherited Caption   :=FWithUserLoggedInCaption;
      inherited Hint      :=FWithUserLoggedInHint;
      inherited ImageIndex:=FWithUserLoggedInImageIndex;
    end else begin
      inherited Caption   :=FWithoutUserLoggedInCaption;
      inherited Hint      :=FWithoutUserLoggedInHint;
      inherited ImageIndex:=FWithoutUserLoggedInImageIndex;
    end;
end;

procedure TLogin_LogoutAction.CanBeAccessed(a: Boolean);
begin
  inherited CanBeAccessed(true); //it can be accessed always.
  UpdateMyState;
end;

constructor TLogin_LogoutAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWithUserLoggedInImageIndex:=-1;
  FWithoutUserLoggedInImageIndex:=-1;
end;

procedure TLogin_LogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TLogin_LogoutAction.ExecuteTarget(Target: TObject);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    if TBasicUserManagement(GetControlSecurityManager.UserManagement).UserLogged then begin
      GetControlSecurityManager.Logout;
    end else begin
      if Assigned(FBeforeLogin) then
        FBeforeLogin(Self);

      GetControlSecurityManager.Login;

      if Assigned(FAfterLogin) then
        FAfterLogin(Self);
    end;
  UpdateMyState;
end;

end.

