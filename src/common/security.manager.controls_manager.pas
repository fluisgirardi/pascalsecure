unit security.manager.controls_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  security.manager.basic_user_management;

type

  {$IFDEF PORTUGUES}
  //: @name define a interface entre o gerente de segurança e os controles seguros.
  {$ELSE}
  //: @name defines a interface between the security manager and secure controls.
  {$ENDIF}
  ISecureControlInterface = interface
    ['{A950009B-A2E7-4ED8-BDB3-B6E191D184FB}']

    {$IFDEF PORTUGUES}
    //: Retorna o código de acesso do controle.
    {$ELSE}
    //: Gets the access code of the control.
    {$ENDIF}
    function GetControlSecurityCode:String;

    {$IFDEF PORTUGUES}
    //: Remove o codigo de segurança do controle, tornando-o inseguro.
    {$ELSE}
    //: Clear the security of the control, making it unsecure.
    {$ENDIF}
    procedure MakeUnsecure;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o controle pelas suas permissões. @seealso(Enabled)
    {$ELSE}
    //: Enables/disables the control. @seealso(Enabled)
    {$ENDIF}
    procedure CanBeAccessed(a:Boolean);
  end;

  TFPGSecureControlsList = specialize TFPGList<ISecureControlInterface>;

  { TpSCADAControlSecurityManager }

  { TControlSecurityManager }

  TControlSecurityManager = class(TComponent)
  private
    FSecureControls:TFPGSecureControlsList;
    FUserManagement:TBasicUserManagement;
    procedure SetUserManagement(um:TBasicUserManagement);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   Login(Userlogin, Userpassword: String; var UID: Integer):Boolean; overload;
    function   Login:Boolean;

    procedure  Logout;
    procedure  Manage;
    function   GetCurrentUserlogin:String;
    function   HasUserLoggedIn:Boolean;
    procedure  TryAccess(sc:String);
    procedure  RegisterControl(control:ISecureControlInterface);
    procedure  UnRegisterControl(control:ISecureControlInterface);
    procedure  UpdateControls;
    function   CanAccess(sc:String):Boolean;
    procedure  ValidateSecurityCode(sc:String);
    procedure  RegisterSecurityCode(sc:String);
    procedure  UnregisterSecurityCode(sc:String);
    function   SecurityCodeExists(sc:String):Boolean;
    function   GetRegisteredAccessCodes:TFPGStringList;
    function   CheckIfUserIsAllowed(sc:String; RequireUserLogin:Boolean; var userlogin:String):Boolean;
  published
    property UserManagement:TBasicUserManagement read FUserManagement write SetUserManagement;
  end;

  function GetControlSecurityManager:TControlSecurityManager;
  procedure SetControlSecurityCode(var CurrentSecurityCode:String; const NewSecurityCode:String; ControlSecurityIntf:ISecureControlInterface);

implementation

uses security.exceptions;

constructor TControlSecurityManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserManagement:=nil;
  FSecureControls:=TFPGSecureControlsList.Create;
end;

destructor TControlSecurityManager.Destroy;
begin
  if FSecureControls.Count>0 then
    raise EControlSecurityManagerStillBeingUsed.Create;
  FreeAndNil(FSecureControls);
  inherited Destroy;
end;

function TControlSecurityManager.Login(Userlogin, Userpassword: String; var UID:Integer): Boolean; overload;
begin
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).Login(Userlogin,Userpassword,UID)
  else
    Result:=false;
end;

function   TControlSecurityManager.Login:Boolean;
begin
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).Login
  else
    Result:=false;
end;

procedure  TControlSecurityManager.Logout;
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).Logout
end;

procedure  TControlSecurityManager.Manage;
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).Manage;
end;

function TControlSecurityManager.GetCurrentUserlogin: String;
begin
  Result:='';
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).CurrentUserLogin;
end;

function TControlSecurityManager.HasUserLoggedIn: Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).UserLogged;
end;

procedure  TControlSecurityManager.TryAccess(sc:String);
begin
  if FUserManagement<>nil then
    if not TBasicUserManagement(FUserManagement).CanAccess(sc) then
      raise ESecuritySystemAccessDenied.Create(sc);
end;

procedure TControlSecurityManager.SetUserManagement(um: TBasicUserManagement);
begin
  if (um<>nil) and (not (um is TBasicUserManagement)) then
    raise EInvalidUserManagementComponent.Create;

  if (um<>nil) and (FUserManagement<>nil) then
    raise EUserManagementIsSet.Create;

  FUserManagement:=um;
  UpdateControls;
end;

procedure  TControlSecurityManager.RegisterControl(control:ISecureControlInterface);
begin
  if FSecureControls.IndexOf(control)=-1 then begin;
    FSecureControls.Add(control);
    control.CanBeAccessed(CanAccess(control.GetControlSecurityCode));
  end;
end;

procedure  TControlSecurityManager.UnRegisterControl(control:ISecureControlInterface);
var
  idx:LongInt;
begin
  idx:=FSecureControls.IndexOf(control);
  if idx<>-1 then
    FSecureControls.Delete(idx);
end;

procedure  TControlSecurityManager.UpdateControls;
var
  c:LongInt;
  intf: ISecureControlInterface;
begin
  for c:=0 to FSecureControls.Count-1 do begin
    intf:=ISecureControlInterface(FSecureControls.Items[c]);
    intf.CanBeAccessed(CanAccess(intf.GetControlSecurityCode));
  end;
end;

function   TControlSecurityManager.CanAccess(sc:String):Boolean;
begin
  Result:=true;

  if sc='' then exit;

  if (FUserManagement<>nil) and (FUserManagement is TBasicUserManagement) then
    Result:=TBasicUserManagement(FUserManagement).CanAccess(sc);
end;

procedure  TControlSecurityManager.ValidateSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).ValidateSecurityCode(sc);
end;

procedure  TControlSecurityManager.RegisterSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).RegisterSecurityCode(sc);
end;

procedure  TControlSecurityManager.UnregisterSecurityCode(sc:String);
var
  being_used:Boolean;
  c:LongInt;
begin
  being_used:=false;
  for c:=0 to FSecureControls.Count-1 do
    being_used:=being_used or (ISecureControlInterface(FSecureControls.Items[c]).GetControlSecurityCode=sc);

  if being_used then
    raise ESecurityCodeIsInUseYet.Create;

  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).UnregisterSecurityCode(sc);
end;

function   TControlSecurityManager.SecurityCodeExists(sc:String):Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).SecurityCodeExists(sc);
end;

function TControlSecurityManager.GetRegisteredAccessCodes: TFPGStringList;
begin
  if FUserManagement=nil then begin
    Result:=TFPGStringList.Create
  end else
    Result:=TBasicUserManagement(FUserManagement).GetRegisteredAccessCodes;
end;

function TControlSecurityManager.CheckIfUserIsAllowed(sc: String;
  RequireUserLogin: Boolean; var userlogin: String): Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).CheckIfUserIsAllowed(sc, RequireUserLogin, userlogin);
end;

var
  QControlSecurityManager:TControlSecurityManager;

function GetControlSecurityManager: TControlSecurityManager;
begin
  Result:=QControlSecurityManager;
end;

procedure SetControlSecurityCode(var CurrentSecurityCode: String;
  const NewSecurityCode: String; ControlSecurityIntf: ISecureControlInterface);
begin
  if CurrentSecurityCode=NewSecurityCode then Exit;

  if Trim(NewSecurityCode)='' then
    ControlSecurityIntf.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(NewSecurityCode);
      if not SecurityCodeExists(NewSecurityCode) then
        RegisterSecurityCode(NewSecurityCode);

      ControlSecurityIntf.CanBeAccessed(CanAccess(NewSecurityCode));
    end;

  CurrentSecurityCode:=NewSecurityCode;
end;

initialization
  QControlSecurityManager:=TControlSecurityManager.Create(nil);
finalization
  FreeAndNil(QControlSecurityManager);

end.

