unit security.actions.login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.manager.basic_user_management,
  security.actions.authorized_by_user_management;

type

  { TpSCADALoginAction }

  TLoginAction = class(TAuthByUserMgntAction)
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  end;

implementation

{ TLoginAction }

procedure TLoginAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(not UserLogged);
end;

procedure TLoginAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TLoginAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Login;
end;

end.

