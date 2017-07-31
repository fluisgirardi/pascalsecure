unit security.actions.logout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.actions.authorized_by_user_management,
  security.manager.basic_user_management;

type

  { TLogoutAction }

  TLogoutAction = class(TAuthByUserMgntAction)
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  end;

implementation

{ TLogoutAction }

procedure TLogoutAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(not UserLogged);
end;

procedure TLogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TLogoutAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Logout;
end;

end.
