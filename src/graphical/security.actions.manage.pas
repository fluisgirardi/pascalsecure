unit security.actions.manage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.actions.authorized_by_user_management,
  security.manager.basic_user_management;

type

  { TManageUsersAndGroupsAction }

  TManageUsersAndGroupsAction = class(TAuthByUserMgntAction)
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  end;

implementation

{ TManageUsersAndGroupsAction }

procedure TManageUsersAndGroupsAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(UserLogged);
end;

procedure TManageUsersAndGroupsAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TManageUsersAndGroupsAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Manage;
end;

end.
