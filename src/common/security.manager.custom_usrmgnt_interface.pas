unit security.manager.custom_usrmgnt_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.schema,
  security.manager.controls_manager;


type
  TCustomUsrMgntInterface = class(TComponent)
  public
    function  Login(out aLogin, aPass:UTF8String):Boolean; virtual; abstract;
    procedure Logout; virtual;
    procedure UserManagement(aSchema:TUsrMgntSchema); virtual; abstract;
  end;

implementation

procedure TCustomUsrMgntInterface.Logout;
begin
  GetControlSecurityManager.Logout;
end;

end.

