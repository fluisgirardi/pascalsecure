unit security.manager.custom_usrmgnt_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.schema;


type
  TCustomUsrMgntInterface = class(TComponent)
  public
    function  Login(out aLogin, aPass:UTF8String):Boolean; virtual; abstract;
    function  Login:Boolean; virtual; abstract;
    function  CanLogout:Boolean; virtual;
    procedure UserManagement(aSchema:TUsrMgntSchema); virtual; abstract;
    procedure FreezeUserLogin; virtual;
    procedure UnfreezeUserLogin; virtual;
    procedure ProcessMessages; virtual;
    function  LoginVisibleBetweenRetries:Boolean; virtual;
  end;

implementation

function TCustomUsrMgntInterface.CanLogout: Boolean;
begin
  Result:=True;
end;

procedure TCustomUsrMgntInterface.FreezeUserLogin;
begin

end;

procedure TCustomUsrMgntInterface.UnfreezeUserLogin;
begin

end;

procedure TCustomUsrMgntInterface.ProcessMessages;
begin

end;

function TCustomUsrMgntInterface.LoginVisibleBetweenRetries: Boolean;
begin
  Result:=false;
end;

end.

