unit security.manager.custom_usrmgnt_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.schema;


type
  IUsrMgntIntf = interface
    ['{38D383C4-E162-4CF3-98A3-8EF4141AD681}']
    //: Return the user management type.
    function  UsrMgntType:TUsrMgntType;
    //: Return the user management schema (all users, groups and authorizations, if availables).
    function  GetUserSchema:TUsrMgntSchema;


    {:
    Updates some user detais, like description, user disabled,
    user level access and authorizations.
    }
    function  UpdateUserDetails(const aUser:TCustomUser):Boolean;

    function  AddUserInGroup(const aUser:TCustomUser; const aGroup:TCustomGroup):Boolean;

    //: Changes the user Password. Should be used in user administration interfaces.
    function  ChangeUserPassword(const aUser:TCustomUser; const aPasswd:UTF8String):Boolean;

    //: Changes password of current user.
    function  ChangeCurrentUserPassword(const CurrentPass, NewPass:UTF8String):Boolean;
  end;

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

