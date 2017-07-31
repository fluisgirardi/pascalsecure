unit security.actions.request_authorized_user;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, SysUtils,
  security.manager.controls_manager;

type

  { TRequestAuthorizedUserAction }

  TRequestAuthorizedUserAction = class(TAction)
  private
    FAuthorizedBy: String;
    FRequireLoginAlways: Boolean;
    FSecurityCode: String;
    procedure SetSecurityCode(AValue: String);
  public
    function Execute: Boolean; override;
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property AuthorizedBy:String read FAuthorizedBy;
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
    property RequireLoginAlways:Boolean read FRequireLoginAlways write FRequireLoginAlways;
  end;

implementation

{ TRequestAuthorizedUserAction }

procedure TRequestAuthorizedUserAction.SetSecurityCode(AValue: String);
begin
  if FSecurityCode=AValue then Exit;

  if Trim(AValue)<>'' then
    with GetControlSecurityManager do begin
      ValidateSecurityCode(AValue);
      if not SecurityCodeExists(AValue) then
        RegisterSecurityCode(AValue);
    end;

  FSecurityCode:=AValue;
end;

function TRequestAuthorizedUserAction.Execute: Boolean;
begin
  if GetControlSecurityManager.CheckIfUserIsAllowed(FSecurityCode, FRequireLoginAlways, FAuthorizedBy) then
    Result:=inherited Execute
  else
    Result:=false;
end;

function TRequestAuthorizedUserAction.HandlesTarget(Target: TObject): Boolean;
begin
  inherited HandlesTarget(Target);
  Result:=true;
end;

end.

