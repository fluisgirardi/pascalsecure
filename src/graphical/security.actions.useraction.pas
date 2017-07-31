unit security.actions.useraction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.actions.authorized_by_user_management;

type

  { TSecureAction }

  TSecureAction = class(TAuthByUserMgntAction)
  protected
    procedure SetSecurityCode(sc:String);
  public
    procedure UpdateTarget(Target: TObject); override;
    function Execute: Boolean; override;
  published
    property DisableIfNotAuthorized;
    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses security.texts;

{ TSecureAction }

procedure TSecureAction.SetSecurityCode(sc: String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

procedure TSecureAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(FAccessAllowed);
  inherited UpdateTarget(Target);
end;

function TSecureAction.Execute: Boolean;
begin
  if GetControlSecurityManager.CanAccess(FSecurityCode) then
    Result:=inherited Execute
  else begin
    Result:=false;
    raise exception.Create(SAccessDenied);
  end;
end;

end.

