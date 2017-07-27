unit security.exceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  security.manager.controls_manager,
  security.texts;

type

  { ESecuritySystemAccessDenied }

  ESecuritySystemAccessDenied = class(Exception)
  public
    constructor Create(Token:UTF8String);
  end;

  { ESecurityMangerAlreadySet }

  ESecurityMangerAlreadySet = class(Exception)
  public
    constructor Create;
  end;

  { ESecurityManagerStillBeingUsed }

  EControlSecurityManagerStillBeingUsed = class(Exception)
  public
    constructor Create;
  end;

  { EInvalidUserManagementComponent }

  EInvalidUserManagementComponent = class(Exception)
  public
    constructor Create;
  end;

  { EUserManagementIsSet }

  EUserManagementIsSet = class(Exception)
  public
    constructor Create;
  end;

  { ESecurityCodeIsInUseYet }

  ESecurityCodeIsInUseYet = class(Exception)
  public
    constructor Create;
  end;

  EInvalidLevelRanges = class(Exception)
  public
    constructor Create(aMinLevel, aMaxLevel:Integer);
  end;

implementation

constructor EInvalidLevelRanges.Create(aMinLevel, aMaxLevel: Integer);
begin
  inherited Create(Format(SInvalidUserSchemaLevels,[aMinLevel,aMaxLevel]));
end;

{ ESecurityCodeIsInUseYet }

constructor ESecurityCodeIsInUseYet.Create;
begin
  inherited Create(SSecurityCodeIsInUseYet);
end;

{ EUserManagementIsSet }

constructor EUserManagementIsSet.Create;
begin
  inherited Create(SUserManagementIsSet);
end;

{ EInvalidUserManagementComponent }

constructor EInvalidUserManagementComponent.Create;
begin
  inherited Create(SInvalidUserManagementComponent);
end;

{ ESecurityManagerStillBeingUsed }

constructor EControlSecurityManagerStillBeingUsed.Create;
begin
  inherited Create(SControlSecurityManagerStillBeingUsed);
end;

{ ESecurityMangerAlreadySet }

constructor ESecurityMangerAlreadySet.Create;
begin
  inherited Create(SUserManagementIsSet);
end;

{ ESecuritySystemAccessDenied }

constructor ESecuritySystemAccessDenied.Create(Token: UTF8String);
begin
  if GetControlSecurityManager.HasUserLoggedIn then
    inherited Create(Format(SUserHasNotAllowedToAccessObject,[GetControlSecurityManager.GetCurrentUserlogin, Token]))
  else
    inherited Create(Format(SNoUserLoggedInToAccessObject,[Token]));
end;

end.

