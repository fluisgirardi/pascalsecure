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

implementation

{ ESecuritySystemAccessDenied }

constructor ESecuritySystemAccessDenied.Create(Token: UTF8String);
begin
  if GetPascalSCADAControlSecurityManager.HasUserLoggedIn then
    inherited Create(Format(SUserHasNotAllowedToAccessObject,[GetPascalSCADAControlSecurityManager.GetCurrentUserlogin, Token]))
  else
    inherited Create(Format(SNoUserLoggedInToAccessObject,[Token]));
end;

end.

