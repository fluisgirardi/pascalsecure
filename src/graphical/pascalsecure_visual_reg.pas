unit pascalsecure_visual_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FormEditingIntf,
  ProjectIntf,
  LazIDEIntf,
  security.controls.SecureBitBtn,
  security.controls.SecureButton;


procedure Register;

implementation

{$R pascalsecure_visual.res}

procedure Register;
begin
  RegisterComponents('PascalSecure_Controls', [TSecureButton,
                                             TSecureBitBtn]);

end;

end.

