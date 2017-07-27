{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalsecure_visual;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalsecure_visual_reg, security.manager.graphical_user_management, 
  security.controls.SecureButton, security.controls.SecureBitBtn, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalsecure_visual_reg', @pascalsecure_visual_reg.Register);
end;

initialization
  RegisterPackage('pascalsecure_visual', @Register);
end.
