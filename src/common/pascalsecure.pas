{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalsecure;

{$warn 5023 off : no warning about unused units}
interface

uses
  security.exceptions, security.manager.basic_user_management, 
  security.manager.controls_manager, security.manager.custom_user_management, 
  security.manager.custom_usrmgnt_interface, security.manager.schema, 
  security.texts, pascalsecure_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalsecure_reg', @pascalsecure_reg.Register);
end;

initialization
  RegisterPackage('pascalsecure', @Register);
end.
