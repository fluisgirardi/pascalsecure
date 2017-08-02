{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalsecure_visual;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalsecure_visual_reg, security.actions.authorized_by_user_management, 
  security.actions.login, security.actions.login_logout, 
  security.actions.logout, security.actions.manage, 
  security.actions.request_authorized_user, security.actions.useraction, 
  security.controls.bitbtn, security.controls.SecureButton, 
  security.controls.checkbox, security.controls.checkgroup, 
  security.controls.checklistbox, security.controls.combobox, 
  security.controls.custom_checkbox, security.controls.dbedit, 
  security.controls.dbmemo, security.controls.dbnavigator, 
  security.controls.dbtext, security.controls.edit, 
  security.controls.groupbox, security.controls.image, security.controls.lbl, 
  security.controls.listbox, security.controls.maskedit, 
  security.controls.memo, security.controls.pagecontrol, 
  security.controls.panel, security.controls.progressbar, 
  security.controls.radiobutton, security.controls.radiogroup, 
  security.controls.scrollbar, security.controls.scrollbox, 
  security.controls.speedbutton, security.controls.statictext, 
  security.controls.SecureTabsheet, security.controls.togglebox, 
  security.controls.trackbar, security.controls.updown, security.forms.form, 
  security.frames.frame, security.manager.graphical_user_management, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalsecure_visual_reg', @pascalsecure_visual_reg.Register);
end;

initialization
  RegisterPackage('pascalsecure_visual', @Register);
end.
