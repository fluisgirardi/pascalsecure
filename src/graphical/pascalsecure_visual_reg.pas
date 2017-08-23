unit pascalsecure_visual_reg;
{
    This file is part of the PascalSECURE Project 2017
    Copyright (c) Fabio Luis Girardi

    See the files COPYING.LGPL and COPYING.modifiedLGPL,
    included in this distribution.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    It is a standalone part of PascalSCADA 1.0
       - A multiplatform SCADA framework for Lazarus.
--------------------------------------------------------------------------------
 Contibutors:

 ******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ActnList,
  FormEditingIntf,
  ProjectIntf,
  LazIDEIntf,
  security.actions.authorized_by_user_management,
  security.actions.login,
  security.actions.login_logout,
  security.actions.logout,
  security.actions.manage,
  security.actions.request_authorized_user,
  security.actions.useraction,
  security.controls.bitbtn,
  security.controls.SecureButton,
  security.controls.checkbox,
  security.controls.checkgroup,
  security.controls.checklistbox,
  security.controls.combobox,
  security.controls.dbedit,
  security.controls.dbmemo,
  security.controls.dbnavigator,
  security.controls.dbtext,
  security.controls.edit,
  security.controls.groupbox,
  security.controls.image,
  security.controls.lbl,
  security.controls.listbox,
  security.controls.maskedit,
  security.controls.memo,
  security.controls.pagecontrol,
  security.controls.panel,
  security.controls.progressbar,
  security.controls.radiobutton,
  security.controls.radiogroup,
  security.controls.scrollbar,
  security.controls.scrollbox,
  security.controls.speedbutton,
  security.controls.statictext,
  security.controls.togglebox,
  security.controls.trackbar,
  security.controls.updown,
  security.forms.form,
  security.frames.frame,
  security.manager.custom_user_management,
  security.manager.graphical_user_management;

type

  TSecureFormFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;

  { TpSCADASecureFrameFileDescriptor }

  TSecureFrameFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;

ResourceString
  SPascalSecurePalette = 'PascalSecure';
  SPascalSecureStdPalette = 'PascalSecure Std. Controls';
  SPascalSecureDBPalette = 'PascalSecure DB Controls';
  SPascalSecureActions = 'PascalSecure Actions';
  SPascalSecure_Form = 'PascalSecure Form';
  SPascalSecure_Form_Desc = 'Create a new unit with a LCL form, with security features.';
  SPascalSecure_Frame = 'PascalSecure Frame';
  SPascalSecure_Frame_Desc = 'Create a new unit with a frame, with security features.';

procedure Register;

implementation

{$R graphical.res}

procedure Register;
begin
  RegisterActions(SPascalSecureActions,[TRequestAuthorizedUserAction,
                                        TLoginAction,
                                        TLogoutAction,
                                        TLogin_LogoutAction,
                                        TManageUsersAndGroupsAction,
                                        TSecureAction],nil);

  RegisterComponents(SPascalSecurePalette,[TUserCustomizedUserManagement,
                                           TGraphicalUsrMgntInterface]);


  RegisterComponents(SPascalSecureStdPalette, [TSecureButton,
                                               TSecureBitBtn,
                                               TSecurePageControl,
                                               TSecureCheckBox,
                                               TSecureCheckGroup,
                                               TSecureCheckListBox,
                                               TSecureComboBox,
                                               TSecureEdit,
                                               TSecureGroupBox,
                                               TSecureImage,
                                               TSecureLabel,
                                               TSecureListBox,
                                               TSecureMaskEdit,
                                               TSecureMemo,
                                               TSecurePanel,
                                               TSecureProgressBar,
                                               TSecureRadioButton,
                                               TSecureRadioGroup,
                                               TSecureScrollBar,
                                               TSecureScrollbox,
                                               TSecureToggleBox,
                                               TSecureTrackBar,
                                               TSecureUpDown]);

  RegisterComponents(SPascalSecureDBPalette, [TSecureDBEdit,
                                              TSecureDBMemo,
                                              TSecureDBNavigator,
                                              TSecureDBText]);



  RegisterProjectFileDescriptor(TSecureFormFileDescriptor.Create);
  RegisterProjectFileDescriptor(TSecureFrameFileDescriptor.Create);
  FormEditingHook.RegisterDesignerBaseClass(TSecureForm);
  FormEditingHook.RegisterDesignerBaseClass(TSecureFrame);


end;


{ TSecureFrameFileDescriptor }

constructor TSecureFrameFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TSecureFrame;
  Name:=SPascalSecure_Frame;
  UseCreateFormStatements:=true;
  RequiredPackages:=RequiredPackages+'LCL;pascalsecure_visual';
end;

function TSecureFrameFileDescriptor.GetLocalizedName: String;
begin
  Result:=SPascalSecure_Frame;
end;

function TSecureFrameFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SPascalSecure_Frame_Desc;
end;

function TSecureFrameFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', security.frames.frame';
end;

{ TPascalSCADASecureFormFileDescriptor }

constructor TSecureFormFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TSecureForm;
  Name:=SPascalSecure_Form;
  UseCreateFormStatements:=true;
  RequiredPackages:=RequiredPackages+'LCL;pascalsecure_visual';
end;

function TSecureFormFileDescriptor.GetLocalizedName: String;
begin
  Result:=SPascalSecure_Form;
end;

function TSecureFormFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SPascalSecure_Form_Desc;
end;

function TSecureFormFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', security.forms.form';
end;

end.



