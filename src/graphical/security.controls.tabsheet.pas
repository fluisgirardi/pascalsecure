unit security.controls.tabsheet;
{
    This file is part of the PascalSECURE Project 2017
    Copyright (c) Fabio Luis Girardi
      Parts: (c) Andreas Frieß

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
  Classes, SysUtils, ComCtrls,
  security.manager.controls_manager;


type

   { TSecureCustomTabSheet }

   TSecureCustomTabSheet = class(TTabSheet,ISecureControlInterface)
   private
     FSecurityCode: String;
     FIsEnabled,
     FIsEnabledBySecurity:Boolean;
     FSecureHide: Boolean;
   protected
     procedure SetSecureHide(AValue: Boolean);
     //: @exclude
     procedure SetSecurityCode(AValue: String); virtual;
     //: @seealso(ISecureControlInterface.GetControlSecurityCode)
     function GetControlSecurityCode: String;
     //: @seealso(ISecureControlInterface.MakeUnsecure)
     procedure MakeUnsecure;
     //: @seealso(ISecureControlInterface.CanBeAccessed)
     procedure CanBeAccessed(a: Boolean);
     //: @exclude
     procedure SetMyVisible(Value: Boolean);
     //: Control security code. Empty disable the security manager over the control.
     property SecurityCode:String read FSecurityCode write SetSecurityCode;
     //: If set the control is hidden, if not the control is disabled
     property SecureHideOrEnable:Boolean read FSecureHide write SetSecureHide;
   public
     constructor Create(TheOwner: TComponent); override;
     destructor Destroy; override;
   end;

   TSecureTabSheet = class(TSecureCustomTabSheet)
   published
     property SecurityCode;
     property SecureHideOrEnable;
   end;

   procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon([TSecureTabSheet]);
end;

{ TSecureCustomTabSheet }

constructor TSecureCustomTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  FSecureHide:=True;
  GetControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomTabSheet.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomTabSheet.SetSecureHide(AValue: Boolean);
begin
  if FSecureHide=AValue then Exit;
  FSecureHide:=AValue;
end;

procedure TSecureCustomTabSheet.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomTabSheet.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomTabSheet.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomTabSheet.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetMyVisible(FIsEnabled);
end;

procedure TSecureCustomTabSheet.SetMyVisible(Value: Boolean);
begin
  FIsEnabled:=Value;
  if FSecureHide then begin
    TabVisible := FIsEnabled and FIsEnabledBySecurity;
    self.Enabled:= True;
  end
  else begin
    TabVisible := True;
    self.Enabled:= FIsEnabled and FIsEnabledBySecurity;;
  end;
end;


end.

