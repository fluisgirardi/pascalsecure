{
    This file is part of the Extended Tabsheet
    Copyright (c) 2016 Andreas Frieß.
	
    See the files COPYING.LGPL and COPYING.modifiedLGPL, 
    included in this distribution.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
	
--------------------------------------------------------------------------------
 Contibutors: 
    
 ******************************************************************************}
unit security.controls.tabsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, security.manager.controls_manager;


type

   { TSecureCustomTabSheet }

   TSecureCustomTabSheet = class(TTabSheet,ISecureControlInterface)
   private
   protected
     FSecurityCode: String;
     FIsEnabled,
     FIsEnabledBySecurity:Boolean;
     FSecureHide: Boolean;
     procedure SetSecureHide(AValue: Boolean);
     procedure SetSecurityCode(AValue: String); virtual;
     function GetControlSecurityCode: String;
     procedure MakeUnsecure;
     procedure CanBeAccessed(a: Boolean);
     procedure SetMyVisible(Value: Boolean);
   public
     constructor Create(TheOwner: TComponent); override;
     destructor Destroy; override;
   published
     property SecurityCode:String read FSecurityCode write SetSecurityCode;
     property SecureHideOrEnable:Boolean read FSecureHide write SetSecureHide;
   end;

   TSecureTabSheet = class(TSecureCustomTabSheet)
   end;

   procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon([TSecureTabSheet]);
end;

{ TSecureCustomTabSheet }

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

constructor TSecureCustomTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  FSecureHide:=True;
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomTabSheet.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
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

end.

