unit security.controls.SecureButton;
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
  StdCtrls,
  Classes,
  security.manager.controls_manager;

type

  { TSecureCustomButton }

  TSecureCustomButton = class(TCustomButton, ISecureControlInterface)
  private
    FSecureHide: Boolean;
    procedure SetSecureHide(AValue: Boolean);
  protected
    FSecurityCode: String;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    //: @exclude
    procedure SetSecurityCode(AValue: String); virtual;

    //: @seealso(ISecureControlInterface.GetControlSecurityCode)
    function GetControlSecurityCode:String; virtual;

    //: @seealso(ISecureControlInterface.MakeUnsecure)
    procedure MakeUnsecure; virtual;

    //: @seealso(ISecureControlInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean); virtual;

    ////: @exclude
    //procedure SetEnabled(Value: Boolean); override;
    //: @exclude
    procedure SetMyVisible(Value: Boolean);
    //: @exclude
    property Enabled read FIsEnabled write SetEnabled default true;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança do controle. Deixe vazio para desabilitar o sistema de segurança.
    {$ELSE}
    //: Control security code. Empty disable the security manager over the control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
    //: If set the control is hidden, if not the control is disabled
    property SecureHideOrEnable:Boolean read FSecureHide write SetSecureHide;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSecureButton = class(TSecureCustomButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SecurityCode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property SecureHideOrEnable;
  end;

implementation

{ TSecureCustomButton }

constructor TSecureCustomButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  FSecureHide:=True;
  GetControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomButton.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomButton.SetSecureHide(AValue: Boolean);
begin
  if FSecureHide=AValue then Exit;
  FSecureHide:=AValue;
end;

procedure TSecureCustomButton.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomButton.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomButton.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomButton.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetMyVisible(FIsEnabled);
end;

//procedure TSecureCustomButton.SetEnabled(Value: Boolean);
//begin
//  FIsEnabled:=Value;
//  SetMyVisible(FIsEnabled);
//  //inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
//end;

procedure TSecureCustomButton.SetMyVisible(Value: Boolean);
begin
  FIsEnabled:=Value;
  if FSecureHide then begin
    inherited Visible := FIsEnabled and FIsEnabledBySecurity;
    inherited Enabled:= True;
  end
  else begin
    inherited Visible := True;
    inherited Enabled:= FIsEnabled and FIsEnabledBySecurity;;
  end;
end;

end.

