unit security.frames.frame;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Controls,
  Classes,
  security.manager.controls_manager;

type

  TSecureFrame = class(TFrame, ISecureControlInterface)
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

    //: @exclude
    procedure SetEnabled(Value: Boolean); override;

    //: @exclude
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    //: @exclude
    property Enabled read FIsEnabled write SetEnabled default true;
    {$IFDEF PORTUGUES}
    //: Codigo de segurança do controle. Deixe vazio para desabilitar o sistema de segurança.
    {$ELSE}
    //: Control security code. Empty disable the security manager over the control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  end;


implementation

{ TSecureFrame }

constructor TSecureFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  if ComponentState*[csDesigning]<>[] then begin
    FIsEnabledBySecurity:=true;
    FSecurityCode:='';
  end;

  GetControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureFrame.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureFrame.SetSecurityCode(AValue: String);
begin
  if [csReading,csLoading]*ComponentState=[] then
    SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface))
  else
    FSecurityCode:=AValue;
end;

function TSecureFrame.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureFrame.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureFrame.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureFrame.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure TSecureFrame.Loaded;
var
  AValue: String;
begin
  inherited Loaded;
  AValue:=FSecurityCode;
  FSecurityCode:='';
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

end.

