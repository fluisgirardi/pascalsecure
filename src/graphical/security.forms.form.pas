unit security.forms.form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Controls,
  Classes,
  security.manager.controls_manager,
  security.exceptions;

type

  { TSecureForm }

  TSecureForm = class(TForm, ISecureControlInterface)
  private
    FAllowUnauthorizedShowForm,
    FAllowUnauthorizedShowFormLoaded: Boolean;
    procedure SetAllowUnauthorizedShowForm(AValue: Boolean);
  protected
    FSecurityCode: String;
    FIsEnabled,
    FIsVisible,
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

    //:@excluee
    procedure SetVisible(Value: boolean); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor  Destroy; override;
    function ShowModal: Integer; override;
  published
    //: @exclude
    property Enabled read FIsEnabled write SetEnabled default true;
    {$IFDEF PORTUGUES}
    //: Codigo de segurança do controle. Deixe vazio para desabilitar o sistema de segurança.
    {$ELSE}
    //: Control security code. Empty disable the security manager over the control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;

    property AllowUnauthorizedShowForm:Boolean read FAllowUnauthorizedShowForm write SetAllowUnauthorizedShowForm default true;

    property Visible stored false;
  end;


implementation

{ TSecureForm }

constructor TSecureForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FIsEnabled:=true;
  FAllowUnauthorizedShowForm:=true;
  if ComponentState*[csDesigning]<>[] then begin
    FIsEnabledBySecurity:=true;
    FSecurityCode:='';
  end;

  GetControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureForm.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

function TSecureForm.ShowModal: Integer;
begin
  if GetControlSecurityManager.CanAccess(FSecurityCode)=false then
    raise ESecuritySystemAccessDenied.Create(FSecurityCode);

  Result:=inherited ShowModal;
end;

procedure TSecureForm.SetSecurityCode(AValue: String);
begin
  if [csReading,csLoading]*ComponentState=[] then
    SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface))
  else
    FSecurityCode:=AValue;
end;

function TSecureForm.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureForm.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureForm.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
  if FAllowUnauthorizedShowForm=false then;
    SetVisible(FIsVisible);
  if (FAllowUnauthorizedShowForm=false) and (a=false) and Visible then
    Close;
end;

procedure TSecureForm.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure TSecureForm.Loaded;
var
  AValue: String;
begin
  inherited Loaded;
  SetAllowUnauthorizedShowForm(FAllowUnauthorizedShowFormLoaded);
  AValue:=FSecurityCode;
  FSecurityCode:='';
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

procedure TSecureForm.SetVisible(Value: boolean);
begin
  FIsVisible:=Value;
  inherited SetVisible(FIsVisible and (FIsEnabledBySecurity or FAllowUnauthorizedShowForm));
end;

procedure TSecureForm.SetAllowUnauthorizedShowForm(AValue: Boolean);
begin
  if [csReading,csLoading]*ComponentState<>[] then begin
    FAllowUnauthorizedShowFormLoaded:=AValue;
    exit;
  end;

  if FAllowUnauthorizedShowForm=AValue then Exit;
  FAllowUnauthorizedShowForm:=AValue;

  if Visible and
     (FAllowUnauthorizedShowForm=false) and
     (not GetControlSecurityManager.CanAccess(FSecurityCode)) then
    Close;
end;

end.

