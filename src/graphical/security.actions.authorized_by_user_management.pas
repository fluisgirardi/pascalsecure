unit security.actions.authorized_by_user_management;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, SysUtils,
  security.manager.controls_manager;

type

  { TAuthByUserMgntAction }

  TAuthByUserMgntAction = class(TCustomAction, ISecureControlInterface)
  private
    FDisableIfNotAuthorized: Boolean;
    procedure SetDisableIfNotAuthorized(AValue: Boolean);
  protected
    FEnabled,
    FAccessAllowed:Boolean;
    FSecurityCode:String;

    procedure SetEnabled(AValue: Boolean); virtual;

    function  GetControlSecurityCode:String; virtual;
    procedure MakeUnsecure; virtual;
    procedure CanBeAccessed(a:Boolean); virtual;
    property DisableIfNotAuthorized:Boolean read FDisableIfNotAuthorized write SetDisableIfNotAuthorized default false;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property DisableIfNoHandler default False;
    property Enabled:Boolean read FEnabled write SetEnabled default true;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property OnExecute;
    property OnHint;
    property OnUpdate;
    property SecondaryShortCuts;
    property ShortCut;
    property Visible;
  end;

implementation

{ TAuthByUserMgntAction }

constructor TAuthByUserMgntAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled:=true;
  FDisableIfNotAuthorized:=false;
  DisableIfNoHandler:=False;
  GetControlSecurityManager.RegisterControl(Self);
end;

destructor TAuthByUserMgntAction.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self);
  inherited Destroy;
end;

procedure TAuthByUserMgntAction.SetDisableIfNotAuthorized(
  AValue: Boolean);
begin
  if FDisableIfNotAuthorized=AValue then Exit;
  FDisableIfNotAuthorized:=AValue;
  CanBeAccessed(GetControlSecurityManager.CanAccess(FSecurityCode));
end;

procedure TAuthByUserMgntAction.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

function TAuthByUserMgntAction.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TAuthByUserMgntAction.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TAuthByUserMgntAction.CanBeAccessed(a: Boolean);
begin
  FAccessAllowed:=a or (FDisableIfNotAuthorized=false);
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

function TAuthByUserMgntAction.HandlesTarget(Target: TObject
  ): Boolean;
begin
  inherited HandlesTarget(Target);
  Result:=true;
end;

end.

