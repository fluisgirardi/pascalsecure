unit security.manager.schema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TAuthorization }

  TAuthorization = class(TObject)
  protected
    FAuthID: Integer;
    FDescription: UTF8String;
  public
    constructor Create(aAuthID:Integer; aDescription:UTF8String);
  published
    property AuthID:Integer read FAuthID;
    property Description:UTF8String read FDescription;
  end;

  TAuthorizationList = specialize TFPGMap<Integer, TAuthorization>;

  { TAuthorizationList }

  TAuthorizations = class(TAuthorizationList)
  public
    function AddAuthorization(aAuthID:Integer; aDescription:UTF8String):TAuthorization;
  end;

  { TCustomUser }

  TCustomUser = class(TObject)
  protected
    //immutable fields.
    FUID:Integer;
    FUserLogin:UTF8String;

    FUserDescription,
    FOldUserDescription:UTF8String;

    FBlockedUser,
    FOldUserState:Boolean;

    procedure SetUserDescription(AValue: UTF8String); virtual;
    procedure SetBlockedUser(AValue: Boolean); virtual;
  public
    constructor Create(aUID:Integer; aUserLogin, aUserDescription:UTF8String;
      aBlockedUser:Boolean);
    function Modified:Boolean; virtual;
    procedure ResetModified; virtual;
  published
    property UID:integer read FUID;
    property Login:UTF8String read FUserLogin;
    property UserDescription:UTF8String read FUserDescription write SetUserDescription;
    property UserBlocked:Boolean read FBlockedUser write SetBlockedUser;
  end;

  TUserList = specialize TFPGMap<Integer, TCustomUser>;

  { TUserWithLevelAccess }

  TUserWithLevelAccess = class(TCustomUser)
  protected
    FUserLevel,
    FOldUserLevel: Integer;
    procedure SetUserLevel(AValue: Integer);
  public
    constructor Create(aUID:Integer; aUserLogin, aUserDescription:UTF8String;
      aBlockedUser:Boolean; aUserLevel:Integer);
    function Modified:Boolean; override;
    procedure ResetModified; override;
  published
    property UserLevel:Integer read FUserLevel write SetUserLevel;
  end;

  TUserLevelList = specialize TFPGMap<Integer, TUserWithLevelAccess>;

  { TAuthorizedUser }

  TAuthorizedUser = class(TCustomUser)
  protected
    FUserAuthorizations:TAuthorizationList;
  public
    constructor Create(aUID:Integer; aUserLogin, aUserDescription:UTF8String;
      aBlockedUser:Boolean);
    destructor Destroy; override;
    function AuthorizationList:TAuthorizationList;
  end;

  TAuthorizedUserList = specialize TFPGMap<Integer, TAuthorizedUser>;

  { TCustomGroup }

  TCustomGroup = class(TObject)
  protected
    FGroupID: Integer;
    FGroupName: UTF8String;
  public
    constructor Create(aGID:Integer; aGroupName:UTF8String);
  published
    property GroupID:Integer read FGroupID;
    property GroupName:UTF8String read FGroupName;
  end;



implementation

{ TCustomGroup }

constructor TCustomGroup.Create(aGID: Integer; aGroupName: UTF8String);
begin
  inherited Create;
  FGroupID:=aGID;
  FGroupName:=aGroupName;
end;

{ TAuthorizedUser }

constructor TAuthorizedUser.Create(aUID: Integer; aUserLogin,
  aUserDescription: UTF8String; aBlockedUser: Boolean);
begin
  inherited Create(aUID,aUserLogin,aUserDescription,aBlockedUser);
  FUserAuthorizations:=TAuthorizationList.Create;
end;

destructor TAuthorizedUser.Destroy;
begin
  FreeAndNil(FUserAuthorizations);
  inherited Destroy;
end;

function TAuthorizedUser.AuthorizationList: TAuthorizationList;
begin
  Result:=FUserAuthorizations;
end;

{ TAuthorizationList }

function TAuthorizations.AddAuthorization(aAuthID: Integer;
  aDescription: UTF8String): TAuthorization;
begin
  Result:=TAuthorization.Create(aAuthID, aDescription);
  Add(aAuthID, Result);
end;

{ TAuthorization }

constructor TAuthorization.Create(aAuthID: Integer; aDescription: UTF8String);
begin
  inherited Create;
  FAuthID:=aAuthID;
  FDescription:=aDescription;
end;

{ TUserWithLevelAccess }

procedure TUserWithLevelAccess.SetUserLevel(AValue: Integer);
begin
  if FUserLevel=AValue then Exit;
  FUserLevel:=AValue;
end;

constructor TUserWithLevelAccess.Create(aUID: Integer; aUserLogin,
  aUserDescription: UTF8String; aBlockedUser: Boolean; aUserLevel: Integer);
begin
  inherited Create(aUID, aUserLogin, aUserDescription, aBlockedUser);
  FUserLevel:=aUserLevel;
  FOldUserLevel:=aUserLevel;
end;

function TUserWithLevelAccess.Modified: Boolean;
begin
  result := inherited Modified or (FUserLevel<>FOldUserLevel)
end;

procedure TUserWithLevelAccess.ResetModified;
begin
  FOldUserLevel := FUserLevel;
end;

{ TCustomUser }

procedure TCustomUser.SetBlockedUser(AValue: Boolean);
begin
  if FBlockedUser=AValue then Exit;
  FOldUserState:=FBlockedUser;
  FBlockedUser:=AValue;
end;

procedure TCustomUser.SetUserDescription(AValue: UTF8String);
begin
  if FUserDescription=AValue then Exit;
  FOldUserDescription:=FUserDescription;
  FUserDescription:=AValue;
end;

constructor TCustomUser.Create(aUID: Integer; aUserLogin,
  aUserDescription: UTF8String; aBlockedUser: Boolean);
begin
  inherited Create;
  FUID:=aUID;
  FUserLogin:=aUserLogin;

  FUserDescription    := aUserDescription;
  FOldUserDescription := aUserDescription;

  FBlockedUser  := aBlockedUser;
  FOldUserState := aBlockedUser;
end;

function TCustomUser.Modified: Boolean;
begin
  Result := (FUserDescription<>FOldUserDescription) or (FBlockedUser<>FOldUserState);
end;

procedure TCustomUser.ResetModified;
begin
  FOldUserDescription:=FUserDescription;
  FOldUserState:=FBlockedUser;
end;

end.

