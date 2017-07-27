unit security.manager.schema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  //: Implements a simple authorization code
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

  //: Implements a list of authorization codes
  TAuthorizationList = specialize TFPGMap<Integer, TAuthorization>;

  //: Implements a list with all authorization codes
  TAuthorizations = class(TAuthorizationList)
  public
    function AddAuthorization(aAuthID:Integer; aDescription:UTF8String):TAuthorization;
  end;

  //: Implements a simple user, with UID, Login, Description and enable/disable option.
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

  //: Implements a list of simple users
  TUserList = specialize TFPGMap<Integer, TCustomUser>;

  //: Implements a user with access level.
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

  //: Implements a list of users with level access.
  TUserLevelList = specialize TFPGMap<Integer, TUserWithLevelAccess>;

  //: Implements a user with user allowed authorizations
  TAuthorizedUser = class(TCustomUser)
  protected
    FUserAuthorizations:TAuthorizationList;
  public
    constructor Create(aUID:Integer; aUserLogin, aUserDescription:UTF8String;
      aBlockedUser:Boolean);
    destructor Destroy; override;
    function AuthorizationList:TAuthorizationList;
  end;

  //: Implements a list of users with specific authorizations
  TAuthorizedUserList = specialize TFPGMap<Integer, TAuthorizedUser>;

  //: Implements a simple usergroup with group-authorizations
  TCustomGroup = class(TObject)
  protected
    FGroupID: Integer;
    FGroupName: UTF8String;
    FAuthorizations:TAuthorizationList;
  public
    constructor Create(aGID:Integer; aGroupName:UTF8String); virtual;
    destructor Destroy; override;
    function GroupAuthorizations:TAuthorizationList;
  published
    property GroupID:Integer read FGroupID;
    property GroupName:UTF8String read FGroupName;
  end;

  {:
  Implements a group were users hence the group authorizations, without user
  specific authorizations.
  }
  TSimpleUserGroup = class(TCustomGroup)
  protected
    FUserList:TUserList;
  public
    constructor Create(aGID: Integer; aGroupName: UTF8String); override;
    destructor Destroy; override;
    function UsersList:TUserList;
  end;

  {:
  Implements a group where users inherit the group authorizations, adding it
  with user specific authorizations.
  }
  TUsersGroup = class(TCustomGroup)
  protected
    FUserList:TAuthorizedUserList;
  public
    constructor Create(aGID: Integer; aGroupName: UTF8String); override;
    destructor Destroy; override;
    function UsersList:TAuthorizedUserList;
  end;


  TUsrMgntSchema = class(TObject);

  TUsrLevelMgntSchema = class(TUsrMgntSchema)
  protected
    FMaxLevel: Integer;
    FMinLevel: Integer;
    FUserList:TUserLevelList;
  public
    constructor Create(aMinLevel, aMaxLevel:Integer);
    destructor Destroy; override;
    function UserList:TUserLevelList;
  published
    property MinLevel:Integer read FMinLevel;
    property MaxLevel:Integer read FMaxLevel;
  end;

  TAuthBasedUsrMgntSchema = class(TUsrMgntSchema)
  protected
    FAuthorizations:TAuthorizations;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Autorizations:TAuthorizations;
  end;

implementation

uses security.exceptions;

constructor TAuthBasedUsrMgntSchema.Create;
begin
  inherited Create;
  FAuthorizations:=TAuthorizations.Create;
end;

destructor TAuthBasedUsrMgntSchema.Destroy;
begin
  FreeAndNil(FAuthorizations);
  inherited Destroy;
end;

function TAuthBasedUsrMgntSchema.Autorizations: TAuthorizations;
begin
  Result:=FAuthorizations;
end;

constructor TUsrLevelMgntSchema.Create(aMinLevel, aMaxLevel: Integer);
begin
  inherited Create;
  FUserList:=TUserLevelList.Create;
  if aMinLevel>=aMaxLevel then
    raise EInvalidLevelRanges.Create(aMinLevel,aMaxLevel);
end;

destructor TUsrLevelMgntSchema.Destroy;
begin
  FreeAndNil(FUserList);
  inherited Destroy;
end;

function TUsrLevelMgntSchema.UserList: TUserLevelList;
begin
  Result:=FUserList;
end;

{ TUsersGroup }

constructor TUsersGroup.Create(aGID: Integer; aGroupName: UTF8String);
begin
  inherited Create(aGID, aGroupName);
  FUserList:=TAuthorizedUserList.Create;
end;

destructor TUsersGroup.Destroy;
begin
  FreeAndNil(FUserList);
  inherited Destroy;
end;

function TUsersGroup.UsersList: TAuthorizedUserList;
begin
  Result:=FUserList;
end;

{ TSimpleUsersGroup }

constructor TSimpleUserGroup.Create(aGID: Integer; aGroupName: UTF8String);
begin
  inherited Create(aGID, aGroupName);
  FUserList:=TUserList.Create;
end;

destructor TSimpleUserGroup.Destroy;
begin
  FreeAndNil(FUserList);
  inherited Destroy;
end;

function TSimpleUserGroup.UsersList: TUserList;
begin
  Result:=FUserList;
end;

{ TCustomGroup }

constructor TCustomGroup.Create(aGID: Integer; aGroupName: UTF8String);
begin
  inherited Create;
  FAuthorizations:=TAuthorizationList.Create;
  FGroupID:=aGID;
  FGroupName:=aGroupName;
end;

destructor TCustomGroup.Destroy;
begin
  FreeAndNil(FAuthorizations);
  inherited Destroy;
end;

function TCustomGroup.GroupAuthorizations: TAuthorizationList;
begin
  Result:=FAuthorizations;
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

