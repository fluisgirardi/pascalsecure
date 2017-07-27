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

  TSimpleUserGroupList = specialize TFPGMap<Integer, TSimpleUserGroup>;

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

  TUsrGroupList = specialize TFPGMap<Integer, TUsersGroup>;

  //: Implements the entire user management schema.
  TUsrMgntSchema = class(TObject);

  {:
  Implements a user level based schema.

  This schema consists in:
  ** A user list where each user has a access leve.
  ** Level limits (Min and Max).
  ** The level that makes a user admin.

  This schema is similar to the security system used in Elipse SCADA
  and in Wonderware Intouch.
  }
  TUsrLevelMgntSchema = class(TUsrMgntSchema)
  protected
    FMaxLevel: Integer;
    FMinLevel: Integer;
    FAdminLevel: Integer;
    FUserList:TUserLevelList;
  public
    constructor Create(aMinLevel, aMaxLevel, aAdminLevel:Integer);
    destructor Destroy; override;
    function UserList:TUserLevelList;
  published
    property AdminLevel:Integer read FAdminLevel;
    property MinLevel:Integer read FMinLevel;
    property MaxLevel:Integer read FMaxLevel;
  end;

  {:
  Implements a user management that uses authorizations to allow/deny access.
  This serve as base for TUsrAuthSchema, TGroupAuthSchema and
  TUsrGroupAuthSchema.
  }
  TAuthBasedUsrMgntSchema = class(TUsrMgntSchema)
  protected
    FAuthorizations:TAuthorizations;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Autorizations:TAuthorizations;
  end;

  {:
  Implements a user management that uses authorizations to allow/deny access,
  where each user has a list of allowed authorizations.

  This schema consists in:
  ** A user list where each user has a list of allowed authorizations.
  ** A list with all authorizations available on the security manager.

  This schema is similar to the securty system used in Siemens WinCC.
  }
  TUsrAuthSchema = class(TAuthBasedUsrMgntSchema)
  protected
    FUserList:TAuthorizedUserList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function UserList:TAuthorizedUserList;
  end;

  {:
  Implements a user management that uses authorizations to allow/deny access,
  where each user has a list of allowed authorizations and inherits the
  authorizations assigned to the group which it belongs.

  This schema consists in:
  ** A list of users, where each user has a list of specific allowed authorizations.
  ** A list of groups, where each group has a list of allowed authorizations,
     where the users of each group will inherit the group authorizations.
  ** A list with all authorizations available on the security manager.

  This schema is similar to the securty system used in Rockwell FactoryTalk.
  }
  TUsrGroupAuthSchema = class(TUsrAuthSchema)
  protected
    FGroupList:TUsrGroupList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GroupList:TUsrGroupList;
  end;

  {:
  Implements a user management that uses authorizations to allow/deny access,
  where each user inherits the authorizations assigned to the group which it
  belongs. On this model, users CAN NOT HAVE SPECIFIC PERMISSIONS.

  This schema consists in:
  ** A list of simple users.
  ** A list of groups, where each group has a list of allowed authorizations,
     where the users of each group will inherit the authorizations assigned to
     the group.
  ** A list with all authorizations available on the security manager.
  }
  TGroupAuthSchema = class(TAuthBasedUsrMgntSchema)
  protected
    FUserList:TUserList;
    FGroupList:TSimpleUserGroupList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function UserList:TUserList;
    function GroupList:TSimpleUserGroupList;
  end;



implementation

uses security.exceptions;

constructor TGroupAuthSchema.Create;
begin
  inherited Create;
  FUserList:=TUserList.Create;
  FGroupList:=TSimpleUserGroupList.Create;
end;

destructor TGroupAuthSchema.Destroy;
begin
  FreeAndNil(FGroupList);
  FreeAndNil(FUserList);
  inherited Destroy;
end;

function TGroupAuthSchema.UserList: TUserList;
begin
  Result:=FUserList;
end;

function TGroupAuthSchema.GroupList: TSimpleUserGroupList;
begin
  Result:=FGroupList;
end;

constructor TUsrGroupAuthSchema.Create;
begin
  inherited Create;
  FGroupList:=TUsrGroupList.Create;
end;

destructor TUsrGroupAuthSchema.Destroy;
begin
  FreeAndNil(FGroupList);
  inherited Destroy;
end;

function TUsrGroupAuthSchema.GroupList: TUsrGroupList;
begin
  Result:=FGroupList;
end;

constructor TUsrAuthSchema.Create;
begin
  inherited Create;
  FUserList:=TAuthorizedUserList.Create;
end;

destructor TUsrAuthSchema.Destroy;
begin
  FreeAndNil(FUserList);
  inherited Destroy;
end;

function TUsrAuthSchema.UserList: TAuthorizedUserList;
begin
  Result:=FUserList;
end;

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

constructor TUsrLevelMgntSchema.Create(aMinLevel, aMaxLevel,
  aAdminLevel: Integer);
begin
  inherited Create;
  FUserList:=TUserLevelList.Create;
  FAdminLevel:=aAdminLevel;
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

