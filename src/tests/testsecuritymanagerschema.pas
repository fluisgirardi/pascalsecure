unit TestSecurityManagerSchema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestSecurityManagerSchema }

  TTestSecurityManagerSchema= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // CRUD TAuthorization(s)
    procedure CR_TAuthorization;
    // CRUD TCustomUser

    // CRUD Schema
    procedure CHK_SchemaType;
    // CRUD Schema levelbased
    procedure CR_SchemaLevel;
    procedure CR_SchemaLevelUser;
    // CRUD user athentication based
    procedure CR_SchemaUserAuth;
    procedure CR_SchemaUserAuthUser;

  end;

implementation
uses
  security.manager.schema; // DUT location


procedure TTestSecurityManagerSchema.CR_TAuthorization;
var
  Dut : TAuthorization;
  ExpInteger : Integer;
  ExpString  : String;
begin
  ExpInteger:= 99;
  ExpString:= 'A short description';
  Dut := TAuthorization.Create(ExpInteger,ExpString);
  try
    AssertEquals('AuthID not equal',ExpInteger,Dut.AuthID);
    AssertEquals('Description not equal',ExpString,Dut.Description);
  finally
    FreeAndNil(Dut);
  end;
end;

procedure TTestSecurityManagerSchema.CHK_SchemaType;
var
  ty1,ty2:TUsrMgntType;
begin
  // Test class function
  ty1:=  TUsrMgntType.umtUnknown;
  ty2:=  TUsrMgntSchema.UsrMgntType;
  AssertTrue('class function TUsrMgntSchema.UsrMgntType not correct', (ty1 = ty2));
  ty1:=  TUsrMgntType.umtLevel;
  ty2:=  TUsrLevelMgntSchema.UsrMgntType;
  AssertTrue('class function TUsrLevelMgntSchema.UsrMgntType not correct', (ty1 = ty2));
  ty1:=  TUsrMgntType.umtUnknown;
  ty2:=  TAuthBasedUsrMgntSchema.UsrMgntType;
  AssertTrue('class function TAuthBasedUsrMgntSchema.UsrMgntType not correct', (ty1 = ty2));
  ty1:=  TUsrMgntType.umtAuthorizationByUser;
  ty2:=  TUsrAuthSchema.UsrMgntType;
  AssertTrue('class function TUsrAuthSchema.UsrMgntType not correct', (ty1 = ty2));
  ty1:=  TUsrMgntType.umtAuthorizationByUserAndGroup;
  ty2:=  TUsrGroupAuthSchema.UsrMgntType;
  AssertTrue('class function TUsrGroupAuthSchema.UsrMgntType not correct', (ty1 = ty2));
  ty1:=  TUsrMgntType.umtAuthorizationByGroup;
  ty2:=  TGroupAuthSchema.UsrMgntType;
  AssertTrue('class function TGroupAuthSchema.UsrMgntType not correct', (ty1 = ty2));
end;

procedure TTestSecurityManagerSchema.CR_SchemaLevel;
var
  ASchema: TUsrMgntSchema;
begin
  // Now create a new one
  ASchema:=TUsrLevelMgntSchema.Create(1, 100, 1);
  try
    AssertTrue('UsrMgntType not correct', (ASchema.UsrMgntType = TUsrMgntType.umtLevel) );
    AssertTrue('TUsrLevelMgntSchema is not correct set', (ASchema is TUsrLevelMgntSchema));
  finally
    ASchema.Free;
  end;
end;

procedure TTestSecurityManagerSchema.CR_SchemaLevelUser;
var
  ASchema: TUsrMgntSchema;
begin
  // Now create a new one
  ASchema:=TUsrLevelMgntSchema.Create(1, 100, 1);
  try
    AssertTrue('TUsrLevelMgntSchema is not correct set', (ASchema is TUsrLevelMgntSchema));
    AssertNotNull('userlist is not valid',TUsrLevelMgntSchema(ASchema).UserList);
    AssertEquals('userlist is not empty',TUsrLevelMgntSchema(ASchema).UserList.Count,0);
    // Add some user
    TUsrLevelMgntSchema(ASchema).UserList.Add(0,TUserWithLevelAccess.Create(0,'root','1','Main administrator',false, 1));
    TUsrLevelMgntSchema(ASchema).UserList.Add(1,TUserWithLevelAccess.Create(1,'andi','2','A user',            false, 1));
    TUsrLevelMgntSchema(ASchema).UserList.Add(2,TUserWithLevelAccess.Create(2,'user','3','Another user',      false, 10));
    AssertEquals('uselist Count should be 3',TUsrLevelMgntSchema(ASchema).UserList.Count,3);
    // Check the userdata
    // user 1
    AssertEquals('User 0 Modified is not false',TUsrLevelMgntSchema(ASchema).UserList.Data[0].Modified,false);
    AssertEquals('User 0 Login is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[0].Login,'root');
    AssertEquals('User 0 UID is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[0].UID,0);
    AssertEquals('User 0 UserLeval is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[0].UserLevel,1);
    AssertEquals('User 0 Password is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[0].Password,'1');
    // user 2
    AssertEquals('User 1 Modified is not false',TUsrLevelMgntSchema(ASchema).UserList.Data[1].Modified,false);
    AssertEquals('User 1 Login is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[1].Login,'andi');
    AssertEquals('User 1 UID is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[1].UID,1);
    AssertEquals('User 1 UserLeval is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[0].UserLevel,1);
    AssertEquals('User 1 Password is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[1].Password,'2');
    // user 3
    AssertEquals('User 2 Modified is not false',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Modified,false);
    AssertEquals('User 2 Login is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Login,'user');
    AssertEquals('User 2 UID is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].UID,2);
    AssertEquals('User 2 UserLeval is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].UserLevel,10);
    AssertEquals('User 2 Password is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Password,'3');
    // Check changes
    TUsrLevelMgntSchema(ASchema).UserList.Data[2].Password := 'xAxA';
    AssertEquals('User 2 Modified is not true',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Modified,true);
    AssertEquals('User 2 Login is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Login,'user');
    AssertEquals('User 2 UID is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].UID,2);
    AssertEquals('User 2 UserLeval is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].UserLevel,10);
    AssertEquals('User 2 Password is not the same',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Password,'xAxA');
    // Reset the modified flag
    TUsrLevelMgntSchema(ASchema).UserList.Data[2].ResetModified;
    AssertEquals('User 2 Modified is not false after reset',TUsrLevelMgntSchema(ASchema).UserList.Data[2].Modified,false);
    // Clear Userlist
    TUsrLevelMgntSchema(ASchema).UserList.Clear;
    AssertEquals('userlist is not empty after clear',TUsrLevelMgntSchema(ASchema).UserList.Count,0);
  finally
    ASchema.Free;
  end;
end;

procedure TTestSecurityManagerSchema.CR_SchemaUserAuth;
var
  ASchema: TUsrMgntSchema;
begin
  // Now create a new one
  ASchema:=TUsrAuthSchema.Create;
  try
    AssertTrue('UsrMgntType not correct', (ASchema.UsrMgntType = TUsrMgntType.umtAuthorizationByUser) );
    AssertTrue('TUsrLevelMgntSchema is not correct set', (ASchema is TUsrAuthSchema));
  finally
    ASchema.Free;
  end;
end;

procedure TTestSecurityManagerSchema.CR_SchemaUserAuthUser;
var
  ASchema: TUsrMgntSchema;
  AUser: TAuthorizedUser;
  AAuthorization: TAuthorization;
begin
  // Now create a new one
  ASchema:=TUsrAuthSchema.Create;
  try
    AssertTrue('TUsrAuthSchema is not correct set', (ASchema is TUsrAuthSchema));
    AssertNotNull('userlist is not valid',TUsrAuthSchema(ASchema).UserList);
    AssertEquals('userlist is not empty',TUsrAuthSchema(ASchema).UserList.Count,0);
    // Add some user
    AUser:= TAuthorizedUser.Create(0,'root','secret1','administrator',false);
    AAuthorization:= TAuthorization.Create(0,'autorizacao1');
    AUser.AuthorizationList.Add(0,AAuthorization);
    AAuthorization:= TAuthorization.Create(0,'autorizacao2');
    AUser.AuthorizationList.Add(0,AAuthorization);
    TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
    // andi
    AUser:= TAuthorizedUser.Create(1,'andi','2sec','User Andi',false);
    AAuthorization:= TAuthorization.Create(0,'autorizacao1');
    AUser.AuthorizationList.Add(0,AAuthorization);
    AAuthorization:= TAuthorization.Create(0,'autorizacao2');
    AUser.AuthorizationList.Add(0,AAuthorization);
    TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
    // user
    AUser:= TAuthorizedUser.Create(2,'user','a0a0','Another User',false);
    AAuthorization:= TAuthorization.Create(0,'autorizacao1');
    AUser.AuthorizationList.Add(0,AAuthorization);
    TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
    // Check the userdata
    AssertEquals('userlist count is not correct',TUsrAuthSchema(ASchema).UserList.Count,3);
    // user 1
    AssertEquals('User 0 Modified is not false',TUsrAuthSchema(ASchema).UserList.Data[0].Modified,false);
    AssertEquals('User 0 Login is not the same',TUsrAuthSchema(ASchema).UserList.Data[0].Login,'root');
    AssertEquals('User 0 UID is not the same',TUsrAuthSchema(ASchema).UserList.Data[0].UID,0);
    AssertEquals('User 0 Password is not the same',TUsrAuthSchema(ASchema).UserList.Data[0].Password,'secret1');
    // - check authorization
    { TODO : check authorization missing}
    // user 2
    AssertEquals('User 1 Modified is not false',TUsrAuthSchema(ASchema).UserList.Data[1].Modified,false);
    AssertEquals('User 1 Login is not the same',TUsrAuthSchema(ASchema).UserList.Data[1].Login,'andi');
    AssertEquals('User 1 UID is not the same',TUsrAuthSchema(ASchema).UserList.Data[1].UID,1);
    AssertEquals('User 1 Password is not the same',TUsrAuthSchema(ASchema).UserList.Data[1].Password,'2sec');
    // - check authorization
    { TODO : check authorization missing}
    // user 3
    AssertEquals('User 2 Modified is not false',TUsrAuthSchema(ASchema).UserList.Data[2].Modified,false);
    AssertEquals('User 2 Login is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].Login,'user');
    AssertEquals('User 2 UID is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].UID,2);
    AssertEquals('User 2 Password is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].Password,'a0a0');
    // - check authorization
    { TODO : check authorization missing}
    // Check changes
    TUsrAuthSchema(ASchema).UserList.Data[2].Password := 'xAxA';
    AssertEquals('User 2 Modified is not true',TUsrAuthSchema(ASchema).UserList.Data[2].Modified,true);
    AssertEquals('User 2 Login is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].Login,'user');
    AssertEquals('User 2 UID is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].UID,2);
    AssertEquals('User 2 Password is not the same',TUsrAuthSchema(ASchema).UserList.Data[2].Password,'xAxA');
    // Reset the modified flag
    TUsrAuthSchema(ASchema).UserList.Data[2].ResetModified;
    AssertEquals('User 2 Modified is not false after reset',TUsrAuthSchema(ASchema).UserList.Data[2].Modified,false);
    // Clear Userlist
    TUsrAuthSchema(ASchema).UserList.Clear;
    AssertEquals('userlist is not empty after clear',TUsrAuthSchema(ASchema).UserList.Count,0);
  finally
    ASchema.Free;
  end;
end;


procedure TTestSecurityManagerSchema.SetUp;
begin
  //
end;

procedure TTestSecurityManagerSchema.TearDown;
begin
  //
end;

initialization

  RegisterTest(TTestSecurityManagerSchema);
end.

