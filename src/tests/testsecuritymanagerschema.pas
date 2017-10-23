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
    with TUsrAuthSchema(ASchema) do begin
      Autorizations.Add(0, TAuthorization.Create(0,'autorizacao1'));
      Autorizations.Add(1, TAuthorization.Create(1,'autorizacao2'));
      Autorizations.Add(2, TAuthorization.Create(2,'autorizacao3'));
      Autorizations.Add(3, TAuthorization.Create(3,'autorizacao4'));
    end;


    AssertTrue('TUsrAuthSchema is not correct set', (ASchema is TUsrAuthSchema));
    AssertNotNull('userlist is not valid',TUsrAuthSchema(ASchema).UserList);
    AssertEquals('userlist is not empty',TUsrAuthSchema(ASchema).UserList.Count,0);
    // Add some user
    AUser:= TAuthorizedUser.Create(0,'root','secret1','administrator',false);
    AUser.AuthorizationList.Add(0, TUsrAuthSchema(ASchema).Autorizations.KeyData[0]);
    AUser.AuthorizationList.Add(1, TUsrAuthSchema(ASchema).Autorizations.KeyData[1]);
    AUser.AuthorizationList.Add(2, TUsrAuthSchema(ASchema).Autorizations.KeyData[2]);
    AUser.AuthorizationList.Add(3, TUsrAuthSchema(ASchema).Autorizations.KeyData[3]);
    TUsrAuthSchema(ASchema).UserList.Add(0,AUser);

    // andi
    AUser:= TAuthorizedUser.Create(10,'andi','2sec','User Andi',false);
    AUser.AuthorizationList.Add(2, TUsrAuthSchema(ASchema).Autorizations.KeyData[2]);
    AUser.AuthorizationList.Add(0, TUsrAuthSchema(ASchema).Autorizations.KeyData[0]);
    TUsrAuthSchema(ASchema).UserList.Add(10,AUser);

    // user
    AUser:= TAuthorizedUser.Create(5,'user','a0a0','Another User',false);
    AUser.AuthorizationList.Add(3, TUsrAuthSchema(ASchema).Autorizations.KeyData[3]);
    TUsrAuthSchema(ASchema).UserList.Add(5,AUser);

    // Check the userdata
    AssertEquals('userlist count is not correct',TUsrAuthSchema(ASchema).UserList.Count,3);
    // user 1
    AssertEquals('User at index 0 Modified is not false',       TUsrAuthSchema(ASchema).User[0].Modified,false);
    AssertEquals('User at index 0 Login is not the same',       TUsrAuthSchema(ASchema).User[0].Login,'root');
    AssertEquals('User at index 0 description is not the same', TUsrAuthSchema(ASchema).User[0].UserDescription,'administrator');
    AssertEquals('User at index 0 UID is not the same',         TUsrAuthSchema(ASchema).User[0].UID,0);
    AssertEquals('User at index 0 Password is not the same',    TUsrAuthSchema(ASchema).User[0].Password,'secret1');

    AssertEquals('UID 0 Modified is not false',                 TUsrAuthSchema(ASchema).UserByUID[0].Modified,false);
    AssertEquals('UID 0 Login is not the same',                 TUsrAuthSchema(ASchema).UserByUID[0].Login,'root');
    AssertEquals('UID 0 description is not the same',           TUsrAuthSchema(ASchema).UserByUID[0].UserDescription,'administrator');
    AssertEquals('UID 0 UID is not the same',                   TUsrAuthSchema(ASchema).UserByUID[0].UID,0);
    AssertEquals('UID 0 Password is not the same',              TUsrAuthSchema(ASchema).UserByUID[0].Password,'secret1');

    AssertEquals('User "root" Modified is not false',           TUsrAuthSchema(ASchema).UserByName['root'].Modified,false);
    AssertEquals('User "root" Login is not the same',           TUsrAuthSchema(ASchema).UserByName['root'].Login,'root');
    AssertEquals('User "root" description is not the same',     TUsrAuthSchema(ASchema).UserByName['root'].UserDescription,'administrator');
    AssertEquals('User "root" UID is not the same',             TUsrAuthSchema(ASchema).UserByName['root'].UID,0);
    AssertEquals('User "root" Password is not the same',        TUsrAuthSchema(ASchema).UserByName['root'].Password,'secret1');

    // - check authorization
    AssertEquals ('User at index 0 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationCount, 4);
    AssertNotNull('User at index 0 cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByID[0]);
    AssertNotNull('User at index 0 cannot access AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByID[1]);
    AssertNotNull('User at index 0 cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByID[2]);
    AssertNotNull('User at index 0 cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByID[3]);

    AssertNotNull('User at index 0 cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByName['autorizacao1']);
    AssertNotNull('User at index 0 cannot access "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByName['autorizacao2']);
    AssertNotNull('User at index 0 cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByName['autorizacao3']);
    AssertNotNull('User at index 0 cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[0]).AuthorizationByName['autorizacao4']);

    AssertEquals ('UID 0 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationCount, 4);
    AssertNotNull('UID 0 cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByID[0]);
    AssertNotNull('UID 0 cannot access AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByID[1]);
    AssertNotNull('UID 0 cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByID[2]);
    AssertNotNull('UID 0 cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByID[3]);

    AssertNotNull('UID 0 cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByName['autorizacao1']);
    AssertNotNull('UID 0 cannot access "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByName['autorizacao2']);
    AssertNotNull('UID 0 cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByName['autorizacao3']);
    AssertNotNull('UID 0 cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[0]).AuthorizationByName['autorizacao4']);

    AssertEquals ('User "root" auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationCount, 4);
    AssertNotNull('User "root" cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByID[0]);
    AssertNotNull('User "root" cannot access AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByID[1]);
    AssertNotNull('User "root" cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByID[2]);
    AssertNotNull('User "root" cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByID[3]);

    AssertNotNull('User "root" cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByName['autorizacao1']);
    AssertNotNull('User "root" cannot access "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByName['autorizacao2']);
    AssertNotNull('User "root" cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByName['autorizacao3']);
    AssertNotNull('User "root" cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['root']).AuthorizationByName['autorizacao4']);

    // user 2
    //index should be 2, because it's orderred by UID, and user "user" has a UID 5 and user "andi" has a UID 10
    AssertEquals('User at index 2 Modified is not false',       TUsrAuthSchema(ASchema).User[2].Modified,false);
    AssertEquals('User at index 2 Login is not the same',       TUsrAuthSchema(ASchema).User[2].Login,'andi');
    AssertEquals('User at index 2 description is not the same', TUsrAuthSchema(ASchema).User[2].UserDescription,'User Andi');
    AssertEquals('User at index 2 UID is not the same',         TUsrAuthSchema(ASchema).User[2].UID,10);
    AssertEquals('User at index 2 Password is not the same',    TUsrAuthSchema(ASchema).User[2].Password,'2sec');

    AssertEquals('UID 10 Modified is not false',                 TUsrAuthSchema(ASchema).UserByUID[10].Modified,false);
    AssertEquals('UID 10 Login is not the same',                 TUsrAuthSchema(ASchema).UserByUID[10].Login,'andi');
    AssertEquals('UID 10 description is not the same',           TUsrAuthSchema(ASchema).UserByUID[10].UserDescription,'User Andi');
    AssertEquals('UID 10 UID is not the same',                   TUsrAuthSchema(ASchema).UserByUID[10].UID,10);
    AssertEquals('UID 10 Password is not the same',              TUsrAuthSchema(ASchema).UserByUID[10].Password,'2sec');

    AssertEquals('User "andi" Modified is not false',           TUsrAuthSchema(ASchema).UserByName['andi'].Modified,false);
    AssertEquals('User "andi" Login is not the same',           TUsrAuthSchema(ASchema).UserByName['andi'].Login,'andi');
    AssertEquals('User "andi" description is not the same',     TUsrAuthSchema(ASchema).UserByName['andi'].UserDescription,'User Andi');
    AssertEquals('User "andi" UID is not the same',             TUsrAuthSchema(ASchema).UserByName['andi'].UID,10);
    AssertEquals('User "andi" Password is not the same',        TUsrAuthSchema(ASchema).UserByName['andi'].Password,'2sec');

    // - check authorization
    AssertEquals ('User at index 2 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationCount, 2);
    AssertNotNull('User at index 2 cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByID[0]);
    AssertNull   ('User at index 2 has access AuthID 1',          TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByID[1]);
    AssertNotNull('User at index 2 cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByID[2]);
    AssertNull   ('User at index 2 has access AuthID 3',          TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByID[3]);

    AssertNotNull('User at index 2 cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByName['autorizacao1']);
    AssertNull   ('User at index 2 has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByName['autorizacao2']);
    AssertNotNull('User at index 2 cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByName['autorizacao3']);
    AssertNull   ('User at index 2 has access to "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[2]).AuthorizationByName['autorizacao4']);

    AssertEquals ('UID 10 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationCount, 2);
    AssertNotNull('UID 10 cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByID[0]);
    AssertNull   ('UID 10 has access to AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByID[1]);
    AssertNotNull('UID 10 cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByID[2]);
    AssertNull   ('UID 10 has access to AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByID[3]);

    AssertNotNull('UID 10 cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByName['autorizacao1']);
    AssertNull   ('UID 10 has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByName['autorizacao2']);
    AssertNotNull('UID 10 cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByName['autorizacao3']);
    AssertNull   ('UID 10 has access to "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[10]).AuthorizationByName['autorizacao4']);

    AssertEquals ('User "andi" auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationCount, 2);
    AssertNotNull('User "andi" cannot access AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByID[0]);
    AssertNull   ('User "andi" has access to AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByID[1]);
    AssertNotNull('User "andi" cannot access AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByID[2]);
    AssertNull   ('User "andi" has access to AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByID[3]);

    AssertNotNull('User "andi" cannot access "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByName['autorizacao1']);
    AssertNull   ('User "andi" has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByName['autorizacao2']);
    AssertNotNull('User "andi" cannot access "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByName['autorizacao3']);
    AssertNull   ('User "andi" has access to "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['andi']).AuthorizationByName['autorizacao4']);


    // user 3
    //index should be 1, because it's orderred by UID, and user "user" has a UID 5 and user "andi" has a UID 10
    AssertEquals('User at index 1 Modified is not false',       TUsrAuthSchema(ASchema).User[1].Modified,false);
    AssertEquals('User at index 1 Login is not the same',       TUsrAuthSchema(ASchema).User[1].Login,'user');
    AssertEquals('User at index 1 description is not the same', TUsrAuthSchema(ASchema).User[1].UserDescription,'Another User');
    AssertEquals('User at index 1 UID is not the same',         TUsrAuthSchema(ASchema).User[1].UID, 5);
    AssertEquals('User at index 1 Password is not the same',    TUsrAuthSchema(ASchema).User[1].Password,'a0a0');

    AssertEquals('UID 10 Modified is not false',                 TUsrAuthSchema(ASchema).UserByUID[5].Modified,false);
    AssertEquals('UID 10 Login is not the same',                 TUsrAuthSchema(ASchema).UserByUID[5].Login,'user');
    AssertEquals('UID 10 description is not the same',           TUsrAuthSchema(ASchema).UserByUID[5].UserDescription,'Another User');
    AssertEquals('UID 10 UID is not the same',                   TUsrAuthSchema(ASchema).UserByUID[5].UID,5);
    AssertEquals('UID 10 Password is not the same',              TUsrAuthSchema(ASchema).UserByUID[5].Password,'a0a0');

    AssertEquals('User "user" Modified is not false',           TUsrAuthSchema(ASchema).UserByName['user'].Modified,false);
    AssertEquals('User "user" Login is not the same',           TUsrAuthSchema(ASchema).UserByName['user'].Login,'user');
    AssertEquals('User "user" description is not the same',     TUsrAuthSchema(ASchema).UserByName['user'].UserDescription,'Another User');
    AssertEquals('User "user" UID is not the same',             TUsrAuthSchema(ASchema).UserByName['user'].UID, 5);
    AssertEquals('User "user" Password is not the same',        TUsrAuthSchema(ASchema).UserByName['user'].Password,'a0a0');

    // - check authorization
    AssertEquals ('User at index 1 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationCount, 1);
    AssertNull   ('User at index 1 has access to AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByID[0]);
    AssertNull   ('User at index 1 has access to AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByID[1]);
    AssertNull   ('User at index 1 has access to AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByID[2]);
    AssertNotNull('User at index 1 cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByID[3]);

    AssertNull   ('User at index 1 has access to "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByName['autorizacao1']);
    AssertNull   ('User at index 1 has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByName['autorizacao2']);
    AssertNull   ('User at index 1 has access to "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByName['autorizacao3']);
    AssertNotNull('User at index 1 cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).User[1]).AuthorizationByName['autorizacao4']);

    AssertEquals ('UID 5 auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationCount, 1);
    AssertNull   ('UID 5 has access to AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByID[0]);
    AssertNull   ('UID 5 has access to AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByID[1]);
    AssertNull   ('UID 5 has access to AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByID[2]);
    AssertNotNull('UID 5 cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByID[3]);

    AssertNull   ('UID 5 has access to "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByName['autorizacao1']);
    AssertNull   ('UID 5 has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByName['autorizacao2']);
    AssertNull   ('UID 5 has access to "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByName['autorizacao3']);
    AssertNotNull('UID 5 cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByUID[5]).AuthorizationByName['autorizacao4']);

    AssertEquals ('User "user" auth count is not correct',    TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationCount, 1);
    AssertNull   ('User "user" has access to AuthID 0',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByID[0]);
    AssertNull   ('User "user" has access to AuthID 1',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByID[1]);
    AssertNull   ('User "user" has access to AuthID 2',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByID[2]);
    AssertNotNull('User "user" cannot access AuthID 3',       TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByID[3]);

    AssertNull   ('User "user" has access to "autorizacao1"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByName['autorizacao1']);
    AssertNull   ('User "user" has access to "autorizacao2"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByName['autorizacao2']);
    AssertNull   ('User "user" has access to "autorizacao3"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByName['autorizacao3']);
    AssertNotNull('User "user" cannot access "autorizacao4"', TAuthorizedUser(TUsrAuthSchema(ASchema).UserByName['user']).AuthorizationByName['autorizacao4']);


    // Check changes
    TUsrAuthSchema(ASchema).UserList.Data[1].Password := 'xAxA';
    AssertEquals('UserList.Data[1] Modified is not true',    TUsrAuthSchema(ASchema).UserList.Data[1].Modified,true);
    AssertEquals('UserList.Data[1] Login is not the same',   TUsrAuthSchema(ASchema).UserList.Data[1].Login,'user');
    AssertEquals('UserList.Data[1] UID is not the same',     TUsrAuthSchema(ASchema).UserList.Data[1].UID,5);
    AssertEquals('UserList.Data[1] Password is not the same',TUsrAuthSchema(ASchema).UserList.Data[1].Password,'xAxA');
    // Reset the modified flag
    TUsrAuthSchema(ASchema).UserList.Data[1].ResetModified;
    AssertEquals('UserList.Data[1] Modified is not false after reset',TUsrAuthSchema(ASchema).UserList.Data[1].Modified,false);
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

