unit TestSecurityManagerSchema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestSecurityManagerSchema }

  TTestSecurityManagerSchema= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // CRUD TAuthorization(s)
    procedure CR_TAuthorization;
    // CRUD TCustomUser

    // CRUD Schema
    procedure CHK_SchemaType;
    procedure CR_SchemaLevel;
    procedure CR_SchemaUserAuth;

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
begin
  // Test class function
  AssertTrue('class function TUsrMgntSchema.UsrMgntType not correct', (TUsrMgntSchema.UsrMgntType = TUsrMgntType.umtUnknown));
  //AssertTrue('class function TUsrLevelMgntSchema.UsrMgntType not correct', (TUsrLevelMgntSchema.UsrMgntType = TUsrMgntType.umtLevel));
  //AssertTrue('class function TAuthBasedUsrMgntSchema.UsrMgntType not correct', (TAuthBasedUsrMgntSchema.UsrMgntType = TUsrMgntType.umtUnknown));
  //AssertTrue('class function TUsrAuthSchema.UsrMgntType not correct', (TUsrAuthSchema.UsrMgntType = TUsrMgntType.umtAuthorizationByUser));
  //AssertTrue('class function TUsrGroupAuthSchema.UsrMgntType not correct', (TUsrGroupAuthSchema.UsrMgntType = TUsrMgntType.umtAuthorizationByUserAndGroup));
  //AssertTrue('class function TGroupAuthSchema.UsrMgntType not correct', (TGroupAuthSchema.UsrMgntType = TUsrMgntType.umtAuthorizationByGroup));
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
//
//    with ASchema as TUsrLevelMgntSchema do begin
//      UserList.Add(0,TUserWithLevelAccess.Create(0,'root','1','Main administrator',false, 1));
//      UserList.Add(1,TUserWithLevelAccess.Create(1,'andi','2','A user',            false, 1));
//      UserList.Add(2,TUserWithLevelAccess.Create(2,'user','3','Another user',      false, 10));
//    end;
    ASchema.Free;
    AssertTrue('class function UsrMgntType not correct', (ASchema.UsrMgntType = TUsrMgntType.umtUnknown));

  finally
    ASchema.Free;
  end;

end;

procedure TTestSecurityManagerSchema.CR_SchemaUserAuth;
begin

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

