unit TestSecurityManagerSchema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestSecurityManagerSchema= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // CRUD TAuthorization(s)
    procedure CR_TAuthorization;
    // CRUD TCustomUser
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

