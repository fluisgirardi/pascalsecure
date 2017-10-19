unit ubuildschema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, security.manager.schema;


function BuildSchemaLVL(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
function BuildSchemaUser(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;


implementation

function BuildSchemaLVL(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
begin
  result := false;
  ASchemaTyp:= TUsrMgntType.umtLevel;
  ASchema:=TUsrLevelMgntSchema.Create(1, 100, 1);
  with ASchema as TUsrLevelMgntSchema do begin
    UserList.Add(0,TUserWithLevelAccess.Create(0,'root','Main administrator',false, 1));
    UserList.Add(1,TUserWithLevelAccess.Create(1,'andi','A user',            false, 1));
    UserList.Add(2,TUserWithLevelAccess.Create(2,'user','Another user',      false, 10));
  end;
end;

function BuildSchemaUser(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
var
  AUser: TAuthorizedUser;
  AAuthorization: TAuthorization;
begin
  result := false;
  ASchemaTyp:= TUsrMgntType.umtAuthorizationByUser;
  ASchema:=TUsrAuthSchema.Create;
  // root
  AUser:= TAuthorizedUser.Create(0,'root','administrator',false);
  AAuthorization:= TAuthorization.Create(0,'autorizacao1');
  AUser.AuthorizationList.Add(0,AAuthorization);
  AAuthorization:= TAuthorization.Create(0,'autorizacao2');
  AUser.AuthorizationList.Add(0,AAuthorization);
  TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
  // andi
  AUser:= TAuthorizedUser.Create(1,'andi','User Andi',false);
  AAuthorization:= TAuthorization.Create(0,'autorizacao1');
  AUser.AuthorizationList.Add(0,AAuthorization);
  AAuthorization:= TAuthorization.Create(0,'autorizacao2');
  AUser.AuthorizationList.Add(0,AAuthorization);
  TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
  // user
  AUser:= TAuthorizedUser.Create(2,'user','Another User',false);
  AAuthorization:= TAuthorization.Create(0,'autorizacao1');
  AUser.AuthorizationList.Add(0,AAuthorization);
  TUsrAuthSchema(ASchema).UserList.Add(0,AUser);
end;




end.

