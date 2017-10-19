unit frmmainauth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, security.actions.login, security.controls.SecureButton,
  security.manager.graphical_user_management, security.actions.manage,
  security.manager.custom_user_management, security.manager.schema;

type

  { TFormAuthBased }

  TFormAuthBased = class(TForm)
    ActionList1: TActionList;
    BuLogout: TButton;
    BuGraphLogin: TButton;
    BuManage: TButton;
    GraphicalUsrMgntInterface1: TGraphicalUsrMgntInterface;
    LoginAction1: TLoginAction;
    ManageUsersAndGroupsAction1: TManageUsersAndGroupsAction;
    Memo1: TMemo;
    CustomizedUserManagement1: TUserCustomizedUserManagement;
    SecureButton1: TSecureButton;
    SecureButton2: TSecureButton;
    UserCustomizedUserManagement1: TUserCustomizedUserManagement;
    procedure BuLogoutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CustomizedUserManagement1CanAccess(securityCode: String;
      var CanAccess: Boolean);
    procedure CustomizedUserManagement1CheckUserAndPass(user,
      pass: String; var aUID: Integer; var ValidUser: Boolean;
      LoginAction: Boolean);
    procedure CustomizedUserManagement1GetUserLogin(var UserInfo: String);
    procedure FormDestroy(Sender: TObject);
    procedure SecureButton1Click(Sender: TObject);
    procedure UserCustomizedUserManagement1GetSchemaType(
      var SchemaType: TUsrMgntType);
    procedure UserCustomizedUserManagement1GetUserName(var UserInfo: String);
    procedure UserCustomizedUserManagement1GetUserSchema(
      var Schema: TUsrMgntSchema);
    procedure UserCustomizedUserManagement1Logout(Sender: TObject);
    procedure UserCustomizedUserManagement1ManageUsersAndGroups(Sender: TObject
      );
  private
    LastValidUser:String;
    MySchemaTyp: TUsrMgntType;
    MySchema: TUsrMgntSchema;
  public

  end;

var
  FormAuthBased: TFormAuthBased;

implementation

uses
  security.manager.controls_manager, ubuildschema;

{$R *.lfm}


{ TFormAuthBased }

procedure TFormAuthBased.CustomizedUserManagement1CheckUserAndPass(user,
  pass: String; var aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean);
var
  aUser: TCustomUser;
begin
  //check the user login and password
  Memo1.Append('CustomizedUserManagement1CheckUserAndPass'+' ' + user + ' '+pass);
  ValidUser:= False;
  LastValidUser:= '';
  if MySchema is TUsrAuthSchema then begin
     aUser := TUsrAuthSchema(MySchema).UserByName[user];
     if aUser <> nil then
        ValidUser:= SameStr(aUser.Password,pass);
  end;
  if ValidUser then
    LastValidUser:=user;
end;

procedure TFormAuthBased.CustomizedUserManagement1GetUserLogin(
  var UserInfo: String);
begin
  //
  Memo1.Append('CustomizedUserManagement1GetUserLogin' + ' ' +UserInfo);
end;


procedure TFormAuthBased.FormDestroy(Sender: TObject);
begin
  if Assigned(MySchema) then
    FreeAndNil(MySchema);
end;

procedure TFormAuthBased.SecureButton1Click(Sender: TObject);
begin
  Memo1.Append('SecureButton Clicked');
end;

procedure TFormAuthBased.UserCustomizedUserManagement1GetSchemaType(
  var SchemaType: TUsrMgntType);
begin
  Memo1.Append('UserCustomizedUserManagement1GetSchemaType');
  SchemaType:= MySchemaTyp;
end;

procedure TFormAuthBased.UserCustomizedUserManagement1GetUserName(
  var UserInfo: String);
begin
  Memo1.Append('UserCustomizedUserManagement1GetUserName');
end;

procedure TFormAuthBased.UserCustomizedUserManagement1GetUserSchema(
  var Schema: TUsrMgntSchema);
begin
  // Only used if OnManageUsersANdGroups not set !!
  Memo1.Append('UserCustomizedUserManagement1GetUserSchema');
  Schema:= MySchema;
end;

procedure TFormAuthBased.UserCustomizedUserManagement1Logout(Sender: TObject);
begin
  Memo1.Append('CustomizedUserManagement1Logout');
  LastValidUser:='';
end;

procedure TFormAuthBased.UserCustomizedUserManagement1ManageUsersAndGroups(
  Sender: TObject);
begin
  Memo1.Append('UserCustomizedUserManagement1ManageUsersAndGroups');
  GraphicalUsrMgntInterface1.UserManagement(MySchema);
end;

procedure TFormAuthBased.CustomizedUserManagement1CanAccess(securityCode: String;
  var CanAccess: Boolean);
var
  aUser: TAuthorizedUser;
begin
  Memo1.Append('CustomizedUserManagement1CanAccess'+' '+securityCode);
  CanAccess:= False;
  //check if the current user can access the securityCode
  if MySchema is TUsrAuthSchema then begin
     aUser := TAuthorizedUser(TUsrAuthSchema(MySchema).UserByName[LastValidUser]);
     if aUser <> nil then
       CanAccess:= (aUser.AuthorizationByName[securityCode] <> nil);
  end;
end;

procedure TFormAuthBased.BuLogoutClick(Sender: TObject);
begin
  Memo1.Append('BuLogoutClick');
  GetControlSecurityManager.Logout;
end;

procedure TFormAuthBased.FormCreate(Sender: TObject);
begin
  MySchema:= nil;
  Memo1.Clear;
  Memo1.Append('Create and build Schema');
  //BuildSchemaUser(MySchemaTyp, MySchema);
  BuildSchemaUser(MySchemaTyp, MySchema);
  Memo1.Append('-------------------------');
  Memo1.Append('Login as Username/Password');
  Memo1.Append('   root     3');
  Memo1.Append('   user     2');
  Memo1.Append('   andi     1');
  Memo1.Append('-------------------------');
end;

end.

