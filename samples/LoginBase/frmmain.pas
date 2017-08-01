unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  security.controls.SecureButton,
  security.manager.custom_user_management;

type

  { TForm1 }

  TForm1 = class(TForm)
    BuLoginAndi: TButton;
    BuLogout: TButton;
    BuLoginB: TButton;
    Memo1: TMemo;
    CustomizedUserManagement1: TUserCustomizedUserManagement;
    SecureButton1: TSecureButton;
    SecureButton2: TSecureButton;
    UserCustomizedUserManagement1: TUserCustomizedUserManagement;
    procedure BuLoginAndiClick(Sender: TObject);
    procedure BuLoginBClick(Sender: TObject);
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
    procedure UserCustomizedUserManagement1Logout(Sender: TObject);
  private
    LastValidUser:String;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  security.manager.controls_manager;
{$R *.lfm}

{ TForm1 }

procedure TForm1.CustomizedUserManagement1CheckUserAndPass(user,
  pass: String; var aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean
  );
begin
  //
  //check the user login and password
  Memo1.Append('CustomizedUserManagement1CheckUserAndPass'+' ' + user + ' '+pass);
  ValidUser:=(((user='andi') and (pass='1')) or ((user='user') and (pass='2')) or ((user='root') and (pass='3')));
  if ValidUser then
    LastValidUser:=user;
end;

procedure TForm1.CustomizedUserManagement1GetUserLogin(
  var UserInfo: String);
begin
  //
  Memo1.Append('CustomizedUserManagement1GetUserLogin' + ' ' +UserInfo);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm1.SecureButton1Click(Sender: TObject);
begin
  Memo1.Append('SecureButton Clicked');
end;

procedure TForm1.UserCustomizedUserManagement1Logout(Sender: TObject);
begin
  Memo1.Append('CustomizedUserManagement1Logout');
  LastValidUser:='';
end;

procedure TForm1.CustomizedUserManagement1CanAccess(securityCode: String;
  var CanAccess: Boolean);
begin
  Memo1.Append('CustomizedUserManagement1CanAccess'+' '+securityCode);
  //check if the current user can access the securityCode
  CanAccess :=((LastValidUser='andi') and (securityCode='autorizacao1')) or
              ((LastValidUser='user') and (securityCode='autorizacao2')) or
              ((LastValidUser='root') and ((securityCode='autorizacao1') or (securityCode='autorizacao2')));

end;

procedure TForm1.BuLoginAndiClick(Sender: TObject);
var
  dummy : Integer;
begin
  Memo1.Append('BuLoginAndiClick');
  GetControlSecurityManager.Logout;
  GetControlSecurityManager.Login('andi','1',dummy);
end;

procedure TForm1.BuLoginBClick(Sender: TObject);
var
  dummy : Integer;
begin
  Memo1.Append('BuLoginBClick');
  GetControlSecurityManager.Logout;
  GetControlSecurityManager.Login('xxx','1',dummy);
end;

procedure TForm1.BuLogoutClick(Sender: TObject);
begin
  Memo1.Append('BuLogoutClick');
  GetControlSecurityManager.Logout;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Append('Create');
end;

end.

